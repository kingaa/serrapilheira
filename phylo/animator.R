animator <- function (
  plots,
  gif_file = "animation.gif",
  mp4 = FALSE,
  webm = FALSE,
  framerate = 10, # frames per second
  progress = FALSE
) {

  gif_file <- as.character(gif_file)[1L]
  mp4 <- as.logical(mp4)
  progress <- as.logical(progress)
  framerate <- as.integer(framerate)
  delay <- round(1/framerate,2)

  require(gifski,quietly=TRUE)
  nframes <- length(plots)
  if (nframes > 1e5)
    stop("maximum of 10^5 frames allowed!",call.=FALSE)

  png_files <- sprintf(
    file.path(tempdir(),"frame%05d.png"),
    seq_len(nframes)
  )

  if (progress) 
    pb <- utils::txtProgressBar(0,length(times),0,style=3)

  for (i in seq_len(nframes)) {
    ggsave(
      plot=pl[[i]],
      filename=png_files[i],
      device="png",dpi=100,
      height=300,width=500,units="px"
    )
    if (progress) setTxtProgressBar(pb,i)
  }

  gifski(png_files,gif_file,delay=delay,loop=TRUE)
  unlink(png_files)

  if (mp4 || webm) {
    ffmpeg <- Sys.which("ffmpeg")
    if (nchar(ffmpeg)==0)
      stop("'ffmpeg' not found: no mp4 produced",call.=FALSE)
  }

  if (mp4) {
    system2(
      ffmpeg,
      args=c("-y","-r",framerate,"-i",gif_file,sub("gif","mp4",gif_file))
    )
  }

  if (webm) {
    system2(
      ffmpeg,
      args=c("-y","-r",framerate,"-i",gif_file,sub("gif","webm",gif_file))
    )
  }

  invisible(NULL)
}
