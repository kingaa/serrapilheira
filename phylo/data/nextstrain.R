library(tidyverse)
library(stringi)
library(phylopomp)

readLines("nextstrain_ncov_gisaid_north-america_all-time_timetree.nwk") |>
  stri_replace_all_fixed("()","b_0_NA") |>
  stri_replace_all_regex(r"{hCoV-19/(.+?)/(.+?):}","b_0_$1:") |>
  stri_replace_all_fixed(")",")g_0_NA") |>
  parse_newick() |>
  plot()
