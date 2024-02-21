default: pomptalk.html

Bilbao.pdf: Bilbao.html
	R -s -e 'pagedown::chrome_print("$^",options=list(paperHeight=7.3125,paperWidth=13))'

.INTERMEDIATE:

include ../rules.mk
