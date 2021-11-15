## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  library(magrittr)
#  
#  # Create temp file name
#  tmp <- file.path(tempdir(), "example.txt")
#  
#  # Create report content
#  tbl <- create_table(mtcars) %>%
#    titles("MTCARS Sample Data") %>%
#    footnotes("* Motor Trend, 1974")
#  
#  # Create report and add content
#  rpt <- create_report(tmp) %>%
#    add_content(tbl)
#  
#  # Write out the report
#  write_report(rpt)
#  # # A report specification: 1 pages
#  # - file_path: 'C:\Users\User\AppData\Local\Temp\RtmpeC1s5u/example.txt'
#  # - output_type: TXT
#  # - units: inches
#  # - orientation: landscape
#  # - margins: top 0.5 bottom 0.5 left 1 right 1
#  # - line size/count: 108/45
#  # - content:
#  # # A table specification:
#  # - data: data.frame 'mtcars[1:10, ]' 10 rows 11 cols
#  # - show_cols: all
#  # - use_attributes: all
#  # - title 1: 'MTCARS Sample Data'
#  # - footnote 1: '* Motor Trend, 1974'
#  
#  writeLines(readLines(tmp, encoding = "UTF-8"))
#  #                        MTCARS Sample Data
#  #
#  #          mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#  #         ---------------------------------------------------
#  #           21   6   160 110  3.9  2.62 16.46  0  1    4    4
#  #           21   6   160 110  3.9 2.875 17.02  0  1    4    4
#  #         22.8   4   108  93 3.85  2.32 18.61  1  1    4    1
#  #         21.4   6   258 110 3.08 3.215 19.44  1  0    3    1
#  #         18.7   8   360 175 3.15  3.44 17.02  0  0    3    2
#  #         18.1   6   225 105 2.76  3.46 20.22  1  0    3    1
#  #         14.3   8   360 245 3.21  3.57 15.84  0  0    3    4
#  #         24.4   4 146.7  62 3.69  3.19    20  1  0    4    2
#  #         22.8   4 140.8  95 3.92  3.15  22.9  1  0    4    2
#  #         19.2   6 167.6 123 3.92  3.44  18.3  1  0    4    4
#  #
#  #         * Motor Trend, 1974

