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
#  tmp <- file.path(tempdir(), "example.pdf")
#  
#  # Create report content
#  tbl <- create_table(mtcars) %>%
#    titles("MTCARS Sample Data") %>%
#    footnotes("* Motor Trend, 1974")
#  
#  # Create report and add content
#  rpt <- create_report(tmp, output_type = "PDF",
#                       font = "Courier", font_size = 12) %>%
#    add_content(tbl)
#  
#  # Write out the report
#  write_report(rpt)
#  
#  # Un-comment to view report
#  # file.show(tmp)

