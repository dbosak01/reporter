## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  
#  # Create temporary path
#  tmp <- file.path(tempdir(), "example9.docx")
#  
#  # Read in prepared data
#  df <- iris[1:25, ]
#  
#  # Create table
#  tbl <- create_table(df, borders = "bottom")
#  
#  
#  # Create report and add content
#  rpt <- create_report(tmp, output_type = "DOCX", orientation = "portrait",
#                       font = "Times", font_size = 12) %>%
#    set_margins(top = 1, bottom = 1) %>%
#    page_header(left = "Client: 1-800-FLOWERS", right = "Study: Iris") %>%
#    titles("Listing 1.0", "Iris Data Listing") %>%
#    add_content(tbl) %>%
#    footnotes("* Bulletin of the American Iris Society, 1935") %>%
#    page_footer(left = Sys.time(),
#                center = "Confidential",
#                right = "Page [pg] of [tpg]")
#  
#  # Write out report
#  write_report(rpt)
#  
#  # View report
#  # file.show(tmp)
#  

