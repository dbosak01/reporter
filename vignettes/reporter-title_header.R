## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  
#  # Create temp file name
#  tmp <- file.path(tempdir(), "example8.rtf")
#  
#  tbl <- create_table(mtcars[1:10, ], width = 7) %>%
#         title_header("Listing 4.0", "MTCARS Data Listing with Title Header",
#                 right = c("Page [pg] of [tpg]",  "Study: Cars"),
#                 blank_row = "both")
#  
#  # Create the report
#  rpt <- create_report(tmp, output_type = "RTF",
#                       font = "Courier", font_size = 12) %>%
#    page_header(left = "Client: Motor Trend", right = "Study: Cars",
#                blank_row = "below") %>%
#    add_content(tbl) %>%
#    set_margins(top = 1, bottom = 1) %>%
#    footnotes("* Motor Trend, 1974") %>%
#    page_footer(left = Sys.time(),
#          center = "Confidential",
#          right = "Page [pg] of [tpg]")
#  
#  # Write the report
#  write_report(rpt)
#  
#  # file.show(tmp)

