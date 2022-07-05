## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  
#  # Create temporary path
#  tmp <- file.path(tempdir(), "example4.pdf")
#  
#  # Dummy text
#  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
#                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
#                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
#                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ",
#                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
#                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
#                "qui officia deserunt mollit anim id est laborum.")
#  
#  # Create text content
#  txt <- create_text(cnt, width = 6)
#  
#  # Prepare data
#  dat <- mtcars[1:10, ]
#  
#  # Create table content
#  tbl <- create_table(dat) %>%
#    titles("Table 1.0", "MTCARS Sample Data") %>%
#    footnotes("* Motor Trend, 1974")
#  
#  # Create report and add both table and text content
#  rpt <- create_report(tmp, font_size = 12,
#                       output_type = "PDF") %>%
#    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
#    add_content(tbl, page_break = FALSE) %>%
#    add_content(txt) %>%
#    page_footer(left = Sys.time(),
#                center = "Confidential",
#                right = "Page [pg] of [tpg]")
#  
#  # Write the report
#  write_report(rpt)
#  
#  # file.show(tmp)
#  

