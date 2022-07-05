## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  
#  # Create temporary path
#  tmp <- file.path(tempdir(), "example5.pdf")
#  
#  # Prepare Data
#  dat <- mtcars[1:10, ]
#  df <- data.frame(vehicle = rownames(dat), dat)
#  
#  # Define Table with spanning headers
#  tbl <- create_table(df) %>%
#    titles("Table 1.0", "MTCARS Spanning Headers") %>%
#    spanning_header(from = "mpg", to = "hp", label = "Span 1", n = 10) %>%
#    spanning_header(from = "drat", to = "qsec", label = "Span 2", n = 10) %>%
#    spanning_header(from = "vs", to = "carb", label = "Span 3", n = 10) %>%
#    spanning_header(from = "drat", to = "carb",
#                    label = "Super Span", level = 2) %>%
#    define(vehicle, label = "Vehicle") %>%
#    define(mpg, format = "%.1f") %>%
#    define(disp, visible = FALSE) %>%
#    define(am, visible = FALSE) %>%
#    footnotes("* Motor Trend, 1974")
#  
#  # Create Report and add table
#  rpt <- create_report(tmp, output_type = "PDF",
#                       font = "Courier", font_size = 12) %>%
#    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
#    add_content(tbl) %>%
#    page_footer(left = Sys.time(),
#              center = "Confidential",
#              right = "Page [pg] of [tpg]")
#  
#  # Write the report
#  res <- write_report(rpt)
#  
#  # file.show(tmp)
#  

