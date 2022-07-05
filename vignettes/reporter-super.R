## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  library(magrittr)
#  
#  # Create temp file path
#  tmp <- file.path(tempdir(), "example13.rtf")
#  
#  # Prepare Data
#  dat <- airquality[sample(1:153, 15), ]
#  dat$Month <-  as.Date(paste0("1973-", dat$Month, "-01"))
#  
#  # Define table
#  tbl <- create_table(dat, show_cols = c("Month", "Day", "Wind", "Temp", "Ozone")) %>%
#    titles("Table 9.6", "Air Quality Sample Report\U00B9",
#           borders = c("top", "bottom"), blank_row = "none") %>%
#    column_defaults(width = .7) %>%
#    define(Month, format = "%B", align = "left", width = 1) %>%
#    define(Temp, format = "%.0f") %>%
#    footnotes("\U00B9 New York, May to September 1973",
#              borders = c("top", "bottom"), blank_row = "none")
#  
#  # Define report
#  rpt <- create_report(tmp, output_type = "RTF", font = "Arial",
#                       font_size = 12, missing = "-") %>%
#    page_header("Sponsor: EPA", "Study: B34958", blank_row = "below") %>%
#    add_content(tbl) %>%
#    page_footer(Sys.Date(), right = "Page [pg] of [tpg]")
#  
#  # Write the report to the file system
#  write_report(rpt)
#  
#  # View report
#  # file.show(tmp)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  # Append superscript using paste0()
#  df[2, "A"] <- paste0(df[2, "A"], "\U00B2")
#  
#  # View data frame
#  df
#  #    var      label           A           B
#  # 1 ampg          N          19          13
#  # 2 ampg       Mean 18.8 (6.5)²  22.0 (4.9)  # Observe superscript in column A
#  # 3 ampg     Median        16.4        21.4
#  # 4 ampg    Q1 - Q3 15.1 - 21.2 19.2 - 22.8
#  # 5 ampg      Range 10.4 - 33.9 14.7 - 32.4
#  # 6  cyl 8 Cylinder 10 ( 52.6%)  4 ( 30.8%)
#  # 7  cyl 6 Cylinder  4 ( 21.1%)  3 ( 23.1%)
#  # 8  cyl 4 Cylinder  5 ( 26.3%)  6 ( 46.2%)
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  
#  # Create complex unicode expression
#  ex1 <- "x\U207D\U207F\U207A\U2074\U207E"
#  
#  # View expression
#  ex1
#  # [1] "x⁽ⁿ⁺⁴⁾"
#  

