## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  library(magrittr)
#  
#  # Create temporary path
#  tmp <- file.path(tempdir(), "example13.rtf")
#  
#  # Read in prepared data
#  df <- read.table(header = TRUE, text = '
#        var     label        A             B
#        "ampg"   "N"          "19"          "13"
#        "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
#        "ampg"   "Median"     "16.4"        "21.4"
#        "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
#        "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
#        "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
#        "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)"
#        "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
#  
#  # Create table
#  tbl <- create_table(df, first_row_blank = TRUE) %>%
#    stub(c("var", "label")) %>%
#    define(var, blank_after = TRUE, label_row = TRUE,
#           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>%
#    define(label, indent = .25) %>%
#    define(A, label = "Group A", align = "center", n = 19) %>%
#    define(B, label = "Group B", align = "center", n = 13) %>%
#    titles("Table 1.0", "MTCARS Summary Table\U00B9", # Superscript in title
#           borders = c("top", "bottom"), blank_row = "none") %>%
#    footnotes("\U00B9 Motor Trend, 1974", # Superscript in footnote
#              valign = "top", blank_row = "none",
#              borders = c("top", "bottom"))
#  
#  
#  # Create report and add content
#  rpt <- create_report(tmp, output_type = "RTF", font = "Arial") %>%
#    set_margins(top = 1, bottom = 1) %>%
#    options_fixed(font_size = 12) %>%
#    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
#    add_content(tbl) %>%
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

