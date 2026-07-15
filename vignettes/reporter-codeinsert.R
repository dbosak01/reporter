## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  df <- read.table(header = TRUE, text = '
#        var     label        A             B
#        "ampg"   "\\li360 N"          "19"          "13"
#        "ampg"   "\\li360 Mean"       "18.8"  "22.0 (4.9)"
#        "ampg"   "\\li360 Median"     "16.4{\\sub mm}"        "21.4{\\super 2}"
#        "ampg"   "\\li360 {\\cf12 Q1 - Q3}"    "15.1 - 21.2" "19.2 - 22.8"
#        "ampg"   "\\li360 Range"      "{\\cf2 10.4 - 33.9}" "14.7 - 32.4"
#        "cyl"    "\\li360 8\\line Cylinder" "\\ul 10 ( 52.6%) \\ul0" "4 ( 30.8%)"
#        "cyl"    "\\li360 6\\line {\\cf11 Cylinder and more perhaps more}" "4 ( 21.1%)"  "3 ( 23.1%)"
#        "cyl"    "\\li360 4\\line Cylinder" "\\ul 5 ( 26.3%) \\ul0"  "6 ( 46.2%)"')
#  

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  fp <- file.path(tempdir(), "example17.rtf")
#  
#  # Create table
#  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) |>
#    stub(c("var", "label"), width = .8) |>
#    define(var, blank_after = TRUE, label_row = TRUE,
#           format = c(ampg = "\\i Here is a italic label \\i0.",
#                      cyl = "\\ul Under line label \\ul0")) |>
#    define(A, label = "\\shad {{\\cf6 Group A}} {common::supsc('2')}\\shad0", align = "center", n = 19) |>
#    define(B, label = "\\outl Group B \\outl0", align = "center", n = 13) |>
#    footnotes("\\i\\fs14 * Italic Small Table Footnote \\fs18")
#  
#  # Create report and add content
#  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
#                       font = "Times") |>
#    report_options(allow_code = TRUE) |>
#    page_header(left = "\\strike Strikethrough header \\strike0", right = "Study: Cars") |>
#    titles("\\b\\i {{\\highlight7 This is yellow italic highlight}} \\b0\\i0",
#           "\\fs0\\fs28 {{\\cf2 This is a blue big title}} \\fs18") |>
#    add_content(tbl) |>
#    footnotes("\\i\\fs14 * Italic Small Report Footnote \\fs18") |>
#    page_footer(left = "Left",
#                center = "Confidential",
#                right = "Page [pg] of [tpg]")
#  

