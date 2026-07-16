## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  fp <- file.path(tempdir(), "example18a.rtf")
#  
#  dat <- iris[1:100, ]
#  dat$Species <- as.character(dat$Species)
#  dat$Species[1:25] <- rep("setosa1", 25)
#  dat$Species[26:50] <- rep("setosa2", 25)
#  
#  tbl <- create_table(dat, auto_page = FALSE) %>%
#    footnotes("First Table with auto_page=FALSE in create_table",
#              "My footnote 2", valign = "bottom") %>%
#    define(Species, page_break = TRUE)
#  
#  tbl2 <- create_table(dat) %>%
#    footnotes("Second Table with auto_page=FALSE in report_options",
#              "My footnote 2", valign = "bottom") %>%
#    define(Species, page_break = TRUE)
#  
#  tbl3 <- create_table(dat, auto_page = TRUE) %>%
#    footnotes("Third Table with auto_page=TRUE in create_table",
#              "My footnote 2", valign = "bottom") %>%
#    define(Species, page_break = TRUE)
#  
#  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
#                       font_size = 10, orientation = "landscape") %>%
#    report_options(auto_page = FALSE) %>%
#    set_margins(top = 1, bottom = 1) %>%
#    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
#    titles("Table 1.0", "My Nice Table") %>%
#    add_content(tbl) %>%
#    add_content(tbl2) %>%
#    add_content(tbl3) %>%
#    page_footer("Left1", "Center1", "Right1")
#  
#  res <- write_report(rpt)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  fp <- file.path(tempdir(), "example18b.rtf")
#  
#  # Read in prepared data
#  df <- read.table(header = TRUE, text = '
#    USUBJID STUDYID  DOMAIN  SUBJID  RFSTDTC       RFENDTC       RFXSTDTC      RFXENDTC      RFICDTC
#    "001"   "ABC"    "DM"    "01"    "2021-12-01"  "2021-12-20"  "2021-12-02"  "2021-12-20"  "2021-12-01"
#    "002"   "ABC"    "DM"    "02"    "2021-12-02"  "2021-12-21"  "2021-12-03"  "2021-12-21"  "2021-12-02"
#    "003"   "ABC"    "DM"    "03"    "2021-12-03"  "2021-12-22"  "2021-12-04"  "2021-12-22"  "2021-12-03"
#    "004"   "ABC"    "DM"    "04"    "2021-12-04"  "2021-12-23"  "2021-12-05"  "2021-12-23"  "2021-12-04"
#    "005"   "ABC"    "DM"    "05"    "2021-12-05"  "2021-12-24"  "2021-12-06"  "2021-12-24"  "2021-12-05"
#    "006"   "ABC"    "DM"    "06"    "2021-12-06"  "2021-12-25"  "2021-12-07"  "2021-12-25"  "2021-12-06"
#    "007"   "ABC"    "DM"    "07"    "2021-12-07"  "2021-12-26"  "2021-12-08"  "2021-12-26"  "2021-12-07"
#    "008"   "ABC"    "DM"    "08"    "2021-12-08"  "2021-12-27"  "2021-12-09"  "2021-12-27"  "2021-12-08"')
#  
#  # Define table
#  tbl <- create_table(df, page_wrap = FALSE) |>
#    titles("page_wrap = FALSE in create_table()")
#  tbl2 <- create_table(df) |>
#    titles("page_wrap = FALSE in report_options()")
#  tbl3 <- create_table(df, page_wrap = TRUE)
#  
#  # Define Report
#  rpt <- create_report(fp, font = "Arial", font_size = 10, units = "cm",
#                       orientation = "portrait") %>%
#    report_options(page_wrap = FALSE) %>%
#    titles("Listing 1.0",
#           "Demographics Dataset") %>%
#    add_content(tbl, align = "left") %>%
#    add_content(tbl2, align = "left") %>%
#    add_content(tbl3, align = "left") %>%
#    page_header("Sponsor", "Drug") %>%
#    page_footer(left = "Time", right = "Page [pg] of [tpg]") %>%
#    footnotes("My footnote")
#  
#  res <- write_report(rpt, output_type = "rtf")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  fp <- file.path(tempdir(), "example18c.rtf")
#  
#  df <- read.table(header = TRUE, text = '
#    var     label        A             B
#    "ampg"   "N"          "19"          "13"
#    "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
#    "ampg"   "Median"     "16.4"        "21.4"
#    "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
#    "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
#    "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
#    "cyl"    "6 Cylinder and more perhaps more" "4 ( 21.1%)"  "3 ( 23.1%)"
#    "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
#  
#  df$sub_title <- "This is a long page by value which would not be inserted any line break characters."
#  
#  # Create table
#  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>%
#    stub(c("var", "label"), width = .8) %>%
#    page_by(sub_title, label = "This is a long label which would make multiple lines:",
#            bold = "label") %>%
#    define(sub_title, visible = FALSE) %>%
#    define(var, blank_after = TRUE, label_row = TRUE,
#           format = c(ampg = "Here is a label\nwith manual line break.",
#                      cyl = "Cylinders")) %>%
#    define(label, indent = .25) %>%
#    define(A, label = "Group A", align = "center") %>%
#    define(B, label = "Group B", align = "center")
#  
#  
#  # Create report and add content
#  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
#                       font = "Arial") %>%
#    report_options(line_break = FALSE) %>%
#    page_header(left = "This is a long left header which would not be inserted any line break characters.",
#                right = "This is a long right header which would not be inserted any line break characters.") %>%
#    titles("Table 1.0",
#           paste0("This is a title which is turned off the automatically line breaks",
#                  " so it should not contain any line break characters and it should",
#                  " prevent from overflow because the wrapping lines are turned off.")) %>%
#    add_content(tbl) %>%
#    footnotes(paste0("This is a long footnote which would not be inserted any line break characters even though it",
#                     " has multiple lines. There should be no overflow because lines have been counted.")) %>%
#    page_footer(left = "This is a long left footer which would not be inserted any line break characters.",
#                center = "This is a long center footer which would not be inserted any line break characters.",
#                right = "This is a long right footer which would not be inserted any line break characters.")
#  
#  res <- write_report(rpt)

