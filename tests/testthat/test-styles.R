
context("Style Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

test_that("style0: Style functions work as expected.", {
  
  res1 <- create_style(font_name = "Arial", font_size = 10,
                       table_header_background = "blue")
  
  expect_equal(res1$font_name, "Arial")
  expect_equal(res1$font_size, 10)
  expect_equal(res1$table_header_background, "blue")

  rpt <- create_report() %>% add_style(res1)
  
  expect_equal(get_style(rpt, "border_color"), "")
  
  
  expect_equal(is.null(rpt$style), FALSE)
  expect_equal(rpt$style$font_name, "Arial")
  
  
  rpt <- create_report() %>% add_style(create_style(border_color = "grey"))
  
  expect_equal(has_style(rpt, "border_color"), TRUE)
  expect_equal(get_style(rpt, "border_color"), "grey")
  
  rpt <- create_report() %>% add_style(theme = "SASDefault")
  
  expect_equal(get_style(rpt, "border_color"), "Grey")
  
})



test_that("style1: SAS theme works with html.", {
  
  
  fp <- file.path(base_path, "html/style1.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars)[1:15], mtcars[1:15, ])
  attr(dat[[3]], "label") <- "Cylin."
  attr(dat[[3]], "width") <- 1
  attr(dat[[3]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "cyl", label = "Span1")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "SASDefault")
  
  res <- write_report(rpt)
  
  if (dev == TRUE)
    file.show(res$modified_path)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})




test_that("style2: add_style() works with html.", {
  
  
  fp <- file.path(base_path, "html/style2.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars[1:15, ]), mtcars[1:15, ])
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"
  
  sty <- create_style(font_name = "Arial", font_size = 12, text_color = "blue",
                      background_color = "pink", title_font_bold = TRUE,
                      title_font_color = "orange", title_background = "brown",
                      border_color = "grey", table_body_background = "yellow",
                      table_body_stripe = "blue", table_stub_background = "purple",
                      table_stub_font_bold = TRUE, table_stub_font_color = "green", 
                      table_header_background = "green", table_header_font_bold = TRUE,
                      table_header_font_color = "brown", footnote_font_bold = TRUE,
                      footnote_font_color = "brown", footnote_background = "orange",
                      table_body_font_color = "red")
  
  tbl <- create_table(dat, borders = "all", first_row_blank = FALSE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("all"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "all", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "drat", label = "Span 1") %>% 
    spanning_header(from = "wt", to ="carb", label = "Span 2")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(sty)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("style3: Test print.style_spec() and get_theme().", {
  
  s1 <- create_style(font_name = "Arial", font_size = 12, 
                     table_body_background = "Blue",
                     table_header_background = "Red")
  if (dev)
    print(s1)
  
  
  s2 <- get_theme("SASDefault")
  
  if (dev)
    print(s2)
  
  if (dev)
    print(s1, verbose = TRUE)
  
  expect_equal(TRUE, TRUE)
  
})


test_that("style4: DarkRed theme works.", {
  
  
  fp <- file.path(base_path, "html/style4.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars)[1:15], mtcars[1:15, ])
  attr(dat[[3]], "label") <- "Cylin."
  attr(dat[[3]], "width") <- 1
  attr(dat[[3]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "cyl", label = "Span1")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "DarkRed")
  
  res <- write_report(rpt)
  
  if (dev == TRUE)
    file.show(res$modified_path)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

test_that("style5: SeaGreen theme works.", {
  
  
  fp <- file.path(base_path, "html/style5.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars)[1:15], mtcars[1:15, ])
  attr(dat[[3]], "label") <- "Cylin."
  attr(dat[[3]], "width") <- 1
  attr(dat[[3]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "cyl", label = "Span1")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "SeaGreen")
  
  res <- write_report(rpt)
  
  if (dev == TRUE)
    file.show(res$modified_path)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

test_that("style6: SlateGrey theme works.", {
  
  
  fp <- file.path(base_path, "html/style6.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars)[1:15], mtcars[1:15, ])
  attr(dat[[3]], "label") <- "Cylin."
  attr(dat[[3]], "width") <- 1
  attr(dat[[3]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "cyl", label = "Span1")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "SlateGrey")
  
  res <- write_report(rpt)
  
  if (dev == TRUE)
    file.show(res$modified_path)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})


test_that("style7: MidnightBlue theme works with html.", {
  
  
  fp <- file.path(base_path, "html/style7.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars)[1:15], mtcars[1:15, ])
  attr(dat[[3]], "label") <- "Cylin."
  attr(dat[[3]], "width") <- 1
  attr(dat[[3]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "cyl", label = "Span1")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "MidnightBlue")
  
  res <- write_report(rpt)
  
  if (dev == TRUE)
    file.show(res$modified_path)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})


test_that("style8: SteelBlue theme works.", {
  
  
  fp <- file.path(base_path, "html/style8.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars)[1:15], mtcars[1:15, ])
  attr(dat[[3]], "label") <- "Cylin."
  attr(dat[[3]], "width") <- 1
  attr(dat[[3]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "cyl", label = "Span1")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "SteelBlue")
  
  res <- write_report(rpt)
  
  if (dev == TRUE)
    file.show(res$modified_path)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

test_that("style9: Plain theme works.", {
  
  
  fp <- file.path(base_path, "html/style9.html")
  # print(fp)
  
  dat <- data.frame(stub = rownames(mtcars)[1:15], mtcars[1:15, ])
  attr(dat[[3]], "label") <- "Cylin."
  attr(dat[[3]], "width") <- 1
  attr(dat[[3]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right") %>% 
    spanning_header(from = "mpg", to = "cyl", label = "Span1")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "Plain")
  
  res <- write_report(rpt)
  
  if (dev == TRUE)
    file.show(res$modified_path)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})


test_that("style10: Header border works as expected.", {
  

  # Create temp file path
  fp <- file.path(base_path, "html/style10.html")
  
  dat <- as.data.frame(HairEyeColor)
  dat <- dat[dat$Freq > 10, ]

  # Define custom style
  sty <- create_style(font_name = "Times",
                      font_size = 10,
                      title_font_size = 12,
                      title_font_bold = TRUE,
                      title_font_color = "Blue",
                      table_header_background = "Grey",
                      table_header_font_bold = TRUE,
                      table_header_font_color = "White",
                      table_body_background = "White",
                      table_body_stripe = "Red")

  # Create table object
  tbl <- create_table(dat, borders = "outside") %>%
  titles("Hair and Eye Colors with Style") %>%
  column_defaults(width = .6) %>% 
  spanning_header(Hair, Freq, label = "Span")

  # Create report and add theme
  rpt <- create_report(fp, output_type = "HTML") %>%
         add_content(tbl) %>%
         add_style(style = sty)

  # Write out the report
  res <- write_report(rpt)

  if (dev)
    file.show(fp)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

test_that("style11: stub border work as expected.", {
  
  
  library(reporter)
  
  # Create temporary path
  fp <- file.path(base_path, "html/style11.html")
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  # Create custom style
  sty <- create_style(font_name = "Arial",
                      font_size = 10,
                      background_color = "WhiteSmoke",
                      border_color = "Grey",
                      title_font_size = 12,
                      title_font_bold = TRUE,
                      title_font_color = "SteelBlue",
                      table_header_background = "Tan",
                      table_header_font_bold = TRUE,
                      table_header_font_color = "White",
                      table_body_background = "White",
                      table_body_stripe = "Wheat",
                      table_stub_background = "Tan",
                      table_stub_font_color = "White",
                      table_stub_font_bold = TRUE
                   )
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>% 
    stub(c("var", "label")) %>% 
    column_defaults(width = 1.25) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center", n = 19) %>% 
    define(B, label = "Group B", align = "center", n = 13)  %>% 
    titles("Table 1.0", "MTCARS Summary Table with Custom Style", 
           borders = "none") %>% 
    footnotes("* Motor Trend, 1974", borders = "outside")
  
  # Create report and add content
  rpt <- create_report(fp, output_type = "HTML", font = "Arial", 
                       font_size = 12) %>% 
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>% 
    add_style(sty)
  
  # Write out report
  res <- write_report(rpt)
  
  if (dev)
    file.show(fp)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})



test_that("style12: Label row is one cell and styles work.", {
  
  
  fp <- file.path(base_path, "html/style12.html")
  
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  ll <- "Here is a super long label to see if it can span the entire table."
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = "all", 
                      header_bold = TRUE) %>% 
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = ll, cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center", n = 19) %>% 
    define(B, label = "Group B", align = "center", n = 13)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "HTML",
                       font = "Times") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = Sys.time(), 
                center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_style(theme = "SteelBlue")
  
  
  
  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)
  
  
})

test_that("style13: Label row is one cell and styles work.", {
  
  res <- cell_style(indicator = fork, bold = TRUE)
  
  expect_equal(res$indicator, "fork")
  expect_equal(res$bold, TRUE)
  
})


test_that("style14: has_cell_style() works as expected.", {
  
  stls <- list(fork = cell_style(bold = TRUE),
               bork = cell_style(bold = TRUE, indicator = "sammy"))
  
  
  expect_equal(has_cell_style("fork", stls), TRUE)
  expect_equal(has_cell_style("forp", stls), FALSE)
  
})


test_that("style15: get_cell_style() works as expected.", {
  
  stls <- list(fork = cell_style(bold = TRUE),
               bork = cell_style(bold = TRUE, indicator = "sammy"))
  
  sty <- get_cell_style("fork", stls)
  
  expect_equal(is.null(sty), FALSE)
  
  sty2 <- get_cell_style("forp", stls)
  
  expect_equal(is.null(sty2), TRUE)
  
})
