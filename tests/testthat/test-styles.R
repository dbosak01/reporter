
context("Style Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

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
  
  rpt <- create_report() %>% add_style(theme = "basic1")
  
  expect_equal(get_style(rpt, "border_color"), "Grey")
  
})



test_that("style1: theme works with html.", {
  
  
  fp <- file.path(base_path, "html/style1.html")
  # print(fp)
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    add_style(theme = "basic1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})




test_that("style2: add_style() works with html.", {
  
  
  fp <- file.path(base_path, "html/style2.html")
  # print(fp)
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "all", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("all"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "all", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right")
  
  rpt <- create_report(fp, output_type = "HTML", orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})
