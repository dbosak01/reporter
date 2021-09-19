
context("RTF2 Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

test_that("rtf2-0a: Fixed report is correct.", {


  fp <- file.path(base_path, "rtf2/test0a.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed") %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})

test_that("rtf2-0b: Fixed report with font_size is correct.", {


  fp <- file.path(base_path, "rtf2/test0b.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed",
                       font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})

test_that("rtf2-0c: Fixed report with font_size options is correct.", {


  fp <- file.path(base_path, "rtf2/test0c.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed") %>%
    options_fixed(font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})

test_that("rtf2-0d: Fixed report with conflicting font size is correct.", {


  fp <- file.path(base_path, "rtf2/test0d.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed",
                       font_size = 8) %>%
    options_fixed(font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})



test_that("rtf2-1: One page text spec works as expected.", {


  fp <- file.path(base_path, "rtf2/test1.rtf")

  txt <- create_text(cnt) %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1") 

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})





test_that("rtf2-2: Two page text spec works as expected in 12pt font.", {


  fp <- file.path(base_path, "rtf2/test2.rtf")

  cnttxt <- paste(rep(cnt, 10), collapse = "")

  txt <- create_text(cnttxt) %>%
    titles("Text 1.0", "My Nice Text") 

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1") %>%
    footnotes("My footnote 1", "My footnote 2")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)


})


test_that("rtf2-3: Three page text spec increased margins works as expected.", {


  fp <- file.path(base_path, "rtf2/test3.rtf")

  cnttxt <- paste(rep(cnt, 10), collapse = "")

  txt <- create_text(cnttxt) %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 2, bottom = 2) %>%
    page_header("Left", c("Right1", "Right2")) %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  #expect_equal(res$pages, 1)


})


test_that("rtf2-4: Two page text spec works as expected in 10pt font.", {


  fp <- file.path(base_path, "rtf2/test4.rtf")

  cnttxt <- paste(rep(cnt, 12), collapse = "")

  txt <- create_text(cnttxt) 

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1") %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)


})


test_that("rtf2-5: Two page text spec works as expected in 8pt font.", {


  fp <- file.path(base_path, "rtf2/test5.rtf")

  cnttxt <- paste(rep(cnt, 20), collapse = "")

  txt <- create_text(cnttxt) 

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 8) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1") %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)


})


test_that("rtf2-6: One page table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test6.rtf")
  
  dat <- mtcars[1:15, ]
  #attr(dat[[2]], "label") <- "Cylin."
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(create_table(dat)) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


test_that("rtf2-7: Three page table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test7.rtf")
  
  dat <- iris
  #attr(dat[[2]], "label") <- "Cylin."
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Irises", "Another Title") 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    footnotes("My footnote 1", "My footnote 2") 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 7)
  
  
})



test_that("rtf2-8: Portrait table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test8.rtf")
  
  dat <- mtcars[1:15, ] 
  
  tbl <- create_table(dat) 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "portrait") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  expect_equal(length(res$column_widths[[1]]), 11)
  expect_equal(res$pages, 1)
  
  
})


test_that("rtf2-9: Wide table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test9.rtf")
  
  dat <- mtcars[1:15, ] 
  
  tbl <- create_table(dat) %>% 
  column_defaults(width = 1)

  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "portrait") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top") %>% # Works!
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  
  
})


test_that("rtf2-10: Preview works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test10.rtf")
  
  dat <- iris
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2") 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt, preview = 2)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  
  
})


test_that("rtf2-11: Forced page wrap works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test11.rtf")
  
  dat <- mtcars
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2") %>% 
    define(wt, page_wrap = TRUE)
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 4)
  
  
})


test_that("rtf2-12: Table Borders work as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test12.rtf")
  
  dat <- mtcars[1:15, ] 
  
  tbl <- create_table(dat, borders = c("left", "right", "bottom", "top"))
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  expect_equal(length(res$column_widths[[1]]), 11)
  expect_equal(res$pages, 1)
  
  
})

# test_that("rtf2-13: Spanning headers work as expected.", {
#   
#   
#   fp <- file.path(base_path, "rtf2/test13.rtf")
#   
#   dat <- mtcars[1:15, ] 
#   
#   tbl <- create_table(dat, borders = "outside") %>% 
#     spanning_header(mpg, disp, "Span 1") %>% 
#     spanning_header(hp, wt, "Span 2") %>% 
#     spanning_header(qsec, vs, "Span 3") %>% 
#     spanning_header(drat, gear, "Super span", level = 2)
#     
#   
#   rpt <- create_report(fp, output_type = "RTF", font = "Arial",
#                        font_size = 10, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
#     titles("Table 1.0", "My Nice Table") %>%
#     add_content(tbl) %>%
#     footnotes("My footnote 1", "My footnote 2") %>%
#     page_footer("Left1", "Center1", "Right1")
#   
#   res <- write_report(rpt)
#   res
#   res$column_widths
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   expect_equal(length(res$column_widths[[1]]), 11)
#   #expect_equal(res$pages, 1)
#   
#   
# })


test_that("rtf2-14: Labels and show_cols work as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test14.rtf")
  
  dat <- mtcars[1:15, ] 
  
  attr(dat$mpg, "label") <- "Miles Per Gallon"
  attr(dat$cyl, "label") <- "Cylinders"
  
  tbl <- create_table(dat, show_cols = 1:5) %>% 
    define(mpg, width = 1.25) %>% 
    define(disp, label = "Displacement")
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res

  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})


test_that("rtf2-15: Valign on report footnotes works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test15.rtf")
  
  dat <- iris[1:50, ] 
  
  tbl <- create_table(dat) 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})


test_that("rtf2-16: Valign on table footnotes works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test16.rtf")
  
  dat <- iris[1:50, ] 
  
  tbl <- create_table(dat) %>% 
    footnotes("My footnote 1", "My footnote 2", valign = "bottom")
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})


test_that("rtf2-17: Table header on table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test17.rtf")
  
  dat <- iris[1:25, ] 
  
  tbl <- create_table(dat) %>% 
    title_header("Table 1.0", "My Nice Table", right = c("Right1", 
                                                   "Right2", "Page [pg] of [tpg]")) %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top")
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})


test_that("rtf2-18: Table header on report works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test18.rtf")
  
  dat <- iris[1:50, ] 
  
  tbl <- create_table(dat) 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>% 
    title_header("Table 1.0", "My Nice Table", right = c("Page [pg] of [tpg]", 
                                                         "Right2", "Right3")) %>%
    footnotes("My footnote 1", "My footnote 2", valign = "bottom")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})

