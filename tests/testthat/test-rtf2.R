
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

fnt <- "Arial"
fsz <- 10

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

  txt <- create_text(cnt, width = 6, borders = "outside", align = "right") %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "right") %>%
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
  expect_equal(res$pages, 3)


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
    attr(dat[[2]], "label") <- "Cylin."
  
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

# Blank row not totally clearing out. Everything else good.
test_that("rtf2-7: Multi page table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test7.rtf")
  
  dat <- iris
 
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Irises", "Another Title") %>% 
    define(Sepal.Length, label = "Sepal Length", width = 1, align = "center") %>% 
    define(Sepal.Width, label = "Sepal Width", width = 1, align = "centre") %>% 
    define(Species, blank_after = TRUE)
  
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
  
  tbl <- create_table(dat, borders = c("left", "right", "bottom", "top")) %>% 
    define(mpg, label = "Miles Per Gallon")
  
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


test_that("rtf2-13: Spanning headers work as expected.", {


  fp <- file.path(base_path, "rtf2/test13.rtf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c("top", "bottom")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>% 
    spanning_header(hp, wt, "Span 2", underline = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span", n = 11, level = 2)


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

})


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

# Works
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


test_that("rtf2-17: Title header on table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test17.rtf")
  
  dat <- iris[1:25, ] 
  
  tbl <- create_table(dat) %>% 
    title_header("Table 1.0", "My Nice Table", 
                 right = c("Right1", 
                           "Right2", "Page [pg] of [tpg]")) %>%
    footnotes("My footnote 1", "My footnote 2")
  
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


test_that("rtf2-19: Title and Footnote borders work as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test19.rtf")
  
  dat <- iris[1:25, ] 
  
  tbl <- create_table(dat, borders = "all") %>% 
    titles("Table 1.0", "My Nice Report with Borders",
                 borders = c("top", "bottom", "left", "right"),
           blank_row = "none") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              borders = c("top", "bottom", "left", "right"), 
              blank_row = "none")
  
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


test_that("rtf2-20: Title Header borders work as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test20.rtf")
  
  dat <- iris[1:25, ] 
  
  tbl <- create_table(dat, borders = "all") %>% 
    title_header("Table 1.0", "My Nice Report with Borders",
                 right = c("Right1", "Right2", "Right3"),
           borders = c("top", "bottom", "left", "right"),
           blank_row = "none") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              borders = c("top", "bottom", "left", "right"), 
              blank_row = "above")
  
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

# Cell wrapping is throwing off line counts
test_that("rtf2-21: Page wrap with spanning header works as expected.", {
  
  fp <- file.path(base_path, "rtf2/test21.rtf")
  
  
  df <- data.frame(vehicle = rownames(mtcars), mtcars, stringsAsFactors = FALSE)
  rownames(df) = NULL
  
  tbl <- create_table(df) %>% 
    spanning_header(2, 5,
                    label = "Span 1", label_align = "center", n = 10) %>%
    spanning_header(6, 8,
                    label = "Span 2", label_align = "center", n = 10) %>%
    spanning_header(9, 12,
                    label = "Span 3", label_align = "center", n = 10) %>%
    spanning_header(6, 12, label = "Super Span",
                    label_align = "center",
                    level = 2) %>%
    define(vehicle, label = "Vehicle", id_var = TRUE) %>% 
    define(mpg, format = "%.1f") %>% 
    define(wt, page_wrap = TRUE) %>% 
    define(vs, page_wrap = TRUE)
  
  rpt <- create_report(fp, output_type = "RTF", 
                       orientation = "portrait", font = fnt) %>%
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test") %>% 
    footnotes("My footnote")
  
  #print(rpt)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  expect_equal(res$pages, 3)
})

# This is a good one for testing height calculations.  Looks good for now.
test_that("rtf2-22: Page by works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test22.rtf")
  
  dat <- iris
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Report with a Page By") %>%
    page_by(Species, label = "Species: ", align = "right")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1") %>% 
    footnotes("My footnote 1", "My footnote 2")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 6)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})

# Works!  
test_that("rtf2-23: Two contents on one page works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test23.rtf")
  
  dat <- mtcars[1:15, ]
  
  tbl <- create_table(dat) %>% 
    titles("Table 1.0", "My Nice Table") %>%
    footnotes("My footnote 1", "My footnote 2")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    add_content(tbl, page_break = FALSE, blank_row = "below") %>%
    add_content(create_text(cnt, width = 5), align = "center") %>% 
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

# Working.
test_that("rtf2-24: Two tables one headerless works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test24.rtf")
  
  dat <- mtcars
  dat2 <- mtcars[16:20, ]
  
  tbl <- create_table(dat) %>% 
    titles("Table 1.0", "My Nice Table") %>% 
    column_defaults(width = .5)

  tbl2 <- create_table(dat2, headerless = TRUE) %>% 
    column_defaults(width = .5)
    
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    add_content(tbl, page_break = FALSE, blank_row = "none") %>%
    add_content(tbl2, align = "center") %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    footnotes("My footnote 1", "My footnote 2")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  
  
})

# Works!
test_that("rtf2-25: Simplest RTF Plot works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf2/test25.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8, borders = c("top", "bottom", "all")) %>% 
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    footnotes("* Motor Trend, 1974") 
  
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial", font_size =10) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "right") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") 
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

# Working but looks like par filler is off by 1 row
test_that("rtf2-26: RTF Table with Plot on same page works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf2/test26.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8)
  tbl <- create_table(mtcars[1:3, ])
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
    set_margins(top = 1, bottom = 1) %>%
    
    add_content(plt, page_break = FALSE, blank_row = "none") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


# Works but putting filler pars where it is not necessary
test_that("rtf2-27: Plot with page by on plot works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf2/test27.rtf")
  
  
  dat <- mtcars[order(mtcars$cyl), ]
  
  p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
  
  
  #dats <- split(p$data, p$data$grp)
  #tbl <- create_table(dat[1:3, ])
  
  plt <- create_plot(p, height = 4, width = 8) %>% 
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
    page_by(cyl, "Cylinders: ") %>% 
    footnotes("* Motor Trend, 1974") 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial", font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt) %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
  
})

# Working but last page needs extra spacer before footnote.
test_that("rtf2-28: Plot with page by on report works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf2/test28.rtf")
  
  
  dat <- mtcars[order(mtcars$cyl), ]
  
  p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
  
  
  #dats <- split(p$data, p$data$grp)
  #tbl <- create_table(dat[1:3, ])
  
  plt <- create_plot(p, height = 4, width = 8)
  
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial", font_size = 10) %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", 
           blank_row = "none", borders = "none") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_by(cyl, "Cylinders: ") %>% 
    add_content(plt) %>%
    footnotes("* Motor Trend, 1974", borders = "none") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
  
})

test_that("rtf2-29: Simplest RTF Plot with valign top works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf2/test29.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8)
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    footnotes("* Motor Trend, 1974", valign = "top") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


# Need extra par before footnote.  Appears to be issue with line counts passed back to content loop.
test_that("rtf2-30: Simplest RTF Plot with valign bottom works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf2/test30.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8) %>%
    footnotes("* Motor Trend, 1974", valign = "bottom")
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("rtf2-31: Simplest RTF Text with valign top works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test31.rtf")
  
  txt <- create_text(cnt, width = 6)
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Text 1.0", "MTCARS Miles per Cylinder Text") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(txt, align = "center") %>%
    footnotes("* Motor Trend, 1974", valign = "top") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("rtf2-32: Simplest RTF Text with valign bottom works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test32.rtf")
  
  txt <- create_text(cnt, width = 6) %>%
    footnotes("* Motor Trend, 1974", valign = "bottom")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(txt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

