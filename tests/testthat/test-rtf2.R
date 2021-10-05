
context("RTF2 Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

fnt <- "Arial"
fsz <- 10

dev <- FALSE


# Basic Tests -------------------------------------------------------------


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
    titles("Text 1.0", "My Nice Text", borders = "outside") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside")

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "right") %>%
    page_footer("Left1", "Center1", "Right1") 

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})




# Last page wraps
test_that("rtf2-2: Two page text spec works as expected in 12pt font.", {


  fp <- file.path(base_path, "rtf2/test2.rtf")

  cnttxt <- paste(rep(cnt, 12), collapse = "")

  txt <- create_text(cnttxt) %>%
    titles("Text 1.0", "My Nice Text") 

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
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

  cnttxt <- paste(rep(cnt, 15), collapse = "")

  txt <- create_text(cnttxt) %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz) %>%
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

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
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

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
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
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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

# Last page wraps
test_that("rtf2-7: Multi page table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test7.rtf")
  
  dat <- iris
 
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Irises", "Another Title") %>% 
    define(Sepal.Length, label = "Sepal Length", width = 1, align = "center") %>% 
    define(Sepal.Width, label = "Sepal Width", width = 1, align = "centre") %>% 
    define(Species, blank_after = TRUE)
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
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
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = 10, orientation = "portrait") %>%
   # set_margins(top = 1, bottom = 1) %>%
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

  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "portrait") %>%
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

# This is awesome. Shows cell wrapping, page break, and valign
test_that("rtf2-11: Forced page wrap works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test11.rtf")
  
  dat <- data.frame(labels = rownames(mtcars), mtcars)
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "bottom") %>% 
    define(labels, id_var = TRUE) %>% 
    define(wt, page_wrap = TRUE)
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
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
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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

# Nice.
test_that("rtf2-13: Spanning headers work as expected.", {


  fp <- file.path(base_path, "rtf2/test13.rtf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c("top", "bottom")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>% 
    spanning_header(hp, wt, "Span 2", underline = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span", n = 11, level = 2)


  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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
  
  tbl <- create_table(dat, show_cols = c("mpg", "hp", "cyl", "disp", "drat")) %>% 
    define(mpg, width = 1.25) %>% 
    define(disp, label = "Displacement")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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
  
  dat <- iris[1:100, ] 
  
  tbl <- create_table(dat) %>% 
    define(Species, page_break = TRUE)
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = fsz, orientation = "landscape") %>%
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
  expect_equal(res$pages, 4)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  class(dat$Species)
})

# Works
test_that("rtf2-16: Valign on table footnotes works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test16.rtf")
  
  dat <- iris[1:100, ] 
  
  tbl <- create_table(dat) %>% 
    footnotes("My footnote 1", "My footnote 2", valign = "bottom") %>% 
    define(Species, page_break = TRUE)
  
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
  expect_equal(res$pages, 4)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})

test_that("rtf2-17: Title header on table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test17.rtf")
  
  dat <- iris[1:25, ] 
  
  tbl <- create_table(dat, width = 9) %>% 
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


test_that("rtf2-18: Title header on report works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test18.rtf")
  
  dat <- iris[1:50, ] 
  
  tbl <- create_table(dat) 
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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
  
  dat <- iris[1:20, ] 
  
  tbl <- create_table(dat, borders = "all") %>% 
    titles("Table 1.0", "My Nice Report with Borders",
                 borders = c("top", "bottom", "left", "right"),
           blank_row = "both") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              borders = c("top", "bottom", "left", "right"), 
              blank_row = "both")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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
           borders = c("outside"),
           blank_row = "both") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              borders = c("top", "bottom", "left", "right"), 
              blank_row = "both")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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

# Works unless drat column is too narrow and spanning label wraps unexpectedly
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
    define(drat, width = 1) %>% 
    define(wt, page_wrap = TRUE) %>% 
    define(vs, page_wrap = TRUE)
  
  rpt <- create_report(fp, output_type = "RTF", 
                       orientation = "portrait", font = fnt) %>%
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test") %>% 
    footnotes("My footnote") %>% 
    page_header("Left") %>% 
    page_footer("Left", right = "Right")
  
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
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Species, label = "Species: ", align = "right", borders = "none")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1") %>% 
    footnotes("My footnote 1", "My footnote 2", borders = "none")
  
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
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size =fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "right") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") 
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


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
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt) %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
  
})


test_that("rtf2-28: Plot with page by on report works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf2/test28.rtf")
  
  
  dat <- mtcars[order(mtcars$cyl), ]
  
  p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
  
  
  #dats <- split(p$data, p$data$grp)
  #tbl <- create_table(dat[1:3, ])
  
  plt <- create_plot(p, height = 4, width = 8)
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
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

test_that("rtf2-33: Table with long cell and label values wraps as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test33.rtf")
  
  
  # Setup
  arm <- c(rep("A", 5), rep("B", 5))
  subjid <- 100:109
  name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
            "al-Kazemi, Najwa \nand more and more", "Schaffer, Ashley", "Laner, Tahma", 
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
            "Smith, Shaileigh")
  sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
  age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
  
  
  # Create data frame
  df <- data.frame(arm, subjid, name, sex, age, stringsAsFactors = FALSE)
  
  
  tbl1 <- create_table(df, first_row_blank = TRUE) %>%
    define(subjid, label = "Subject ID for a patient", n = 10, align = "left", 
           width = 1) %>%
    define(name, label = "Subject Name", width = 1) %>%
    define(sex, label = "Sex", n = 10, align = "center") %>%
    define(age, label = "Age", n = 10) %>%
    define(arm, label = "Arm",
           blank_after = TRUE,
           dedupe = TRUE)
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, 
                       font_size = fsz) %>%
    titles("Table 1.0", align = "center") %>%
    
    add_content(tbl1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  

  
})

# Works on all combinations on font and font size.  Needed adjustments to row height.
test_that("rtf2-34: Table with break between sections works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test34.rtf")
  
  
  # Setup
  subjid <- 100:109
  name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
            "al-Kazemi, Najwa", "Schaffer, Ashley", "Laner, Tahma", 
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
            "Smith, Shaileigh")
  sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
  age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
  arm <- c(rep("A", 5), rep("B", 5))
  
  # Create data frame
  df <- data.frame(subjid, name, sex, age, arm)
  
  
  tbl1 <- create_table(df, first_row_blank = TRUE) %>%
    define(subjid, label = "Subject ID", align = "left", width = 1) %>% 
    define(name, label = "Subject Name", width = 1) %>% 
    define(sex, label = "Sex") %>% 
    define(age, label = "Age") %>% 
    define(arm, label = "Arm", 
           blank_after = FALSE, 
           dedupe = TRUE, 
           align = "right") #%>% 
   # spanning_header(sex, arm, label = "Here is a spanning header")
  
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
   # options_fixed(line_count = 46) %>% 
    titles("Table 1.0", "Analysis Data Subject Listing\n And more stuff", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R", 
              "Here is a big long footnote that is going to wrap\n at least once") %>%
    page_footer(left = "Time", center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  
  
  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)

  
})


test_that("rtf2-35: Title Header and page header/footer wrapping work as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test35.rtf")
  
  dat <- iris[1:10, ] 
  
  tbl <- create_table(dat, borders = "none") %>% 
    title_header("Table 1.0", "My Nice Report with Borders",
                 right = c("Right1", "Right2", 
                           "Right3 long enough to wrap around at least once"),
                 borders = "none",
                 blank_row = "none") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "bottom",
              borders = "none", 
              blank_row = "above")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_header(c("Left1", "Left2\nwrap"), "Right 1") %>% 
    page_footer("Left1", 
                "Center1 here is a whole bunch of stuff to try and make it wrap", 
                "Right1\nwrap\n and wrap again")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})


test_that("rtf2-36: Title and Footnote widths work as expected on table.", {
  
  
  fp <- file.path(base_path, "rtf2/test36.rtf")
  
  dat <- iris[1:20, ] 
  
  tbl <- create_table(dat) %>% 
    titles("Table 1.0", "My Nice Report with Widths",
           width = "page") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              width = "page")
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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


test_that("rtf2-37: Title and Footnote widths work as expected on report", {
  
  
  fp <- file.path(base_path, "rtf2/test37.rtf")
  
  dat <- iris[1:20, ] 
  
  tbl <- create_table(dat, borders = "all") 
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1") %>% 
    titles("Table 1.0", "My Nice Report with Widths",
           borders = c("top", "bottom", "left", "right"),
           width = 7) %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              borders = c("top", "bottom", "left", "right"), 
               width = 7)
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})


test_that("rtf2-38: Title and Footnote specific widths work as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test38.rtf")
  
  dat <- iris[1:20, ] 
  
  tbl <- create_table(dat, borders = "all") %>% 
    titles("Table 1.0", "My Nice Report with Borders",
           borders = c("top", "bottom", "left", "right"),
            width = 7) %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              borders = c("top", "bottom", "left", "right"), 
              width = 7)
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
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

test_that("rtf2-39: One page table works as expected in centimeters and times.", {
  
  
  fp <- file.path(base_path, "rtf2/test39.rtf")
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  
  rpt <- create_report(fp, output_type = "RTF", font = "Times",
                       font_size = fsz, orientation = "landscape", 
                       units = "cm") %>%
    set_margins(top = 3, bottom = 3) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(create_table(dat)) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("rtf2-40: One page table works as expected in courier.", {
  
  
  fp <- file.path(base_path, "rtf2/test40.rtf")
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  
  rpt <- create_report(fp, output_type = "RTF", font = "Courier",
                       font_size = fsz, orientation = "landscape", 
                       units = "cm") %>%
    set_margins(top = 3, bottom = 3) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(create_table(dat)) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

# Good for testing borders and spacing are working correctly
# Need to get this working.
test_that("rtf2-41: Page by with borders works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test41.rtf")
  
  dat <- iris
  
  brdrs <- "outside"
  
  tbl <- create_table(dat, borders = "all") %>% 
    titles("Table 1.0", "My Nice Report with a Page By", borders = brdrs) %>%
    page_by(Species, label = "Species: ", align = "left", borders = brdrs,
            blank_row = "below") %>% 
    footnotes("My footnote 1", "My footnote 2", borders = brdrs)
  
  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 6)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})


# User Tests --------------------------------------------------------------

test_that("rtf2-user1: demo table works.", {
  
  if (dev) {
    library(tidyr)
    library(dplyr)
    
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "rtf2/user1.rtf")
    
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    
    data_demo <- subset(data_demo, data_demo$ARM != "SCREEN FAILURE")
    
    
    sex_decode <- c("M" = "Male",
                    "F" = "Female")
    
    race_decode <- c("WHITE" = "White",
                     "BLACK OR AFRICAN AMERICAN" = "Black or African American",
                     "ASIAN" = "Asian or Pacific Islander",
                     "NATIVE AMERICAN" = "Native American",
                     "UNKNOWN" = "Unknown")
    
    arm_pop <- table(data_demo$ARM) 
    
    
    demo_age <-
      data_demo %>%
      group_by(ARM) %>%
      summarise(across(.cols = AGE,
                       .fns = list(N      = ~ fmt_n(.),
                                   Mean   = ~ fmt_mean_sd(.),
                                   Median = ~ fmt_median(.),
                                   `Q1 - Q3` = ~ fmt_quantile_range(.),
                                   Range  = ~ fmt_range(.)
                       ))) %>%
      pivot_longer(-ARM,
                   names_to  = c("var", "label"),
                   names_sep = "_",
                   values_to = "value") %>%
      pivot_wider(names_from = ARM,
                  values_from = "value")
    
    
    
    demo_sex <-
      data_demo %>%
      add_count(ARM, SEX,  name = "n_SEX") %>%
      select(ARM, SEX, n_SEX) %>%
      distinct() %>%
      pivot_longer(cols = c(SEX),
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_SEX,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(sex_decode),
                            labels = sex_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"]))
    
    
    
    demo_race <-
      data_demo %>%
      add_count(ARM, RACE, name = "n_RACE") %>%
      select(ARM, RACE, n_RACE) %>%
      distinct() %>%
      pivot_longer(cols = RACE,
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_RACE,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(race_decode),
                            labels = race_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
      arrange(var, label)
    
    
    demo <- bind_rows(demo_age, demo_sex, demo_race)
    
    
    #View(demo)
    
    
    # Stub decode
    block_fmt <- c(AGE = "Age", SEX = "Sex", RACE = "Race")
    
    # Define table
    tbl <- create_table(demo, first_row_blank = TRUE) %>%
      column_defaults(from = "ARM A", to = "ARM D", width = 1.25) %>% 
      define(var, blank_after = TRUE, dedupe = TRUE,
             format = block_fmt, label = "") %>%
      define(label, label = "") %>%
      define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
      define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
      define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
      define(`ARM D`, align = "center", label = "Competitor", n = 38)
    
    # Define Report
    rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
      set_margins(top = 1, bottom = 1) %>% 
      options_fixed(font_size = 10) %>% 
      titles("Table 14.1/4",
             "Demographics and Baseline to Characteristics",
             "Specify Population Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼") %>%
      add_content(tbl) %>% 
      footnotes("Special symbols \U221e to mess things up: Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼") %>%   
      footnotes("Special symbols µ Ω £ there to mess things up: ", "Page [pg] of [tpg]") %>% 
      page_header("Left µ Ω £ ", "Right") %>% 
      page_footer("Time µ Ω £ ", right = "Page [pg] of [tpg]")
    
    # Write out report
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    

    
  } else 
    expect_equal(TRUE, TRUE)
  
  
})

test_that("rtf2-user2: demo table with stub works.", {
  
  
  if (dev) {
    
    library(tidyr)
    library(dplyr)
    
    
    
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "rtf2/user2.rtf")
    
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    data_demo <- subset(data_demo, data_demo$ARM != "SCREEN FAILURE")
    
    
    sex_decode <- c("M" = "Male",
                    "F" = "Female")
    
    race_decode <- c("WHITE" = "White",
                     "BLACK OR AFRICAN AMERICAN" = "Black or African American",
                     "ASIAN" = "Asian or Pacific Islander",
                     "NATIVE AMERICAN" = "Native American",
                     "UNKNOWN" = "Unknown")
    
    arm_pop <- table(data_demo$ARM) 
    
    
    demo_age <-
      data_demo %>%
      group_by(ARM) %>%
      summarise(across(.cols = AGE,
                       .fns = list(N      = ~ fmt_n(.),
                                   Mean   = ~ fmt_mean_sd(.),
                                   Median = ~ fmt_median(.),
                                   `Q1 - Q3` = ~ fmt_quantile_range(.),
                                   Range  = ~ fmt_range(.)
                       ))) %>%
      pivot_longer(-ARM,
                   names_to  = c("var", "label"),
                   names_sep = "_",
                   values_to = "value") %>%
      pivot_wider(names_from = ARM,
                  values_from = "value")
    
    
    
    demo_sex <-
      data_demo %>%
      add_count(ARM, SEX,  name = "n_SEX") %>%
      select(ARM, SEX, n_SEX) %>%
      distinct() %>%
      pivot_longer(cols = c(SEX),
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_SEX,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(sex_decode),
                            labels = sex_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"]))
    
    
    
    demo_race <-
      data_demo %>%
      add_count(ARM, RACE, name = "n_RACE") %>%
      select(ARM, RACE, n_RACE) %>%
      distinct() %>%
      pivot_longer(cols = RACE,
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_RACE,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(race_decode),
                            labels = race_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
      arrange(var, label)
    
    
    demo <- bind_rows(demo_age, demo_sex, demo_race)
    
    
    #View(demo)
    
    
    # Stub decode
    block_fmt <- c(AGE = "Age", SEX = "Sex", RACE2 = "Race")
    
    # Define table
    tbl <- create_table(demo, first_row_blank = TRUE, borders = "all") %>%
      stub(c("var", "label"), width = 2.5) %>% 
      column_defaults(width = 1) %>% 
      define(var, blank_after = TRUE, 
             format = block_fmt, label = "", label_row = TRUE) %>%
      define(label, label = "", indent = .25) %>%
      define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
      define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
      define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
      define(`ARM D`, align = "center", label = "Competitor", n = 38) %>% 
      titles("Table 14.1/4",
             "Demographics and Baseline Characteristics",
             "Specify Population", borders = "outside", blank_row = "both",
             align = "right") %>%
      footnotes("Here is a footnote", "Here is another footnote",
                borders = "outside", blank_row = "both", align = "right")  
    
    # Define Report
    rpt <- create_report(fp, output_type = "RTF", 
                         font = "Arial", font_size = 10) %>%
      add_content(tbl, align = "right") %>% 
      page_header("Sponsor", "Drug") %>% 
      page_footer(left = "Time", right = "Page [pg] of [tpg]") #%>% 
      #page_by(var = "var", label = "Variable: ")
    
    # Write out report
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    
    
  } else
    expect_equal(TRUE, TRUE)
  
})

test_that("user3: listings works.", {
  if (dev == TRUE) {
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "rtf2/user3.rtf")
    
    # Removing to make last page exactly equal to available rows on page.
    # In this case, any added blank rows should be skipped.
    fil <- c("ABC-14-124",
             "ABC-15-153",
             "ABC-15-154",
             "ABC-15-155",
             "ABC-15-156",
             "ABC-16-045",
             "ABC-16-046",
             "ABC-16-047",
             "ABC-16-157",
             "ABC-16-158",
             "ABC-16-159", 
             "ABC-16-160")
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    data_demo <- data_demo[!data_demo$USUBJID %in% fil, ]
    
    
    # Test that any assigned formats are applied
    attr(data_demo$SUBJID, "width") <- 1
    attr(data_demo$SUBJID, "justify") <- "left"
    attr(data_demo$SUBJID, "format") <- "S:%s"
    #print(widths(data_demo))
    
    # Define table
    tbl <- create_table(data_demo) %>% 
      define(USUBJID, id_var = TRUE) 
    
    
    # Define Report
    rpt <- create_report(fp, font = "Arial", font_size = 10) %>%
      titles("Listing 1.0",
             "Demographics Dataset") %>%
      add_content(tbl, align = "left") %>% 
      page_header("Sponsor", "Drug") %>% 
      page_footer(left = "Time", right = "Page [pg] of [tpg]")
    
    #Write out report
    res <- write_report(rpt, output_type = "RTF")
    
    expect_equal(file.exists(fp), TRUE)
    
  
    
    # pdfpth <- file.path(base_path, "user/user3.pdf")
    # write_report(rpt, pdfpth, output_type = "PDF")
    # expect_equal(file.exists(pdfpth), TRUE)
  } else 
    expect_equal(TRUE, TRUE)
  
  
})


test_that("user4: listing in cm and times works.", {
  if (dev == TRUE) {
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "rtf2/user4.rtf")
    
    # Removing to make last page exactly equal to available rows on page.
    # In this case, any added blank rows should be skipped.
    fil <- c("ABC-14-124",
             "ABC-15-153",
             "ABC-15-154",
             "ABC-15-155",
             "ABC-15-156",
             "ABC-16-045",
             "ABC-16-046",
             "ABC-16-047",
             "ABC-16-157",
             "ABC-16-158",
             "ABC-16-159", 
             "ABC-16-160")
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    data_demo <- data_demo[!data_demo$USUBJID %in% fil, ]
    
    
    # Test that any assigned formats are applied
    attr(data_demo$SUBJID, "width") <- 2.54
    attr(data_demo$SUBJID, "justify") <- "left"
    attr(data_demo$SUBJID, "format") <- "S:%s"
    #print(widths(data_demo))
    
    # Define table
    tbl <- create_table(data_demo) %>% 
      define(USUBJID, id_var = TRUE) 
    
    
    # Define Report
    rpt <- create_report(fp, font = "Times", font_size = 10, units = "cm") %>%
      titles("Listing 1.0",
             "Demographics Dataset") %>%
      add_content(tbl, align = "left") %>% 
      page_header("Sponsor", "Drug") %>% 
      page_footer(left = "Time", right = "Page [pg] of [tpg]")
    
    #Write out report
    res <- write_report(rpt, output_type = "RTF")
    
    expect_equal(file.exists(fp), TRUE)
    
    
    
    # pdfpth <- file.path(base_path, "user/user3.pdf")
    # write_report(rpt, pdfpth, output_type = "PDF")
    # expect_equal(file.exists(pdfpth), TRUE)
  } else 
    expect_equal(TRUE, TRUE)
  
  
})



