
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


# Basic Tests 1 - 10 ------------------------------------------------------


test_that("rtf2-0a: Fixed report is correct.", {


  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test0a.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed") %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)


})

test_that("rtf2-0b: Fixed report with font_size is correct.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test0b.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed",
                       font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})

test_that("rtf2-0c: Fixed report with font_size options is correct.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test0c.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed") %>%
    options_fixed(font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})

test_that("rtf2-0d: Fixed report with conflicting font size is correct.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test0d.rtf")

  rpt <- create_report(fp, output_type = "RTF", font = "fixed",
                       font_size = 8) %>%
    options_fixed(font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})



test_that("rtf2-1: One page text spec works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test1.rtf")

  txt <- create_text(cnt, width = 6, borders = "outside", align = "right") %>%
    titles("Text 1.0", "My Nice Text", borders = "outside", font_size = 12) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside")

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = 10) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "right") %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})



test_that("rtf2-2: Two page text spec works as expected in 12pt font.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-3: Three page text spec increased margins works as expected.", {

  if (dev == TRUE) {


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
  expect_equal(res$pages, 4)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-4: Two page text spec works as expected in 10pt font.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-5: Two page text spec works as expected in 8pt font.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-6: One page table works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test6.rtf")

  dat <- mtcars[1:15, ]
    attr(dat[[2]], "label") <- "Cylin."

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table", borders = "outside", bold = TRUE) %>%
    add_content(create_table(dat, borders = "outside")) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside") %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-7: Multi page table works as expected.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)
})



test_that("rtf2-8: Portrait table works as expected.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-9: Wide table works as expected.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)
})


test_that("rtf2-10: Preview works as expected.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

})

# Basic Tests 11 - 20 ------------------------------------------------------

# This is awesome. Shows cell wrapping, page break, and valign
# Very good for testing.
test_that("rtf2-11: Forced page wrap works as expected.", {


  fp <- file.path(base_path, "rtf2/test11.rtf")

  dat <- data.frame(labels = rownames(mtcars), mtcars)

  tbl <- create_table(dat, borders = "all") %>%
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

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

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
                       font_size = 9, orientation = "portrait") %>%
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
  expect_equal(res$pages, 6)
  expect_equal(length(res$column_widths[[1]]), 5)


})

test_that("rtf2-17: Title header on table works as expected.", {


  fp <- file.path(base_path, "rtf2/test17.rtf")

  dat <- iris[1:25, ]

  tbl <- create_table(dat, width = 8.9) %>%
    title_header("Table 1.0", "My Nice Table",
                 right = c("Right1",
                           "Right2", "Page [pg] of [tpg]")) %>%
    footnotes("My footnote 1", "My footnote 2") #%>%
    #define(Petal.Length, page_wrap = TRUE)

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

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-20: Title Header borders work as expected.", {

  if (dev == TRUE) {


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
  expect_equal(res$pages, 2)
  expect_equal(length(res$column_widths[[1]]), 5)

  } else
    expect_equal(TRUE, TRUE)


})

# Basic Tests 21 - 30 ------------------------------------------------------

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
    page_header("Left", "Right") %>%
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

  if (dev == TRUE) {


  library(ggplot2)

  fp <- file.path(base_path, "rtf2/test25.rtf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8, borders = c("top", "bottom", "all")) %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none") %>%
    footnotes("* Motor Trend, 1974", borders = "none")


  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size =fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "right") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)
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

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

})

test_that("rtf2-29: Simplest RTF Plot with valign top works as expected.", {

  if (dev) {
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

  } else {

   expect_equal(TRUE, TRUE)
  }


})



test_that("rtf2-30: Simplest RTF Plot with valign bottom works as expected.", {

  if (dev) {
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

  } else {

    expect_equal(TRUE, TRUE)
  }


})

# Basic Tests 31 - 40 ------------------------------------------------------

test_that("rtf2-31: Simplest RTF Text with valign top works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test31.rtf")

  txt <- create_text(cnt, width = 6, borders = "outside")


  rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Text 1.0", "MTCARS Miles per Cylinder Text", borders = "outside") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(txt, align = "center") %>%
    footnotes("* Motor Trend, 1974", valign = "top", borders = "outside") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})

test_that("rtf2-32: Simplest RTF Text with valign bottom works as expected.", {

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

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


  tbl1 <- create_table(df, first_row_blank = FALSE) %>%
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

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)

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

  if (dev == TRUE) {


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

  } else
    expect_equal(TRUE, TRUE)
})

# Basic Tests 41 - 50 ------------------------------------------------------

# Good for testing borders and spacing are working correctly
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
  expect_equal(res$pages, 9)
  expect_equal(length(res$column_widths[[1]]), 5)


})

# Test for borders and page wraps
test_that("rtf2-42: Long table with borders and footnotes on report.", {


  fp <- file.path(base_path, "rtf2/test42.rtf")

  dat <- data.frame(labels = rownames(mtcars), mtcars)

  tbl <- create_table(dat, borders = "all") %>%
    define(labels, id_var = TRUE) %>%
    define(wt, page_wrap = TRUE)

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "bottom")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 4)


})



test_that("rtf2-43: use_attributes parameter table works as expected.", {

  if (dev == TRUE) {


  fp1 <- file.path(base_path, "rtf2/test43a.rtf")
  fp2 <- file.path(base_path, "rtf2/test43b.rtf")
  fp3 <- file.path(base_path, "rtf2/test43c.rtf")


  dat <- mtcars[1:10, ]
  attr(dat$mpg, "label") <- "Miles per gallon"
  attr(dat$cyl, "format") <- "%.1f"
  attr(dat$hp, "width") <- 2
  fattr(dat$vs) <- list(width = 2, justify = "center")

  tbl <- create_table(dat)

  # Test default
  rpt <- create_report(fp1, output_type = "RTF", font = fnt) %>%
    add_content(tbl)

  res <- write_report(rpt)

  expect_equal(file.exists(fp1), TRUE)
  expect_equal(res$pages, 1)

  # Test none
  tbl <- create_table(dat, use_attributes = "none")

  rpt <- create_report(fp2, output_type = "RTF", font = fnt) %>%
    add_content(tbl)

  res <- write_report(rpt)

  expect_equal(file.exists(fp2), TRUE)


  # Test some
  tbl <- create_table(dat, use_attributes = c("format", "width"))

  rpt <- create_report(fp3, output_type = "RTF", font = fnt) %>%
    add_content(tbl)

  res <- write_report(rpt)

  expect_equal(file.exists(fp3), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})

test_that("rtf2-45: Title bold and font size works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test45.rtf")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"

  tbl <- create_table(dat, borders = "outside") %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"),
           width = "content", font_size = 14, bold = TRUE) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none",
              align = "left", width = "content") %>%
    define(wt, width = 1, label = "Weight", align = "center",
           label_align = "right")

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 9, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"),
                blank_row = "below") %>%
    add_content(tbl, align = "center")  %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)
})



test_that("rtf2-46: 9 pt font inches works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test46.rtf")

  rpt <- create_report(fp, output_type = "RTF", font_size = 9,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)


})

test_that("rtf2-47: 9 pt font cm works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test47.rtf")

  rpt <- create_report(fp, output_type = "RTF", font_size = 9,
                       font = "Courier",
                       orientation = "portrait",
                       units = "cm") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)

})

test_that("rtf2-48: 11 pt font inches works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test48.rtf")

  rpt <- create_report(fp, output_type = "RTF", font_size = 11,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)
})

test_that("rtf2-49: 11 pt font cm works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "rtf2/test49.rtf")

  rpt <- create_report(fp, output_type = "RTF", font_size = 11,
                       font = "Courier",
                       orientation = "portrait",
                       units = "cm") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)

})

test_that("rtf2-50: Spanning headers borders work as expected.", {


  fp <- file.path(base_path, "rtf2/test50.rtf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c("outside")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>%
    spanning_header(hp, wt, "Span 2", underline = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span", n = 11, level = 2) %>%
    titles("Table 1.0", "My Nice Table", borders = "outside") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside")

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl)


  res <- write_report(rpt)
  res
  res$column_widths

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})


# Basic Tests 51-60 -------------------------------------------------------


test_that("rtf2-51: RTF Image file works as expected.", {

  if (dev == TRUE) {


  library(ggplot2)

  fp <- file.path(base_path, "rtf2/test51.rtf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  pltpath <- file.path(base_path, "rtf2/test51.jpg")
  ggsave(pltpath, width = 8, height = 4,
         units = "in",
         dpi = 300)

  plt <- create_plot(pltpath, height = 4, width = 8)


  rpt <- create_report(fp, output_type = "RTF", font = "Arial") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)
})

test_that("rtf2-52: Table with break between sections works as expected.", {


  fp <- file.path(base_path, "rtf2/test52.rtf")


  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B
      "ampg"   "\\fi-87\\li267 N"          "19"          "13"
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Median"     "16.4"        "21.4"
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE) %>%
    stub(c("var", "label")) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19) %>%
    define(B, label = "Group B", align = "center", n = 13)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                       font = "Times") %>%
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
    titles("Table 1.0", "MTCARS Summary Table") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left",
                center = "Confidential",
                right = "Page [pg] of [tpg]")



  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)


})

test_that("rtf2-53: Text with line feed works as expected.", {

  fp <- file.path(base_path, "rtf2/test53.rtf")

  cnt2 <- paste0("Hello here \nis something ", cnt)

  txt <- create_text(cnt2) %>%
    titles("Report 1.0", "Simple Text Report") %>%
    footnotes("My footnote")

  rpt <- create_report(fp, orientation = "portrait",
                       output_type = "RTF", font = "Arial") %>%
    add_content(txt)

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
})


test_that("rtf2-54: Header_bold works as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "rtf2/test54.rtf")

  tbl <- create_table(dat, header_bold = TRUE, borders = "all") %>%
    column_defaults(width = 1) %>%
    titles("Report 1.0", "Simple Report", borders = "outside",
           blank_row = "none", bold = TRUE) %>%
    footnotes("My footnote", blank_row = "none")

  rpt <- create_report(fp, orientation = "portrait",
                       output_type = "RTF", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1)

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("rtf2-55: Titles and footnotes in header and footer works as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "rtf2/test55.rtf")

  tbl <- create_table(dat) %>%
    column_defaults(width = 1)

  rpt <- create_report(fp, orientation = "landscape",
                       output_type = "RTF", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    titles("Report 1.0", "Simple Report",
           blank_row = "none", header = TRUE, columns = 1) %>%
    footnotes("My footnote", "Another footnote", "And another",
              blank_row = "none", footer = TRUE) %>%
    page_footer("Left", "Center", "Right")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("rtf2-56: Titles and footnotes variations in header and footer work as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "rtf2/test56.rtf")

  tbl <- create_table(dat) %>%
    column_defaults(width = 1)

  rpt <- create_report(fp, orientation = "landscape",
                       output_type = "RTF", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1) %>%
    page_header("Left", "Right") %>%
    titles("Report 1.0", "Simple Report", align = "left", width = 6,
           blank_row = "none", header = TRUE) %>%
    titles("Report 1.0", "Simple Report", align = "right", width = 6,
           blank_row = "below", header = TRUE, borders = "bottom") %>%
    footnotes("My footnote", blank_row = "none", footer = TRUE, borders = "top") %>%
    footnotes("My footnote2", blank_row = "none", footer = TRUE, align = "right") %>%
    footnotes("My footnote3", blank_row = "none", footer = TRUE, align = "center") %>%
    page_footer("Left", "Center", "Right")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("rtf2-57: Titles and footnotes in header and footer no page header/footer works as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "rtf2/test57.rtf")

  tbl <- create_table(dat) %>%
    column_defaults(width = 1)

  rpt <- create_report(fp, orientation = "landscape",
                       output_type = "RTF", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1, bottom = 1) %>%
    titles("Report 1.0 here is a big long title", "Simple Report", align = "left",
           blank_row = "none", header = TRUE) %>%
    titles("Report 1.0", "Simple Report", align = "center", width = 6,
           blank_row = "below", header = TRUE) %>%
    footnotes("My footnote1", "My footnote2", blank_row = "none", footer = TRUE)

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("rtf2-58: Label row is one cell.", {


  fp <- file.path(base_path, "rtf2/test58.rtf")


  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B
      "ampg"   "N"          "19"          "13"
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Median"     "16.4"        "21.4"
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
      "cyl"    "6 Cylinder and more perhaps more" "4 ( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')

  ll <- "Here is a super long label to see if it can span the entire table."

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>%
    stub(c("var", "label"), width = .8) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = ll, cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19) %>%
    define(B, label = "Group B", align = "center", n = 13)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                       font = "Times") %>%
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
    titles("Table 1.0", "MTCARS Summary Table") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left",
                center = "Confidential",
                right = "Page [pg] of [tpg]")



  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)


})

test_that("rtf2-59: Blank after on invisible column.", {

  fp <- file.path(base_path, "rtf2/test59.rtf")

  tbl <- create_table(iris, borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


test_that("rtf2-60: Blank nested stub works as expected.", {


  fp <- file.path(base_path, "rtf2/test60.rtf")


  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B
      "ampg"   "Stats"    "19"          "13"
      "ampg"   "Stats"    "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Stats"    "16.4"        "21.4"
      "ampg"   "Stats"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Stats"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
      "cyl"    "" "4 ( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "" "5 ( 26.3%)"  "6 ( 46.2%)"')

  ll <- "Here is a super long label to see if it can span the entire table."

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>%
    stub(c("var", "label")) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = ll, cyl = "Cylinders")) %>%
    define(label, indent = .25, dedupe = TRUE) %>%
    define(A, label = "Group A", align = "center", n = 19) %>%
    define(B, label = "Group B", align = "center", n = 13)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                       font = "Times") %>%
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
    titles("Table 1.0", "MTCARS Summary Table") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left",
                center = "Confidential",
                right = "Page [pg] of [tpg]")



  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)

})


# Basic Tests 61-70 -------------------------------------------------------



test_that("rtf2-61: Page header width works.", {

  fp <- file.path(base_path, "rtf2/test61.rtf")

  tbl <- create_table(iris, borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    page_header("Left and here is a really long left cell text to put it",
                "Right", width = 8) %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-62: Carriage return in label row works.", {


  fp <- file.path(base_path, "rtf2/test62.rtf")


  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B
      "ampg"   "N"          "19"          "13"
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Median"     "16.4"        "21.4"
      "ampg"   "Q1 - Q3"    "15.1 - \n21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')

  ll <- "Here is a super long label to \nsee if it can span the entire table."

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>%
    stub(c("var", "label")) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = ll, cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19) %>%
    define(B, label = "Group B", align = "center", n = 13)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                       font = "Times") %>%
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
    titles("Table 1.0", "MTCARS Summary Table") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left",
                center = "Confidential",
                right = "Page [pg] of [tpg]")



  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)


})


test_that("rtf2-63: Glue works.", {

  # if (dev) {

    fp <- file.path(base_path, "rtf2/test63.rtf")


    # Read in prepared data
    df <- read.table(header = TRUE, text = '
        var     label        A             B
        "ampg"   "N"          "19"          "13"
        "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
        "ampg"   "Median"     "16.4"        "21.4"
        "ampg"   "Q1 - Q3"    "15.1 - \n21.2" "19.2 - 22.8"
        "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
        "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)"
        "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)"
        "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')

    ll <- "Here is a super long label to \nsee if it can span the entire table."

    # Create table
    tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>%
      stub(c("var", "label"), label = "Stub{supsc('2')}") %>%
      define(var, blank_after = TRUE, label_row = TRUE,
             format = c(ampg = ll, cyl = "Cylinders")) %>%
      define(label, indent = .25) %>%
      define(A, label = "Group A{supsc('1')}", align = "center", n = 19) %>%
      define(B, label = "Group B", align = "center", n = 13)


    # Create report and add content
    rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                         font = "Times") %>%
      page_header(left = c("Client: Motor Trend{supsc('6')}",
                           "Test{supsc('8')}"), right = "Study: Cars") %>%
      titles("Table 1.0", "MTCARS Summary Table{supsc('3')}") %>%
      add_content(tbl) %>%
      footnotes("* Motor Trend, 1974{supsc('4')}") %>%
      page_footer(left = "Left{supsc('6')}",
                  center = "Confidential",
                  right = "Page [pg] of [tpg]")



    res <- write_report(rpt)
    res
    expect_equal(file.exists(fp), TRUE)

  # } else {
  # 
  #   expect_equal(TRUE, TRUE)
  # 
  # }


})


test_that("rtf2-64: Title columns work 1 column.", {

  fp <- file.path(base_path, "rtf2/test64.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below", columns =  1, align = "left") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-65: Title columns work 2 columns.", {

  fp <- file.path(base_path, "rtf2/test65.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame", "Left", "Right",
           blank_row = "below", columns =  2) %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-66: Title columns work 3 columns.", {

  fp <- file.path(base_path, "rtf2/test66.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame", "My right thing", "", "Center",
           blank_row = "below", columns =  3) %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-67: Multiple title blocks work as expected.", {

  fp <- file.path(base_path, "rtf2/test67.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below", columns =  1, align = "center", width = 7,
           borders = "all") %>%
    titles("Table 2.0", "IRIS Data Frame2", "Left", "Right",
           blank_row = "below", columns =  2, borders = "all") %>%
    titles("Table 3.0", "IRIS Data Frame3", "My right thing", "", "Center",
           blank_row = "below", columns =  3, borders = "all") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-68: Custom page size works as expected.", {

  fp <- file.path(base_path, "rtf2/test68.rtf")

  tbl <- create_table(iris[1:15, ]) %>%
    define(Species, visible = FALSE)

  ttl <- c("Title1", "Title2", "Title3")

  rpt <- create_report(fp, output_type = "RTF",
                       font = "Courier",
                       paper_size = c(6.5, 7.5),
                       orientation = "portrait") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles(ttl,
           blank_row = "below", columns =  1, align = "center",
           borders = "none") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


test_that("rtf2-69: Breaks removed after titles and footnotes.", {

  fp <- file.path(base_path, "rtf2/test69.rtf")

  tbl <- create_table(iris[1:15, ], borders = "outside") %>%
    define(Species, visible = FALSE) %>%
    titles("Here is a title") %>%
    footnotes("Here is a footnote")


  rpt <- create_report(fp, output_type = "RTF",
                       font = "Courier",
                       orientation = "landscape") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")



  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


test_that("rtf2-70: Breaks removed after 2 titles and footnotes.", {

  fp <- file.path(base_path, "rtf2/test70.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, visible = FALSE) %>%
    titles("Here is a title") %>%
    titles("There is a title") %>%
    footnotes("Here is a footnote") %>%
    footnotes("There is a footnote")


  rpt <- create_report(fp, output_type = "RTF",
                       font = "Courier",
                       orientation = "landscape") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")



  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


# Basic Tests 71-80 -------------------------------------------------------


test_that("rtf2-71: RTF Plot with path logs as expected.", {

  if (dev == TRUE) {


    fp <- file.path(base_path, "rtf2/test71.rtf")

    tmp <- file.path(base_path, "plot.jpg")

    jpeg(tmp, width = 600, height = 500)

    plot(mtcars$mpg)


    dev.off()

    plt <- create_plot(tmp, height = 5, width = 6, borders = c("top", "bottom", "all")) %>%
      titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none") %>%
      footnotes("* Motor Trend, 1974", borders = "none")


    rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size =fsz) %>%
      page_header("Client", "Study: XYZ") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(plt) %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    #print(res)

    #print(res)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)
})



test_that("rtf2-72: Symbols are proper orientation on portrait.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test72.rtf")

    dat <- mtcars[1:10, ]


    tbl <- create_table(dat) %>%
      titles("My Table{symbol('dagger')}")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl)

    res <- write_report(rpt)


    #file.show(res$modified_path)

    expect_equal(file.exists(fp), TRUE)


  } else {

    expect_equal(TRUE, TRUE)

  }

})


test_that("rtf2-73: Symbols are proper orientation on landscape.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test73.rtf")

    dat <- mtcars[1:10, ]


    tbl <- create_table(dat) %>%
      titles("My Table{symbol('dagger')}")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "landscape") %>%
      add_content(tbl)

    res <- write_report(rpt)


    #file.show(res$modified_path)

    expect_equal(file.exists(fp), TRUE)


  } else {

    expect_equal(TRUE, TRUE)

  }

})


test_that("rtf2-74: Multi-page continuous tables work as expected.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test74.rtf")

    dat <- iris


    tbl <- create_table(dat, continuous = TRUE, borders = "all") %>%
     # titles("My Title") %>%
      footnotes("My footnotes", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE)

    res <- write_report(rpt)


   # file.show(res$modified_path)

    expect_equal(file.exists(fp), TRUE)
   # expect_equal(res$pages, 3)


  } else {

    expect_equal(TRUE, TRUE)

  }

})

# Works!
test_that("rtf2-75: Simplest EMF Plot works as expected.", {

  if (dev == TRUE) {

    library(devEMF)

    fp <- file.path(base_path, "rtf2/test75.rtf")

    tmp <- file.path(base_path, "rtf2/plot.emf")

    emf(tmp)

    plot(1, 1)


    dev.off()

    plt <- create_plot(tmp, height = 5, width = 6, borders = c("top", "bottom", "all")) %>%
      titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none") %>%
      footnotes("* Motor Trend, 1974", borders = "none")


    rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size =fsz) %>%
      page_header("Client", "Study: XYZ") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(plt) %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    #print(res)

    #print(res)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)
})

# Works!
test_that("rtf2-76: Patchwork Plot works as expected.", {

  if (dev == TRUE) {


    library(ggplot2)
    library(patchwork)

    fp <- file.path(base_path, "rtf2/test76.rtf")

    p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

    p2 <-  ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

    ptch <- (p | p2)

    plt <- create_plot(ptch, height = 4, width = 8, borders = c("top", "bottom", "all")) %>%
      titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none") %>%
      footnotes("* Motor Trend, 1974", borders = "none")


    rpt <- create_report(fp, output_type = "RTF", font = fnt, font_size =fsz) %>%
      page_header("Client", "Study: XYZ") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(plt, align = "right") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    #print(res)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("rtf2-77: One-page continuous tables work as expected.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test77.rtf")

    dat <- iris[1:15, ]


    tbl <- create_table(dat, continuous = TRUE, borders = "all") %>%
      titles("My title") %>%
      footnotes("My footnotes", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE)

    res <- write_report(rpt)


   # file.show(res$modified_path)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)


  } else {

    expect_equal(TRUE, TRUE)

  }

})


test_that("rtf2-78: Two one-page continuous tables work as expected.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test78.rtf")

    dat <- iris[1:15, ]


    tbl1 <- create_table(dat, continuous = TRUE, borders = "all") %>%
      titles("My title 1") %>%
      footnotes("My footnotes 1", blank_row = "none")

    tbl2 <- create_table(dat, continuous = TRUE, borders = "all") %>%
      titles("My title 2") %>%
      footnotes("My footnotes 2", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl1) %>%
      add_content(tbl2) %>%
      footnotes("Here", footer = TRUE)

    res <- write_report(rpt)


   # file.show(res$modified_path)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 2)


  } else {

    expect_equal(TRUE, TRUE)

  }

})


test_that("rtf2-79: Two multi-page continuous tables work as expected.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test79.rtf")

    dat <- iris[1:125, ]


    tbl1 <- create_table(dat, continuous = TRUE, borders = "all") %>%
      titles("My title 1") %>%
      footnotes("My footnotes 1", blank_row = "none")

    tbl2 <- create_table(dat, continuous = TRUE, borders = "all") %>%
      titles("My title 2") %>%
      footnotes("My footnotes 2", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl1) %>%
      add_content(tbl2) %>%
      footnotes("Here", footer = TRUE) %>%
      page_header("Left", "Right") %>%
      page_footer(right = "Page [pg] of [tpg]")

    res <- write_report(rpt)


   # file.show(res$modified_path)

    expect_equal(file.exists(fp), TRUE)
    #expect_equal(res$pages, 6)


  } else {

    expect_equal(TRUE, TRUE)

  }

})

test_that("rtf2-80: Two tables one continuous works as expected.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test80.rtf")

    dat <- iris[1:125, ]


    tbl1 <- create_table(dat, continuous = TRUE, borders = "all") %>%
      titles("My title 1") %>%
      footnotes("My footnotes 1", blank_row = "none")

    tbl2 <- create_table(dat, continuous = FALSE, borders = "all") %>%
      titles("My title 2") %>%
      footnotes("My footnotes 2", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl1) %>%
      add_content(tbl2) %>%
      footnotes("Here", footer = TRUE) %>%
      page_header("Left", "Right") %>%
      page_footer(right = "Page [pg] of [tpg]")

    res <- write_report(rpt)


 #   file.show(res$modified_path)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 7)


  } else {

    expect_equal(TRUE, TRUE)

  }

})


# Basic Tests 81-90 -------------------------------------------------------



# Can't reproduce Raphael's problem
test_that("rtf2-81: Column widths work as expected.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test81.rtf")

    dat <- mtcars[, 1:9]


    tbl <- create_table(dat, width = 8.84) %>%
      titles("My title") %>%
      column_defaults(width = .75) %>%
      define(mpg, width = 2.09) %>%
      define(cyl, width = 1.5) %>%
      footnotes("My footnotes", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait",
                         paper_size = c(9.5, 11.5)) %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE) %>%
      set_margins(top = .25, bottom = .25, left = .25, right = .25)

    res <- write_report(rpt)


    #file.show(res$modified_path)

    res$column_widths

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)


  } else {

    expect_equal(TRUE, TRUE)

  }

})


test_that("rtf2-82: Basic cell style bold works as expected.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test82.rtf")

    dat <- mtcars[, 1:5]
    dat$hpflg <- ifelse(dat$hp > 100, TRUE, FALSE)


    tbl <- create_table(dat, width = 7) %>%
      titles("My title") %>%
      column_defaults(width = .75) %>%
      define(mpg, width = 1, style = cell_style(bold = TRUE)) %>%
      define(cyl, width = 1) %>%
      define(disp, style = cell_style(bold = TRUE)) %>%
      define(hp, style = cell_style(bold = TRUE, indicator = hpflg)) %>%
      define(hpflg, visible = FALSE) %>%
      footnotes("My footnotes", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE)

    res <- write_report(rpt)


    # file.show(res$modified_path)

    res$column_widths

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)


  } else {

    expect_equal(TRUE, TRUE)

  }

})

test_that("rtf2-83: Bolding works with stub.", {


  fp <- file.path(base_path, "rtf2/test83.rtf")


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

  df$cylflg <- ifelse(df$var == "cyl", TRUE, FALSE)

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE) %>%
    column_defaults(vars = c("stub", "A"),
                    style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    stub(c("var", "label"),
         style = cell_style(bold = TRUE, indicator = "labelrow")) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19,
           style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    define(B, label = "Group B", align = "center", n = 13,
           style = cell_style(bold = TRUE, indicator = "datarow")) %>%
    define(cylflg, visible = FALSE)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                       font = "Times") %>%
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
    titles("Table 1.0", "MTCARS Summary Table") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left",
                center = "Confidential",
                right = "Page [pg] of [tpg]")



  res <- write_report(rpt)

  # file.show(res$modified_path)
  res
  expect_equal(file.exists(fp), TRUE)


})


test_that("rtf2-84: Bold cell style with column defaults.", {

  if (dev) {

    fp <- file.path(base_path, "rtf2/test84.rtf")

    dat <- mtcars[, 1:5]
    dat$hpflg <- ifelse(dat$hp > 100, TRUE, FALSE)


    tbl <- create_table(dat) %>%
      titles("My title") %>%
      column_defaults(width = .75, vars = c("cyl", "disp", "hp"),
                      style = cell_style(bold=TRUE, indicator = hpflg)) %>%
      define(mpg) %>%
      define(cyl) %>%
      define(disp) %>%
      define(hp) %>%
      define(hpflg, visible = FALSE) %>%
      footnotes("My footnotes", blank_row = "none")


    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE)

    res <- write_report(rpt)


   # file.show(res$modified_path)

    res$column_widths

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)


  } else {

    expect_equal(TRUE, TRUE)

  }

})


test_that("rtf2-85: Bolding, column defaults, and stub works.", {


  fp <- file.path(base_path, "rtf2/test85.rtf")


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

  df$cylflg <- ifelse(df$var == "cyl", FALSE, TRUE)

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE) %>%
    column_defaults(style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    stub(c("var", "label"), width = 2,
         style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19) %>%
    define(B, label = "Group B", align = "center", n = 13,
           style = cell_style(bold = TRUE, indicator = "datarow")) %>%
    define(cylflg, visible = FALSE)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                       font = "Times") %>%
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
    titles("Table 1.0", "MTCARS Summary Table") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left",
                center = "Confidential",
                right = "Page [pg] of [tpg]")

  res <- write_report(rpt)

  # file.show(res$modified_path)
  res
  expect_equal(file.exists(fp), TRUE)


})



test_that("rtf2-86: Spanning header bold work as expected.", {


  fp <- file.path(base_path, "rtf2/test86.rtf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c( "none")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>%
    spanning_header(hp, wt, "Span 2", underline = FALSE, bold = TRUE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span",
                    n = 11, level = 2, bold = TRUE) %>%
    titles("Table 1.0", "My Nice Table", blank_row = "none",
           borders = c("top", "bottom")) %>%
    footnotes("My footnote 1", "My footnote 2",
              blank_row = "none", borders = c("top", "bottom"))

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)
  res
  res$column_widths

  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 1)

})


test_that("rtf2-87: Italic footnotes work as expected.", {


  fp <- file.path(base_path, "rtf2/test87.rtf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c( "none")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>%
    spanning_header(hp, wt, "Span 2", underline = FALSE, bold = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span",
                    n = 11, level = 2, bold = FALSE) %>%
    titles("Table 1.0", "My Nice Table", blank_row = "none",
           borders = c("top", "bottom")) %>%
    footnotes("My italic footnote1", "My italic footnote2", italics = TRUE,
              blank_row = "none", borders = "top") %>%
    footnotes("My italic footnote2", "My footnote 2",
              blank_row = "none", borders = c("bottom"))

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)
  res
  res$column_widths

  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 1)

})


test_that("rtf2-88: Footnote columns work 1 column.", {

  fp <- file.path(base_path, "rtf2/test88.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)  %>%
    footnotes("Here is a footnote", "And another",
               borders = "all", columns = 1)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below", columns =  1, align = "left") %>%
    footnotes("Here is a footnote", "And another", columns = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-89: Footnote columns work 2 columns.", {

  fp <- file.path(base_path, "rtf2/test89.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = FALSE, visible = FALSE) %>%
    footnotes("Here is a footnote", "And another",
              borders = "all", columns = 2)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame", "Left", "Right",
           blank_row = "below", columns =  2) %>%
    footnotes("Here is a footnote that is really long and is going to wrap",
              "And another",
              "Another left", "Another right", columns = 2)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-90: Footnote columns work 3 columns.", {

  fp <- file.path(base_path, "rtf2/test90.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = FALSE, visible = FALSE)  %>%
    footnotes("Here is a footnote", "And another", "And more",
              "", "centered",
              borders = "all", columns = 3, blank_row = "both")

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame", "My right thing", "", "Center",
           blank_row = "below", columns =  3) %>%
    footnotes("Here is a footnote that is really long an is going to wrap a few times",
              "And another", "and more",
              "", "Footnote Center", columns = 3)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


# Basic Tests 91-100 ------------------------------------------------------



test_that("rtf2-91: Multiple footnote blocks work as expected.", {

  fp <- file.path(base_path, "rtf2/test91.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE) %>%
    footnotes("Footnote left", "Footnote right", columns = 2)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Table 1.0", "IRIS Data Frame",
           blank_row = "below", columns =  1, align = "center", width = 7,
           borders = "all") %>%
    footnotes("Table 2.0", "IRIS Data Frame2", "Left", "Right",
           blank_row = "below", columns =  2, borders = "all") %>%
    footnotes("Table 3.0", "IRIS Data Frame3", "My right thing", "", "Center",
           blank_row = "below", columns =  3, borders = "all") %>%
    titles("Here is a title", "And another")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("rtf2-92: Page by with wrap works as expected.", {


  fp <- file.path(base_path, "rtf2/test92.rtf")

  dat <- iris
  dat$Pgby <- as.character(dat$Species)
  dat$Pgby <- paste0("Flower Type\n", dat$Pgby)


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, label = "Species: ", align = "right", borders = "none") %>%
    define(Pgby, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_header("Left", "Right") %>%
    page_footer("Left1", "Center1", "Right1") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none")

  res <- write_report(rpt)
  res
  res$column_widths

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 9)
  expect_equal(length(res$column_widths[[1]]), 5)


})

test_that("rtf2-93: Page by with wrap works as expected.", {


  fp <- file.path(base_path, "rtf2/test93.rtf")

  fmt1 <- c(setosa = 1, versicolor = 2, virginica = 3)
  fmt2 <- value(condition(x == 1, "Setosa"),
                condition(x == 2, "Versicolor"),
                condition(x == 3, "Virginica"))

  dat <- iris
  fmtval <- fmt1[dat$Species]
  names(fmtval) <- NULL
  dat$Pgby <- fmtval

  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, align = "left", label = "Flower:", borders = "none", format = fmt2) %>%
    define(Pgby, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF",
                       orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_header("Left", "Right") %>%
    page_footer("Left1", "Center1", "Right1") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none")

  res <- write_report(rpt)
  res
  res$column_widths

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 6)
  expect_equal(length(res$column_widths[[1]]), 5)


})

test_that("test94: Label with invisible column works as expected.", {

  fp <- file.path(base_path, "rtf2/test94.rtf")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE, label = "Fork") %>%
    footnotes("Left", "right", columns = 2)

  rpt <- create_report(fp, output_type = "RTF") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Footnote1", "IRIS Data Frame",
              blank_row = "below", columns =  1, align = "center", width = 7,
              borders = "all") %>%
    titles("Table 1.0", "My little title")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


test_that("test95: Page break with blank row after works as expected.", {

  fp <- file.path(base_path, "rtf2/test95.rtf")

  dat <- sort(iris, by = c("Species", "Petal.Width"))

  PG <- c(rep(1, 25), rep(2, 25),
               rep(4, 25), rep(5, 25),
               rep(7, 25), rep(8, 25))
  dat$PG <- PG


  tbl <- create_table(dat, borders = "all") %>%
    stub(c("Species", "Petal.Width"), label = "My stuff") %>%
    define(Species, visible = TRUE, label_row = TRUE) %>%
    define(Petal.Width, blank_after = TRUE, indent = .25) %>%
    define(PG, page_break = TRUE, visible = TRUE) %>%
    footnotes("Left", "right", columns = 2)

  rpt <- create_report(fp, output_type = "RTF", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "Page [pg] of [tpg]") %>%
    footnotes("Footnote1", "IRIS Data Frame",
              blank_row = "below", columns =  1, align = "center", width = 7,
              borders = "all") %>%
    titles("Table 1.0", "My little title")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 11)

})

test_that("rtf2-96: Outside borders on continuous tables work as expected.", {
  
  if (dev) {
    
    fp <- file.path(base_path, "rtf2/test96.rtf")
    
    dat <- iris
    
    
    tbl <- create_table(dat, continuous = TRUE, borders = "outside") %>%
      # titles("My Title") %>%
      footnotes("My footnotes", blank_row = "none")
    
    
    rpt <- create_report(fp, output_type = "RTF",
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE)
    
    res <- write_report(rpt)
    
    
    # file.show(res$modified_path)
    
    expect_equal(file.exists(fp), TRUE)
    # expect_equal(res$pages, 3)
    
    
  } else {
    
    expect_equal(TRUE, TRUE)
    
  }
  
})



# User Tests --------------------------------------------------------------

test_that("user1: demo table works.", {

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
    tbl <- create_table(demo, first_row_blank = TRUE, borders = c("outside")) %>%
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
      footnotes("Special symbols \U221e to mess things up: Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼 标题") %>%
      footnotes("Special symbols µ Ω £ there to mess things up: ", "Page [pg] of [tpg]",
                align = 'right', italics = TRUE) %>%
      page_header("Left µ Ω £ ", "Right") %>%
      page_footer("Time µ Ω £ ", right = "Page [pg] of [tpg]")

    # Write out report
    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)



  } else
    expect_equal(TRUE, TRUE)


})

test_that("user2: demo table with stub works.", {


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

    race_decode <- c("WHITE" = "White more",
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
      stub(c("var", "label"), width = 1.5) %>%
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
    names(data_demo)
    # Define table
    tbl <- create_table(data_demo) %>%
      define(USUBJID, id_var = TRUE)


    # Define Report
    rpt <- create_report(fp, font = "Times", font_size = 8,
                         orientation = "landscape") %>%
      titles("Listing 1.0",
             "Demographics Dataset") %>%
      add_content(tbl, align = "left") %>%
      page_header("Sponsor", "Drug") %>%
      page_footer(left = "Time", right = "Page [pg] of [tpg]") %>%
      footnotes("My footnotes")

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
    rpt <- create_report(fp, font = "Arial", font_size = 10, units = "cm",
                         orientation = "portrait") %>%
      titles("Listing 1.0",
             "Demographics Dataset") %>%
      add_content(tbl, align = "left") %>%
      page_header("Sponsor", "Drug") %>%
      page_footer(left = "Time", right = "Page [pg] of [tpg]") %>%
      footnotes("My footnote")

    #Write out report
    res <- write_report(rpt, output_type = "RTF")

    #print(res$column_widths)

    expect_equal(file.exists(fp), TRUE)



    # pdfpth <- file.path(base_path, "user/user3.pdf")
    # write_report(rpt, pdfpth, output_type = "PDF")
    # expect_equal(file.exists(pdfpth), TRUE)
  } else
    expect_equal(TRUE, TRUE)


})

test_that("user5: Portrait in 12pt Arial works as expected.", {

  if (dev == TRUE) {

    dir_data <- file.path(data_dir, "data")

    fp <- file.path(base_path, "rtf2/user5.rtf")

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

    # Create table
    tbl <- create_table(df, first_row_blank = TRUE) %>%
      define(var, label = "Variable", blank_after = TRUE, dedupe = TRUE,
             format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>%
      define(label, label = "") %>%
      define(A, label = "Group A", align = "center", n = 19) %>%
      define(B, label = "Group B", align = "center", n = 13)


    # Create report and add content
    rpt <- create_report(fp, orientation = "portrait", output_type = "RTF",
                         font = "Arial", font_size = 12) %>%
      page_header(left = "Client: Motor Trend", right = "Study: Cars") %>%
      titles("Table 1.0", "MTCARS Summary Table") %>%
      add_content(tbl) %>%
      footnotes("* Motor Trend, 1974") %>%
      page_footer(left = "Left",
                  center = "Confidential",
                  right = "Page [pg] of [tpg]")

    # Write out report
    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("user6: Borders with spanning headers work as expected.", {

  if (dev == TRUE) {

  df <- read.table(header = TRUE, text = '
      var     label           A             B             C
      "AGE"   "n"             "19"          "13"          "32"
      "AGE"   "Mean"          "18.8 (6.5)"  "22.0 (4.9)"  "20.0 (5.9)"
      "AGE"   "Median"        "16.4"        "21.4"        "20.1"
      "AGE"   "Q1 - Q3"       "15.1 - 21.2" "19.2 - 22.8" "15.2 - 21.8"
      "RACE"  "White"         "10 (52.6)"   "4 (30.8)"    "14 (43.8)"
      "RACE"  "Black"         "4 (21.1)"    "3 (23.1)"    "7 (21.9)"
      "RACE"  "Others\U1D47"  "5 (26.3)"    "6 (46.2)"    "11 (34.4.2)"
      ')

  var_fmt <- c(AGE = "Age (yrs)", RACE = "Race - n (%)")


  fp <- file.path(base_path, "rtf2/user6.rtf")

  tbl <- create_table(df, first_row_blank = TRUE, borders = c("outside")) %>%
    stub(vars = c("var", "label"), " ", width = 2.5) %>%
    spanning_header(from = "A", to = "B", label = "Treatments\U1D43") %>%
    define(var, blank_after = TRUE, format = var_fmt, label_row = TRUE) %>%
    define(label, indent=0.25) %>%
    define(A,  align = "center", label = "Placebo\n(N = 19)") %>%
    define(B,  align = "center", label = "Drug\n(N = 13)") %>%
    define(C,  align = "center", label = "Total\n(N = 32)") %>%
    footnotes("\U1D43 study drug", blank_row="none" ) %>%
    footnotes("\U1D47 Asian, Japanese and Chinese", blank_row="none")

  rpt <- create_report(fp, output_type = "RTF",
                       font = "Arial") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer(left = paste("Date:", "Left"), right = "Page [pg] of [tpg]", blank_row="none") %>%
    footnotes("Program: C:/Users/Home/AppData/Local/Temp/tdemo.R", blank_row="above")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  # file.show(fp)


  } else
    expect_equal(TRUE, TRUE)

})

# This is good
test_that("user7: Check footnotes on page by.", {

  if (dev == TRUE) {

  fp <- file.path(base_path, "rtf2/user7")


  df <- read.table(header = TRUE, text = '
      var     stat        A             B
      "Age (yrs)"   "n"          "19"          "13"
      " "   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
      " "   "Median"     "16.4"        "21.4"
      " "   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      " "   " "          " "           " "
      "Race"  "White" "10 ( 52.6%)" "4 ( 30.8%)"
      " "     "Black" "4 ( 21.1%)"  "3 ( 23.1%)"
      " "     "Others\U1D47" "5 ( 26.3%)"  "6 ( 46.2%)"')

  df1 <- df
  df2 <- df
  df3 <- df

  df1$SEX <- 'Female'
  df2$SEX <- 'Male'
  df3$SEX <- 'Other'

  df <- rbind(df1, df2, df3)

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders=c("top")) %>%
    page_by(SEX, "Sex: ", align = "left", blank_row="none") %>%
    # stub(c("var", "label")) %>%
    column_defaults(width = 1.25) %>%

    spanning_header(from = "A", to = "B", label = "Treatments\U1D43") %>%

    define(SEX, visible = FALSE) %>%
    define(var, label = " ", align = "left") %>%
    define(stat,label = " ", align = "left") %>%
    define(A,   label = "Treament A", align = "center", n = 19) %>%

    define(B,   label = "Treament B", align = "center", n = 13)  %>%
    titles("Table 1.1 Demographics", "Randomised Population", font_size = 10) %>%

    footnotes("Page [pg] of [tpg]", align = "right", blank_row="none", borders=c("top")) %>%
    footnotes("\U1D43 study drug treatments", blank_row="none", borders=c("top")  ) %>%
    footnotes("\U1D47 Asian, Japanese and Chinese", blank_row="none", borders="bottom")

  rpt <- create_report(fp, output_type = "RTF", font = "Arial", font_size = 10) %>%

    #This is page header and it goes into the header of the table
    page_header("Protocol: 9999") %>%

    add_content(tbl) %>%

    page_footer(left = paste("Date:", "Left"), right = "Page [pg] of [tpg]", blank_row="none") %>%
    footnotes("Program: C:/Users/Home/AppData/Local/Temp/tdemo.R", blank_row="above", valign = "bottom")



    res <- write_report(rpt)

    expect_equal(file.exists(res$modified_path), TRUE)
    expect_equal(res$pages, 3)

  } else
    expect_equal(TRUE, TRUE)

})

