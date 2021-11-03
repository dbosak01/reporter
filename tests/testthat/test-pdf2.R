
context("PDF2 Tests")

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



test_that("pdf2-0a: Fixed report is correct.", {
  
  
  fp <- file.path(base_path, "pdf2/test0a.pdf")
  
  rpt <- create_report(fp, output_type = "PDF", font = "fixed") %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


test_that("pdf2-0b: Fixed report with font_size is correct.", {


  fp <- file.path(base_path, "pdf2/test0b.pdf")

  rpt <- create_report(fp, output_type = "PDF", font = "fixed",
                       font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})

test_that("pdf2-0c: Fixed report with font_size options is correct.", {


  fp <- file.path(base_path, "pdf2/test0c.pdf")

  rpt <- create_report(fp, output_type = "PDF", font = "fixed") %>%
    options_fixed(font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})

test_that("pdf2-0d: Fixed report with conflicting font size is correct.", {


  fp <- file.path(base_path, "pdf2/test0d.pdf")

  rpt <- create_report(fp, output_type = "PDF", font = "fixed",
                       font_size = 8) %>%
    options_fixed(font_size = 12) %>%
    titles("Table 0.0", "Baseline Characteristics") %>%
    add_content(create_table(mtcars[1:10, ]))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})





test_that("pdf2-1a: Simple report with titles on report is correct.", {
  
  
  fp <- file.path(base_path, "pdf2/test1a.pdf")
  
  rpt <- create_report(fp, output_type = "PDF", font = "Arial", 
                       font_size = 12) %>%
    set_margins(top = 1, left = 1, right = 1, bottom = 1) %>% 
    page_header(c("Left1", "Another left"), 
                c("Right1", "Right2", "Another right"),  blank_row = "below") %>% 
    titles("Table 0.0", "Baseline Characteristics", align = "center",
           blank_row = "below", bold = TRUE) %>%
    add_content(create_text(cnt, width = 6, align = "left"), align = "center") %>% 
    footnotes("My footnote1", "My footnote2", valign = "top", blank_row = "none",
              width = "page", align = "left") %>% 
    page_footer(c("Left1", "Left2"), "Center", "Right")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("pdf2-1b: Report with title header is correct.", {
  
  
  fp <- file.path(base_path, "pdf2/test1b.pdf")
  
  rpt <- create_report(fp, output_type = "PDF", font = "Arial", 
                       font_size = 12) %>%
    set_margins(top = 1, left = 1, right = 1, bottom = 1) %>% 
    title_header("Table 0.0", "Baseline Characteristics", right = "right") %>%
    add_content(create_text(cnt, width = 6)) %>% 
    page_footer(c("Left1", "Left2"), "Center", c("Right1", "Right2", "Right3"))
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

# Need to fix footnotes on content
test_that("pdf2-1c: Report with titles on content is correct.", {
  
  
  fp <- file.path(base_path, "pdf2/test1c.pdf")
  
  txt <- create_text(cnt, width = 6) %>% 
    titles("Table 0.0", "Baseline Characteristics", align = "center") %>%
    footnotes("My footnote1", "My footnote2") 
  
  rpt <- create_report(fp, output_type = "PDF", font = "Arial", 
                       font_size = 12) %>%
    set_margins(top = 1, left = 1, right = 1, bottom = 1) %>% 
    page_header(c("Left1", "Another left"), 
                c("Right1", "Right2", "Another right"),  blank_row = "below") %>% 
    add_content(txt) %>% 
    page_footer(c("Left1", "Left2"), "Center", "Right")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

# Borders not working yet
test_that("pdf2-1d: One page text spec with borders works as expected.", {
  
  
  fp <- file.path(base_path, "pdf2/test1d.pdf")
  
  txt <- create_text(cnt, width = 6, borders = "outside", align = "right") %>%
    titles("Text 1.0", "My Nice Text", borders = "outside", font_size = 12) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside")
  
  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = 10) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "right") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("pdf2-2: Two page text spec works as expected in 12pt font.", {


  fp <- file.path(base_path, "pdf2/test2.pdf")

  cnttxt <- paste(rep(cnt, 12), collapse = "")

  txt <- create_text(cnttxt) %>%
    titles("Text 1.0", "My Nice Text")

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
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


test_that("pdf2-3: Three page text spec increased margins works as expected.", {


  fp <- file.path(base_path, "pdf2/test3.pdf")

  cnttxt <- paste(rep(cnt, 15), collapse = "")

  txt <- create_text(cnttxt, width = 6) %>%
    titles("Text 1.0", "My Nice Text", align = "left") %>%
    titles("Text 1.0", "My Nice Text", align = "right") %>%
    footnotes("My footnote 1", "My footnote 2", align = "left") %>% 
    footnotes("My footnote 3", "My footnote 4", align = "right")

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = fsz) %>%
    set_margins(top = 2, bottom = 2) %>%
    page_header("Left", c("Right1", "Page [pg] of [tpg]")) %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 6)


})


test_that("pdf2-4: Two page text spec works as expected in 10pt font.", {


  fp <- file.path(base_path, "pdf2/test4.pdf")

  cnttxt <- paste(rep(cnt, 12), collapse = "")

  txt <- create_text(cnttxt, width = 6) %>% 
    title_header("Text 1.0", right = "My Nice Text", blank_row = "above",
                 width = "page") %>%
    title_header("Text 2.0", right = "My Nice Text2", blank_row = "below", 
                 width = "page") %>%
    footnotes("My footnote 1", "My footnote 2", width = "page")

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = 10) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1") 


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)


})


test_that("pdf2-5: Two page text spec works as expected in 8pt font.", {


  fp <- file.path(base_path, "pdf2/test5.pdf")

  cnttxt <- paste(rep(cnt, 20), collapse = "")

  txt <- create_text(cnttxt)

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = 8) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    titles("Text 1.0", "My Nice Text", bold = TRUE) %>%
    footnotes("My footnote 1", "My footnote 2")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)


})


test_that("pdf2-6: One page table works as expected.", {


  fp <- file.path(base_path, "pdf2/test6.pdf")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = fsz, orientation = "landscape", 
                       units = "inches") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table", borders = "outside", bold = TRUE) %>%
    add_content(create_table(dat, borders = "outside")) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside") %>%
    page_footer("Left1", "Center1", "Right1", blank_row = "above")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})


test_that("pdf2-7: Multi page table works as expected.", {


  fp <- file.path(base_path, "pdf2/test7.pdf")

  dat <- iris


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = 1, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1, align = "centre") %>%
    define(Species, blank_after = TRUE)

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    footnotes("My footnote 1", "My footnote 2")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 8)


})



test_that("pdf2-8: Portrait table works as expected.", {


  fp <- file.path(base_path, "pdf2/test8.pdf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat)

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = 8, orientation = "portrait") %>%
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


test_that("pdf2-9: Wide table works as expected.", {


  fp <- file.path(base_path, "pdf2/test9.pdf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat) %>%
    column_defaults(width = 1)


  rpt <- create_report(fp, output_type = "PDF", font = fnt,
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


test_that("pdf2-10: Preview works as expected.", {


  fp <- file.path(base_path, "pdf2/test10.pdf")

  dat <- iris

  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "PDF", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt, preview = 2)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)


})

# Basic Tests 11 - 20 ------------------------------------------------------

# This is awesome. Shows cell wrapping, page break, and valign
# Very good for testing.
test_that("pdf2-11: Forced page wrap works as expected.", {


  fp <- file.path(base_path, "pdf2/test11.pdf")

  dat <- data.frame(labels = rownames(mtcars), mtcars)

  tbl <- create_table(dat, borders = "all") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "bottom") %>%
    define(labels, id_var = TRUE) %>%
    define(wt, page_wrap = TRUE)

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 4)


})


# test_that("pdf2-12: Table Borders work as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test12.pdf")
#   
#   dat <- mtcars[1:15, ] 
#   
#   tbl <- create_table(dat, borders = c("left", "right", "bottom", "top")) %>% 
#     define(mpg, label = "Miles Per Gallon")
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
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
#   expect_equal(res$pages, 1)
#   
#   
# })

# Nice.
test_that("pdf2-13: Spanning headers work as expected.", {


  fp <- file.path(base_path, "pdf2/test13.pdf")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c("top", "bottom")) %>%
    spanning_header(cyl, disp, "Span 1 and go an go", n = 10, label_align = "left") %>%
    spanning_header(hp, wt, "Span 2", n = 11, underline = TRUE) %>%
    spanning_header(qsec, vs, "Span 3") %>%
    spanning_header(drat, gear, "Super Duper\n Big Ol' Span", n = 25, level = 2)


  rpt <- create_report(fp, output_type = "PDF", font = fnt,
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


test_that("pdf2-14: Labels and show_cols work as expected.", {


  fp <- file.path(base_path, "pdf2/test14.pdf")

  dat <- mtcars[1:15, ]

  attr(dat$mpg, "label") <- "Miles Per Gallon"
  attr(dat$cyl, "label") <- "Cylinders"

  tbl <- create_table(dat, show_cols = c("mpg", "hp", "cyl", "disp", "drat")) %>%
    define(mpg, width = 1.25) %>%
    define(disp, label = "Displacement")

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
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


test_that("pdf2-15: Valign on report footnotes works as expected.", {


  fp <- file.path(base_path, "pdf2/test15.pdf")

  dat <- iris[1:100, ]

  tbl <- create_table(dat) %>%
    define(Species, page_break = TRUE)

  rpt <- create_report(fp, output_type = "PDF", font = "Arial",
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
test_that("pdf2-16: Valign on table footnotes works as expected.", {


  fp <- file.path(base_path, "pdf2/test16.pdf")

  dat <- iris[1:100, ]

  tbl <- create_table(dat) %>%
    footnotes("My footnote 1", "My footnote 2", valign = "bottom") %>%
    define(Species, page_break = TRUE)

  rpt <- create_report(fp, output_type = "PDF", font = "Arial",
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

# Page numbering not working
test_that("pdf2-17: Title header on table works as expected.", {


  fp <- file.path(base_path, "pdf2/test17.pdf")

  dat <- iris[1:25, ]

  tbl <- create_table(dat, width = 9) %>%
    title_header("Table 1.0", "My Nice Table",
                 right = c("Right1",
                           "Right2", "Page [pg] of [tpg]")) %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "PDF", font = "Arial",
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


test_that("pdf2-18: Title header on report works as expected.", {


  fp <- file.path(base_path, "pdf2/test18.pdf")

  dat <- iris[1:50, ]

  tbl <- create_table(dat)

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
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


# test_that("pdf2-19: Title and Footnote borders work as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test19.pdf")
#   
#   dat <- iris[1:20, ] 
#   
#   tbl <- create_table(dat, borders = "all") %>% 
#     titles("Table 1.0", "My Nice Report with Borders",
#            borders = c("top", "bottom", "left", "right"),
#            blank_row = "both") %>%
#     footnotes("My footnote 1", "My footnote 2", valign = "top",
#               borders = c("top", "bottom", "left", "right"), 
#               blank_row = "both")
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(tbl) %>%
#     page_footer("Left1", "Center1", "Right1")
#   
#   res <- write_report(rpt)
#   res
#   res$column_widths
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   expect_equal(length(res$column_widths[[1]]), 5)
#   
#   
# })
# 
# 
# test_that("pdf2-20: Title Header borders work as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test20.pdf")
#   
#   dat <- iris[1:25, ] 
#   
#   tbl <- create_table(dat, borders = "all") %>% 
#     title_header("Table 1.0", "My Nice Report with Borders",
#                  right = c("Right1", "Right2", "Right3"),
#                  borders = c("outside"),
#                  blank_row = "both") %>%
#     footnotes("My footnote 1", "My footnote 2", valign = "top",
#               borders = c("top", "bottom", "left", "right"), 
#               blank_row = "both")
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(tbl) %>%
#     page_footer("Left1", "Center1", "Right1")
#   
#   res <- write_report(rpt)
#   res
#   res$column_widths
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   expect_equal(length(res$column_widths[[1]]), 5)
#   
#   
# })
# 
# # Basic Tests 21 - 30 ------------------------------------------------------
# 
# # Works unless drat column is too narrow and spanning label wraps unexpectedly
# test_that("pdf2-21: Page wrap with spanning header works as expected.", {
#   
#   fp <- file.path(base_path, "pdf2/test21.pdf")
#   
#   
#   df <- data.frame(vehicle = rownames(mtcars), mtcars, stringsAsFactors = FALSE)
#   rownames(df) = NULL
#   
#   tbl <- create_table(df) %>% 
#     spanning_header(2, 5,
#                     label = "Span 1", label_align = "center", n = 10) %>%
#     spanning_header(6, 8,
#                     label = "Span 2", label_align = "center", n = 10) %>%
#     spanning_header(9, 12,
#                     label = "Span 3", label_align = "center", n = 10) %>%
#     spanning_header(6, 12, label = "Super Span",
#                     label_align = "center",
#                     level = 2) %>%
#     define(vehicle, label = "Vehicle", id_var = TRUE) %>% 
#     define(mpg, format = "%.1f") %>% 
#     define(drat, width = 1) %>% 
#     define(wt, page_wrap = TRUE) %>% 
#     define(vs, page_wrap = TRUE)
#   
#   rpt <- create_report(fp, output_type = "PDF", 
#                        orientation = "portrait", font = fnt) %>%
#     add_content(tbl) %>% 
#     titles("Table 1.0", "MTCARS Subset Test") %>% 
#     footnotes("My footnote") %>% 
#     page_header("Left") %>% 
#     page_footer("Left", right = "Right")
#   
#   #print(rpt)
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   
#   expect_equal(res$pages, 3)
# })

# This is a good one for testing height calculations.  Looks good for now.
test_that("pdf2-22: Page by works as expected.", {


  fp <- file.path(base_path, "pdf2/test22.pdf")

  dat <- iris

  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Species, label = "Species: ", align = "center", borders = "none", 
            blank_row = "below")

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
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

# # Works!  
# test_that("pdf2-23: Two contents on one page works as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test23.pdf")
#   
#   dat <- mtcars[1:15, ]
#   
#   tbl <- create_table(dat) %>% 
#     titles("Table 1.0", "My Nice Table") %>%
#     footnotes("My footnote 1", "My footnote 2")
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
#     add_content(tbl, page_break = FALSE, blank_row = "below") %>%
#     add_content(create_text(cnt, width = 5), align = "center") %>% 
#     page_footer("Left1", "Center1", "Right1")
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   
#   
# })
# 
# # Working.
# test_that("pdf2-24: Two tables one headerless works as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test24.pdf")
#   
#   dat <- mtcars
#   dat2 <- mtcars[16:20, ]
#   
#   tbl <- create_table(dat) %>% 
#     titles("Table 1.0", "My Nice Table") %>% 
#     column_defaults(width = .5)
#   
#   tbl2 <- create_table(dat2, headerless = TRUE) %>% 
#     column_defaults(width = .5)
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
#     add_content(tbl, page_break = FALSE, blank_row = "none") %>%
#     add_content(tbl2, align = "center") %>% 
#     page_footer("Left1", "Center1", "Right1") %>% 
#     footnotes("My footnote 1", "My footnote 2")
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 2)
#   
#   
# })


test_that("pdf2-25: Simplest PDF2 Plot works as expected.", {

  library(ggplot2)

  fp <- file.path(base_path, "pdf2/test25.pdf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8, borders = c("top", "bottom", "all")) %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", 
           borders = "none", blank_row = "both") %>%
    footnotes("* Motor Trend, 1974", borders = "none", blank_row = "both", 
              valign = "top", width = "page", align = "center")


  rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size =fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})


# test_that("pdf2-26: Table with Plot on same page works as expected.", {
#   
#   library(ggplot2)
#   
#   fp <- file.path(base_path, "pdf2/test26.pdf")
#   
#   p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
#   
#   plt <- create_plot(p, height = 4, width = 8)
#   tbl <- create_table(mtcars[1:3, ])
#   
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
#     page_header("Client", "Study: XYZ") %>%
#     titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     
#     add_content(plt, page_break = FALSE, blank_row = "none") %>%
#     add_content(tbl) %>%
#     footnotes("* Motor Trend, 1974") %>%
#     page_footer("Time", "Confidential", "Page [pg] of [tpg]")
#   
#   
#   res <- write_report(rpt)
#   
#   #print(res)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   
#   
# })
# 
# 
# 
# test_that("pdf2-27: Plot with page by on plot works as expected.", {
#   
#   library(ggplot2)
#   
#   fp <- file.path(base_path, "pdf2/test27.pdf")
#   
#   
#   dat <- mtcars[order(mtcars$cyl), ]
#   
#   p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
#   
#   
#   #dats <- split(p$data, p$data$grp)
#   #tbl <- create_table(dat[1:3, ])
#   
#   plt <- create_plot(p, height = 4, width = 8) %>% 
#     titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
#     page_by(cyl, "Cylinders: ") %>% 
#     footnotes("* Motor Trend, 1974") 
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
#     page_header("Client", "Study: XYZ") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(plt) %>%
#     page_footer("Time", "Confidential", "Page [pg] of [tpg]")
#   
#   
#   res <- write_report(rpt)
#   
#   #print(res)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 3)
#   
#   
# })
# 
# 
# test_that("pdf2-28: Plot with page by on report works as expected.", {
#   
#   library(ggplot2)
#   
#   fp <- file.path(base_path, "pdf2/test28.pdf")
#   
#   
#   dat <- mtcars[order(mtcars$cyl), ]
#   
#   p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
#   
#   
#   #dats <- split(p$data, p$data$grp)
#   #tbl <- create_table(dat[1:3, ])
#   
#   plt <- create_plot(p, height = 4, width = 8)
#   
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
#     page_header("Client", "Study: XYZ") %>%
#     titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", 
#            blank_row = "none", borders = "none") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_by(cyl, "Cylinders: ") %>% 
#     add_content(plt) %>%
#     footnotes("* Motor Trend, 1974", borders = "none") %>%
#     page_footer("Time", "Confidential", "Page [pg] of [tpg]")
#   
#   
#   res <- write_report(rpt)
#   
#   #print(res)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 3)
#   
#   
# })

test_that("pdf2-29: Simplest Plot with valign top works as expected.", {

  library(ggplot2)

  fp <- file.path(base_path, "pdf2/test29.pdf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8)


  rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
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



test_that("pdf2-30: Simplest RTF Plot with valign bottom works as expected.", {

  library(ggplot2)

  fp <- file.path(base_path, "pdf2/test30.pdf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8) %>%
    footnotes("* Motor Trend, 1974", valign = "bottom")


  rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
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

# # Basic Tests 31 - 40 ------------------------------------------------------

test_that("pdf2-31: Simplest PDF with valign top works as expected.", {


  fp <- file.path(base_path, "pdf2/test31.pdf")

  txt <- create_text(cnt, width = 6, borders = "outside")


  rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
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


})

test_that("pdf2-32: Simplest Text with valign bottom works as expected.", {


  fp <- file.path(base_path, "pdf2/test32.pdf")

  txt <- create_text(cnt, width = 6) %>%
    footnotes("* Motor Trend, 1974", valign = "bottom")

  rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
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

# Works except label headers need to move down
test_that("pdf2-33: Table with long cell and label values wraps as expected.", {


  fp <- file.path(base_path, "pdf2/test33.pdf")


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


  rpt <- create_report(fp, output_type = "PDF", font = "Courier",
                       font_size = fsz) %>%
    titles("Table 1.0", align = "center") %>%

    add_content(tbl1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)



})

# # Works on all combinations on font and font size.  Needed adjustments to row height.
# test_that("pdf2-34: Table with break between sections works as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test34.pdf")
#   
#   
#   # Setup
#   subjid <- 100:109
#   name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
#             "al-Kazemi, Najwa", "Schaffer, Ashley", "Laner, Tahma", 
#             "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
#             "Smith, Shaileigh")
#   sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
#   age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
#   arm <- c(rep("A", 5), rep("B", 5))
#   
#   # Create data frame
#   df <- data.frame(subjid, name, sex, age, arm)
#   
#   
#   tbl1 <- create_table(df, first_row_blank = TRUE) %>%
#     define(subjid, label = "Subject ID", align = "left", width = 1) %>% 
#     define(name, label = "Subject Name", width = 1) %>% 
#     define(sex, label = "Sex") %>% 
#     define(age, label = "Age") %>% 
#     define(arm, label = "Arm", 
#            blank_after = FALSE, 
#            dedupe = TRUE, 
#            align = "right") #%>% 
#   # spanning_header(sex, arm, label = "Here is a spanning header")
#   
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
#     page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
#     # options_fixed(line_count = 46) %>% 
#     titles("Table 1.0", "Analysis Data Subject Listing\n And more stuff", 
#            "Safety Population", align = "center") %>%
#     footnotes("Program Name: table1_0.R", 
#               "Here is a big long footnote that is going to wrap\n at least once") %>%
#     page_footer(left = "Time", center = "Confidential", 
#                 right = "Page [pg] of [tpg]") %>%
#     add_content(tbl1) 
#   
#   
#   res <- write_report(rpt)
#   res
#   expect_equal(file.exists(fp), TRUE)
#   
#   
# })
# 
# 
# test_that("pdf2-35: Title Header and page header/footer wrapping work as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test35.pdf")
#   
#   dat <- iris[1:10, ] 
#   
#   tbl <- create_table(dat, borders = "none") %>% 
#     title_header("Table 1.0", "My Nice Report with Borders",
#                  right = c("Right1", "Right2", 
#                            "Right3 long enough to wrap around at least once"),
#                  borders = "none",
#                  blank_row = "none") %>%
#     footnotes("My footnote 1", "My footnote 2", valign = "bottom",
#               borders = "none", 
#               blank_row = "above")
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(tbl) %>%
#     page_header(c("Left1", "Left2\nwrap"), "Right 1") %>% 
#     page_footer("Left1", 
#                 "Center1 here is a whole bunch of stuff to try and make it wrap", 
#                 "Right1\nwrap\n and wrap again")
#   
#   res <- write_report(rpt)
#   res
#   res$column_widths
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   expect_equal(length(res$column_widths[[1]]), 5)
#   
#   
# })
# 
# 
# test_that("pdf2-36: Title and Footnote widths work as expected on table.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test36.pdf")
#   
#   dat <- iris[1:20, ] 
#   
#   tbl <- create_table(dat) %>% 
#     titles("Table 1.0", "My Nice Report with Widths",
#            width = "page") %>%
#     footnotes("My footnote 1", "My footnote 2", valign = "top",
#               width = "page")
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(tbl) %>%
#     page_footer("Left1", "Center1", "Right1")
#   
#   res <- write_report(rpt)
#   res
#   res$column_widths
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   expect_equal(length(res$column_widths[[1]]), 5)
#   
#   
# })
# 

test_that("pdf2-37: Title and Footnote widths work as expected on report", {


  fp <- file.path(base_path, "pdf2/test37.pdf")

  dat <- iris[1:20, ]

  tbl <- create_table(dat, borders = "all")

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1") %>%
    titles("Table 1.0", "My Nice Report with Widths",
           borders = c("top", "bottom", "left", "right"),
           width = 7, align = "left") %>%
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


test_that("pdf2-38: Title and Footnote specific widths work as expected.", {


  fp <- file.path(base_path, "pdf2/test38.pdf")

  dat <- iris[1:20, ]

  tbl <- create_table(dat, borders = "all") %>%
    titles("Table 1.0", "My Nice Report with Borders",
           borders = c("top", "bottom", "left", "right"),
           width = 7, align = "left") %>%
    footnotes("My footnote 1", "My footnote 2", valign = "top",
              borders = c("top", "bottom", "left", "right"),
              width = 7)

  rpt <- create_report(fp, output_type = "PDF", font = fnt,
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

# Works
test_that("pdf2-39: One page table works as expected in centimeters and times.", {


  fp <- file.path(base_path, "pdf2/test39.pdf")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."

  rpt <- create_report(fp, output_type = "PDF", font = "Times",
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

# Title not aligned properly
test_that("pdf2-40: One page table works as expected in courier.", {


  fp <- file.path(base_path, "pdf2/test40.pdf")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."

  rpt <- create_report(fp, output_type = "PDF", font = "Courier",
                       font_size = fsz, orientation = "landscape",
                       units = "cm") %>%
    set_margins(top = 3, bottom = 3) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table", align = "center") %>%
    add_content(create_table(dat)) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})

# # Basic Tests 41 - 50 ------------------------------------------------------
# 
# # Good for testing borders and spacing are working correctly
# test_that("pdf2-41: Page by with borders works as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test41.pdf")
#   
#   dat <- iris
#   
#   brdrs <- "outside"
#   
#   tbl <- create_table(dat, borders = "all") %>% 
#     titles("Table 1.0", "My Nice Report with a Page By", borders = brdrs) %>%
#     page_by(Species, label = "Species: ", align = "left", borders = brdrs,
#             blank_row = "below") %>% 
#     footnotes("My footnote 1", "My footnote 2", borders = brdrs)
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(tbl) %>%
#     page_footer("Left1", "Center1", "Right1")
#   
#   res <- write_report(rpt)
#   res
#   res$column_widths
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 6)
#   expect_equal(length(res$column_widths[[1]]), 5)
#   
#   
# })
# 
# # Test for borders and page wraps
# test_that("pdf2-42: Long table with borders and footnotes on report.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test42.pdf")
#   
#   dat <- data.frame(labels = rownames(mtcars), mtcars)
#   
#   tbl <- create_table(dat, borders = "all") %>% 
#     define(labels, id_var = TRUE) %>% 
#     define(wt, page_wrap = TRUE)
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = 12, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", c("Right1")) %>%
#     add_content(tbl) %>%
#     page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>% 
#     titles("Table 1.0", "My Nice Irises", "Another Title") %>%
#     footnotes("My footnote 1", "My footnote 2", valign = "bottom") 
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 4)
#   
#   
# })


# Works
test_that("pdf2-43: use_attributes parameter table works as expected.", {


  fp1 <- file.path(base_path, "pdf2/test43a.pdf")
  fp2 <- file.path(base_path, "pdf2/test43b.pdf")
  fp3 <- file.path(base_path, "pdf2/test43c.pdf")


  dat <- mtcars[1:10, ]
  attr(dat$mpg, "label") <- "Miles per gallon"
  attr(dat$cyl, "format") <- "%.1f"
  attr(dat$hp, "width") <- 2
  fattr(dat$vs) <- list(width = 2, justify = "center")

  tbl <- create_table(dat)

  # Test default
  rpt <- create_report(fp1, output_type = "PDF", font = fnt) %>%
    add_content(tbl)

  res <- write_report(rpt)

  expect_equal(file.exists(fp1), TRUE)
  expect_equal(res$pages, 1)

  # Test none
  tbl <- create_table(dat, use_attributes = "none")

  rpt <- create_report(fp2, output_type = "PDF", font = fnt) %>%
    add_content(tbl)

  res <- write_report(rpt)

  expect_equal(file.exists(fp2), TRUE)


  # Test some
  tbl <- create_table(dat, use_attributes = c("format", "width"))

  rpt <- create_report(fp3, output_type = "PDF", font = fnt) %>%
    add_content(tbl)

  res <- write_report(rpt)

  expect_equal(file.exists(fp3), TRUE)
  expect_equal(res$pages, 1)

})

# Works
test_that("pdf2-45: Title bold and font size works as expected.", {


  fp <- file.path(base_path, "pdf2/test45.pdf")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"

  tbl <- create_table(dat, borders = "outside") %>%
    titles("Table 1.0", 
           paste("My Nice Table that will go on and on and eventually will",
                 "wrap around I hope so maybe I can test this feature of the ",
                 "application"), borders = c("none"),
           width = "content", font_size = 14, bold = TRUE) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none",
              align = "left", width = "content") %>%
    define(wt, width = 1, label = "Weight", align = "center",
           label_align = "right")

  rpt <- create_report(fp, output_type = "PDF", font = "Arial",
                       font_size = 9, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"),
                blank_row = "below") %>%
    add_content(tbl, align = "center")  %>%
    page_footer("Left1", "Center1", 
                paste("Right1 here is one that is really, ", 
                "really long that I hope will wrap around"))

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})



test_that("pdf2-46: 9 pt font inches works as expected.", {


  fp <- file.path(base_path, "pdf2/test46.pdf")

  rpt <- create_report(fp, output_type = "PDF", font_size = 9,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)


})

# Working but misaligned.
test_that("pdf2-47: 9 pt font cm works as expected.", {


  fp <- file.path(base_path, "pdf2/test47.pdf")

  rpt <- create_report(fp, output_type = "PDF", font_size = 9,
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


})

test_that("pdf2-48: 11 pt font inches works as expected.", {


  fp <- file.path(base_path, "pdf2/test48.pdf")

  rpt <- create_report(fp, output_type = "PDF", font_size = 11,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)


})

# Not
test_that("pdf2-49: 11 pt font cm works as expected.", {


  fp <- file.path(base_path, "pdf2/test49.pdf")

  rpt <- create_report(fp, output_type = "PDF", font_size = 11,
                       font = "Courier",
                       orientation = "portrait", 
                       units = "cm") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris), align = "left") %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)


})

# test_that("pdf2-50: Spanning headers borders work as expected.", {
#   
#   
#   fp <- file.path(base_path, "pdf2/test50.pdf")
#   
#   dat <- mtcars[1:15, ]
#   
#   tbl <- create_table(dat, borders = c("body")) %>%
#     spanning_header(cyl, disp, "Span 1", label_align = "left") %>% 
#     spanning_header(hp, wt, "Span 2", underline = FALSE) %>%
#     spanning_header(qsec, vs, "Span 3", n = 10) %>%
#     spanning_header(drat, gear, "Super Duper\nWrapped Span", n = 11, level = 2) %>% 
#     titles("Table 1.0", "My Nice Table", borders = "outside") %>%
#     footnotes("My footnote 1", "My footnote 2", borders = "outside") 
#   
#   rpt <- create_report(fp, output_type = "PDF", font = fnt,
#                        font_size = fsz, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(tbl)
#   
#   
#   res <- write_report(rpt)
#   res
#   res$column_widths
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   
# })
# 
# # User Tests --------------------------------------------------------------
# 
# test_that("pdf2-user1: demo table works.", {
#   
#   if (dev) {
#     library(tidyr)
#     library(dplyr)
#     
#     # Data Filepath
#     dir_data <- file.path(data_dir, "data")
#     
#     fp <- file.path(base_path, "pdf2/user1.pdf")
#     
#     
#     # Load Data
#     data_demo   <- file.path(dir_data, "dm.csv") %>%
#       read.csv() 
#     
#     
#     data_demo <- subset(data_demo, data_demo$ARM != "SCREEN FAILURE")
#     
#     
#     sex_decode <- c("M" = "Male",
#                     "F" = "Female")
#     
#     race_decode <- c("WHITE" = "White",
#                      "BLACK OR AFRICAN AMERICAN" = "Black or African American",
#                      "ASIAN" = "Asian or Pacific Islander",
#                      "NATIVE AMERICAN" = "Native American",
#                      "UNKNOWN" = "Unknown")
#     
#     arm_pop <- table(data_demo$ARM) 
#     
#     
#     demo_age <-
#       data_demo %>%
#       group_by(ARM) %>%
#       summarise(across(.cols = AGE,
#                        .fns = list(N      = ~ fmt_n(.),
#                                    Mean   = ~ fmt_mean_sd(.),
#                                    Median = ~ fmt_median(.),
#                                    `Q1 - Q3` = ~ fmt_quantile_range(.),
#                                    Range  = ~ fmt_range(.)
#                        ))) %>%
#       pivot_longer(-ARM,
#                    names_to  = c("var", "label"),
#                    names_sep = "_",
#                    values_to = "value") %>%
#       pivot_wider(names_from = ARM,
#                   values_from = "value")
#     
#     
#     
#     demo_sex <-
#       data_demo %>%
#       add_count(ARM, SEX,  name = "n_SEX") %>%
#       select(ARM, SEX, n_SEX) %>%
#       distinct() %>%
#       pivot_longer(cols = c(SEX),
#                    names_to  = "var",
#                    values_to = "label") %>%
#       pivot_wider(names_from  = ARM,
#                   values_from = n_SEX,
#                   values_fill = 0) %>%
#       mutate(label = factor(label, levels = names(sex_decode),
#                             labels = sex_decode),
#              `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
#              `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
#              `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
#              `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"]))
#     
#     
#     
#     demo_race <-
#       data_demo %>%
#       add_count(ARM, RACE, name = "n_RACE") %>%
#       select(ARM, RACE, n_RACE) %>%
#       distinct() %>%
#       pivot_longer(cols = RACE,
#                    names_to  = "var",
#                    values_to = "label") %>%
#       pivot_wider(names_from  = ARM,
#                   values_from = n_RACE,
#                   values_fill = 0) %>%
#       mutate(label = factor(label, levels = names(race_decode),
#                             labels = race_decode),
#              `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
#              `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
#              `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
#              `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
#       arrange(var, label)
#     
#     
#     demo <- bind_rows(demo_age, demo_sex, demo_race)
#     
#     
#     #View(demo)
#     
#     
#     # Stub decode
#     block_fmt <- c(AGE = "Age", SEX = "Sex", RACE = "Race")
#     
#     # Define table
#     tbl <- create_table(demo, first_row_blank = TRUE) %>%
#       column_defaults(from = "ARM A", to = "ARM D", width = 1.25) %>% 
#       define(var, blank_after = TRUE, dedupe = TRUE,
#              format = block_fmt, label = "") %>%
#       define(label, label = "") %>%
#       define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
#       define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
#       define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
#       define(`ARM D`, align = "center", label = "Competitor", n = 38)
#     
#     # Define Report
#     rpt <- create_report(fp, output_type = "PDF", font = fnt, font_size = fsz) %>%
#       set_margins(top = 1, bottom = 1) %>% 
#       options_fixed(font_size = 10) %>% 
#       titles("Table 14.1/4",
#              "Demographics and Baseline to Characteristics",
#              "Specify Population Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼") %>%
#       add_content(tbl) %>% 
#       footnotes("Special symbols \U221e to mess things up: Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼") %>%   
#       footnotes("Special symbols µ Ω £ there to mess things up: ", "Page [pg] of [tpg]") %>% 
#       page_header("Left µ Ω £ ", "Right") %>% 
#       page_footer("Time µ Ω £ ", right = "Page [pg] of [tpg]")
#     
#     # Write out report
#     res <- write_report(rpt)
#     
#     expect_equal(file.exists(fp), TRUE)
#     
#     
#     
#   } else 
#     expect_equal(TRUE, TRUE)
#   
#   
# })
# 
# test_that("pdf2-user2: demo table with stub works.", {
#   
#   
#   if (dev) {
#     
#     library(tidyr)
#     library(dplyr)
#     
#     
#     
#     # Data Filepath
#     dir_data <- file.path(data_dir, "data")
#     
#     fp <- file.path(base_path, "pdf2/user2.pdf")
#     
#     
#     # Load Data
#     data_demo   <- file.path(dir_data, "dm.csv") %>%
#       read.csv() 
#     
#     data_demo <- subset(data_demo, data_demo$ARM != "SCREEN FAILURE")
#     
#     
#     sex_decode <- c("M" = "Male",
#                     "F" = "Female")
#     
#     race_decode <- c("WHITE" = "White",
#                      "BLACK OR AFRICAN AMERICAN" = "Black or African American",
#                      "ASIAN" = "Asian or Pacific Islander",
#                      "NATIVE AMERICAN" = "Native American",
#                      "UNKNOWN" = "Unknown")
#     
#     arm_pop <- table(data_demo$ARM) 
#     
#     
#     demo_age <-
#       data_demo %>%
#       group_by(ARM) %>%
#       summarise(across(.cols = AGE,
#                        .fns = list(N      = ~ fmt_n(.),
#                                    Mean   = ~ fmt_mean_sd(.),
#                                    Median = ~ fmt_median(.),
#                                    `Q1 - Q3` = ~ fmt_quantile_range(.),
#                                    Range  = ~ fmt_range(.)
#                        ))) %>%
#       pivot_longer(-ARM,
#                    names_to  = c("var", "label"),
#                    names_sep = "_",
#                    values_to = "value") %>%
#       pivot_wider(names_from = ARM,
#                   values_from = "value")
#     
#     
#     
#     demo_sex <-
#       data_demo %>%
#       add_count(ARM, SEX,  name = "n_SEX") %>%
#       select(ARM, SEX, n_SEX) %>%
#       distinct() %>%
#       pivot_longer(cols = c(SEX),
#                    names_to  = "var",
#                    values_to = "label") %>%
#       pivot_wider(names_from  = ARM,
#                   values_from = n_SEX,
#                   values_fill = 0) %>%
#       mutate(label = factor(label, levels = names(sex_decode),
#                             labels = sex_decode),
#              `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
#              `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
#              `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
#              `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"]))
#     
#     
#     
#     demo_race <-
#       data_demo %>%
#       add_count(ARM, RACE, name = "n_RACE") %>%
#       select(ARM, RACE, n_RACE) %>%
#       distinct() %>%
#       pivot_longer(cols = RACE,
#                    names_to  = "var",
#                    values_to = "label") %>%
#       pivot_wider(names_from  = ARM,
#                   values_from = n_RACE,
#                   values_fill = 0) %>%
#       mutate(label = factor(label, levels = names(race_decode),
#                             labels = race_decode),
#              `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
#              `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
#              `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
#              `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
#       arrange(var, label)
#     
#     
#     demo <- bind_rows(demo_age, demo_sex, demo_race)
#     
#     
#     #View(demo)
#     
#     
#     # Stub decode
#     block_fmt <- c(AGE = "Age", SEX = "Sex", RACE2 = "Race")
#     
#     # Define table
#     tbl <- create_table(demo, first_row_blank = TRUE, borders = "all") %>%
#       stub(c("var", "label"), width = 2.5) %>% 
#       column_defaults(width = 1) %>% 
#       define(var, blank_after = TRUE, 
#              format = block_fmt, label = "", label_row = TRUE) %>%
#       define(label, label = "", indent = .25) %>%
#       define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
#       define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
#       define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
#       define(`ARM D`, align = "center", label = "Competitor", n = 38) %>% 
#       titles("Table 14.1/4",
#              "Demographics and Baseline Characteristics",
#              "Specify Population", borders = "outside", blank_row = "both",
#              align = "right") %>%
#       footnotes("Here is a footnote", "Here is another footnote",
#                 borders = "outside", blank_row = "both", align = "right")  
#     
#     # Define Report
#     rpt <- create_report(fp, output_type = "PDF", 
#                          font = "Arial", font_size = 10) %>%
#       add_content(tbl, align = "right") %>% 
#       page_header("Sponsor", "Drug") %>% 
#       page_footer(left = "Time", right = "Page [pg] of [tpg]") #%>% 
#     #page_by(var = "var", label = "Variable: ")
#     
#     # Write out report
#     res <- write_report(rpt)
#     
#     expect_equal(file.exists(fp), TRUE)
#     
#     
#   } else
#     expect_equal(TRUE, TRUE)
#   
# })
# 
# test_that("user3: listings works.", {
#   if (dev == TRUE) {
#     # Data Filepath
#     dir_data <- file.path(data_dir, "data")
#     
#     fp <- file.path(base_path, "pdf2/user3.pdf")
#     
#     # Removing to make last page exactly equal to available rows on page.
#     # In this case, any added blank rows should be skipped.
#     fil <- c("ABC-14-124",
#              "ABC-15-153",
#              "ABC-15-154",
#              "ABC-15-155",
#              "ABC-15-156",
#              "ABC-16-045",
#              "ABC-16-046",
#              "ABC-16-047",
#              "ABC-16-157",
#              "ABC-16-158",
#              "ABC-16-159", 
#              "ABC-16-160")
#     
#     # Load Data
#     data_demo   <- file.path(dir_data, "dm.csv") %>%
#       read.csv() 
#     
#     data_demo <- data_demo[!data_demo$USUBJID %in% fil, ]
#     
#     
#     # Test that any assigned formats are applied
#     attr(data_demo$SUBJID, "width") <- 1
#     attr(data_demo$SUBJID, "justify") <- "left"
#     attr(data_demo$SUBJID, "format") <- "S:%s"
#     #print(widths(data_demo))
#     names(data_demo)
#     # Define table
#     tbl <- create_table(data_demo) %>% 
#       define(USUBJID, id_var = TRUE) 
#     
#     
#     # Define Report
#     rpt <- create_report(fp, font = "Arial", font_size = 12, 
#                          orientation = "portrait") %>%
#       titles("Listing 1.0",
#              "Demographics Dataset") %>%
#       add_content(tbl, align = "left") %>% 
#       page_header("Sponsor", "Drug") %>% 
#       page_footer(left = "Time", right = "Page [pg] of [tpg]") %>% 
#       footnotes("My footnotes")
#     
#     #Write out report
#     res <- write_report(rpt, output_type = "PDF")
#     
#     expect_equal(file.exists(fp), TRUE)
#     
#     
#     
#     # pdfpth <- file.path(base_path, "user/user3.pdf")
#     # write_report(rpt, pdfpth, output_type = "PDF")
#     # expect_equal(file.exists(pdfpth), TRUE)
#   } else 
#     expect_equal(TRUE, TRUE)
#   
#   
# })
# 
# 
# test_that("user4: listing in cm and times works.", {
#   if (dev == TRUE) {
#     # Data Filepath
#     dir_data <- file.path(data_dir, "data")
#     
#     fp <- file.path(base_path, "pdf2/user4.pdf")
#     
#     # Removing to make last page exactly equal to available rows on page.
#     # In this case, any added blank rows should be skipped.
#     fil <- c("ABC-14-124",
#              "ABC-15-153",
#              "ABC-15-154",
#              "ABC-15-155",
#              "ABC-15-156",
#              "ABC-16-045",
#              "ABC-16-046",
#              "ABC-16-047",
#              "ABC-16-157",
#              "ABC-16-158",
#              "ABC-16-159", 
#              "ABC-16-160")
#     
#     # Load Data
#     data_demo   <- file.path(dir_data, "dm.csv") %>%
#       read.csv() 
#     
#     data_demo <- data_demo[!data_demo$USUBJID %in% fil, ]
#     
#     
#     # Test that any assigned formats are applied
#     attr(data_demo$SUBJID, "width") <- 2.54
#     attr(data_demo$SUBJID, "justify") <- "left"
#     attr(data_demo$SUBJID, "format") <- "S:%s"
#     #print(widths(data_demo))
#     
#     # Define table
#     tbl <- create_table(data_demo) %>% 
#       define(USUBJID, id_var = TRUE) 
#     
#     
#     # Define Report
#     rpt <- create_report(fp, font = "Times", font_size = 10, units = "cm") %>%
#       titles("Listing 1.0",
#              "Demographics Dataset") %>%
#       add_content(tbl, align = "left") %>% 
#       page_header("Sponsor", "Drug") %>% 
#       page_footer(left = "Time", right = "Page [pg] of [tpg]") %>% 
#       footnotes("My footnote")
#     
#     #Write out report
#     res <- write_report(rpt, output_type = "PDF")
#     
#     print(res$column_widths)
#     
#     expect_equal(file.exists(fp), TRUE)
#     
#     
#     
#     # pdfpth <- file.path(base_path, "user/user3.pdf")
#     # write_report(rpt, pdfpth, output_type = "PDF")
#     # expect_equal(file.exists(pdfpth), TRUE)
#   } else 
#     expect_equal(TRUE, TRUE)
#   
#   
# })
# 
# test_that("user5: Portrait in 12pt Arial works as expected.", {
#   
#   if (dev == TRUE) {
#     
#     dir_data <- file.path(data_dir, "data")
#     
#     fp <- file.path(base_path, "pdf2/user5pdf")
#     
#     # Read in prepared data
#     df <- read.table(header = TRUE, text = '
#         var     label        A             B          
#         "ampg"   "N"          "19"          "13"         
#         "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
#         "ampg"   "Median"     "16.4"        "21.4"       
#         "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
#         "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
#         "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
#         "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
#         "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
#     
#     # Create table
#     tbl <- create_table(df, first_row_blank = TRUE) %>% 
#       define(var, label = "Variable", blank_after = TRUE, dedupe = TRUE,
#              format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
#       define(label, label = "") %>% 
#       define(A, label = "Group A", align = "center", n = 19) %>% 
#       define(B, label = "Group B", align = "center", n = 13)
#     
#     
#     # Create report and add content
#     rpt <- create_report(fp, orientation = "portrait", output_type = "PDF",
#                          font = "Arial", font_size = 12) %>% 
#       page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
#       titles("Table 1.0", "MTCARS Summary Table") %>% 
#       add_content(tbl) %>% 
#       footnotes("* Motor Trend, 1974") %>%
#       page_footer(left = Sys.time(), 
#                   center = "Confidential", 
#                   right = "Page [pg] of [tpg]")
#     
#     # Write out report
#     res <- write_report(rpt)
#     
#     expect_equal(file.exists(fp), TRUE)
#     
#   } else 
#     expect_equal(TRUE, TRUE)
#   
# })
# 
