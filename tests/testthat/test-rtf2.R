
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
  #expect_equal(res$pages, 1)


})





test_that("rtf2-2: Two page text spec works as expected in 12pt font.", {


  fp <- file.path(base_path, "rtf2/test2.rtf")

  cnttxt <- paste(rep(cnt, 10), collapse = "")

  txt <- create_text(cnttxt) %>%
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
  #expect_equal(res$pages, 1)


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

  txt <- create_text(cnttxt) %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  #expect_equal(res$pages, 1)


})


test_that("rtf2-5: Two page text spec works as expected in 8pt font.", {


  fp <- file.path(base_path, "rtf2/test5.rtf")

  cnttxt <- paste(rep(cnt, 20), collapse = "")

  txt <- create_text(cnttxt) %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 8) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  #expect_equal(res$pages, 1)


})


test_that("rtf2-6: One page table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test6.rtf")
  
  dat <- mtcars[1:15, ]
  #attr(dat[[2]], "label") <- "Cylin."
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(create_table(dat)) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  #expect_equal(res$pages, 1)
  
  
})


test_that("rtf2-7: Three page table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test7.rtf")
  
  dat <- iris
  #attr(dat[[2]], "label") <- "Cylin."
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2") 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  #expect_equal(res$pages, 1)
  
  
})




