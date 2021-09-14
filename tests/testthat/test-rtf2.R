
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


test_that("rtf2-1: Simplest table works as expected.", {


  fp <- file.path(base_path, "rtf2/test1.rtf")

  dat <- mtcars[1:15, ]
  #attr(dat[[2]], "label") <- "Cylin."

  rpt <- create_report(fp, output_type = "RTF", font = "fixed",
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

test_that("rtf2-2: get_cell_borders works as expected.", {


  expect_equal(get_cell_borders(1, 1, 4, 4, c("all")),
               "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs\\clbrdrr\\brdrs")

  expect_equal(get_cell_borders(4, 1, 4, 4, c("all")),
               "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs\\clbrdrr\\brdrs")

  expect_equal(get_cell_borders(1, 1, 4, 4, c("outside")),
               "\\clbrdrt\\brdrs\\clbrdrl\\brdrs")

  expect_equal(get_cell_borders(4, 1, 4, 4, c("outside")),
               "\\clbrdrb\\brdrs\\clbrdrl\\brdrs")

  expect_equal(get_cell_borders(2, 2, 4, 4, c("outside")),
               "")

  expect_equal(get_cell_borders(2, 4, 4, 4, c("right")),
               "\\clbrdrr\\brdrs")

  expect_equal(get_cell_borders(2, 4, 4, 4, c("inside")),
               "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs")


})


test_that("rtf2-3: One page text spec works as expected.", {


  fp <- file.path(base_path, "rtf2/test3.rtf")

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



test_that("rtf2-4: Page template functions work as expected.", {

  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello") %>%
    footnotes("Goodbye")

  rpt <- page_setup_rtf(rpt)

  t <- get_titles_rtf(rpt$titles, 6, rpt)
  t
  expect_equal(t$rtf,
    "\\trowd\\trgaph0\\cellx8640\\qc Hello\\cell\\row\n\\par\n\\pard")
  expect_equal(t$lines, 2)
 # expect_equal(t$twips, 576)

  f <- get_footnotes_rtf(rpt$footnotes, 6, rpt)
  f
  expect_equal(f$rtf,
               "\\line\n\\trowd\\trgaph0\\cellx8640\\ql Goodbye\\cell\\row\n\\pard")
  expect_equal(f$lines, 2)
 # expect_equal(f$twips, 576)


  rpt2 <- create_report("", font = "Arial", font_size = 12) %>%
    title_header("Hello", right = paste("Right here is something",
    "really long that will wrap and wrap and wrap and wrap keep wrapping"))

  rpt2 <- page_setup_rtf(rpt2)

  th <- get_title_header_rtf(rpt2$title_hdr, 6, rpt2)
  th
  expect_equal(nchar(th$rtf) > 1, TRUE)
  expect_equal(th$lines, 5)
#  expect_equal(th$twips, 1440)

  expect_equal(get_lines_rtf(cnt, 9, "Arial", 10), 3)

  rpt3 <- create_report("", font = "Arial", font_size = 12) %>%
    page_header(left= c("Hello"),
                right = paste("Right here is something that might wrap.",
                                       "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer")) %>%
    page_footer("Left", "Center", "Right here is something")

  rpt3 <- page_setup_rtf(rpt3)

  ph <- get_page_header_rtf(rpt3)
  ph

  expect_equal(ph$lines, 3)
  #expect_equal(ph$twips, 864)

  pf <- get_page_footer_rtf(rpt3)
  pf

  expect_equal(pf$lines, 1)
  #expect_equal(pf$twips, 288)


})


test_that("rtf2-5: split_text_rtf works as expected.", {

  res <- split_text_rtf(cnt, 4, 5, "Arial", 12, "inches")

  res
  expect_equal(length(res), 2)


  res <- split_text_rtf(cnt, 4, 3, "Arial", 12, "inches", 2)

  res
  expect_equal(length(res), 4)


})

test_that("rtf2-6: Two page text spec works as expected in 12pt font.", {


  fp <- file.path(base_path, "rtf2/test6.rtf")

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


test_that("rtf2-7: Three page text spec increased margins works as expected.", {


  fp <- file.path(base_path, "rtf2/test7.rtf")

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


test_that("rtf2-8: Two page text spec works as expected in 10pt font.", {


  fp <- file.path(base_path, "rtf2/test8.rtf")

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


test_that("rtf2-9: Two page text spec works as expected in 8pt font.", {


  fp <- file.path(base_path, "rtf2/test9.rtf")

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

test_that("rtf2-10: Long table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf2/test10.rtf")
  
  dat <- iris
  #attr(dat[[2]], "label") <- "Cylin."
  
  rpt <- create_report(fp, output_type = "RTF", font = "fixed",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Irises") %>%
    add_content(create_table(dat)) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  #expect_equal(res$pages, 1)
  
  
})

test_that("rtf2-11: get_width function works as expected.", {
  
  res1 <- get_text_width("This is cool.", "Arial", 12)
  res1
  
  res2 <- get_text_width("This is cool.", "Arial", 10)
  res2
  
  expect_equal(res1 > res2, TRUE)

  
  res3 <- get_text_width("This is cool.", "Courier", 10)
  res3
  expect_equal(res2 < res3, TRUE)
  

})



