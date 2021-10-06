context("Page Template RTF Tests")



test_that("get_titles_rtf function works as expected.", {
  
  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello", blank_row = "below") %>%
    footnotes("Goodbye", blank_row = "below")
  
  rpt <- page_setup_rtf(rpt)
  
  t <- get_titles_rtf(rpt$titles, 6, rpt)
  t
  expect_equal(t$rtf,
        paste0("\\trowd\\trgaph0\\trqc\\cellx12960\\qc Hello\\cell\\row\n",
        "\\trowd\\trgaph0\\trqc\\cellx12960\\qc\\cell\\row\n"))
  expect_equal(t$lines, 2)
  # expect_equal(t$twips, 576)
  
})


test_that("get_footnotes_rtf function works as expected.", {
  
  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello") %>%
    footnotes("Goodbye")
  
  rpt <- page_setup_rtf(rpt)
  
  f <- get_footnotes_rtf(rpt$footnotes, 6, rpt)
  f
  expect_equal(f$rtf,
      paste0("\\trowd\\trgaph0\\trqc\\cellx12960\\ql\\cell\\row\n", 
             "\\trowd\\trgaph0\\trqc\\cellx12960\\ql Goodbye\\cell\\row\n"))
  expect_equal(f$lines, 2)
  # expect_equal(f$twips, 576)
  
})

test_that("get_title_header_rtf function works as expected.", {

  rpt2 <- create_report("", font = "Arial", font_size = 12) %>%
    title_header("Hello", right = paste("Right here is something",
     "really long that will wrap and wrap and wrap and wrap keep wrapping"))
  
  rpt2 <- page_setup_rtf(rpt2)
  
  th <- get_title_header_rtf(rpt2$title_hdr, 6, rpt2)
  th
  expect_equal(nchar(th$rtf) > 1, TRUE)
  expect_equal(th$lines, 4)
  #  expect_equal(th$twips, 1440)
  
})


test_that("get_page_header_rtf works as expected.", {
  
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
})


test_that("get_page_footer_rtf works as expected.", {
  
  rpt3 <- create_report("", font = "Arial", font_size = 12) %>%
    page_header(left= c("Hello"),
                right = paste("Right here is something that might wrap.",
                              "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer")) %>%
    page_footer("Left", "Center", "Right here is something")
  
  rpt3 <- page_setup_rtf(rpt3)
  
  pf <- get_page_footer_rtf(rpt3)
  pf
  
  expect_equal(pf$lines, 1)
  #expect_equal(pf$twips, 288)
  
  
})



test_that("get_pageby_rtf works as expected.", {
  
  tbl <- create_table(mtcars) %>% 
    page_by(cyl, "Cylinders:")
  
  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello") %>%
    footnotes("Goodbye") %>% 
    add_content(tbl)
  
  rpt <- page_setup_rtf(rpt)
  
  rpt$line_height
  
  res <- get_page_by_rtf(tbl$page_by, 6, "fork", rpt, "left")
  res  
  
  expect_equal(res$lines, 2)
  expect_equal(res$rtf, 
    paste0("\\trowd\\trgaph0\\trql\\cellx8640\\ql Cylinders:fork\\cell\\row\n", 
           "\\trowd\\trgaph0\\trql\\cellx8640\\ql\\cell\\row\n"))
  
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
  
  expect_equal(get_cell_borders(2, 4, 4, 4, "all", "B"), 
               "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrr\\brdrs")    
  
  expect_equal(get_cell_borders(2, 3, 4, 4, "all", "B"), 
               "\\clbrdrt\\brdrs\\clbrdrb\\brdrs")  
  
  expect_equal(get_cell_borders(2, 1, 4, 4, "all", "B"), 
               "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs") 
  
})


test_that("page_template_rtf works as expected.", {
  
  
  tbl <- create_table(mtcars) %>% 
    page_by(cyl, "Cylinders:")
  
  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    page_header("left", "right") %>% 
    titles("Hello") %>%
    footnotes("Goodbye") %>% 
    add_content(tbl) %>% 
    page_footer("left", right = "right")
  
  
  rpt <- page_setup_rtf(rpt)
  

  
  res <- page_template_rtf(rpt)
  
  res
  expect_equal(res$lines, 6)
  expect_equal(res$titles$lines, 2)
  
  
})

test_that("get_page_numbers_rtf works as expected.", {
  
  res <- get_page_numbers_rtf("Page [pg] of [tpg]")
  
  res
  expect_equal(res, "Page \\chpgn  of {\\field{\\*\\fldinst  NUMPAGES }}")
  
  
})
