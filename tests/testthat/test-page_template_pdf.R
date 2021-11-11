context("Page Template PDF Tests")

test_that("split_string_text and page_text produce proper results.", {
  
  cs <- c("Left")
  
  pdf(NULL)
  par(family = "sans", ps = 10)
  
  
  tmp <- split_string_text(cs, 6.5, "inches")
  tmp
  
  ret <- page_text(tmp$text, 12, 
                   xpos = get_points_left(1, 
                   6.5,
                   tmp$widths,
                   units = "inches"),
                   ypos = 72)
  ret
  
  expect_equal(ret$text, "Left")
  expect_equal(ret$xpos, 75)
  expect_equal(ret$ypos, 72)
  
  cs <- c("Right")
  
  tmp <- split_string_text(cs, 6.5, "inches")
  tmp
  
  ret <- page_text(tmp$text, 12, 
                   xpos = get_points_right(1, 
                                          6.5,
                                          tmp$widths,
                                          units = "inches"),
                   ypos = 72)
  ret
  
  expect_equal(ret$text, "Right")
  expect_equal(round(ret$xpos), 442)
  expect_equal(ret$ypos, 72)
  
  dev.off()
  
  
})

test_that("page_template_pdf is working as expected.", {
  
  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    page_header("Left", "Right") %>% 
    titles("Hello", blank_row = "below") %>%
    footnotes("Goodbye", blank_row = "below")
  
  rpt <- page_setup_pdf(rpt)
  
  expect_equal(rpt$page_template$page_header$lines, 1)
  expect_equal(length(rpt$page_template$page_header$pdf), 2)
  
  
})
          
        
test_that("get_titles_pdf function works as expected.", {
  
  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello", blank_row = "below") %>%
    footnotes("Goodbye", blank_row = "below")
  
  rpt <- page_setup_pdf(rpt)
  rpt$page_template$titles
  
  expect_equal(rpt$page_template$titles$lines, 2) 

  
})


test_that("get_footnotes_pdf function works as expected.", {

  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello") %>%
    footnotes("Goodbye")

  rpt <- page_setup_pdf(rpt)

  f <- rpt$page_template$footnotes
  f

  expect_equal(f$lines, 2)

})

test_that("get_title_header_pdf function works as expected.", {

  rpt2 <- create_report("", font = "Arial", font_size = 12) %>%
    title_header("Hello", right = paste("Right here is something",
                                        "really long that will wrap and wrap", 
                                        "and wrap and wrap keep wrapping"))

  rpt2 <- page_setup_pdf(rpt2)

  th <-rpt2$page_template$title_hdr
  th
  expect_equal(length(th$pdf), 4)
  expect_equal(th$lines, 4)

})


test_that("get_page_header_pdf works as expected.", {

  rpt1 <- create_report("", font = "Arial", font_size = 12) %>%
    page_header(left= c("Hello"), right = "there")

  rpt1 <- page_setup_pdf(rpt1)

  ph <- get_page_header_pdf(rpt1)
  ph

  expect_equal(ph$lines, 1)
  
  
  rpt2 <- create_report("", font = "Arial", font_size = 12) %>%
    page_header(left= c("Hello", "Goodbye"), right = "there") 
  
  rpt2 <- page_setup_pdf(rpt2)
  
  ph <- get_page_header_pdf(rpt2)
  ph
  
  expect_equal(ph$lines, 2)
  
  
  rpt3 <- create_report("", font = "Arial", font_size = 12) %>%
    page_header(left= c("Hello\nthere", "Goodbye"), right = "Right") 
  
  rpt3 <- page_setup_pdf(rpt3)
  
  ph <- get_page_header_pdf(rpt3)
  ph
  
  expect_equal(ph$lines, 3)

})


test_that("get_page_footer_pdf works as expected.", {

  rpt3 <- create_report("", font = "Arial", font_size = 12) %>%
    page_footer("Left", "Center", c("Right here", "is something"))

  rpt3 <- page_setup_pdf(rpt3)

  pf <- rpt3$page_template$page_footer
  pf

  expect_equal(pf$lines, 3)


})



test_that("get_pageby_pdf works as expected.", {

  tbl <- create_table(mtcars) %>%
    page_by(cyl, "Cylinders:")

  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello") %>%
    footnotes("Goodbye") %>%
    add_content(tbl)

  rpt <- page_setup_pdf(rpt)

  rpt$line_height

  res <- get_page_by_pdf(tbl$page_by, 6, "fork", rpt, "left")
  res

  expect_equal(res$lines, 2)
  expect_equal(length(res$pdf), 1)

})


test_that("rtf2-2: get_cell_borders_pdf works as expected.", {


  expect_equal(get_cell_borders_pdf(1, 1, 4, 4, c("all")),
               "")

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


test_that("page_template_pdf works as expected.", {


  tbl <- create_table(mtcars) %>%
    page_by(cyl, "Cylinders:")

  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    page_header("left", "right") %>%
    titles("Hello") %>%
    footnotes("Goodbye") %>%
    add_content(tbl) %>%
    page_footer("left", right = "right")


  rpt <- page_setup_pdf(rpt)



  res <- page_template_pdf(rpt)

  res
  expect_equal(res$lines, 7)
  expect_equal(res$titles$lines, 2)


})

