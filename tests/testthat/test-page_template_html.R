context("Page Template HTML Tests")



test_that("get_titles_html function works as expected.", {
  
  
  rpt <- create_report("", output_type = "HTML", font = "Arial", 
                       font_size = 12) %>%
    titles("Hello", blank_row = "below") %>%
    footnotes("Goodbye", blank_row = "below")
  
  rpt <- page_setup_html(rpt)
  
  t <- get_titles_html(rpt$titles, 6, rpt)
  t
  expect_equal(t$html,
               paste0("<table ",
                      "style=\"width:9in;text-align: center;\">", 
                      "\n<tr><td>Hello</td></tr>\n",
                      "<tr><td>&nbsp;</td></tr>\n</table>"))
  expect_equal(t$lines, 2)
  
})



test_that("get_footnotes_html function works as expected.", {
  
  
  rpt <- create_report("", output_type = "HTML", font = "Arial", 
                       font_size = 12) %>%
    titles("Hello", blank_row = "below") %>%
    footnotes("Goodbye", blank_row = "below")
  
  rpt <- page_setup_html(rpt)
  
  t <- get_footnotes_html(rpt$footnotes, 6, rpt)
  t
  expect_equal(t$html,
               paste0("<table ",
                      "style=\"width:9in;text-align: left;\">", 
                      "\n<tr><td>Goodbye</td></tr>\n",
                      "<tr><td>&nbsp;</td></tr>\n</table>"))
  expect_equal(t$lines, 2)
  
})



test_that("get_title_header_html function works as expected.", {

  rpt2 <- create_report("", font = "Arial", font_size = 12) %>%
    title_header("Hello", right = paste("Right here is something",
    "really long that will wrap and wrap and wrap and wrap keep wrapping"))

  rpt2 <- page_setup_html(rpt2)

  th <- get_title_header_html(rpt2$title_hdr, 6, rpt2)
  th
  expect_equal(nchar(th$html) > 1, TRUE)
  expect_equal(th$lines, 2) # For now
  #  expect_equal(th$twips, 1440)

})


test_that("get_page_header_html works as expected.", {

  rpt3 <- create_report("", font = "Arial", font_size = 12) %>%
    page_header(left= c("Hello"),
                right = paste("Right here is something that might wrap.",
                              "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer")) %>%
    page_footer("Left", "Center", "Right here is something")

  rpt3 <- page_setup_html(rpt3)

  ph <- get_page_header_html(rpt3)
  ph

  expect_equal(ph$lines, 1) # For now
  #expect_equal(ph$twips, 864)
})


test_that("get_page_footer_html works as expected.", {

  rpt3 <- create_report("", font = "Arial", font_size = 12) %>%
    page_header(left= c("Hello"),
                right = paste("Right here is something that might wrap.",
                              "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer",
                              "If it is long enough so let's make it longer")) %>%
    page_footer("Left", "Center", "Right here is something")

  rpt3 <- page_setup_html(rpt3)

  pf <- get_page_footer_html(rpt3)
  pf

  expect_equal(pf$lines, 2)
  
})



test_that("get_pageby_html works as expected.", {

  tbl <- create_table(mtcars) %>%
    page_by(cyl, "Cylinders:")

  rpt <- create_report("", font = "Arial", font_size = 12) %>%
    titles("Hello") %>%
    footnotes("Goodbye") %>%
    add_content(tbl)

  rpt <- page_setup_rtf(rpt)

  rpt$line_height

  res <- get_page_by_html(tbl$page_by, 6, "fork", rpt, "left")
  res

  expect_equal(res$lines, 2)
  expect_equal(res$html,
               paste0("<table style=\"text-align: left;width:6in;\">\n",
                      "<tr><td style=\"\">Cylinders:fork</td></tr>\n", 
                      "<tr><td>&nbsp;</td></tr>\n</table>"))

})

# 
# test_that("rtf2-2: get_cell_borders works as expected.", {
#   
#   
#   expect_equal(get_cell_borders(1, 1, 4, 4, c("all")),
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs\\clbrdrr\\brdrs")
#   
#   expect_equal(get_cell_borders(4, 1, 4, 4, c("all")),
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs\\clbrdrr\\brdrs")
#   
#   expect_equal(get_cell_borders(1, 1, 4, 4, c("outside")),
#                "\\clbrdrt\\brdrs\\clbrdrl\\brdrs")
#   
#   expect_equal(get_cell_borders(4, 1, 4, 4, c("outside")),
#                "\\clbrdrb\\brdrs\\clbrdrl\\brdrs")
#   
#   expect_equal(get_cell_borders(2, 2, 4, 4, c("outside")),
#                "")
#   
#   expect_equal(get_cell_borders(2, 4, 4, 4, c("right")),
#                "\\clbrdrr\\brdrs")
#   
#   expect_equal(get_cell_borders(2, 4, 4, 4, c("inside")),
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs")
#   
#   expect_equal(get_cell_borders(2, 4, 4, 4, "all", "B"), 
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrr\\brdrs")    
#   
#   expect_equal(get_cell_borders(2, 3, 4, 4, "all", "B"), 
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs")  
#   
#   expect_equal(get_cell_borders(2, 1, 4, 4, "all", "B"), 
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs") 
#   
# })
# 
# 
# test_that("page_template_rtf works as expected.", {
#   
#   
#   tbl <- create_table(mtcars) %>% 
#     page_by(cyl, "Cylinders:")
#   
#   rpt <- create_report("", font = "Arial", font_size = 12) %>%
#     page_header("left", "right") %>% 
#     titles("Hello") %>%
#     footnotes("Goodbye") %>% 
#     add_content(tbl) %>% 
#     page_footer("left", right = "right")
#   
#   
#   rpt <- page_setup_rtf(rpt)
#   
#   
#   
#   res <- page_template_rtf(rpt)
#   
#   res
#   expect_equal(res$lines, 6)
#   expect_equal(res$titles$lines, 2)
#   
#   
# })
# 
# test_that("get_page_numbers_rtf works as expected.", {
#   
#   res <- get_page_numbers_rtf("Page [pg] of [tpg]")
#   
#   res
#   expect_equal(res, "Page \\chpgn  of {\\field{\\*\\fldinst  NUMPAGES }}")
#   
#   
# })
# 
# 
# 
