context("Page Template HTML Tests")



test_that("get_titles_html function works as expected.", {
  
  
  rpt <- create_report("", output_type = "HTML", font = "Arial", 
                       font_size = 12) %>%
    titles("Hello", "here is \n something", blank_row = "below") %>%
    footnotes("Goodbye", blank_row = "below")
  
  rpt <- page_setup_html(rpt)
  
  t <- get_titles_html(rpt$titles, 6, rpt)
  t
  expect_equal(length(t$html), 1)
  expect_equal(t$lines, 4)

})

test_that("get_titles_html function works as expected with font_size.", {
  
  #Different title font size will change title lines
  rpt <- create_report("", output_type = "HTML", font = "Arial", 
                       font_size = 9) %>%
    titles("a a a a a a a a a",
           width = 1)
  
  rpt <- page_setup_html(rpt)
  t9 <- get_titles_html(rpt$titles, 1, rpt)
  expect_equal(length(t9$html), 1)
  expect_equal(t9$lines, 2)
  
  rpt <- create_report("", output_type = "HTML", font = "Arial", 
                       font_size = 9) %>%
    titles("a a a a a a a a a",
           font_size = 14,
           width = 1)
  
  rpt <- page_setup_html(rpt)
  t14 <- get_titles_html(rpt$titles, 1, rpt)
  expect_equal(length(t14$html), 1)
  expect_equal(t14$lines, 3)
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
                      "\n<tr><td style=\"width:9in;text-align: left;", 
                      "vertical-align:text-top;\">Goodbye</td>\n",
                      "</tr>\n<tr><td colspan=\"1\">&nbsp;</td></tr>\n</table>"))
  expect_equal(t$lines, 2)
  
})

test_that("get_footnotes_html function works as expected with font_size.", {
  
  rpt <- create_report("", output_type = "HTML", font = "Arial", 
                       font_size = 12) %>%
    footnotes("a a a a a a a a a",
              width = 1)
  
  rpt <- page_setup_html(rpt)
  f12 <- rpt$page_template$footnotes
  
  expect_equal(f12$lines, 3)
  expect_equal(f12$html,
               paste0("<table style=\"width:1in;text-align: left;",
                      "\">\n<tr><td colspan=\"1\">&nbsp;",
                      "</td></tr>\n<tr><td style=\"width:1in;",
                      "text-align: left;vertical-align:text-top;",
                      "\">a&nbsp;a&nbsp;a&nbsp;a&nbsp;a&nbsp;a&nbsp;",
                      "a<br>a&nbsp;a</td>\n</tr>\n</table>")
               )
  
  rpt <- create_report("", output_type = "HTML", font = "Arial", 
                       font_size = 12) %>%
    footnotes("a a a a a a a a a",
              width = 1, font_size = 8)
  
  rpt <- page_setup_html(rpt)
  f8 <- rpt$page_template$footnotes
  
  expect_equal(f8$lines, 2)
  expect_equal(f8$html,
               paste0("<table style=\"width:1in;text-align: left;",
                      "\">\n<tr><td colspan=\"1\">&nbsp;",
                      "</td></tr>\n<tr><td style=\"width:1in;",
                      "font-size:8pt;text-align: left;vertical-align:text-top;",
                      "\">a&nbsp;a&nbsp;a&nbsp;a&nbsp;a&nbsp;a&nbsp;a&nbsp;",
                      "a&nbsp;a</td>\n</tr>\n</table>")
               )
  
})


test_that("get_title_header_html function works as expected.", {

  rpt2 <- create_report("", font = "Arial", font_size = 12) %>%
    title_header("Hello", right = paste("Right here is something",
    "really long that will wrap and wrap and wrap and wrap keep wrapping"))

  rpt2 <- page_setup_html(rpt2)

  th <- get_title_header_html(rpt2$title_hdr, 6, rpt2)
  th
  expect_equal(nchar(th$html) > 1, TRUE)
  expect_equal(th$lines, 4) 

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

  expect_equal(ph$lines, 3) 

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
                      "<tr><td style=\"\">Cylinders:&nbsp;fork</td></tr>\n", 
                      "<tr><td style=\"\">&nbsp;</td></tr>\n</table>"))

})


test_that("get_cell_borders_html works as expected.", {
  
  expect_equal(get_cell_borders_html(1, 1, 1, 1, c("all")),
      paste0("border-top:thin solid black;border-bottom:thin solid black;",
             "border-left:thin solid black;border-right:thin solid black;"))
  
  expect_equal(get_cell_borders_html(1, 1, 1, 1, c("all"), exclude = "bottom"),
               paste0("border-top:thin solid black;",
                      "border-left:thin solid black;border-right:thin solid black;"))
  
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
