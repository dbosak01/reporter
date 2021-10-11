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
               paste0("<table><tr><td>Hello</td></tr>\n",
                      "<tr><td></td></tr>\n</table>"))
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
               paste0("<table><tr><td>Goodbye</td></tr>\n",
                      "<tr><td></td></tr>\n</table>"))
  expect_equal(t$lines, 2)
  
})
