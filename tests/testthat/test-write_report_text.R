context("Write Report Text Tests")

test_that("page_template_text() works as expected.", {
  
  
  rpt <- create_report("fork.out") %>% 
    titles("Here is a test title.") %>% 
    footnotes("Test footnote")
  
  rpt$line_size <- 140
  
  
  ptt <- page_template_text(rpt)
  
  expect_equal(length(ptt$titles), 1)
  expect_equal(length(ptt$footnotes), 1)
  expect_equal(length(ptt$page_header), 0)
  expect_equal(length(ptt$page_footer), 0)
  
  
  rpt1 <- create_report("fork.out") 
  
  rpt1$line_size <- 140
  
  
  ptt1 <- page_template_text(rpt1)
  
  expect_equal(length(ptt1$titles), 0)
  expect_equal(length(ptt1$footnotes), 0)
  expect_equal(length(ptt1$page_header), 0)
  expect_equal(length(ptt1$page_footer), 0)
  
  
  
  rpt2 <- create_report("fork.out") %>% 
    page_header(left = c("Something1", "Something2"), right = "Nothing") %>% 
    titles("Here is a test title.", "There is a title") %>% 
    footnotes("Test footnote", "Test footnote 2") %>% 
    page_footer(center = "confidential")
  
  rpt2$line_size <- 140
  
  ptt2 <- page_template_text(rpt2)
  
  expect_equal(length(ptt2$titles), 2)
  expect_equal(length(ptt2$footnotes), 2)
  expect_equal(length(ptt2$page_header), 2)
  expect_equal(length(ptt2$page_footer), 2)
  
  
})
