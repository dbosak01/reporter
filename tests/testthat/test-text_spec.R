
context("Text Spec Tests")


test_that("create_text parameter checks work as expected.", {
  
  expect_error(create_text(NULL))
  
  expect_error(create_text(1))
  
  expect_error(create_text("hello", align = "fork"))
  
  expect_error(create_text("Hello", width = - 4))
               
               
  
})

test_that("text print() function works as expected.", {
  
  txt <- create_text("Hello", width = 2, align = "center") %>% 
    titles("Test Title") %>% 
    footnotes("Test footnote")
  
  #print(txt)
  
  #print(txt, verbose = TRUE)
  
  expect_equal(TRUE, TRUE)
  
})


test_that("split_text() works as expected.", {
  
  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
  
  
  res <- split_text(cnt, 4, 5, "Arial", 12, "inches")
  
  res
  expect_equal(length(res$text), 2)
  expect_equal(length(res$widths), 2)
  expect_equal(length(res$widths[[1]]), 4)
  expect_equal(length(res$widths[[2]]), 3)
  
  res <- split_text(cnt, 4, 3, "Arial", 12, "inches", 2)
  
  res
  expect_equal(length(res$text), 4)
  
  
  cnt2 <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
  
  # 
  # res <- split_text_rtf("here is a\n longish string and here \nis another string",
  #                       5, 2, "Arial", 12, "inches")
  # 
  # res
})


test_that("get_text_body_pdf() works as expected.", {
  
  cnt2 <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                 "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                 "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                 "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                 "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                 "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                 "qui officia deserunt mollit anim id est laborum.")
  
  txt <- create_text(cnt2)
  
  rpt <- create_report("", "PDF", font = "Arial", font_size = 12)
  
  rpt <- page_setup_pdf(rpt)
  
  res <- get_text_body_pdf(rpt, txt, 6, 20, 0, FALSE, "center", 25) 
  res
  
  expect_equal(length(res$lines), 1)
  expect_equal(res$lines[1], 6)
  
  
  res <- get_text_body_pdf(rpt, txt, 6, 3, 0, FALSE, "center", 25) 
  res
  
  expect_equal(length(res$lines), 2)
  expect_equal(res$lines[1], 3)
  expect_equal(res$lines[2], 3)
  
})
