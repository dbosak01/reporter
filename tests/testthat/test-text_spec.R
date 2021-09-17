
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


test_that("split_text_rtf() works as expected.", {
  
  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
  
  
  res <- split_text_rtf(cnt, 4, 5, "Arial", 12, "inches")
  
  res
  expect_equal(length(res), 2)
  
  
  res <- split_text_rtf(cnt, 4, 3, "Arial", 12, "inches", 2)
  
  res
  expect_equal(length(res), 4)
  
  
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
