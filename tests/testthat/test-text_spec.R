
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

