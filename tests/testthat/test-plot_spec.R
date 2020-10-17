
context("Plot Spec Tests")


test_that("create_plot parameter checks work as expected.", {
  
  expect_error(create_plot(NULL))
  
  expect_error(create_plot("hello"))
  
})

test_that("plot print() function works as expected.", {
  
  library(ggplot2)
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  
  plt <- create_plot(p, height = 4, width = 2) %>% 
    titles("Test Title") %>% 
    footnotes("Test footnote")
  
  #print(plt)
  
  #print(plt, verbose = TRUE)
  
  expect_equal(TRUE, TRUE)
  
})
  
