
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
  
  res <- capture.output(print(plt))
  
  #print(plt)
  
  expect_equal(length(res) > 0, TRUE)
  
  res <- capture.output(print(plt, verbose = TRUE))
  
  #print(plt, verbose = TRUE)
  
  expect_equal(length(res) > 0, TRUE)
  
  
  
  
})
  
test_that("plot accepts survival plots as expected.", {
  
  p <- list()
  class(p) <- c("ggcoxzph", "ggsurv", "list")
  
  plt <- create_plot(p, height = 4, width = 2)
  
  expect_equal(plt$plot, p) 
  
  class(p) <- c("fork", "list")
  
  expect_error(create_plot(p, 4, 2))
  
  class(p) <- c("gg", "list")
  
  plt <- create_plot(p, height = 4, width = 2)
  
  expect_equal(plt$plot, p)
})
