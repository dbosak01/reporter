context("Page Template Tests")


test_that("titles function works as expected.", {
  
  
  ttl <- titles(list(), "My title 1", "My title 2", 
                align = "left", blank_row = "both")
  
  
  expect_equal(ttl$titles[[1]]$align, "left")
  expect_equal(ttl$titles[[1]]$blank_row, "both")
  expect_equal(length(ttl$titles[[1]]$titles), 2)
  
})
  

test_that("get_titles function works as expected.", {
  
  
  ttl <- titles(list(), "My title 1", "My title 2", 
                align = "left", blank_row = "both")
  
  expect_error(get_titles(ttl, 30))
  
  res3 <- get_titles(ttl$titles, 30)
  
  res3
  
  expect_equal(length(res3), 4)
  expect_equal(nchar(res3[2]), 30)
  
})
  

test_that("footnotes function works as expected.", {
  
  
  ftn <- footnotes(list(), "My footnote 1", "My footnote 2", 
                align = "left", blank_row = "both")
  
  
  expect_equal(ftn$footnotes[[1]]$align, "left")
  expect_equal(ftn$footnotes[[1]]$blank_row, "both")
  expect_equal(length(ftn$footnotes[[1]]$footnotes), 2)
  
})


test_that("get_footnotes function works as expected.", {
  
  
  ftn <- footnotes(list(), "My footnote 1", "My footnote 2", 
                   align = "left", blank_row = "both")
  
  expect_error(get_footnotes(ftn, 30))
  
  res3 <- get_footnotes(ftn$footnotes, 30)
  
  res3
  
  expect_equal(length(res3), 4)
  expect_equal(nchar(res3[2]), 30)
  
})

