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


test_that("page_header function works as expected.", {
  
  
  ph <- page_header(list(), left = c("Left 1", "Left 2"), 
                right = c("Right 1", "Right 2", "Right 3"),
                blank_row = "below")
  
  
  expect_equal(length(ph$page_header_left), 2)
  expect_equal(length(ph$page_header_right), 3)
  expect_equal(ph$page_header_blank_row, "below")
  
  expect_error(page_header(list(), left = c("a", "b", "c", "d", "e", "f")))
  expect_error(page_header(list(), blank_row = "above"))
               
  
})


test_that("page_footer function works as expected.", {
  
  
  ph <- page_footer(list(), left = c("Left 1", "Left 2"), 
                    center = "center",
                    right = c("Right 1", "Right 2", "Right 3"),
                    blank_row = "above")
  
  
  expect_equal(length(ph$page_footer_left), 2)
  expect_equal(length(ph$page_footer_center), 1)
  expect_equal(length(ph$page_footer_right), 3)
  expect_equal(ph$page_footer_blank_row, "above")
  
  expect_error(page_header(list(), left = c("a", "b", "c", "d", "e", "f")))
  expect_error(page_header(list(), blank_row = "above"))
  
})

