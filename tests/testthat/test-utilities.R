context("Utilities Tests")

test_that("concatenation operator works as expected.", {
  
  expect_equal("1" %+% "2", "12")
  
})


test_that("gen_groups() works as expected.", {
  
  expect_equal(gen_groups(10, 3),
               c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4))
  
  expect_equal(gen_groups(12, c(3, 2, 5, 2)),
               c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4))

  expect_equal(gen_groups(10, 3, last_indices = TRUE), 
               c(3, 6, 9, 10))

  expect_equal(gen_groups(12, c(3, 2, 5, 2), last_indices = TRUE),
               c(3, 5, 10, 12))

})

test_that("get_font_family() works as expected.", {
  
  expect_equal(get_font_family("Arial"), "sans")
  expect_equal(get_font_family("Calibri"), "sans")
  expect_equal(get_font_family("Courier New"), "mono")
  expect_equal(get_font_family("Times New Roman"), "serif")
  expect_error(get_font_family("Wingdings"))
  
})


test_that("get_page_size() works as expected.", {
  ret <- get_page_size("letter", "inches")
  
  expect_equal(ret[1], 8.5)
  expect_equal(ret[2], 11)
  
  ret <- get_page_size("A4", "cm")
  expect_equal(ret[1], 21)
  expect_equal(ret[2], 29.7)
  
})


test_that("add_blank_rows() works as expected.", {
  
  tdat <- iris
  tdat$cat <- c(rep("A", 25), rep("B", 25))
  
  res <- add_blank_rows(tdat, location = "both", vars = c("cat", "Species"))
  
  expect_equal(nrow(res), nrow(iris) + 12)
  
  expect_equal(".blank" %in% names(res), TRUE) 
  
  res2 <- add_blank_rows(tdat, location = "below", vars = c("Species"))
  
  expect_equal(nrow(res2), nrow(iris) + 3)

  res3 <- add_blank_rows(iris, location = "above")
  
  expect_equal(nrow(res3), nrow(iris) + 1)
  
})
  
