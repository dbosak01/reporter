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
