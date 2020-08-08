context("Create Table Text Tests")

test_that("get_justify() works as expected.", {
  
  expect_equal(get_justify(NA), "left")
  expect_equal(get_justify("right"), "right")
  expect_equal(get_justify("center"), "centre")
  expect_equal(get_justify("fork"), "left")
})
