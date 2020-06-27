context("Hello Tests")


test_that("hello returns 1", {

  ret <- hello()

  expect_equal(ret, 1)
})
