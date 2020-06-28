context("Utilities Tests")


test_that("get_font_family() works as expected.", {
  
  expect_equal(get_font_family("Arial"), "sans")
  expect_equal(get_font_family("Calibri"), "sans")
  expect_equal(get_font_family("Courier New"), "mono")
  expect_equal(get_font_family("Times New Roman"), "serif")
  expect_error(get_font_family("Wingdings"))
  
})
