context("Report Spec Tests")


test_that("create_report sets default values appropriately", {
  
  ret <- create_report()
  
  expect_equal(ret$orientation, "landscape")
  expect_equal(ret$output_type, "text")
  
})


test_that("create_report changes parameters appropriately", {
  
  ret <- create_report(output_type = "docx", orientation = "portrait")
  
  expect_equal(ret$orientation, "portrait")
  expect_equal(ret$output_type, "docx")
  
})



test_that("create_report traps invalid parameters", {
  
  expect_error(create_report(orientation = "porait"))
  expect_error(create_report(output_type = "txt"))
  
  
})


test_that("options_text sets default parameters appropriately", {
  
  ret <- create_report()
  
  ret <- options_text(ret)
  
  expect_equal(ret$cpi, 12)
  
})


test_that("options_text changes parameters appropriately", {
  
  ret <- create_report()
  
  ret <- options_text(ret, cpi = 10)
  
  expect_equal(ret$cpi, 10)
  
})


test_that("options_text traps invalid parameters appropriately", {
  
  ret <- create_report()
  
  expect_error(options_text(ret, cpi = 15))
  
  
})
