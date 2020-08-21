context("Report Spec Tests")


test_that("create_report sets default values appropriately", {
  
  ret <- create_report()
  
  expect_equal(ret$orientation, "landscape")
  expect_equal(ret$output_type, "text")
  
})


test_that("create_report changes parameters appropriately", {
  
  ret <- create_report(output_type = "text", orientation = "portrait")
  
  expect_equal(ret$orientation, "portrait")
  expect_equal(ret$output_type, "text")
  
})



test_that("create_report traps invalid parameters", {
  
  expect_error(create_report(orientation = "porait"))
  expect_error(create_report(output_type = "txt"))
  
  
})


test_that("options_fixed sets default parameters appropriately", {
  
  ret <- create_report()
  
  ret <- options_fixed(ret)
  
  expect_equal(ret$cpuom, 12)
  
})


test_that("options_fixed changes parameters appropriately", {
  
  ret <- create_report()
  
  ret <- options_fixed(ret, cpuom = 10)
  
  expect_equal(ret$cpuom, 10)
  
})


test_that("options_fixed traps invalid parameters appropriately", {
  
  ret <- create_report()
  
  expect_error(options_fixed(ret, cpuom = 15))
  
  
})

test_that("Titles, footnotes, header, and footer limits work as expected.", {
    
  rpt <- create_report("fork.out")
  st <- rep("W", 50)
  
  expect_error(titles(rpt, st))
  expect_error(footnotes(rpt, st))
  expect_error(page_header(rpt, left=st))
  expect_error(page_footer(rpt, left=st))


})

test_that("add_content works as expected.", {
  
  rpt <- create_report("fork.out")
  
  rpt <- add_content(rpt, "", page_break = FALSE)
  
  # Should put a page break token before the content
  expect_equal(length(rpt$content), 1)
  expect_equal(rpt$content[[1]]$page_break, FALSE)
  
  # Invalid value
  expect_error(add_content(rpt, "", page_break = "sam"))

})
