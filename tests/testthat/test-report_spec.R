context("Report Spec Tests")


test_that("create_report sets default values appropriately", {
  
  ret <- create_report()
  
  expect_equal(ret$orientation, "landscape")
  expect_equal(ret$output_type, "TXT")
  
})


test_that("create_report changes parameters appropriately", {
  
  ret <- create_report(output_type = "txt", orientation = "portrait")
  
  expect_equal(ret$orientation, "portrait")
  expect_equal(ret$output_type, "TXT")
  
})



test_that("create_report traps invalid parameters", {
  
  expect_error(create_report(orientation = "porait"))
  expect_error(create_report(output_type = "text"))
  
  
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
  
  expect_error(options_fixed(ret, cpuom = 16))
  
  
})

test_that("Titles, footnotes, header, and footer limits work as expected.", {
    
  rpt <- create_report("fork.out")
  st <- rep("W", 50)
  
  expect_error(titles(rpt, st))
  expect_error(footnotes(rpt, st))
  expect_error(page_header(rpt, left=st))
  expect_error(page_footer(rpt, left=st))


})

test_that("Footnotes traps invalid parameter as expected.", {
  
  rpt <- create_report("fork.out")
  
  expect_error(footnotes(rpt, align = "error"))
  # expect_error(footnotes(rpt, valign = "error"))
  expect_error(footnotes(rpt, blank_row = "error"))
  expect_error(footnotes(rpt, borders = "error"))
  
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


test_that("create_report parameter checks work as expected.", {
  
  
  expect_error(create_report(units = "fork"))
  expect_error(create_report(output_type = "fork"))
  expect_error(create_report(orientation = "fork"))
  expect_error(create_report(paper_size = "fork"))
  
  
  rpt <- create_report()
  expect_error(write_report(rpt))
  
  expect_error(write_report("fork"))
  expect_error(write_report(NA))
  
})

test_that("line_size and line_count parameter checks work as expected.", {

  rpt <- create_report()
  expect_error(options_fixed(rpt, line_size = "a"))  
  expect_error(options_fixed(rpt, line_size = -35))
  expect_error(options_fixed(rpt, line_count = "a"))
  expect_error(options_fixed(rpt, line_count = -876))
})




test_that("options_fixed parameter checks work as expected.", {
  
  
  rpt <- create_report()
  
  expect_error(options_fixed(rpt, editor = "fork"))
  expect_error(options_fixed(rpt, cpuom = -2))
  expect_error(options_fixed(rpt, lpuom = 2356))
  
})


test_that("preview parameter checks work as expected.", {
  
  rpt <- create_report()
  
  expect_error(write_report(rpt, preview = - 1))
  expect_error(write_report(rpt, preview = "a"))
  
})


test_that("font_type parameter checks work as expected.", {
  
  expect_error(create_report(font_type = "fork"))
  expect_error(create_report(font_type = NULL))
  
})


test_that("title_header function works as expected.", {
  
  tbl <- create_table(mtcars)
  
  th <- tbl %>% title_header("Table 1.0", "MTCARS Sample Data",
                             right = c("One", "Two"), blank_row = "below")
  
  expect_equal(is.null(th$title_hdr[[1]]), FALSE)
  expect_equal(length(th$title_hdr[[1]]$titles), 2)
  expect_equal(length(th$title_hdr[[1]]$right), 2)
  expect_equal(th$title_hdr[[1]]$blank_row, "below")

  
})


test_that("page_by function works as expected.", {
  
  tbl <- create_table(mtcars)
  
  pg <- tbl %>% page_by(mpg, "MPG: ", "right", blank_row = "below")
  
  expect_equal(is.null(pg$page_by), FALSE)
  expect_equal(pg$page_by$var, "mpg")
  expect_equal(pg$page_by$label, "MPG: ")
  expect_equal(pg$page_by$align, "right")
  expect_equal(pg$page_by$blank_row, "below")
  
  pg <- tbl %>% page_by("mpg")
  
  expect_equal(is.null(pg$page_by), FALSE)
  expect_equal(pg$page_by$var, "mpg")
  expect_equal(is.null(pg$page_by$label), FALSE)
  expect_equal(pg$page_by$label, "mpg: ")
  expect_equal(pg$page_by$align, "left")
  expect_equal(pg$page_by$blank_row, "below")
  
  
})




