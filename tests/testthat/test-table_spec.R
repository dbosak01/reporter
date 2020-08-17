context("Table Spec Tests")

test_that("table spec constructor operator works as expected.", {
  

  
  tbl <- create_table(mtcars[1:10, ], show_cols = "none",
                      first_row_blank = TRUE, align = "left") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  expect_equal(tbl$show_cols, "none")
  expect_equal(tbl$first_row_blank, TRUE)
  expect_equal(nrow(tbl$data), 10)
  expect_equal(tbl$align, "left")
  expect_equal(length(tbl$col_defs), 3)
  
  
})


test_that("spanning header constructor works as expected.", {
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    spanning_header(span_cols = c("mpg", "cyl", "disp"),
                    label = "Span 1", n = 25, label_align = "left") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  s <- tbl$col_spans
  
  expect_equal(length(s), 1)
  expect_equal(s[[1]]$label, "Span 1")
  expect_equal(s[[1]]$label_align, "left")
  expect_equal(s[[1]]$n, 25)
  expect_equal(s[[1]]$span_cols, c("mpg", "cyl", "disp"))
  expect_equal(length(tbl$col_defs), 3)
  
  expect_error(spanning_header(tbl, span_cols = c("mpgg", "Cl", "disp")))
  expect_error(spanning_header(tbl, span_cols = 1:25))
  expect_error(spanning_header(tbl, span_cols = c("mpg", "cyl", "disp"),
                               level = "one"))
  expect_error(spanning_header(tbl, span_cols = c("mpg", "cyl", "disp"),
                               label_align = "lefty"))
  
})
