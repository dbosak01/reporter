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
