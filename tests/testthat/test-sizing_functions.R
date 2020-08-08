context("Sizing Functions Tests")

test_that("get_table_cols() works as expected.", {
  

  
  tbl <- create_table(mtcars[1:10, ], show_cols = "none") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst1 <- get_table_cols(tbl)
  
  expect_equal(lst1, c("mpg", "cyl", "hp", control_cols))
  
  tbl2 <- create_table(mtcars[1:10, ], show_cols = "all") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst2 <- get_table_cols(tbl2)
  
  expect_equal(length(lst2), 11 + length(control_cols))
  
  tbl3 <- create_table(mtcars[1:10, ], show_cols = c("mpg", "disp", "wt")) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst3 <- get_table_cols(tbl3)
  
  expect_equal(lst3, c("mpg", "disp", "wt", "cyl", "hp", control_cols))
  
  
})
