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



test_that("get_page_breaks works as expected", {
  
  dat <- iris
  dat$..page <- NA
  mdm <- get_page_breaks(dat, 50)
  
  expect_equal( unique(mdm$..page), c(1, 2, 3))
  
  
})


test_that("get_splits_text works as expected", {
  
  dat <- iris
  dat$..page <- NA
  w <- c(Sepal.Length = 1.5, Sepal.Width = 1.5, Petal.Length = 1.5,
         Petal.Width = 1.5, Species = 1.25)
  
  mdm <- get_splits_text(dat, w, 50)
  
  expect_equal(length(mdm), 3)
  expect_equal(nrow(mdm[[1]]), 50)
  expect_equal(nrow(mdm[[2]]), 50)
  expect_equal(nrow(mdm[[3]]), 50)
  
  
  mdm2 <- get_splits_text(dat, w, 60)
  expect_equal(length(mdm2), 3)
  expect_equal(nrow(mdm2[[1]]), 60)
  expect_equal(nrow(mdm2[[2]]), 60)
  expect_equal(nrow(mdm2[[3]]), 30)
    
})


test_that("get_data_size_text works as expected", {
  
  
  rpt <- create_report("fork.out")
  rpt$line_size <- 140
  rpt$body_line_count <- 40
  rpt$char_width <- 0.08333333
  
  w <- c(one = .25, two = .5, three = 1, four = 2)
  
  l <- c(one = "A really long treatment name", two = "Min", 
         three = "Max", four = "An even longer name") 
  
  res <- get_data_size_text(rpt, w, l)
  
  expect_equal(res[["width"]], 140)
  expect_equal(res[["height"]], 34)
  
})
