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
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  dat <- iris
  dat$..page <- NA
  mdm <- get_page_breaks(dat, 50, 0, off)
  
  expect_equal( unique(mdm$..page), c(1, 2, 3))
  
  
})


test_that("get_splits_text works as expected", {
  
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  
  dat <- iris
  dat$..page <- NA
  w <- c(Sepal.Length = 1.5, Sepal.Width = 1.5, Petal.Length = 1.5,
         Petal.Width = 1.5, Species = 1.25)
  
  mdm <- get_splits_text(dat, w, 50, 0, off, list())
  
  expect_equal(length(mdm), 3)
  expect_equal(nrow(mdm[[1]]), 50)
  expect_equal(nrow(mdm[[2]]), 50)
  expect_equal(nrow(mdm[[3]]), 50)
  
  
  mdm2 <- get_splits_text(dat, w, 60, 0, off, list())
  expect_equal(length(mdm2), 3)
  expect_equal(nrow(mdm2[[1]]), 60)
  expect_equal(nrow(mdm2[[2]]), 60)
  expect_equal(nrow(mdm2[[3]]), 30)
  
  mdm <- get_splits_text(dat, w, 50, 10, off, list())
  
  expect_equal(length(mdm), 4)
  expect_equal(nrow(mdm[[1]]), 40)
  expect_equal(nrow(mdm[[2]]), 50)
  expect_equal(nrow(mdm[[3]]), 50)
  expect_equal(nrow(mdm[[4]]), 10)
    
})



test_that("prep_data works as expected", {
  
  smp <- sample(1:nrow(iris), 20)
  dat <- iris[smp, ]
  datx <- dat[order(dat$Species), ]
  datx$Species <- as.character(datx$Species)
  mw <- max(nchar(datx$Species))
  
  
  tbl <- create_table(datx) %>% 
    define(Species, blank_after = TRUE, dedupe = TRUE, indent = .25)

  
  d <- prep_data(datx, tbl$col_defs, .0833333)
  
  expect_equal(sum(d$..blank != ""), 3)
  expect_equal(max(nchar(d$Species)) > mw, TRUE)
  
})
