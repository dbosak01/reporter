context("Table Spec Tests")

test_that("table spec constructor operator works as expected.", {
  

  
  tbl <- create_table(mtcars[1:10, ], show_cols = "none",
                      first_row_blank = TRUE) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  expect_equal(tbl$show_cols, "none")
  expect_equal(tbl$first_row_blank, TRUE)
  expect_equal(nrow(tbl$data), 10)
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

test_that("stub function works as expected.", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  rownames(dat) <- NULL
  
  dat <- dat[order(dat$mpg_cat), c("mpg_cat", "name", "mpg", "cyl", "disp", "hp")]
  
  dat2 <- add_blank_rows(dat, "above", vars = "mpg_cat")
  
  dat2$mpg_cat <- ifelse(!duplicated(dat2$mpg_cat ), 
                        dat2$mpg_cat, NA) 
  
  dat2$stub <- ifelse(is.na(dat2$mpg_cat), paste0("  ", dat2$name), dat2$mpg_cat)
  
  dat2$stub <- fapply(dat2$stub,  justify = "left")
  
  dat3 <- dat2[, c("stub", "mpg", "cyl", "disp", "hp")]
  
  

  
})
