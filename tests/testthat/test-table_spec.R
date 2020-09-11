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
  
  tbl <- create_table(dat) %>%
    stub(label = "Stub", vars = c("mpg_cat", "name")) %>% 
      define(mpg_cat, label_row = TRUE, dedupe = TRUE) %>% 
      define(name, indent = .25)
  
  
  expect_equal(tbl$col_defs[[1]]$label_row, TRUE)
  expect_equal(is.null(tbl$stub), FALSE)
  
  
})



test_that("page break parameter works as expected.", {
  
  dat <- mtcars
  rownames(dat) <- NULL
  
  dat$pg <- c(rep(1, 16), rep(2, 16))
  

  
  tbl <- create_table(dat) %>%
    define(pg, page_break = TRUE)

  
  expect_equal(tbl$col_defs[[1]]$page_break, TRUE)
  expect_equal(tbl$page_var, "pg")

  
  
})
