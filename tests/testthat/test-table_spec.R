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
  
  # Unquoted names and other parameters
  tbl <- create_table(mtcars[1:10, ]) %>% 
    spanning_header(from = mpg, to = disp, 
                    label = "Span 1", n = 25, label_align = "left") %>% 
    spanning_header(from = hp, to = qsec, 
                    label = "Span 2", n = 30) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  s <- tbl$col_spans
  
  expect_equal(length(s), 2)
  expect_equal(s[[1]]$label, "Span 1")
  expect_equal(s[[1]]$label_align, "left")
  expect_equal(s[[1]]$n, 25)
  expect_equal(s[[1]]$from, "mpg")
  expect_equal(s[[1]]$to, "disp")
  expect_equal(s[[2]]$label, "Span 2")
  expect_equal(s[[2]]$n, 30)
  expect_equal(length(tbl$col_defs), 3)
  
  # Quoted Names
  tbl <- create_table(mtcars[1:10, ]) %>% 
    spanning_header(from = "mpg", to = "disp")
  s <- tbl$col_spans
  expect_equal(s[[1]]$from, "mpg")
  expect_equal(s[[1]]$to, "disp")
  
  # Positions
  tbl <- create_table(mtcars[1:10, ]) %>% 
    spanning_header(from = 1, to = 3)
  s <- tbl$col_spans
  expect_equal(s[[1]]$from, "mpg")
  expect_equal(s[[1]]$to, "disp")
  
  # Parameter checks
  expect_error(spanning_header(tbl, "mpgg", "disP"))
  expect_error(spanning_header(tbl, 1, 25))
  expect_error(spanning_header(tbl, mpg, disp,
                               level = "one"))
  expect_error(spanning_header(tbl, "mpg", "disp",
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

test_that("use_attributes parameter works as expected.", {
  
  # Check default
  tbl <- create_table(mtcars) 
  expect_equal(tbl$use_attributes, c("label", "width", "justify", "format" ))
  
  # Check none
  tbl <- create_table(mtcars, use_attributes = "none") 
  expect_equal(tbl$use_attributes, c(""))
  
  # Check some
  tbl <- create_table(mtcars, use_attributes = c("justify", "format")) 
  expect_equal(tbl$use_attributes, c("justify", "format"))
  
  # Check invalid
  expect_error(create_table(mtcars, use_attributes = c("justify", "fork")) )
  
  
  
})



test_that("column_defaults function works as expected.", {
  
  
  tbl <- create_table(mtcars)
  cd <- column_defaults(tbl, c(mpg, cyl, disp)) 
  
  expect_equal(cd$col_dflts[[1]]$vars, c("mpg", "cyl", "disp"))
  expect_equal(is.null(cd$col_dflts[[1]]$from), TRUE)
  expect_equal(is.null(cd$col_dflts[[1]]$to), TRUE)
  
  cd <- column_defaults(tbl, from = mpg, to = disp) 
  
  expect_equal(is.null(cd$col_dflts[[1]]$vars), TRUE)
  expect_equal(cd$col_dflts[[1]]$from, "mpg")
  expect_equal(cd$col_dflts[[1]]$to, "disp")
  
  
  cd <- column_defaults(tbl, c("mpg", "cyl", "disp")) 
  
  expect_equal(cd$col_dflts[[1]]$vars, c("mpg", "cyl", "disp"))
  expect_equal(is.null(cd$col_dflts[[1]]$from), TRUE)
  expect_equal(is.null(cd$col_dflts[[1]]$to), TRUE)
  
  cd <- column_defaults(tbl, from = "mpg", to = "disp") 
  
  expect_equal(is.null(cd$col_dflts[[1]]$vars), TRUE)
  expect_equal(cd$col_dflts[[1]]$from, "mpg")
  expect_equal(cd$col_dflts[[1]]$to, "disp")
  
  
  expect_error(column_defaults(tbl, fork))
  expect_error(column_defaults(tbl, from = fork, to = disp))
  expect_error(column_defaults(tbl, from = mpg, to = fork))
  expect_error(column_defaults(tbl, from = mpg))
  expect_error(column_defaults(tbl, to = mpg))

  
  cd <- column_defaults(tbl, from = "mpg", to = "disp", width = 10,
                        align = "left", label = "fork", label_align = "right",
                        format = "%.1f", n = 12)
  expect_equal(cd$col_dflts[[1]]$width, 10)
  expect_equal(cd$col_dflts[[1]]$align, "left")
  expect_equal(cd$col_dflts[[1]]$label, "fork")
  expect_equal(cd$col_dflts[[1]]$label_align, "right")
  expect_equal(cd$col_dflts[[1]]$format, "%.1f")
  expect_equal(cd$col_dflts[[1]]$n, 12)
  
  
  
})




test_that("define_c function works as expected.", {
  
  
  tbl <- create_table(mtcars)
  d <- define_c("mpg", width = 2)
  
  expect_equal("col_def" %in% class(d), TRUE )
  expect_equal(d$width, 2)
  expect_equal(d$var, "mpg")
  
})


test_that("print.tbl_spec() works as expected.", {
  
  tbl <- create_table(mtcars) %>% 
    titles("Table 1", "My title") %>% 
    titles("Here is a much longer title to see what happens when I print it") %>% 
    footnotes("My footnote 1", "My footnote 2") %>% 
    spanning_header(from = "mpg", to = "disp", label = "My label") %>% 
    define(mpg, width = 2, label = "Miles per Gallon") %>% 
    define(cyl, align = "left")
  
  tbl
  
  tbl2 <- create_table(mtcars) 
  
  tbl2
  
  
  tbl3 <- create_table(mtcars) %>% 
    titles("Table 1", "My title") %>% 
    titles("Here is a much longer title to see what happens when I print it") %>% 
    footnotes("My footnote 1", "My footnote 2") %>% 
    spanning_header(from = "mpg", to = "disp", label = "My label") %>% 
    spanning_header(from = "vs", to = "carb", label = "My label 2") %>% 
    define(mpg, width = 2, label = "Miles per Gallon") %>% 
    define(cyl, align = "left")
  
  tbl3
  
  expect_equal(TRUE, TRUE)

})
