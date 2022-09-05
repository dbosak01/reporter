context("Sizing Functions Tests")

options("logr.output" = FALSE)

test_that("get_table_cols() works as expected.", {
  
  control_cols <- c("..blank", "..page", "..row", "..page_by")
  
  tbl <- create_table(mtcars[1:10, ], show_cols = "none") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst1 <- get_table_cols(tbl, control_cols)
  
  expect_equal(lst1, c("mpg", "cyl", "hp", control_cols))
  
  tbl2 <- create_table(mtcars[1:10, ], show_cols = "all") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst2 <- get_table_cols(tbl2, control_cols)
  
  expect_equal(length(lst2), 11 + length(control_cols))
  
  tbl3 <- create_table(mtcars[1:10, ], show_cols = c("mpg", "disp", "wt")) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst3 <- get_table_cols(tbl3, control_cols)
  
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
  dat$..blank <- ""
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
  
   mw <- max(nchar(as.character(iris$Species)))
  
  
  tbl <- create_table(iris) %>% 
    define(Species, blank_after = TRUE, dedupe = TRUE, indent = .25)

  
  d <- prep_data(iris, tbl, .0833333, "")

  
  expect_equal(sum(d$..blank != ""), 3)
  expect_equal(max(nchar(d$Species)) > mw, TRUE)
  
})


test_that("get_labels works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    stub(c("mpg_cat", "name"), label = "Stub") %>% 
    define(cyl, label = "Cylinders") %>% 
    define(disp, label = "Displacement")
  
  lbls <- get_labels(dat, tbl)
  
  expect_equal(lbls[["cyl"]], "Cylinders")
  expect_equal(lbls[["hp"]], "hp")
  expect_equal(lbls[["disp"]], "Displacement")
  expect_equal(lbls[["stub"]], "Stub")
  
})

test_that("get_aligns works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    stub(c("mpg_cat", "name"), label = "Stub") %>% 
    define(cyl, label = "Cylinders", align = "right") %>% 
    define(disp, label = "Displacement", align = "center")
  
  algn <- get_aligns(dat, tbl)
  
  expect_equal(algn[["cyl"]], "right")
  expect_equal(algn[["stub"]], "left")
  expect_equal(algn[["disp"]], "center")

  
})

test_that("get_label_aligns works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    stub(c("mpg_cat", "name"), label = "Stub", label_align = "center") %>% 
    define(cyl, label = "Cylinders", label_align = "left") %>% 
    define(disp, label = "Displacement", label_align = "center")
  
  ls <- rep("right", 13) 
  names(ls) <- names(dat)
  
  lbls <- get_label_aligns(tbl, ls)
  
  expect_equal(lbls[["cyl"]], "left")
  expect_equal(lbls[["hp"]], "right")
  expect_equal(lbls[["disp"]], "center")
  expect_equal(lbls[["stub"]], "center")
  
})

test_that("get_col_formats works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    define(cyl, label = "Cylinders", format = "%.1f") %>% 
    define(disp, label = "Displacement", format = "%.2f")
  
  lbls <- get_col_formats(dat, tbl)
  
  
  expect_equal(lbls[["cyl"]], "%.1f")
  expect_equal(is.null(lbls[["hp"]]), TRUE)
  expect_equal(lbls[["disp"]], "%.2f")

  
})

test_that("get_page_wraps works as expected.", {
  
  
  tbl <- create_table(mtcars) %>% 
    define(mpg, width = 1) %>% 
    define(cyl, width = 1) %>% 
    define(disp, width = 1) %>% 
    define(hp, width = 1) %>% 
    define(drat, width = 1) %>% 
    define(wt, width = 1)
  
  wdths <- rep(1, 11)
  names(wdths) <- names(mtcars)

  res <- get_page_wraps(4, tbl, wdths, .2, c())
    
  expect_equal(length(res), 4)
  expect_equal(all(c("mpg", "cyl", "disp") %in% res[[1]]), TRUE)
  expect_equal(all(c("mpg", "cyl", "disp") %in% res[[2]]), FALSE)

  
})


test_that("get_col_widths_rtf works as expected.", {
  
  # dat, ts, labels, font, 
  # font_size, uom, gutter_width
  
  df <- mtcars
  
  tbl <- create_table(df) %>% 
    define(mpg, label = "Miles Per Gallon") %>% 
    define(cyl, width = 1.5) %>% 
    define(disp, width = 2) 

  lbls <- get_labels(df, tbl)
    
  res <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2)
  res
  
  expect_equal(res[["mpg"]] < 1, TRUE)
  expect_equal(res[["cyl"]], 1.5)
  expect_equal(res[["disp"]], 2)
  expect_equal(res[["hp"]] < .5, TRUE)
  
  
})


test_that("stub_dedupe works as expected", {
  
  df <- mtcars
  
  tbl <- create_table(df) %>% 
    stub(c("cyl", "mpg"),  width = 2, label = "Fork") %>%
    define(cyl,) %>% 
    define(mpg, dedupe = TRUE) %>% 
    define(disp, width = 2) 
  
  res <- stub_dedupe(tbl$stub, tbl$col_defs)
  
  expect_equal(res, TRUE)
  
  tbl <- create_table(df) %>% 
    stub(c("cyl", "mpg"),  width = 2, label = "Fork") %>%
    define(cyl,) %>% 
    define(mpg) %>% 
    define(disp, width = 2) 
  
  res <- stub_dedupe(tbl$stub, tbl$col_defs)
  
  expect_equal(res, FALSE)
  
})

