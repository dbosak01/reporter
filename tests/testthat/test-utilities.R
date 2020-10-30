context("Utilities Tests")


test_that("gen_groups() works as expected.", {
  
  expect_equal(gen_groups(10, 3),
               c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4))
  
  expect_equal(gen_groups(12, c(3, 2, 5, 2)),
               c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4))

  expect_equal(gen_groups(10, 3, last_indices = TRUE), 
               c(3, 6, 9, 10))

  expect_equal(gen_groups(12, c(3, 2, 5, 2), last_indices = TRUE),
               c(3, 5, 10, 12))

})

test_that("get_font_family() works as expected.", {
  
  expect_equal(get_font_family("Arial"), "sans")
  expect_equal(get_font_family("Calibri"), "sans")
  expect_equal(get_font_family("Courier New"), "mono")
  expect_equal(get_font_family("Times New Roman"), "serif")
  expect_error(get_font_family("Wingdings"))
  
})


test_that("get_page_size() works as expected.", {
  ret <- get_page_size("letter", "inches")
  
  expect_equal(ret[1], 8.5)
  expect_equal(ret[2], 11)
  
  ret <- get_page_size("A4", "cm")
  expect_equal(ret[1], 21)
  expect_equal(ret[2], 29.7)
  
})


test_that("add_blank_rows() works as expected.", {
  
  tdat <- iris[order(iris$Species, decreasing = TRUE), ]
  tdat$cat <- c(rep("A", 25), rep("B", 25))
  
  res <- add_blank_rows(tdat, location = "both", vars = c("cat", "Species"))
  
  expect_equal(nrow(res), nrow(iris) + 12)
  
  expect_equal("..blank" %in% names(res), TRUE) 
  
  res2 <- add_blank_rows(tdat, location = "below", vars = c("Species"))
  
  expect_equal(nrow(res2), nrow(iris) + 3)

  res3 <- add_blank_rows(iris, location = "above")
  
  expect_equal(nrow(res3), nrow(iris) + 1)
  
})



test_that("split_cells works as expected.", {
  
  
  a1 <- c("one", "two", "three")
  b1 <- c("here is a\n long value", "Some sort of value", 
          "Another value that goes on and on")
  c1 <- c("here is a longer value", "Really", "Medium value")
  ..blank <- ""
  ..page <- NA
  ..row <- NA
  
  df <- data.frame(a1, b1, c1, ..blank, ..page, ..row)
  df
  
  w <- c(a1 = .5, b1 = 1.5, c1 = 1.25)
  
  conversion_factor <- .083333 
  
  wc <- floor(w / conversion_factor)

  
  res1 <- split_cells(df, wc)
  res1
  
  expect_equal(nrow(res1), 5) 
  
})

test_that("align_cells works as expected.", {
  
  
  x <- list(a1 = c("a", "b", "c"), 
            b1  = c("a"), 
            c1 = c("a", "b", "c", "d", "e"))
  
  g <- align_cells(x, 5)
  
  expect_equal(nrow(g), 5)
  
})


test_that("clear_missing works as expected.", {
  
  
  df <- data.frame(a = c(1, 2, 3), b = c("A", "B", "C"))
  
  df2 <- clear_missing(df)

  expect_equal(class(df2$a), "character")
  expect_equal(sum(is.na(df2$a)), 0)
  
})


test_that("push_down works as expected.", {
  
  x <- data.frame(a1 = c("a", "b", "c", "", ""), 
            b1  = c("a", rep("", 4)), 
            c1 = c("a", "b", "c", "d", "e"))
  
  res2 <- push_down(x)
  
  expect_equal(res2$a1, c("", "", "a", "b", "c"))
  expect_equal(res2$b1, c("", "", "", "", "a"))
  expect_equal(res2$c1, c("a", "b", "c", "d", "e"))
  
})

test_that("create_stub works as expected.", {
  
  x <- data.frame(a1 = c("a", "a", "a", "a", "a", "a"), 
                  b1  = c(NA, "b", "b", NA, "b", "b"), 
                  c1 = c(NA, NA, "c", NA, NA, "c"), 
                  d1 = c("Here", "is", "some", "stuff", "shouldn't", "touch"))
  
  
  tbl <- create_table(x) %>% stub(c("a1", "b1", "c1"))
  
  d <- create_stub(x, tbl)
  
  expect_equal(d$stub, c("a", "b", "c", "a", "b", "c"))
  
})


test_that("quote_names function works as expected.", {

  expect_equal(quote_names(c("fork")), c("fork"))
  expect_equal(quote_names(c(fork)), c("fork"))
  expect_equal(quote_names("fork"), c("fork"))
  expect_equal(quote_names("fork"), c("fork"))
  expect_equal(quote_names(c("fork", "bork")), c("fork", "bork"))
  expect_equal(quote_names(c(fork, bork)), c("fork", "bork"))
  
})

test_that("set_column_defaults function works as expected.", {
  
  tbl <- create_table(mtcars) 
  tbl <- column_defaults(tbl, c(mpg, cyl, disp), width = 2, format = "%.1f")
  tbl <- define(tbl, mpg, width = 3, format = "%.2f")
  
  res <- set_column_defaults(tbl, names(mtcars))

  
  expect_equal(length(res), 3)
  expect_equal(res[["mpg"]]$width, 3)
  expect_equal(res[["mpg"]]$format, "%.2f")
  expect_equal(res[["cyl"]]$width, 2)
  expect_equal(res[["cyl"]]$format, "%.1f")
  expect_equal(res[["disp"]]$width, 2)
  expect_equal(res[["disp"]]$format, "%.1f")
  
})
  

test_that("set_column_defaults function with range works as expected.", {
  
  tbl <- create_table(mtcars) 
  tbl <- column_defaults(tbl, from = mpg, to = drat, width = 2, format = "%.1f")
  tbl <- define(tbl, mpg, width = 3, format = "%.2f")
  
  res <- set_column_defaults(tbl, names(mtcars))
  
  
  expect_equal(length(res), 5)
  expect_equal(res[["mpg"]]$width, 3)
  expect_equal(res[["mpg"]]$format, "%.2f")
  expect_equal(res[["cyl"]]$width, 2)
  expect_equal(res[["cyl"]]$format, "%.1f")
  expect_equal(is.null(res[["vs"]]$width), TRUE)
  expect_equal(is.null(res[["vs"]]$format), TRUE)
  
})


test_that("set_column_defaults function all cols works as expected.", {
  
  tbl <- create_table(mtcars) 
  tbl <- column_defaults(tbl, width = 2, format = "%.1f")
  tbl <- define(tbl, mpg, width = 3, format = "%.2f")
  
  res <- set_column_defaults(tbl, names(mtcars))
  
  
  expect_equal(length(res), 11)
  expect_equal(res[["mpg"]]$width, 3)
  expect_equal(res[["mpg"]]$format, "%.2f")
  expect_equal(res[["cyl"]]$width, 2)
  expect_equal(res[["cyl"]]$format, "%.1f")
  expect_equal(res[["vs"]]$width, 2)
  expect_equal(res[["vs"]]$format, "%.1f")
  
})

# Commented out this test because the dm_final.rds file creates a dependency 
# on higher level of R > 3.5.
test_that("data with extra names on variables doesn't crash labels function.", {
  
  # fp <- file.path(base_path, "user/user8.out")
  # 
  # dat <- readRDS(file.path(base_path, "./data/dm_final.rds"))
  # 
  # #names(dat) 
  # 
  # #dat1 <- mtcars
  # 
  # #names(dat1$cyl) <- c("fork", "bork", "spork") 
  # 
  # names(dat$statistic)
  # tbl <- create_table(dat)
  # 
  # labels <- get_labels(dat, tbl)
  # 
  # names(labels)
  # length(labels)
  # 
  # ncol(dat)
  # 
  # names(dat)
  # 
  # labels[names(dat)]
  
  expect_equal(TRUE, TRUE)
  
})

