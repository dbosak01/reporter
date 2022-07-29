
context("Attributes Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ",
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

fnt <- "Arial"
fsz <- 10

dev <- FALSE


# Basic Tests 1 - 10 ------------------------------------------------------


test_that("attr0: Title attribute class works.", {
  
  
  
  t1 <- ttl(c("Title1", "Title2"), bold = TRUE, font_size = 12,
                   align = "right", borders = c("top", "bottom"),
                   blank_row = "above", width = 8)
  
  t1
  expect_equal(length(t1$titles), 2)
  expect_equal(t1$align, "right")
  expect_equal(t1$blank_row, "above")
  expect_equal(t1$bold, TRUE)
  expect_equal(t1$font_size, 12)
  expect_equal(t1$borders[1], "top")
  expect_equal(t1$borders[2], "bottom")
  expect_equal(t1$width, 8)
  
})

test_that("attr1: Footnote attribute class works.", {
  
  
  
  f1 <- ftn(c("Foot1", "foot2"), valign = "top", width = 8,
                   align = "right", borders = c("top", "bottom"),
                   blank_row = "above")
  
  f1
  expect_equal(length(f1$footnotes), 2)
  expect_equal(f1$align, "right")
  expect_equal(f1$valign, "top")
  expect_equal(f1$blank_row, "above")
  expect_equal(f1$borders[1], "top")
  expect_equal(f1$borders[2], "bottom")
  expect_equal(f1$width, 8)
  
})


test_that("attr2: Span attribute class works.", {
  
  
  
  s1 <- span(from = "col1", to = "col2", label = "here",
                   label_align = "right", level = 2, underline = TRUE)
  
  s1
  expect_equal(s1$label, "here")
  expect_equal(s1$label_align, "right")
  expect_equal(s1$underline, TRUE)
  expect_equal(s1$from, "col1")
  expect_equal(s1$to, "col2")
  expect_equal(s1$level, 2)
  
})



test_that("attr3: apply_attributes works with title.", {
  
  
  dt <- mtcars
  
  tbl <- create_table(dt)
  
  attr(dt, "titles") <- list(ttl(c("Title1", "Title2")))

  
  expect_equal(is.null(attr(dt, "titles")), FALSE)
  
  tbl2 <- apply_attributes(tbl, dt)
  
  ttl <- tbl2$titles[[1]]
  
  expect_equal(length(ttl$titles), 2)
  expect_equal(ttl$width, "content")
  expect_equal(ttl$bold, FALSE)
  expect_equal(ttl$blank_row, "below")
  expect_equal(ttl$align, "center")

  
})



test_that("attr4: apply_attributes works with footnote.", {
  
  
  dt <- mtcars
  
  tbl <- create_table(dt)
  
  
  attr(dt, "footnotes") <- list(ftn(c("Foot1", "Foot2")))
  

  
  expect_equal(is.null(attr(dt, "footnotes")), FALSE)
  
  tbl2 <- apply_attributes(tbl, dt)
  
  ttl <- tbl2$footnotes[[1]]
  
  expect_equal(length(ttl$footnotes), 2)
  expect_equal(ttl$width, "content")
  expect_equal(ttl$valign, "top")
  expect_equal(ttl$blank_row, "above")
  expect_equal(ttl$align, "left")
  
  
})

test_that("attr4: apply_attributes works with spanning_header", {
  
  
  dt <- mtcars
  
  tbl <- create_table(dt)
  
  attr(dt, "spans") <- list(span(from = "mpg", to = "disp", 
                                     label = "Fork"))
  

  
  expect_equal(is.null(attr(dt, "spans")), FALSE)
  
  tbl2 <- apply_attributes(tbl, dt)
  
  spn <- tbl2$col_spans[[1]]
  
  expect_equal(length(spn$span_cols), 3)
  expect_equal(spn$label, "Fork")
  expect_equal(spn$label_align, "center")
  expect_equal(spn$underline, TRUE)
  expect_equal(spn$level, 1)
  
  
})


test_that("attr5: Attribute classes work.", {
  
  
  dt <- mtcars
  
  attr(dt, "titles") <- list(ttl(c("Title1", "Title2")))
  attr(dt, "footnotes") <- list(ftn(c("foot1", "foot2")))
  attr(dt, "spans") <- list(span("mpg", "disp", "Here"))
  
  tbl <- create_table(dt)

  tbl
  
  expect_equal(length(tbl$titles), 1)
  expect_equal(length(tbl$footnotes), 1)
  expect_equal(length(tbl$col_spans), 1)
  
})

test_that("attr6: Create report works with attributes.", {
  
  
  fp <- file.path(base_path, "attr/test6.txt")
  
  dt <- mtcars
  
  attr(dt, "titles") <- list(ttl(c("Title1", "Title2")))
  attr(dt, "footnotes") <- list(ftn(c("foot1", "foot2")))
  attr(dt, "spans") <- list(span("mpg", "disp", "Here"))
  
  tbl <- create_table(dt)
  
  rpt <- create_report(fp) %>% 
    add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

