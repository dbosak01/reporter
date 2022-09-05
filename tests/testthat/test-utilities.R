context("Utilities Tests")


test_that("utils1: gen_groups() works as expected.", {
  
  expect_equal(gen_groups(10, 3),
               c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4))
  
  expect_equal(gen_groups(12, c(3, 2, 5, 2)),
               c(1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 4, 4))

  expect_equal(gen_groups(10, 3, last_indices = TRUE), 
               c(3, 6, 9, 10))

  expect_equal(gen_groups(12, c(3, 2, 5, 2), last_indices = TRUE),
               c(3, 5, 10, 12))

})

test_that("utils2: get_font_family() works as expected.", {
  
  expect_equal(get_font_family("Arial"), "sans")
  expect_error(get_font_family("Calibri"))
  expect_equal(get_font_family("Courier"), "mono")
  expect_equal(get_font_family("Times"), "serif")
  expect_error(get_font_family("Wingdings"))
  
})


test_that("utils3: get_page_size() works as expected.", {
  ret <- get_page_size("letter", "inches")
  
  expect_equal(ret[1], 8.5)
  expect_equal(ret[2], 11)
  
  ret <- get_page_size("A4", "cm")
  expect_equal(ret[1], 21)
  expect_equal(ret[2], 29.7)
  
})


test_that("utils4: add_blank_rows() works as expected.", {
  
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



test_that("utils5: split_cells works as expected.", {
  
  
  a1 <- c("one", "two", "three")
  b1 <- c("here is a\n long value", "Some sort of value", 
          "Another value that goes on and on")
  c1 <- c("here is a longer value", "Really", "Medium value")
  ..blank <- ""
  ..page <- NA
  ..row <- NA
  
  df <- data.frame(a1, b1, c1, ..blank, ..page, ..row, stringsAsFactors = FALSE)
  df
  
  w <- c(a1 = .5, b1 = 1.5, c1 = 1.25)
  
  conversion_factor <- .083333 
  
  wc <- floor(w / conversion_factor)

  
  res1 <- split_cells(df, wc)
  res1
  
  expect_equal(nrow(res1), 5) 
  
})

test_that("utils6: align_cells works as expected.", {
  
  
  x <- list(a1 = c("a", "b", "c"), 
            b1  = c("a"), 
            c1 = c("a", "b", "c", "d", "e"))
  
  g <- align_cells(x, 5)
  
  expect_equal(nrow(g), 5)
  
})


test_that("utils7: clear_missing works as expected.", {
  
  
  df <- data.frame(a = c(1, 2, 3), b = c("A", "B", "C"), 
                   stringsAsFactors = FALSE)
  
  df2 <- clear_missing(df)

  expect_equal(class(df2$a), "character")
  expect_equal(sum(is.na(df2$a)), 0)
  
})


test_that("utils8: push_down works as expected.", {
  
  x <- data.frame(a1 = c("a", "b", "c", "", ""), 
            b1  = c("a", rep("", 4)), 
            c1 = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  
  res2 <- push_down(x)
  
  expect_equal(res2$a1, c("", "", "a", "b", "c"))
  expect_equal(res2$b1, c("", "", "", "", "a"))
  expect_equal(res2$c1, c("a", "b", "c", "d", "e"))
  
})

test_that("utils9: create_stub works as expected.", {
  
  x <- data.frame(a1 = c("a", "a", "a", "a", "a", "a"), 
                  b1  = c(NA, "b", "b", NA, "b", "b"), 
                  c1 = c(NA, NA, "c", NA, NA, "c"), 
                  d1 = c("Here", "is", "some", "stuff", "shouldn't", "touch"), 
                  stringsAsFactors = FALSE)
  x
  
  tbl <- create_table(x) %>% stub(c("a1", "b1", "c1"))
  
  d <- create_stub(x, tbl)
  
  expect_equal(d$stub, c("a", "b", "c", "a", "b", "c"))
  
})


test_that("utils10: quote_names function works as expected.", {

  expect_equal(quote_names(c("fork")), c("fork"))
  expect_equal(quote_names(c(fork)), c("fork"))
  expect_equal(quote_names("fork"), c("fork"))
  expect_equal(quote_names("fork"), c("fork"))
  expect_equal(quote_names(c("fork", "bork")), c("fork", "bork"))
  expect_equal(quote_names(c(fork, bork)), c("fork", "bork"))
  
})

test_that("utils11: set_column_defaults function works as expected.", {
  
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
  

test_that("utils12: set_column_defaults function with range works as expected.", {
  
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


test_that("utils13: set_column_defaults function all cols works as expected.", {
  
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
test_that("utils14: data with extra names on variables doesn't crash labels function.", {
  
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


test_that("utils15: split_cells_variable works as expected.", {
  
  # split_cells_variable <- function(x, col_widths, font, font_size, units)
  
  df <- data.frame(car = c("Dart", "Bart", "Here is another car name"),
                   cyl = c(8, 3, 4),
                   disp = c("My lovely disp and it gets longer", 
                            "Here is a big long string value to exceed 2 inches",
                            "Short"), stringsAsFactors = FALSE)
  
  tbl <- create_table(df) %>% 
    define(car, label = "Cars", width = 1) %>% 
    define(cyl, width = 1.5) %>% 
    define(disp, width = 2) 
  
  lbls <- get_labels(df, tbl)
  
  cw <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2)
  cw
  
  res <- split_cells_variable(df, cw, "Arial", 12, "inches", "RTF")
  res$widths
  
  expect_equal(any(class(res$data) == "data.frame"), TRUE)
  expect_equal(res$data[1, "..row"], 2)  
  expect_equal(res$data[3, "..row"], 3) 
  
})


test_that("utils16: get_width function works as expected.", {
  
  res1 <- get_text_width("This is cool.", "Arial", 12)
  res1
  
  res2 <- get_text_width("This is cool.", "Arial", 10)
  res2
  
  expect_equal(res1 > res2, TRUE)
  
  
  res3 <- get_text_width("This is cool.", "Courier", 10)
  res3
  expect_equal(res2 < res3, TRUE)
  
  
  res4 <- get_text_width("This is cool.", "Courier", 10, units = "cm")
  res4
  expect_equal(res3 == cin(res4), TRUE)
  
})

test_that("utils17: has_bottom_footnotes works as expected.", {
  
  rpt <- create_report("") 
  
  expect_equal(has_bottom_footnotes(rpt), FALSE)
  
  rpt2 <- rpt %>% 
    footnotes("Hello")
  
  expect_equal(has_bottom_footnotes(rpt2), TRUE)
  
  rpt3 <- rpt %>% 
    footnotes("Hello", valign = "top")
  
  expect_equal(has_bottom_footnotes(rpt3), FALSE)
  
})

test_that("utils18: has_top_footnotes works as expected.", {
  
  rpt <- create_report("") 
  
  expect_equal(has_top_footnotes(rpt), FALSE)
  
  rpt2 <- rpt %>% 
    footnotes("Hello")
  
  expect_equal(has_top_footnotes(rpt2), FALSE)
  
  rpt3 <- rpt %>% 
    footnotes("Hello", valign = "top")
  
  expect_equal(has_top_footnotes(rpt3), TRUE)
  
})

test_that("utils19: get_spanning_info works as expected.", {
  
  
  fp <- ""
  
  dat <- mtcars[1:15, ]
  
  tbl <- create_table(dat, borders = "none") %>%
    spanning_header(mpg, disp, "Span 1") %>%
    spanning_header(hp, wt, "Span 2") %>%
    spanning_header(qsec, vs, "Span 3") %>%
    spanning_header(drat, gear, "Super span", level = 2)
  
  
  rpt <- create_report(fp, output_type = "RTF", font = "fixed",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  rpt <- page_setup(rpt)
  
  cols <- rep(.6, ncol(dat))
  names(cols) <- names(dat)
  
  pi <- list(keys = names(dat), col_width = cols)
  w <- round(pi$col_width / rpt$char_width) - 1
  
  res <- get_spanning_info(rpt, tbl, pi, w)
  res
  
  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 6)
  
})

test_that("utils20: get_lines_rtf works as expected.", {
  
  
  expect_equal(get_lines_rtf("Here There", 3, "Arial", 10), 1)
  expect_equal(get_lines_rtf("Here\nThere", 3, "Arial", 10), 2)
  
  expect_equal(get_lines_rtf("Here\nThere now here is something else\nanother split", 
                             2, "Arial", 12), 4)
  
  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
  
  expect_equal(get_lines_rtf(cnt, 9, "Arial", 10), 3)
  
  

})


test_that("utils21: split_string_rtf() works as expected.", {
  
  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat.\n Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
  
  pdf(NULL)
  par(family = "sans", ps = 10)
  
  res <- split_string_rtf(cnt, 4, "inches")
  res
  
  
  expect_equal(length(res$rtf), 1)
  expect_equal(res$line, 8)
  
  res2 <- split_string_rtf(NA, 4, "inches")
  res2
  
  expect_equal(length(res2$rtf), 1)
  expect_equal(res2$lines, 1)
  
  res3 <- split_string_rtf("hereisareallylongstringlongerthanoneinch", 
                           1, "inches")
  res3
  
  expect_equal(length(res3$rtf), 1)
  expect_equal(res3$lines, 1)
  
  
  res4 <- split_string_rtf("here is areallylongstringlongerthan one inch", 
                           1, "inches")
  res4
  
  expect_equal(length(res4$rtf), 1)
  expect_equal(res4$lines, 3)
  
  
  res5 <- split_string_rtf("here is areally long string longer than 3 cm", 
                           3, "cm")
  res5
  
  expect_equal(length(res4$rtf), 1)
  expect_equal(res4$lines, 3)
  
  res6 <- split_string_rtf("here \nis a\nreally \nlong string\n longer than 3 cm", 
                           3, "cm")
  res6
  
  expect_equal(length(res6$rtf), 1)
  expect_equal(res6$lines, 5)
  
  
  dev.off()
  

  
  
})

test_that("utils22: split_cells_variable() works as expected.", {
  
  dat <- data.frame(col1 = c("hello", "there", "here is a big long\nline to wrap and wrap"),
                    col2 = c(1, 2, 3),
                    col3 = c("my big string I want to wrap", "fork", "bork"),
                    ..row = c(NA, NA, NA), stringsAsFactors = FALSE)
  
  res <- split_cells_variable(dat, c(col1 = 1, col2 = 1, col3 = 1), 
                              "Arial", 12, "inches", "RTF")
  res
  
  expect_equal(nrow(res$data), 3)
  expect_equal(ncol(res$data), 4)
  expect_equal(res$data$..row, c(3, 1, 4))
  expect_equal(length(res), 2)
  expect_equal(length(res$widths), 3)
  expect_equal(length(res$widths[[3]]$col1), 4)
  expect_equal(all(res$widths[[3]]$col1 < 1), TRUE)
  
})


test_that("utils23: get_image_html works as expected.", {
  
  
  tmp <-  tempfile(fileext = ".jpg")
  tmp2 <- tempfile(fileext = ".html")
  
  plt <- list(height = 4, width = 8)
  
  res <- get_image_html(tmp, tmp2, plt, "inches")
  res
  
  expect_equal(length(res), 1)
  expect_equal(substr(res, 1, 4), "<img")
  
})


test_that("utils24: get_outer_borders works as expected.", {
  
 
  expect_equal(is.null(get_outer_borders("none")), TRUE) 
  expect_equal(get_outer_borders("all"), c("top", "bottom", "left", "right")) 
  expect_equal(get_outer_borders(c("top", "bottom")), c("top", "bottom")) 
  expect_equal(get_outer_borders(c("outside", "inside")), 
               c("top", "bottom", "left", "right")) 
  
})

test_that("utils25: split_strings() works as expected.", {
  
  pdf(NULL)
  par(family = "sans", ps = 10)
  
  res <- split_strings("Here is a big long string that will not fit in one inch",
                       1, "inches")
  
  dev.off()
  
  res
  
  expect_equal(length(res$text), 4)
  expect_equal(length(res$widths), 4)
  
})


test_that("utils26: split_string_text() works as expected.", {
  
  pdf(NULL)
  par(family = "sans", ps = 10)
  
  res <- split_string_text("Here is a big long string that will not fit in one inch",
                       1, "inches")

  
  dev.off()
  
  res
  
  expect_equal(length(res$text), 4)
  expect_equal(res$lines, 4)
  expect_equal(length(res$widths), 4)
  
})

test_that("utils27: get_points_* functions work as expected.", {
  
  w <- c(1, .8, .5)
  
  
  res1 <- get_points_left(1, 4, w, "inches")
  res1
  
  expect_equal(res1, c(75, 75, 75))
  
  res2 <- get_points_right(1, 2, w, "inches")
  res2
  
  expect_equal(res2, c(72, 86.4, 108))
  
  res3 <- get_points_center(1, 2, w, "inches")
  res3
  
  expect_equal(res3, c(73, 80.2, 91))
})

test_that("utils28: apply_widths works as expected.", {
  
  df1 <- data.frame(col1 = c("here is a something", "fork", "bork"),
                    col2 = c("", 3, 43),
                    col3 = c("", "B", "C"), stringsAsFactors = FALSE,
                    "..blank" = c("L", "", ""))
  
  wdth <- c(col1 = 4, col2 = 3, col3 = 5)
  jst <- c(col1 = "left", col2 = "right", col3 = "center")
  
  res1 <- apply_widths(df1, wdth, jst)
  
  res1$col1
  res1$col2 
  res1$col3
  
  expect_equal(res1$col1[2], "fork")
  expect_equal(nchar(res1$col2[1]), 0)
  expect_equal(nchar(res1$col3[1]), 0)             

})

test_that("utils29: invisible functions work as expected.", {
  
  lst <- c("A", "B", "..blank", "..x.C")
  
  expect_equal(has_invisible(lst), TRUE)
  expect_equal(is.invisible("..x.C"), TRUE)
  expect_equal(is.invisible("..blank"), FALSE)
  expect_equal(is.invisiblev(lst), c(A = FALSE, B = FALSE, 
                                     ..blank = FALSE, ..x.C = TRUE))
  
  expect_equal(translate_invisible("C", lst), "..x.C")
  expect_equal(translate_invisible("D", lst), "D")
  
})
