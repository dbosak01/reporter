context("Create Table RTF Tests")

test_that("get_table_body_rtf works as expected.", {

  fp <- ""
  
  dat <- mtcars[1:10, 1:5]
  
  tbl <- create_table(dat) %>%
    define("mpg", indent = 0.25)
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  rpt <- page_setup_rtf(rpt)
  
  wdth <- rep(1, 5)
  names(wdth) <- names(dat)
  
  algns <- rep("center", 5)
  names(algns) <- names(dat)
  
  
  res <- get_table_body_rtf(rpt, dat, wdth, algns,  "center", 
                            "none", FALSE, styles = list(), ts = tbl)

  res  
  
  # Check if indenting code is added
  expect_true(any(grepl("\\li360 22.8", res$rtf)))
  
  # Check cell padding is added
  expect_true(any(grepl("\\li40", res$rtf)))
  
  expect_equal(length(res$rtf), 10)
  expect_equal(res$lines, 10)
  
})

test_that("get_table_header_rtf works as expected.", {
  
  fp <- ""
  
  dat <- mtcars[1:10, 1:5]
  
  tbl <- create_table(dat) 
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  rpt <- page_setup_rtf(rpt)
  
  wdth <- rep(1, 5)
  names(wdth) <- names(dat)
  
  algns <- rep("center", 5)
  names(algns) <- names(dat)
  
  nms <- names(dat)
  names(nms) <- nms
  nms[1] <- "Miles Per Gallon"
  nms[2] <- "Wrap One Two"
  

  res <- get_table_header_rtf(rpt, tbl, wdth, nms, algns, "center")
  
  res  
  
  expect_equal(length(res$rtf), 1)
  expect_equal(res$lines, 2)
  
})

test_that("get_content_offsets_rtf works as expected.", {
  
  fp <- ""
  
  dat <- mtcars[1:10, 1:5]
  
  tbl <- create_table(dat) %>% 
    titles("Table 1.0", "My Nice Table") %>% 
    footnotes("Here is a footnote")
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  rpt <- page_setup_rtf(rpt)
  
  wdth <- rep(1, 5)
  names(wdth) <- names(dat)
  
  algns <- rep("center", 5)
  names(algns) <- names(dat)
  
  nms <- names(dat)
  names(nms) <- nms
  nms[1] <- "Miles/Gallon"
  
  pi <- list(keys = names(dat), col_width = wdth, label = nms,
                 label_align = algns, table_align = "center")
  
  res <- get_content_offsets_rtf(rpt, tbl, pi, "both")
  res
  
  expect_equal(res$lines[["upper"]], 4)
  expect_equal(res$lines[["lower"]], 2)
  expect_equal(res$lines[["blank_upper"]], 1)
  expect_equal(res$lines[["blank_lower"]], 1)
  
})

test_that("create_table_rtf works as expected.", {

  fp <- ""
  
  dat <- mtcars[1:10, 1:5]
  
  tbl <- create_table(dat) %>% 
    define("mpg", indent = 0.25) %>%
    titles("Table 1.0", "My Nice Table") %>% 
    footnotes("Here is a footnote")
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")
  
  rpt <- page_setup_rtf(rpt)
  
  wdth <- rep(1, 5)
  names(wdth) <- names(dat)
  
  algns <- rep("center", 5)
  names(algns) <- names(dat)
  
  nms <- names(dat)
  names(nms) <- nms
  nms[1] <- "Miles/Gallon"
  
  pi <- list(keys = names(dat), col_width = wdth, label = nms,
             label_align = algns, table_align = "center", data = dat, ts = tbl)
  
  res <- create_table_rtf(rpt, tbl, pi, "below", FALSE, 0, styles = list())
  res
  
  # Check if indenting RTF code is added
  expect_true(any(grepl("\\li360 21", res$rtf)))
  
  # Doesn't seem right.  Come back to this.
  expect_equal(length(res$rtf), 20)
  expect_equal(res$lines, 17) 
  
})

test_that("get_spanning_header_rtf works as expected.", {
  
  
  fp <- ""
  
  dat <- mtcars[1:15, ]
  
  tbl <- create_table(dat, borders = "none") %>%
    spanning_header(mpg, disp, "Span 1", n = 10) %>%
    spanning_header(hp, wt, "Span 2") %>%
    spanning_header(qsec, vs, "Span 3") %>%
    spanning_header(drat, gear, "Super span", level = 2)
  
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")
  
  rpt <- page_setup_rtf(rpt)
  
  cols <- rep(.6, ncol(dat))
  names(cols) <- names(dat)
  
  pi <- list(keys = names(dat), col_width = cols, table_align = "center")
  
  res <- get_spanning_header_rtf(rpt, tbl, pi)
  res
  
  expect_equal(length(res), 3)
  expect_equal(length(res$rtf), 2)
  expect_equal(res$lines, 3)
  
})



test_that("get_page_footnotes_rtf works as expected.", {
  
  
  tbl1 <- create_table(iris)
  
  rpt1 <- create_report("", font = "Courier") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1) %>% 
    footnotes("Here is a footnote", valign = "top")
  
  rpt1 <- page_setup_rtf(rpt1)
  
  res1 <- get_page_footnotes_rtf(rpt1, tbl1, 6,
                                  0, 25, TRUE, "below", "center")
  
  expect_equal(res1$lines, 2)
  
  tbl2 <- create_table(iris)
  
  rpt2 <- create_report("", font = "Arial") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1) %>% 
    footnotes("Here is a footnote", valign = "bottom")
  
  rpt2 <- page_setup_rtf(rpt2)
  
  res2 <- get_page_footnotes_rtf(rpt2, tbl2, 6,
                                  0, 25, TRUE, "below", "center")  
  
  expect_equal(res2$lines, 13)
  
  
  tbl3 <- create_table(iris)  %>% 
    footnotes("Here is a footnote", valign = "bottom")
  
  rpt3 <- create_report("", font = "Times") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1)
  
  rpt3 <- page_setup_rtf(rpt3)
  
  res3 <- get_page_footnotes_rtf(rpt3, tbl3, 6,
                                  0, 25, TRUE, "below", "center")  
  
  expect_equal(res3$lines, 15)
  
  tbl4 <- create_table(iris)  %>% 
    footnotes("Here is a footnote", valign = "top")
  
  rpt4 <- create_report("", font = "Times") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1)
  
  rpt4 <- page_setup_rtf(rpt4)
  
  res4 <- get_page_footnotes_rtf(rpt4, tbl4, 26,
                                  0, 25, TRUE, "below", "center")  
  
  expect_equal(res4$lines, 2)
  
})



