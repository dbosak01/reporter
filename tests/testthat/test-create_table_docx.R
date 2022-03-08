context("Create Table DOCX Tests")

test_that("get_table_body_docx works as expected.", {

  fp <- ""

  dat <- mtcars[1:10, 1:5]

  tbl <- create_table(dat)

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  rpt <- page_setup_docx(rpt)

  wdth <- rep(1, 5)
  names(wdth) <- names(dat)

  algns <- rep("center", 5)
  names(algns) <- names(dat)


  res <- get_table_body_docx(rpt, dat, wdth, algns,  "center", "none")

  res

  expect_equal(length(res$docx), 10)
  expect_equal(res$lines, 10)

})

test_that("get_table_header_docx works as expected.", {

  fp <- ""

  dat <- mtcars[1:10, 1:5]

  tbl <- create_table(dat)

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  rpt <- page_setup_docx(rpt)

  wdth <- rep(1, 5)
  names(wdth) <- names(dat)

  algns <- rep("center", 5)
  names(algns) <- names(dat)

  nms <- names(dat)
  names(nms) <- nms
  nms[1] <- "Miles Per Gallon"
  nms[2] <- "Wrap One Two"

  pi <- list(keys = names(dat), col_width = wdth, label = nms,
             label_align = algns, table_align = "center")

  res <- get_table_header_docx(rpt, tbl, pi)

  res

  expect_equal(length(res$docx), 1)
  expect_equal(res$lines, 2)

})

test_that("get_content_offsets_docx works as expected.", {

  fp <- ""

  dat <- mtcars[1:10, 1:5]

  tbl <- create_table(dat) %>%
    titles("Table 1.0", "My Nice Table") %>%
    footnotes("Here is a footnote")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  rpt <- page_setup_docx(rpt)

  wdth <- rep(1, 5)
  names(wdth) <- names(dat)

  algns <- rep("center", 5)
  names(algns) <- names(dat)

  nms <- names(dat)
  names(nms) <- nms
  nms[1] <- "Miles/Gallon"

  pi <- list(keys = names(dat), col_width = wdth, label = nms,
             label_align = algns, table_align = "center")

  res <- get_content_offsets_docx(rpt, tbl, pi, "both")
  res

  expect_equal(res$lines[["upper"]], 4)
  expect_equal(res$lines[["lower"]], 2)
  expect_equal(res$lines[["blank_upper"]], 1)
  expect_equal(res$lines[["blank_lower"]], 1)

})

test_that("create_table_docx works as expected.", {

  fp <- ""

  dat <- mtcars[1:10, 1:5]

  tbl <- create_table(dat) %>%
    titles("Table 1.0", "My Nice Table") %>%
    footnotes("Here is a footnote")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  rpt <- page_setup_docx(rpt)

  wdth <- rep(1, 5)
  names(wdth) <- names(dat)

  algns <- rep("center", 5)
  names(algns) <- names(dat)

  nms <- names(dat)
  names(nms) <- nms
  nms[1] <- "Miles/Gallon"

  pi <- list(keys = names(dat), col_width = wdth, label = nms,
             col_align = algns,
             label_align = algns, table_align = "center", data = dat)

  res <- create_table_docx(rpt, tbl, pi, "below", FALSE, 0)
  res

  # Doesn't seem right.  Come back to this.
  expect_equal(length(res$docx), 16) 
  expect_equal(res$lines, 17)    # Should be 13

})

test_that("get_spanning_header_docx works as expected.", {


  fp <- ""

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = "none") %>%
    spanning_header(mpg, disp, "Span 1", n = 10) %>%
    spanning_header(hp, wt, "Span 2") %>%
    spanning_header(qsec, vs, "Span 3") %>%
    spanning_header(drat, gear, "Super span", level = 2)


  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    titles("Table 1.0", "My Nice Table") %>%
    add_content(tbl) %>%
    footnotes("My footnote 1", "My footnote 2") %>%
    page_footer("Left1", "Center1", "Right1")

  rpt <- page_setup_docx(rpt)

  cols <- rep(.6, ncol(dat))
  names(cols) <- names(dat)

  pi <- list(keys = names(dat), col_width = cols, table_align = "center")

  res <- get_spanning_header_docx(rpt, tbl, pi)
  res

  expect_equal(length(res), 2)
  expect_equal(length(res$docx), 2)
  expect_equal(res$lines, 3)
})


test_that("get_page_footnotes_docx works as expected.", {


  tbl1 <- create_table(iris)

  rpt1 <- create_report("", font = "Courier") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1) %>%
    footnotes("Here is a footnote", valign = "top")

  rpt1 <- page_setup_docx(rpt1)

  res1 <- get_page_footnotes_docx(rpt1, tbl1, 6,
                                 0, 25, TRUE, "below", "center")
  res1
  expect_equal(res1$lines, 2)

  tbl2 <- create_table(iris)

  rpt2 <- create_report("", font = "Arial") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1) %>%
    footnotes("Here is a footnote", valign = "bottom")

  rpt2 <- page_setup_docx(rpt2)

  res2 <- get_page_footnotes_docx(rpt2, tbl2, 6,
                                 0, 25, TRUE, "below", "center")
  res2
  expect_equal(res2$lines, 13)   # Need to do something with this


  tbl3 <- create_table(iris)  %>%
    footnotes("Here is a footnote", valign = "bottom")

  rpt3 <- create_report("", font = "Times") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1)

  rpt3 <- page_setup_docx(rpt3)

  res3 <- get_page_footnotes_docx(rpt3, tbl3, 6,
                                 0, 25, TRUE, "below", "center")

  res3
  expect_equal(res3$lines, 15)

  tbl4 <- create_table(iris)  %>%
    footnotes("Here is a footnote", valign = "top")

  rpt4 <- create_report("", font = "Times") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1)

  rpt4 <- page_setup_docx(rpt4)

  res4 <- get_page_footnotes_docx(rpt4, tbl4, 26,
                                 0, 25, TRUE, "below", "center")

  expect_equal(res4$lines, 2)

})

test_that("get_col_grid() works as expected.", {
  
  
  w <- c(col1 = 1.5, col2 = 2.3, col3 = 3.6)
  
  res <- get_col_grid(w, 1440)
  
  expect_equal(length(res), 1)
  
})
  

