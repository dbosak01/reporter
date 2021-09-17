context("Create Table RTF Tests")

test_that("get_table_body_rtf works as expected.", {

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
  
  
  res <- get_table_body_rtf(rpt, dat, wdth, algns,  "center", "none")

  res  
  
  expect_equal(length(res), 10)
  
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
  nms[1] <- "Miles Per Gallon (MPG) and more and more"
  

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
             label_align = algns, table_align = "center", data = dat)
  
  res <- create_table_rtf(rpt, tbl, pi, "below", FALSE, 0)
  res
  
  expect_equal(length(res), 14)
  
})
