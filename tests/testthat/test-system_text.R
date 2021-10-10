context("Text Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
    "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
    "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
    "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
    "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
    "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
    "qui officia deserunt mollit anim id est laborum.")

test_that("create_text function output works as expected.", {
  
  

  # Create the report object
  rpt <- create_report("dummy.txt", orientation="portrait") %>% 
    add_content(create_text(cnt))
  rpt$line_size <- 104
  rpt$body_line_count <- 55
  
  res1 <- create_text_pages_text(rpt, rpt$content[[1]] , 10)
  

  expect_equal(length(res1), 1)
  expect_equal(length(res1[[1]]), 6)
  
})


test_that("text1: Simplest text output works as expected.", {
  
  fp <- file.path(base_path, "text/text1.out")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 1.0", "Simple Text Report") %>% 
    add_content(create_text(cnt)) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})



test_that("text2: Harder text output works as expected.", {
  
  fp <- file.path(base_path, "text/text2.out")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 2.0", "Harder Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(create_text(cnt)) %>% 
    page_footer(left = "Time", 
                center = "Confidential", 
                right ="Page [pg] of [tpg]")

  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text3: Even harder text output works as expected.", {
  
  fp <- file.path(base_path, "text/text3.out")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 3.0", "Even Harder Text Report") %>% 
    page_header(left = "Page [pg] of [tpg]", right = "Study: 123") %>% 
    add_content(create_text(cnt)) %>% 
    add_content(create_text(cnt)) %>% 
    add_content(create_text(cnt)) %>% 
    add_content(create_text(cnt)) %>% 
    page_footer(left = "Time", 
                center = "Confidential", 
                right =c("Total Pages [tpg]"))
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)

  expect_equal(length(lns), res$pages * 60)
})


test_that("text4: Long text output works as expected.", {
  
  fp <- file.path(base_path, "text/text4.out")

  l <- paste(rep(cnt, 10), collapse = "\n\n")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 4.0", "Long Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(create_text(l)) %>% 
    page_footer(left = "Page [pg] of [tpg]", 
                center = "Confidential", 
                right = "Time")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * 60)
  
})

test_that("text5: Table and Text output works as expected.", {
  
  fp <- file.path(base_path, "text/text5.out")
  
  tbl1 <- mtcars[1:10, ]
  tbl2 <- mtcars[11:20, ]
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 5.0", "Table and Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(create_table(tbl1), page_break = FALSE) %>% 
    add_content(create_text("* NOTE: Car information from 1971."), 
                align = "left") %>% 
    add_content(create_table(tbl2), page_break = FALSE) %>% 
    add_content(create_text("* NOTE: Car information from 1972."), 
                align = "left") %>% 
    page_footer(left = "Time", 
                center = "Confidential", 
                right ="Page [pg] of [tpg]")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text6: Very Long text output works as expected.", {
  
  debug <- TRUE
  
  if (debug) {
    fp <- file.path(base_path, "text/text6.out")
  
    l <- paste(rep(cnt, 1000), collapse = "\n\n")
    
    rpt <- create_report(fp, orientation = "portrait") %>%
      titles("Report 6.0", "Very long Text Report") %>% 
      page_header(left = "Client: ABC", right = "Study: 123") %>% 
      add_content(create_text(l)) %>% 
      page_footer(left = "Time", 
                  center = "Confidential", 
                  right ="Page [pg] of [tpg]")
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    
    lns <- readLines(fp)
    
    expect_equal(length(lns), res$pages * res$line_count)
  
  } else {
   expect_equal(TRUE, TRUE) 
  }
    
})


test_that("text7: Text page setup landscape works as expected.", {
  
  fp <- file.path(base_path, "text/text7.out")
  
  txt1 <- create_text(cnt) %>% 
    titles("Test Title ", "Test Titles 2", "Test Title 3") %>% 
    footnotes("Footnote 1", "Foonote 2", "Footnote 3", "Footnote 4", "Footnote 5")
  
  rpt <- create_report(fp, orientation = "landscape") %>%
    titles("Report 5.0", "Table and Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    page_footer(left = "Time", 
                center = "Confidential", 
                right ="Page [pg] of [tpg]")
  
  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  expect_equal(length(lns), 135)
})

# This test also puts the table footnote block on a page boundary 
test_that("text8: Text page setup portrait works as expected.", {
  
  fp <- file.path(base_path, "text/text8.out")
  
  txt1 <- create_text(cnt) %>% 
    titles("Test Title ", "Test Titles 2", "Test Title 3") %>% 
    footnotes("Footnote 1", "Foonote 2", "Footnote 3", "Footnote 4", "Footnote 5")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 5.0", "Table and Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) %>% 
    page_footer(left = "Time", 
                center = "Confidential", 
                right ="Page [pg] of [tpg]")
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  expect_equal(length(lns), 120)
})


test_that("text9: Title header with text output works as expected.", {
  
  fp <- file.path(base_path, "text/text9.out")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    title_header("Report 1.0", "Simple Text Report", 
                 right = c("Client", "Study"), blank_row = "below") %>% 
    add_content(create_text(cnt)) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text10: Title header with text output works as expected.", {
  
  fp <- file.path(base_path, "text/text10.out")
  
  txt <- create_text(cnt, width = 5) %>% 
    title_header("Report 1.0", "Simple Text Report", 
                 right = c("Client", "Study"), blank_row = "below") 
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    add_content(txt)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text11: Title header with text output and border works as expected.", {
  
  fp <- file.path(base_path, "text/text11.out")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    title_header("Report 1.0", "Simple Text Report", 
                 right = c("Client", "Study"), blank_row = "below",
                 borders = "all") %>% 
    add_content(create_text(cnt)) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text12: Title and footnote with borders on text output works as expected.", {
  
  fp <- file.path(base_path, "text/text12.out")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    page_header("Left", "Right") %>% 
    titles("Report 1.0", "Simple Text Report", blank_row = "below",
           borders = "all") %>%
    footnotes("Footnote one", "Footnote 2", borders = "all") %>% 
    add_content(create_text(cnt)) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text13: Title header with text output and border works as expected.", {
  
  fp <- file.path(base_path, "text/text13.out")
  
  
  txt <- create_text(cnt) %>% 
    title_header("Report 1.0", "Simple Text Report", 
                 right = c("Client", "Study"), blank_row = "below",
                 borders = "all") %>% 
    footnotes("Here is a footnote", borders = "all")
    
  rpt <- create_report(fp, orientation = "portrait") %>%
    add_content(txt) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text14: Title and footnote with borders on text output works as expected.", {
  
  fp <- file.path(base_path, "text/text14.out")
  
  txt <- create_text(cnt) %>% 
    titles("Report 1.0", "Simple Text Report", blank_row = "below",
           borders = "all") %>%
    footnotes("Footnote one", "Footnote 2", borders = "all") 
    
  rpt <- create_report(fp, orientation = "portrait") %>%
    page_header("Left", "Right") %>% 
    page_footer("Other left", "", "Other Right") %>% 
    add_content(txt) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text15: Text borders work as expected.", {
  
  fp <- file.path(base_path, "text/text15.out")
  
  txt <- create_text(cnt, borders = "all") %>% 
    titles("Report 1.0", "Simple Text Report", blank_row = "below",
           borders = "none") %>%
    footnotes("Footnote one", "Footnote 2", borders = "none") 
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    page_header("Left", "Right") %>% 
    page_footer("Other left", "", "Other Right") %>% 
    add_content(txt) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})


test_that("text16: Two page text spec with titles and footnotes.", {
  
  
  fp <- file.path(base_path, "text/text16.out")
  
  cnttxt <- paste(rep(cnt, 20), collapse = "")
  
  txt <- create_text(cnttxt)  %>%
    titles("Text 1.0", "My Nice Text") %>%
    footnotes("My footnote 1", "My footnote 2")
  
  rpt <- create_report(fp, output_type = "TXT") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt) %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
  
})

