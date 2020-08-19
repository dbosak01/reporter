context("Text Tests")

base_path <- "c:/packages/rptr/tests/testthat"

base_path <- "./"

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
    "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
    "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
    "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
    "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
    "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
    "qui officia deserunt mollit anim id est laborum.")

test_that("create_text function output works as expected.", {
  
  

  # Create the report object
  rpt <- create_report("dummy.txt", orientation="portrait") 
  rpt$line_size <- 104
  rpt$body_line_count <- 55
  
  res1 <- create_text_pages_text(rpt, create_text(cnt), 10, "none")
  
  res1

  expect_equal(length(res1), 1)
  expect_equal(length(res1[[1]]), 5)
  
})


test_that("text1: Simplest text output works as expected.", {
  
  fp <- file.path(base_path, "text/text1.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 1.0", "Simple Text Report") %>% 
    add_content(create_text(cnt)) 
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
})



test_that("text2: Harder text output works as expected.", {
  
  fp <- file.path(base_path, "text/text2.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 2.0", "Harder Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(create_text(cnt)) %>% 
    page_footer(left = Sys.time(), 
                center = "Confidential", 
                right ="Page [pg] of [tpg]")
  
  rpt
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
})


test_that("text3: Even harder text output works as expected.", {
  
  fp <- file.path(base_path, "text/text3.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 3.0", "Even Harder Text Report") %>% 
    page_header(left = "Page [pg] of [tpg]", right = "Study: 123") %>% 
    add_content(create_text(cnt)) %>% 
    add_content(create_text(cnt)) %>% 
    add_content(create_text(cnt)) %>% 
    add_content(create_text(cnt)) %>% 
    page_footer(left = Sys.time(), 
                center = "Confidential", 
                right =c("Total Pages [tpg]"))
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
})


test_that("text4: Long text output works as expected.", {
  
  fp <- file.path(base_path, "text/text4.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  l <- paste(rep(cnt, 10), collapse = "\n\n")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 4.0", "Long Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(create_text(l)) %>% 
    page_footer(left = "Page [pg] of [tpg]", 
                center = "Confidential", 
                right = Sys.time())
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
})

test_that("text5: Table and Text output works as expected.", {
  
  fp <- file.path(base_path, "text/text5.out")
  
  if (file.exists(fp))
    file.remove(fp)

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
    page_footer(left = Sys.time(), 
                center = "Confidential", 
                right ="Page [pg] of [tpg]")
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
})


test_that("text6: Very Long text output works as expected.", {
  
  fp <- file.path(base_path, "text/text6.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  l <- paste(rep(cnt, 1000), collapse = "\n\n")
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Report 6.0", "Very long Text Report") %>% 
    page_header(left = "Client: ABC", right = "Study: 123") %>% 
    add_content(create_text(l)) %>% 
    page_footer(left = Sys.time(), 
                center = "Confidential", 
                right ="Page [pg] of [tpg]")
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
})
