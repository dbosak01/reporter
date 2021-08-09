context("Print Utilities Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()


test_that("title and footnote printing functions work as expected.", {
  
  txt <- create_text("Test") %>% 
    titles("One") %>% 
    footnotes("Three")
  
  
  res1 <- capture.output(print(txt))
  expect_equal(length(res1) > 0, TRUE)
  
  txt2 <- create_text("Test") %>% 
    title_header("One") %>% 
    footnotes("Three")
  
  
  res2 <- capture.output(print(txt2))
  expect_equal(length(res2) > 0, TRUE)
  
  
})


test_that("report printing functions work as expected.", {
  
  txt <- create_text("Test") %>% 
    titles("One") %>% 
    footnotes("Three")
  
  
  rpt <- create_report("") %>% 
    add_content(txt)
  
  
  res1 <- capture.output(print(rpt))
  expect_equal(length(res1) > 0, TRUE)
  
  txt2 <- create_table(mtcars) %>% 
    title_header("One") %>% 
    footnotes("Three")
  
  rpt2 <- create_report("") %>% 
    add_content(txt2)
  
  
  res2 <- capture.output(print(rpt2))
  expect_equal(length(res2) > 0, TRUE)
  
  
  res3 <- capture.output(print(rpt2, verbose = TRUE))
  expect_equal(length(res3) > 0, TRUE)
  
  
})


test_that("write_registration_file works as expected", {
  
  pth <- file.path(base_path, "reg.txt")
  
  write_registration_file(pth)
  
  expect_equal(TRUE, TRUE)
  
})

