
context("write_docx Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."



test_that("create_new_docx() function works as expected.", {


  
  p <- create_new_docx()


  expect_true(file.exists(p))
  
  expect_true(file.exists(file.path(p, "_rels")))
  
  expect_true(file.exists(file.path(p, "word")))
  
  expect_true(file.exists(file.path(p, "docProps")))
  
  expect_true(file.exists(file.path(p, "[Content_Types].xml")))
  
  expect_true(file.exists(file.path(p, "word/webSettings.xml")))
  
  expect_true(file.exists(file.path(p, "word/fontTable.xml")))
  
  expect_true(file.exists(file.path(p, "word/styles.xml")))
  
  expect_true(file.exists(file.path(p, "docProps/app.xml")))
  
  expect_true(file.exists(file.path(p, "docProps/core.xml")))
  
  expect_true(file.exists(file.path(p, "_rels/.rels")))
  
  expect_true(file.exists(file.path(p, "word/_rels/document.xml.rels")))
  
  expect_true(file.exists(file.path(p, "word/footnotes.xml")))
  
  expect_equal(file.exists(file.path(p, "word/endnotes.xml")), TRUE)
  
})



test_that("write_docx() function works as expected.", {
  
  p <- create_new_docx()
  
  fp <-  file.path(base_path, "docx/test.docx")
  

  write_docx(p, fp)
 
  
  expect_equal(file.exists(fp), TRUE)
  

  
})
  

