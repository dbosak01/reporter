
context("write_docx Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."



test_that("create_new_docx() function works as expected.", {


  
  p <- create_new_docx("Arial", 12, 0)


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
  
  p <- create_new_docx("Arial", 10, 0)
  
  fp <-  file.path(base_path, "docx/test.docx")
  

  write_docx(p, fp)
 
  
  expect_equal(file.exists(fp), TRUE)
  

  
})


test_that("get_docx_document() works as expected.", {
  
  fp <-  file.path(base_path, "docx/test1.docx")
  
  txt <- create_text("Fork")
  
  rpt <- create_report(fp, output_type = "DOCX", font = "Arial") %>%
    add_content(txt)
  
  rpt <- page_setup_docx(rpt)
  
  res <- get_docx_document(rpt)
  
  expect_true(length(res) > 0)
  
  
})


test_that("para() function works as expected.", {
  
  txt <- "here is \n a nice text string."
  
  
  res <- para(txt)
  
  res
  
  expect_equal(nchar(res) > 100, TRUE)
  
})

test_that("para() function works as expected with indentation.", {
  
  txt <- "here is \n a nice text string."
  
  
  res <- para(txt, indent_left = 360)
  
  res
  
  expect_true(
    any(grepl("<w:ind w:left=\"360\" w:right=\"0\"/></w:pPr><w:r><w:t xml:space=\"preserve\">here is ",
              res))
  )
  
  expect_equal(nchar(res) > 100, TRUE)
  
})
  

