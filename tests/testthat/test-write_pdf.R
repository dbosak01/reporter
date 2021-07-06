
context("write_pdf Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

test_that("pdf_array and render.pdf_array work as expected.", {
  
  
  arr <- pdf_array(1, 2, "three")
  
  
  expect_equal(length(arr), 3)
  expect_equal(arr[[2]], 2)
  expect_equal(arr[[3]], "three")
  
  rnd <- render(arr)
  
  
  expect_equal(rnd, "[1 2 three]")
  
})



test_that("pdf_dictionary and render.pdf_dictionary work as expected.", {
  
  
  d <- pdf_dictionary(A=1, B = 2, C = "three")
  
  
  expect_equal(length(d), 3)
  expect_equal(d[[2]], 2)
  expect_equal(d[[3]], "three")
  
  rnd <- render(d)
  
  
  rnd
  expect_equal(rnd, "<</A 1 /B 2 /C three>>")
  
})


test_that("render.pdf_dictionary with combo elements work as expected.", {
  
  
  d <- pdf_dictionary(A=1, B = pdf_array(1, 2, 3, 4), C = "three", 
                      D = pdf_dictionary(R="Hi", Q = "Spork"))
  
  
  expect_equal(length(d), 4)
  expect_equal(d[[1]], 1)
  expect_equal(render(d[[2]]), "[1 2 3 4]")
  expect_equal(d[[3]], "three")
  
  rnd <- render(d)
  
  
  rnd
  expect_equal(rnd, "<</A 1 /B [1 2 3 4] /C three /D <</R Hi /Q Spork>>>>")
  
})



test_that("pdf_object and render.pdf_object work as expected.", {
  
  
  d <- pdf_object(1, pdf_dictionary(Type = "/Catalog", 
                                    Pages = "2 0 R"))
  
  
  expect_equal(length(d), 3)
  expect_equal(d$id, 1)
  expect_equal(d$version, 0)
  expect_equal(render(d$parameters), "<</Type /Catalog /Pages 2 0 R>>")

  
  rnd <- render(d)
  
  
  #cat(rnd)
  
  
  expect_equal(rnd, "1 0 obj<</Type /Catalog /Pages 2 0 R>>\nendobj\n")
  
})


test_that("pdf_stream and render.pdf_stream work as expected.", {
  
  
  d <- pdf_stream(1, "Here is some text")
  
  
  expect_equal(length(d), 2)
  expect_equal(d$id, 1)
  expect_equal(d$contents, "Here is some text")
  
  
  rnd <- render(d)
  
  
  #cat(rnd)
  
  
  expect_equal(rnd, paste0("1 0 obj<</Length 17>>\nstream\n", 
                           "Here is some text\nendstream\nendobj\n"))
  
})




test_that("render.xref works as expected.", {
  
  d <- render.xref(c(20, 68, 124, 224, 267, 334), 1, 514)
  
  cat(d)
  
  expect_equal(length(d), 1)
  expect_equal(nchar(d), 187)
  
  
})

test_that("pdf_document and render.pdf_document work as expected.", {
  
  d <- pdf_document(pdf_object(1, pdf_dictionary(Type = "/Catalog", 
                                                 Pages = "2 0 R")),
                    pdf_stream(2, "Here is some text"))
  
  class(d)
  
  expect_equal(length(d), 2)
  expect_equal(d[[1]]$id, 1)
  expect_equal(d[[2]]$id, 2)
  expect_equal(d[[2]]$contents, "Here is some text")
  
  
  rnd <- render(d)
  
  
  cat(rnd)
  
  
  expect_equal(rnd, paste0("%PDF-1.7\n",
                           "%âãÏÓ\n",
                           "1 0 obj<</Type /Catalog /Pages 2 0 R>>\n",
                           "endobj\n",
                           "2 0 obj<</Length 17>>\nstream\n", 
                           "Here is some text\nendstream\nendobj\n",
                           "xref\n0 3\n",
                           "0000000000 65535 f\n",
                           "0000000065 00000 n\n",
                           "0000000129 00000 n\n",
                           "trailer<</Size 3 /Root 1 0 R>>\n",
                           "startxref\n125\n%%EOF"))
  
})

test_that("create full document works as expected.", {
  
  
  
  strm <- pdf_stream(6, paste0(
                     "BT /F1 12 Tf 175 600 Td (Hello)Tj ET\n",
                     "BT /F1 12 Tf 175 580 Td (World)Tj ET\n"))
  
  doc <- pdf_document(pdf_header(), strm)
  
  expect_equal(length(doc), 6)
  expect_equal("pdf_document" %in% class(doc), TRUE)
  
  res <- render(doc)
  
  cat(res)
  
  fp <- file.path(base_path, "pdf/direct1.pdf")
  
  
  
  f <- file(fp, open="w+", encoding = "native.enc")
  
  
  writeLines(enc2utf8(res), con = f, useBytes = TRUE)
  
  
  close(f)
  
  
  
})

