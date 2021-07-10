
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
  
  
  expect_true(grep("1 0 obj<</Length", rnd, fixed = TRUE, value = FALSE) > 0)
  
  expect_true(grep(">>\nstream\nHere is some text\nendstream\nendobj\n",
                   rnd,
                   fixed = TRUE, value = FALSE) > 0)
  
  
})


test_that("pdf_info works as expected.", {
  
  inf <- pdf_info(25,
                  author = "David",
                  keywords = c("PDF", "document"),
                  title = "My Doc",
                  subject = "My Subject")
                  

  inf
  
  expect_equal(inf[["id"]], 25)
  expect_equal(inf$producer, paste0("reporter v", 
                                    getNamespaceVersion("reporter")))
  expect_equal(inf$keywords, c("PDF", "document"))
  expect_equal(inf$title, "My Doc")
  expect_equal(inf$subject, "My Subject")
  expect_equal(inf$author, "David")
  expect_equal(is.null(inf$create_date), FALSE)
  expect_equal(is.null(inf$mod_date), FALSE)
  
  
  res <- render.pdf_info(inf)
  
  #cat(res)
  
  expect_equal(chars(res) > 240, TRUE)
  
})


test_that("render.xref works as expected.", {
  
  d <- render.xref(c(20, 68, 124, 224, 267, 334), 1, 10, 514)
  
  #cat(d)
  
  expect_equal(length(d), 1)
  
  if (Sys.info()[["sysname"]] == "Windows")
     expect_equal(chars(d), 211)
  else 
    expect_equal(chars(d), 208)
  
  
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
  expect_equal("pdf_document" %in% class(d), TRUE) 
  
  
  rnd <- render(d)
  
  
  #cat(rnd)
  if (Sys.info()[["sysname"]] == "Windows")
    expect_equal(chars(rnd), 261)
  else 
    expect_equal(chars(rnd), 249)
  
  
  # paste0("%PDF-1.7\n",
  #        "%\u0203\u00e3\u00cf\u00d3\n",
  #        "1 0 obj<</Type /Catalog /Pages 2 0 R>>\n",
  #        "endobj\n",
  #        "2 0 obj<</Length 17>>\nstream\n", 
  #        "Here is some text\nendstream\nendobj\n",
  #        "xref\n0 3\n",
  #        "0000000000 65535 f\n",
  #        "0000000020 00000 n\n",
  #        "0000000068 00000 n\n",
  #        "trailer <</Size 3 /Root 1 0 R>>\n",
  #        "startxref\n137\n%%EOF")
  # 
  

})


test_that("pdf_header works as expected.", {
  
  # Check one page
  hdr <- pdf_header(pagecount = 1)
  
  expect_equal(length(hdr), 5)
  
  hdrtxt <- render(pdf_document(hdr))
  
  #cat(hdrtxt)
  
  expect_equal(nchar(hdrtxt) > 0, TRUE)
  
  # Check two pages
  hdr <- pdf_header(pagecount = 2)
  
  expect_equal(length(hdr), 6)
  
  hdrtxt <- render(pdf_document(hdr))
  
  #cat(hdrtxt)
  
  expect_equal(nchar(hdrtxt) > 0, TRUE)
  
  # Check three pages
  hdr <- pdf_header(pagecount = 3)
  
  expect_equal(length(hdr), 7)
  
  hdrtxt <- render(pdf_document(hdr))
  
  #cat(hdrtxt)
  
  expect_equal(nchar(hdrtxt) > 0, TRUE)
  
})

test_that("create full document works as expected.", {
  
  
  
  strm <- pdf_stream(6, c(
                     "BT /F1 12 Tf 175 600 Td (Hello here is some more)Tj ET",
                     "BT /F1 12 Tf 175 580 Td (There I like text.)Tj ET",
                     "BT /F1 12 Tf 175 560 Td (World and more)Tj ET", 
                     "BT /F1 12 Tf 175 540 Td (And some more)Tj ET"))
  
  doc <- pdf_document(pdf_header(1), strm)
  
  expect_equal(length(doc), 6)
  expect_equal("pdf_document" %in% class(doc), TRUE)
  
  res <- render(doc)
  
  #cat(res)
  
  fp <- file.path(base_path, "pdf/direct1.pdf")
  

  f <- file(fp, open="w+", encoding = "native.enc")
  
  
  writeLines(enc2utf8(res), con = f, useBytes = TRUE)
  
  
  close(f)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("chars function works as expected.", {
  
  
  tmp <- c("there",
           "here\n")
  
  res <- chars(tmp)
  
  
  if (Sys.info()["sysname"] == "Windows") {
  
    expect_equal(res, 9)
  } else {
    
    expect_equal(res, 10)
    
  }
  
  
  tmp <- c("%PDF-1.P7\n",
           "%âãÏÓ\n")
  
  
  res <- chars(tmp)
  
  
  if (Sys.info()["sysname"] == "Windows") {
    expect_equal(res, 20)
    
  } else {
    
    expect_equal(res, 20)
  }
  
  
}) 


test_that("get_text_stream function works as expected.", {
  
  contents <- c("Hello", "goodbye", "later")
  
  
  res <- get_text_stream(contents, 50, 600, 20, 12, 100)
  
  
  expect_equal(length(res), 3)
  
  expect_equal(res[[3]], "BT /F1 12 Tf 100 Tz 50 560 Td (later)Tj ET")
  
})

test_that("basic write_pdf works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct2.pdf")
  
  cnts <- list()
  cnts[[1]] <- c("Hello", "There", "Here is some text")
  
  write_pdf(fp, cnts, fontsize = 8, info = FALSE)

  expect_equal(file.exists(fp), TRUE)  
  
})


test_that("write_pdf with info works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct3.pdf")
  
  cnts <- list()
  cnts[[1]] <- c("Hello", "There", "Here is some text")
  
  write_pdf(fp, cnts, info = TRUE, author = "David Bosak",
            keywords = "one two three", subject = "Reporting",
            title = "PDF 1.0")
  
  expect_equal(file.exists(fp), TRUE)  
  
})


test_that("write_pdf with info defaults work as expected.", {
  
  fp <- file.path(base_path, "pdf/direct4.pdf")
  
  cnts <- list()
  cnts[[1]] <- c("Hello", "There", "Here is some text")
  
  write_pdf(fp, cnts, info = TRUE, margin_left = .5, margin_top = .5)
  
  expect_equal(file.exists(fp), TRUE)  
  
})


test_that("Two page pdf works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct5.pdf")
  
  lst <- list()
  lst[[1]] <- c("Hello", "There", "Here is some text")
  lst[[2]] <- c("Hey!", "Here is a second page!")
  
  write_pdf(fp, lst, info = TRUE, author = "David Bosak",
            keywords = "one two three", subject = "Reporting",
            title = "PDF 1.0", fontname = "XTCHNU+LMMono10-Regular")
  
  expect_equal(file.exists(fp), TRUE)  
  
})


test_that("Three page pdf works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct6.pdf")
  
  lst <- list()
  lst[[1]] <- c("Hello", "There", "Here is some text")
  lst[[2]] <- c("Hey!", "Here is a second page!")
  lst[[3]] <- c("Third page!")
  
  write_pdf(fp, lst, info = TRUE, author = "David Bosak",
            keywords = "one two three", subject = "Reporting",
            title = "PDF 1.0")
  
  expect_equal(file.exists(fp), TRUE)  
  
})

test_that("Simplest direct table works as expected.", {
  
  

    
    fp <- file.path(base_path, "pdf/direct7.pdf")
    
    rpt <- create_report(fp, output_type = "PDF") %>%
      add_content(create_table(mtcars[1:10, ]), align = "left") %>% 
      set_margins(top = .5)
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    

  
})

test_that("Direct table with 2 pages works as expected.", {
  
  

  fp <- file.path(base_path, "pdf/direct8.pdf")
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    add_content(create_table(mtcars[1:10, ])) %>% 
    add_content(create_table(mtcars[11:32, ])) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  
})



test_that("Different font works as expected.", {
  
  
  
  fp <- file.path(base_path, "pdf/direct9.pdf")
  
  lst <- list()
  lst[[1]] <- c("disp", "There", "Here is some text")

  
  write_pdf(fp, lst, info = TRUE, author = "David Bosak",
            keywords = "one two three", subject = "Reporting",
            title = "PDF 1.0", fontsize = 10)
  
  expect_equal(file.exists(fp), TRUE)  
  
  
  
})
