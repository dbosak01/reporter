
context("write_pdf Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

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
  
  
  d <- pdf_text_stream(1, "Here is some text")
  
  
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
  
  expect_equal(chars(res) >= 238, TRUE)
  
})


test_that("render.xref works as expected.", {
  
  d <- render.xref(c(20, 68, 124, 224, 267, 334), 1, 10, 514)
  
  #cat(d)
  
  expect_equal(length(d), 1)
  
  expect_equal(chars(d), 208)
  
  
})

test_that("pdf_document and render.pdf_document work as expected.", {
  
  d <- pdf_document(pdf_object(1, pdf_dictionary(Type = "/Catalog", 
                                                 Pages = "2 0 R")),
                    pdf_text_stream(2, "Here is some text"))
  
  class(d)
  
  expect_equal(length(d), 2)
  expect_equal(d[[1]]$id, 1)
  expect_equal(d[[2]]$id, 2)
  expect_equal(d[[2]]$contents, "Here is some text")
  expect_equal("pdf_document" %in% class(d), TRUE) 
  
  
  rnd <- render(d)
  
  # rnd
  #cat(rawToChar(rnd))
  expect_equal(length(rnd), 245)
  
  
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


test_that("get_header works as expected.", {
  
  # Check one page
  hdr <- get_header(page_count = 1,
                    font_name = "Arial",
                    page_ids = c(3))
  
  expect_equal(length(hdr), 3)
  
  hdrtxt <- render(pdf_document(hdr))
  
  #cat(rawToChar(hdrtxt))
  
  # if (Sys.info()[["sysname"]] == "Windows")
    expect_equal(length(hdrtxt), 369)
  # else 
  #   expect_equal(length(hdrtxt), 370)
  
  # Check two pages
  hdr <- get_header(page_count = 2,
                    page_ids = c(3, 4),
                    page_height = 500,
                    page_width = 600)
  
  expect_equal(length(hdr), 3)
  
  hdrtxt <- render(pdf_document(hdr))
  
  #cat(rawToChar(hdrtxt))
  
  # if (Sys.info()[["sysname"]] == "Windows")
    expect_equal(length(hdrtxt), 377)
  # else 
  #   expect_equal(length(hdrtxt), 378)
  
  # Check three pages
  hdr <- get_header(page_count = 3,
                    page_ids = c(3, 4, 5))
  
  expect_equal(length(hdr), 3)
  
  hdrtxt <- render(pdf_document(hdr))
  
  #cat(rawToChar(hdrtxt))
  
  # if (Sys.info()[["sysname"]] == "Windows")
    expect_equal(length(hdrtxt), 383)
  # else
  #   expect_equal(length(hdrtxt), 384)
  
})

test_that("pdf_page works as expected.", {
  
  pg1 <- pdf_page(5, 6)
  
  expect_equal(length(pg1), 4)
  expect_equal(pg1$id, 5)
  expect_equal(pg1$content_id, 6)
  expect_equal(unclass(pg1$parameters$Resources$ProcSet), list("/PDF", "/Text"))
  
  
  pg1r <- render(pg1)
  
  pg1r
  
  pg2 <- pdf_page(5, 6, c(7, 8))
  
  expect_equal(length(pg2), 5)
  expect_equal(pg2$id, 5)
  expect_equal(pg2$content_id, 6)
  expect_equal(unclass(pg2$parameters$Resources$ProcSet), list("/PDF", 
                                                     "/Text", 
                                                     "/ImageB", 
                                                     "/ImageC", 
                                                     "/ImageI"))
  
  pg2r <- render(pg2)
  
  pg2r
  
  expect_equal(pg2$graphic_ids, c(7, 8))

})

test_that("create full document works as expected.", {
  
  
  pg <- pdf_page(4, 5)
  
  strm <- pdf_text_stream(5, c(
                     "BT /F1 12 Tf 175 600 Td (Hello here is some more)Tj ET",
                     "BT /F1 12 Tf 175 580 Td (There I like text.)Tj ET",
                     "BT /F1 12 Tf 175 560 Td (World and more)Tj ET", 
                     "BT /F1 12 Tf 175 540 Td (And some more)Tj ET"))
  

  
  doc <- pdf_document(get_header(1, page_ids = 4), pg, strm)
  
  expect_equal(length(doc), 5)
  expect_equal("pdf_document" %in% class(doc), TRUE)
  
  res <- render(doc)
  
  #cat(rawToChar(res))
  
  fp <- file.path(base_path, "pdf/direct1.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  

  f <- file(fp, open="wb", encoding = "native.enc")
  
  
  writeBin(res, con = f, useBytes = TRUE)
  
  
  close(f)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("chars function works as expected.", {
  
  
  tmp <- c("there",
           "here\n")
  
  res <- chars(tmp)
  
  expect_equal(res, 10)
    

  tmp <- c("%PDF-1.P7\n",
           "%fork\n")
  
  res <- chars(tmp)

  expect_equal(res, 16)
  
  
}) 


test_that("get_text_stream function works as expected.", {
  
  contents <- c("Hello", "goodbye", "later")
  
  res <- get_text_stream(contents, 50, 600, 20, 12, 100)
  
  
  expect_equal(length(res), 3)
  
  expect_equal(res[[3]], "BT /F1 12 Tf 100 Tz 50 560 Td (later)Tj ET")
  
})



test_that("vraw function works as expected.", {
  
  str <- list("Hello\n", "thereÏ\n", charToRaw("fork\n"))
  
  str2 <- vraw(str)
  
  expect_equal(length(str2), 3)
  
  expect_equal(length(unlist(str2)), 19)
  
  
})


test_that("get_image_text works as expected.", {
  
  res <- get_image_text(img_ref = 23, height = 200, width = 400, 
                        xpos= 100, ypos = 150)
  
  
  expect_equal(res[1], "q")
  expect_equal(res[2], "400 0 0 200 100 150 cm")
  expect_equal(res[3], "/X23 Do")
  expect_equal(res[4], "Q")
  
  
})

test_that("pdf_image_stream works as expected.", {
  
  fp <- file.path(data_dir, "data/dot.jpg")
  
  f <- file(fp, open="rb", encoding = "native.enc")
  
  png <- readBin(f, "raw", 10000)
  
  close(f)
  
  
  strm <- pdf_image_stream(6, 60, 60, png)
  
  strm
  
  expect_equal(length(strm), 5)
  
  res <- render.pdf_image_stream(strm, view = TRUE)
  
  res
  
  expect_equal(length(res), 6)
  
  
})




test_that("create_pdf and add_page work as expected.", {
  
  p <- create_pdf(filename = "mypath.pdf",
                  orientation = "portrait",
                  fontsize = 12,
                  fontname = "Arial",
                  page_height = 200,
                  page_width = 500,
                  units = "cm",
                  margin_top = 3,
                  margin_left = 4) %>% 
    add_page(page_text("here is some text"), 
             page_image("myimagefile.jpg", 
                        200, 600, align= "left",
                        xpos = 25, ypos = 30)) %>% 
    add_info(author = "David",
             subject = "Stuff",
             keywords = "reports",
             title = "My Title")
  
  
  
    expect_equal(p$filename, "mypath.pdf")
    expect_equal(p$page_height, 200)
    expect_equal(p$fontname, "Arial")
    expect_equal(p$fontsize, 12)
    expect_equal(p$margin_top, 3)
    expect_equal(p$margin_left, 4)
    expect_equal(p$info, TRUE)
    expect_equal(p$author, "David")
    expect_equal(p$title, "My Title")
    expect_equal(p$subject, "Stuff")
    expect_equal(p$keywords, "reports")
    expect_equal(p$orientation, "portrait")
    expect_equal(p$units, "cm")
    expect_equal(length(p$pages), 1)
    txt <- p$pages[[1]][[1]]
    expect_equal(txt$text, "here is some text")
    img <- p$pages[[1]][[2]]
    expect_equal(img$filename, "myimagefile.jpg")
    expect_equal(img$height, 200)
    expect_equal(img$width, 600)
    expect_equal(is.null(img$align), TRUE)
    expect_equal(img$xpos, 25)
    expect_equal(img$ypos, 30)
  
})



test_that("get_image_stream works as expected.", {

  fp <- file.path(data_dir, "data/dot.jpg")
  
  res <- get_image_stream(fp)



  fp <- file.path(base_path, "pdf/dot2.jpg")
  
  if (file.exists(fp))
    file.remove(fp)

  con <- file(fp, "wb", encoding = "native.enc")

  writeBin(res, con, useBytes = TRUE)

  close(con)
  
  
  expect_equal(file.exists(fp), TRUE)


})

test_that("get_pages works as expected.", {
  
  fp <- file.path(data_dir, "data/dot.jpg")
  
  
  rpt <- create_pdf() %>% 
    add_page(page_text("Hello")) %>% 
    add_page(page_text("There")) %>% 
    add_page(page_text("Image page"),
             page_image(fp, height = 60, width = 62, xpos = 100, ypos = 150))
  
  rpt 
  
  expect_equal(length(rpt$pages), 3)
  expect_equal(length(rpt$pages[[3]]), 2)
  
  res <- get_pages(rpt$pages, 
                   margin_left = rpt$margin_left, 
                   margin_top = rpt$margin_top, 
                   page_height = rpt$page_height, 
                   page_width = rpt$page_width,
                   fontsize = rpt$fontsize)
  
  
  res
  
  expect_equal(res$page_ids, c(4, 6, 8))
  expect_equal(length(res$objects), 7)
  
  
})

test_that("basic write_pdf works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct2.pdf")
  
  
  r <- create_pdf(fp) %>% 
    add_page(page_text(c("Hello", "There", "Here is some text")))
  
  expect_equal(r$filename, fp)
  expect_equal(r$fontname, "Courier")
  expect_equal(length(r$pages), 1)
  expect_equal(r$pages[[1]][[1]]$text, c("Hello", "There", "Here is some text"))
  

  write_pdf(r)
  
  expect_equal(file.exists(fp), TRUE)  
  
})


test_that("write_pdf with info works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct3.pdf")

  r <- create_pdf(fp) %>% 
    add_page(page_text( c("Hello", "There", "Here is some text"))) %>% 
    add_info(author = "David Bosak",
             keywords = "one two three", subject = "Reporting",
             title = "PDF 1.0")
    

  write_pdf(r)

  expect_equal(file.exists(fp), TRUE)
  
})


test_that("write_pdf with info defaults work as expected.", {
  
  fp <- file.path(base_path, "pdf/direct4.pdf")

  
  r <- create_pdf(fp, margin_top = .5, margin_left = .5) %>% 
    add_page(page_text( c("Hello", "There", "Here is some text"))) 
  
  
  write_pdf(r)

  expect_equal(file.exists(fp), TRUE)
  
})


test_that("Two page pdf works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct5.pdf")

  
  r <- create_pdf(fp) %>% 
    add_page(page_text(c("Hello", "There", "Here is some text"))) %>% 
    add_page(page_text(c("Hey!", "Here is a second page!"))) 
    

  write_pdf(r)

  expect_equal(file.exists(fp), TRUE)
  
})


test_that("Three page pdf works as expected.", {
  
  fp <- file.path(base_path, "pdf/direct6.pdf")
  
  
  r <- create_pdf(fp) %>% 
    add_page(page_text(c("Hello", "There", "Here is some text"))) %>% 
    add_page(page_text(c("Hey!", "Here is a second page!"))) %>% 
    add_page(page_text(c("A third page!")))
  
  
  write_pdf(r)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("Simplest direct table works as expected.", {
  
  
  fp <- file.path(base_path, "pdf/direct7.pdf")
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    add_content(create_table(mtcars[1:10, ]), align = "left") %>% 
    page_footer("Page [pg] of [tpg]") %>% 
    set_margins(top = .5) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  
})



test_that("Direct table with 2 pages works as expected.", {
  
  
  
  fp <- file.path(base_path, "pdf/direct8.pdf")
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    add_content(create_table(mtcars[1:10, ]), page_break = FALSE) %>% 
    add_content(create_text("Hello")) %>% 
    add_content(create_text("There")) %>% 
    page_footer(right = "Page [pg] of [tpg]")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  
})




test_that("simple jpg document works as expected.", {
  
  library(ggplot2)
  
  ip <- file.path(data_dir, "data/plot.jpg")
  fp <- file.path(base_path, "pdf/direct9.pdf")
  
  # 
  # if (file.exists(ip))
  #   file.remove(ip)
  # 
  # p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  # 
  # ggsave(filename= ip, plot = p, width = 9, height = 5, units = "in")
  
  r <- create_pdf(fp) %>% 
    add_page(page_image(ip, height = 5, width = 9, xpos = 1, ypos = 1))
  
  
  write_pdf(r)
  
  expect_equal(file.exists(fp), TRUE)    
  

  
})


test_that("Test jpg alignment works as expected.", {
  
  library(ggplot2)
  
  ip <- file.path(data_dir, "data/plot.jpg")

  
  # 
  # if (file.exists(ip))
  #   file.remove(ip)
  # 
  # p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  # 
  # ggsave(filename= ip, plot = p, width = 6, height = 3, units = "in")
  
  
  fp <- file.path(base_path, "pdf/direct10a.pdf")
  
  r <- create_pdf(fp) %>% 
    add_page(page_image(ip, height = 3, width = 6, align = "left", line_start = 1))
  
  
  write_pdf(r)
  
  expect_equal(file.exists(fp), TRUE)   
  
  fp <- file.path(base_path, "pdf/direct10b.pdf")
  
  r <- create_pdf(fp) %>% 
    add_page(page_image(ip, height = 3, width = 6, align = "right", line_start = 1))
  
  
  write_pdf(r)
  
  expect_equal(file.exists(fp), TRUE)   
  
  
  fp <- file.path(base_path, "pdf/direct10c.pdf")
  
  r <- create_pdf(fp) %>% 
    add_page(page_image(ip, height = 3, width = 6, align = "center", line_start = 1))
  
  
  write_pdf(r)
  
  
  expect_equal(file.exists(fp), TRUE)    
  
  
  
})

# Work on this
test_that("simple jpg document works as expected in centimeters.", {

  library(ggplot2)

  ip <- file.path(data_dir, "data/plot.jpg")
  fp <- file.path(base_path, "pdf/direct11.pdf")


  # if (file.exists(ip))
  #   file.remove(ip)
  # 
  # p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  # 
  # ggsave(filename= ip, plot = p, width = 23, height = 13,
  #        units = "cm")

  r <- create_pdf(fp, page_height = 27.94, page_width = 21.59, units = "cm") %>%
    add_page(page_image(ip, height = 13, width = 23, xpos = 2.5,
                        ypos = 2.5, units = "cm"))


  write_pdf(r)

  expect_equal(file.exists(fp), TRUE)

})

test_that("Simplest direct table right aligned works as expected.", {
  
  
  fp <- file.path(base_path, "pdf/direct12.pdf")
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    add_content(create_table(mtcars[1:10, ]), align = "right") %>% 
    set_margins(top = .5)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  
})


test_that("Simplest direct plot works as expected.", {


  library(ggplot2)

  fp <- file.path(base_path, "pdf/direct13.pdf")
  
  if (file.exists(fp))
    file.remove(fp)

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8)


  rpt <- create_report(fp, output_type = "PDF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)



})


test_that("viconv function works as expected.", {
  
  v <- c("A B", "â ã")
  
  str <-  viconv(enc2utf8(v))

  expect_equal(Encoding(str), c("unknown", "latin1"))
  
  
  # str3 <- viconv(enc2utf8(v), as_raw = TRUE)
  # 
  # expect_equal(str3[[1]], charToRaw("A B"))
  # 
  # charToRaw(v[2])
  # 
  # r <- raw(3)
  # 
  # r[1] <- as.raw(226)
  # r[2] <- as.raw(32)
  # r[3] <- as.raw(227)
  # 
  # expect_equal(str3[[2]], r)

})


test_that("PDF with special chars works as expected.", {
  
  # 
  # str <- "Here are some special chars \u0203\u00e3\u00cf\u00d3"
  # str2 <- charToRaw(str)
  # 
  # hc <- paste0(str2, collapse = " ")
  # 
  # str3 <- paste0("<", hc, ">")
  library(magrittr)
  
  # ± ≠ ⋜ ⋝ ° ƒ ×
  str4 <- c("Here are some special chars â ã Ï Ó µ ¿ ‰ + - / %", 
            "Special symbols µ £ ¿ there  to mess things up ° ^ é ñ ± € ∆ ƒ") # ∆ ∈ ∑ √ ∫ Привет , 
               # "\xe2 \xe3 \xcf \xd3 \265 \277 \211 \234 £\n",
               # "Ω ± ∑ π α β")
  str4
  
  # str5 <- enc2utf8(str4)
  # 
  # Encoding(str5)
  # 
  # str6 <- iconv(str5, from ="UTF-8", to = "CP1252", sub = "Unicode")
  # Encoding(str6)
  
  # f <- file(file.path(base_path, "pdf/fork.txt"), open = "wt", encoding = "native.enc")
  # 
  # writeLines(enc2utf8(str4), con = f, useBytes = TRUE)
  # writeLines(enc2utf8(trimws(str4, which = "right")), con = f, useBytes = TRUE)
  # 
  # 
  # close(f)
  
  
  # stri_enc_detect(str4)
  # 
  # str5 <- stri_pad_right(str4, 100)
  # 
  # stri_enc_detect(str5)
  
  # lns <- stri_encode(str4, from = NULL,
  #                    to = "ANSI1251")
  # 
  # 
  # str5 <- charToRaw(as.character(str4, "!"))
  # stri_enc_detect(str5)
  # 
  # rawToChar(str5)
# 
#   str6 <- paste0("<", paste0(str5, collapse = " "), ">")
  # 
  fp <- file.path(base_path, "pdf/direct14")
  
  txt <- create_text(str4) %>% 
    titles(str4) %>% 
    footnotes("Ï Ó µ ¿")
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    titles("Ï Ó µ ¿", borders = "bottom") %>% 
    page_header("special chars â ã Ï Ó µ") %>% #  Здраво
    page_footer("special chars â ã Ï Ó µ") %>% #  Привет
    add_content(txt, align = "left") %>% 
    footnotes(str4)
  
  
  res1 <- write_report(rpt, output_type = "PDF")
  res2 <- write_report(rpt, output_type = "TXT")
  res3 <- write_report(rpt, output_type = "RTF")

  expect_equal(file.exists(paste0(res1$file_path, ".pdf")), TRUE)
  expect_equal(file.exists(paste0(res2$file_path, ".txt")), TRUE)
  expect_equal(file.exists(paste0(res3$file_path, ".rtf")), TRUE)
  
  # cat(str4)
  

  
})

