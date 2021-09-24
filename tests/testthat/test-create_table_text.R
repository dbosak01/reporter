context("Create Table Text Tests")

test_that("get_justify() works as expected.", {
  
  expect_equal(get_justify(NA), "left")
  expect_equal(get_justify("right"), "right")
  expect_equal(get_justify("center"), "centre")
  expect_equal(get_justify("fork"), "left")
})


test_that("get_table_header works as expected.", {
  

  rpt <- create_report("fork.out")
  rpt$line_size <- 108
  
  tbl <- create_table(mtcars)
  
  lbl <- names(mtcars)
  names(lbl) <- names(mtcars)
  lbl
  
  lbl[["vs"]] <- "vs\n(n=25)" 
  
  aln <- c(unlist(rep("left",11)))
  names(aln) <- names(mtcars)
  w <- c(unlist(rep(.5, 11)))
  names(w) <- names(mtcars)
  w[["vs"]] <- 1

  p <- page_info(mtcars, keys = names(mtcars), font_name = "Courier New", 
                 col_width = w, 
                 col_align = aln,
                 label = lbl , label_align = aln)
  
  tbl$data
  res1 <- get_table_header(rpt, tbl, p)
  
  
  expect_equal(length(res1), 3)
  expect_equal(trimws(res1[[1]]), "vs")
      
})


test_that("get_spanning_header works as expected.", {
  
  dat <- mtcars[1:10, ]
  rpt <- create_report("fork.out")
  rpt$line_size <- 108
  
  tbl <- create_table(dat) %>% 
    spanning_header("mpg","disp", label = "Span 1",
                    label_align = "center", n = 24) %>% 
    spanning_header("hp", "wt", label = "Span 2",
                    label_align = "center", n = 25)  %>%
    spanning_header("qsec", "am",
                    label = "Span 3", label_align = "center", n = 26) %>%
    spanning_header(2, 5, level = 2,
                    label = "Span 4", label_align = "center") %>%
    spanning_header(6, 9, level = 2,
                    label = "Span 5", label_align = "center")
  
  lbl <- names(mtcars)
  names(lbl) <- names(mtcars)

  
  aln <- c(unlist(rep("right",11)))
  names(aln) <- names(mtcars)
  w <- c(unlist(rep(.5, 11)))
  names(w) <- names(mtcars)
  w[["qsec"]] <- .75
  
  p <- page_info(dat, keys = names(mtcars), font_name = "Courier New", 
                 col_width = w, 
                 col_align = aln,
                 label = lbl , label_align = aln)
  

  res1 <- get_spanning_header(rpt, tbl, p)
  res1
  
  expect_equal(length(res1), 5)
  expect_equal(trimws(res1[[1]]), "Span 4                   Span 5")
  
})


test_that("get_page_footnotes_text works as expected.", {
  
  
  tbl1 <- create_table(iris)
  
  rpt1 <- create_report("") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1) %>% 
    footnotes("Here is a footnote", valign = "top")
  
  rpt1 <- page_setup(rpt1)
  
  res1 <- get_page_footnotes_text(rpt1, tbl1, 6,
                                  0, 25, TRUE, "below")
  
  expect_equal(length(res1), 18)
  
  tbl2 <- create_table(iris)
  
  rpt2 <- create_report("") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1) %>% 
    footnotes("Here is a footnote", valign = "bottom")
  
  rpt2 <- page_setup(rpt2)
  
  res2 <- get_page_footnotes_text(rpt2, tbl2, 6,
                                  0, 25, TRUE, "below")  
  
  expect_equal(length(res2), 16)
  
  
  tbl3 <- create_table(iris)  %>% 
    footnotes("Here is a footnote", valign = "bottom")
  
  rpt3 <- create_report("") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1)
  
  rpt3 <- page_setup(rpt3)
  
  res3 <- get_page_footnotes_text(rpt3, tbl3, 6,
                                  0, 25, TRUE, "below")  
  
  expect_equal(length(res3), 18)
  
  tbl4 <- create_table(iris)  %>% 
    footnotes("Here is a footnote", valign = "top")
  
  rpt4 <- create_report("") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl1)
  
  rpt4 <- page_setup(rpt4)
  
  res4 <- get_page_footnotes_text(rpt4, tbl4, 26,
                                  0, 25, TRUE, "below")  
  
  expect_equal(length(res4), 18)
  
})


