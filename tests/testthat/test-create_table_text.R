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
    spanning_header(span_cols = c("mpg", "cyl", "disp"), label = "Span 1",
                    label_align = "center", n = 24) %>% 
    spanning_header(span_cols = c("hp", "drat", "wt"), label = "Span 2",
                    label_align = "center", n = 25)  %>%
    spanning_header(span_cols = c("qsec", "vs", "am"),
                    label = "Span 3", label_align = "center", n = 26) %>%
    spanning_header(span_cols = c(2:5), level = 2,
                    label = "Span 4", label_align = "center") %>%
    spanning_header(span_cols = c(6:9), level = 2,
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
  
  # expect_equal(length(res1), 3)
  # expect_equal(trimws(res1[[1]]), "vs")
  
})

