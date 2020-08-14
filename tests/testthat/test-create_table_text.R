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
  p

  res1 <- get_table_header(rpt, tbl, p)
  
  res1
      
})
