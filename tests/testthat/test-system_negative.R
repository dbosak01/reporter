context("Negative System Tests")


base_path <- "c:/packages/rptr/tests/testthat"

base_path <- "."

test_that("neg1: Unsorted Page by generates warning.", {
  
  fp <- file.path(base_path, "neg/neg1.out")
  
  
  dat <- iris[order(iris$Sepal.Width), ]
  tbl <- create_table(dat) %>% 
    define(Species, visible = FALSE)
  
  rpt <- create_report(fp) %>% 
    page_header("Client", "Study") %>% 
    titles("Table 1.0", "IRIS Data Frame") %>% 
    page_by(Species, "Species: ") %>% 
    footnotes("Here is a footnote") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]") %>% 
    add_content(tbl) 
  
  
  expect_message(write_report(rpt))
  
  expect_equal(file.exists(fp), TRUE)

  
})


test_that("neg2:  Page by variable not on plot generates error.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "neg/neg2.rtf")
  

  dat <- mtcars[order(mtcars$cyl), ]

  p <- ggplot(mtcars, aes(x=mpg, y=disp)) + geom_point()
  
  plt <- create_plot(p, 5, 7)
  
  rpt <- create_report(fp, output_type = "RTF") %>% 
    page_header("Client", "Study") %>% 
    titles("Table 1.0", "MTCARS Data Frame") %>% 
    page_by(Species, "Cylinders: ") %>% 
    footnotes("Here is a footnote") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]") %>% 
    add_content(plt) 
  
  #res <- write_report(rpt)
  
  expect_error(write_report(rpt))
  
  expect_equal(file.exists(fp), FALSE)
  
  
})

test_that("neg3: Invalid column names in show_cols generate error.", {

  
  fp <- file.path(base_path, "output/neg3.out")
  
  tbl <- create_table(mtcars[1:10, ], show_cols = c("mpg", "cyl", "fork", "bork"))
  
  rpt <- create_report(fp) %>% 
    add_content(tbl)
  

  expect_error(write_report(rpt))
  
  
})


test_that("neg4: Invalid column names in define functions generate error.", {
  
  
  
  expect_error(create_table(mtcars[1:10, ]) %>% 
    define(mpg) %>% 
    define(cyl) %>% 
    define(fork))
  

})
