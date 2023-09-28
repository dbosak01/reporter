context("Negative System Tests")


base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

# This warning was being triggered unnecessarily.  Not working properly.
# Comment out for now as it's really not that important.
# User is responsible for their own sort.
# test_that("neg1: Unsorted Page by generates warning.", {
#   
#   fp <- file.path(base_path, "neg/neg1.out")
#   
#   
#   dat <- iris[order(iris$Sepal.Width), ]
#   tbl <- create_table(dat) %>% 
#     define(Species, visible = FALSE)
#   
#   rpt <- create_report(fp) %>% 
#     page_header("Client", "Study") %>% 
#     titles("Table 1.0", "IRIS Data Frame") %>% 
#     page_by(Species, "Species: ") %>% 
#     footnotes("Here is a footnote") %>% 
#     page_footer("Time", right = "Page [pg] of [tpg]") %>% 
#     add_content(tbl) 
#   
#   
#   expect_message(write_report(rpt))
#   
#   expect_equal(file.exists(fp), TRUE)
# 
#   
# })


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


test_that("neg5: Page header and footer generate error when added to table.", {
  
  expect_error(create_table(iris) %>% 
    page_header("Client", "Study"))
    
  expect_error(create_table(iris) %>% 
                 page_footer("Client", "Study"))
    
})

test_that("neg6: Title header generates error when added to invalid object", {
  
  expect_error(list() %>% 
                 title_header("Client", "Study"))
  
  expect_error(c() %>% 
                 title_header("Client", "Study"))
  
})


test_that("neg7: Column doesn't exist in show_cols does not generate error.", {
  
  
  fp <- file.path(base_path, "neg/neg7.txt")
  
  
  tbl <- create_table(mtcars, show_cols = c("mpg", "cyl", "disp")) %>% 
                 define(mpg) %>% 
                 define(cyl) %>% 
                 define(hp)
  
  rpt <- create_report(fp) %>% add_content(tbl)
  
  expect_error(write_report(rpt), NA)
  
  
  
})



test_that("neg8: Long page header generates error.", {
  
  
  fp <- file.path(base_path, "neg/neg8.txt")
  
  
  tbl <- create_table(mtcars, show_cols = c("mpg", "cyl", "disp")) %>% 
    define(mpg) %>% 
    define(cyl) %>% 
    define(hp)
  
  rpt <- create_report(fp) %>% add_content(tbl) %>% 
    page_header("Here is a a whole bunch of stuff intended to go over the page",
                "width because it is way too long. Way too long. Way too long.")
  
  expect_error(write_report(rpt))
  
  
  
})


test_that("neg9: Long page footer generates error.", {
  
  
  fp <- file.path(base_path, "neg/neg9.txt")
  
  
  tbl <- create_table(mtcars, show_cols = c("mpg", "cyl", "disp")) %>% 
    define(mpg) %>% 
    define(cyl) %>% 
    define(hp)
  
  rpt <- create_report(fp) %>% add_content(tbl) %>% 
    page_footer("Here is a a whole bunch of stuff intended to go over the page",
                "width because it is way too long. Way too long. Way too long.")
  
  expect_error(write_report(rpt))
  
  
  
})

