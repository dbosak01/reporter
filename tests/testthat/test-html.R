context("HTML Tests")

base_path <- "c:/packages/reporter/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

fnt <- "Arial"
fsz <- 10

dev <- FALSE

# Basic Tests -------------------------------------------------------------


test_that("html1: Basic table works as expected.", {
  
  
  fp <- file.path(base_path, "html/test1.html")
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "left"
  
  tbl <- create_table(dat, borders = "outside") %>%
    titles("Table 1.0", "My Nice Table", borders = "outside", 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside", 
              align = "left", width = "content")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>%
    add_content(tbl)  %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

test_that("html2: Basic table with title header works as expected.", {
  
  
  fp <- file.path(base_path, "html/test2.html")
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  
  tbl <- create_table(dat, borders = "outside") %>%
    title_header("Table 1.0", "My Nice Table", right = "Right", 
                 borders = "outside", 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside", 
              align = "left", width = "content")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>%
    add_content(tbl)  %>%
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

# Not working
# test_that("html3: Multi page table works as expected.", {
#   
#   
#   fp <- file.path(base_path, "html/test3.html")
#   
#   dat <- iris
#   
#   
#   tbl <- create_table(dat, borders = "none") %>% 
#     titles("Table 1.0", "My Nice Irises", "Another Title") %>% 
#     define(Sepal.Length, label = "Sepal Length", width = 1, align = "center") %>% 
#     define(Sepal.Width, label = "Sepal Width", width = 1, align = "centre") %>% 
#     define(Species, blank_after = TRUE)
#   
#   rpt <- create_report(fp, output_type = "HTML", font = fnt,
#                        font_size = 12, orientation = "landscape") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", c("Right1")) %>%
#     add_content(tbl, blank_row = "none") %>%
#     page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
#     footnotes("My footnote 1", "My footnote 2") 
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 7)
#   
#   
# })



# 
# test_that("html2: Basic text works as expected.", {
#   
#   fp <- file.path(base_path, "html/test2.html")
#   
#   txt <- create_text(cnt, width = 6, borders = "outside", align = "right") %>%
#     titles("Text 1.0", "My Nice Text", borders = "outside") %>%
#     footnotes("My footnote 1", "My footnote 2", borders = "outside")
#   
#   rpt <- create_report(fp, output_type = "HTML", font = fnt,
#                        font_size = fsz) %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", "Right") %>%
#     add_content(txt, align = "right") %>%
#     page_footer("Left1", "Center1", "Right1") 
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   
# })
# 
# 
# 
# test_that("html3: Basic plot works as expected.", {
#   
#   
#   library(ggplot2)
#   
#   fp <- file.path(base_path, "html/test3.html")
#   
#   p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
#   
#   plt <- create_plot(p, height = 4, width = 8, borders = c("top", "bottom", "all")) %>% 
#     titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none") %>%
#     footnotes("* Motor Trend, 1974", borders = "none") 
#   
#   
#   rpt <- create_report(fp, output_type = "HTML", font = fnt, font_size =fsz) %>%
#     page_header("Client", "Study: XYZ") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     add_content(plt, align = "right") %>%
#     page_footer("Time", "Confidential", "Page [pg] of [tpg]") 
#   
#   
#   res <- write_report(rpt)
#   
#   #print(res)
#   
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
#   
# })
