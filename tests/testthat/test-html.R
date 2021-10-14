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
  attr(dat[[2]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "outside") %>%
    titles("Table 1.0", "My Nice Table", borders = c("outside"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside", 
              align = "left", width = "page") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
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
  
  tbl <- create_table(dat, borders = c("outside")) %>%
    title_header("Table 1.0", "My Nice Table", right = "Right", 
                 borders = c("outside"), blank_row = "both",
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

test_that("html3: Spanning headers work as expected.", {
  
  
  fp <- file.path(base_path, "html/test3")
  
  dat <- mtcars[1:15, ]
  
  tbl <- create_table(dat, borders = c("none")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>% 
    spanning_header(hp, wt, "Span 2", underline = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span", 
                    n = 11, level = 2) %>%
    titles("Table 1.0", "My Nice Table") %>%
    footnotes("My footnote 1", "My footnote 2") 
  
  rpt <- create_report(fp, output_type = "HTML", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
    add_content(tbl) %>% 
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 1)
  
})


test_that("html4: Multi page table works as expected.", {


  fp <- file.path(base_path, "html/test4.html")

  dat <- iris


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = 1.5, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1.25, align = "centre") %>%
    define(Species, blank_after = TRUE)

  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    footnotes("My footnote 1", "My footnote 2")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 7)


})



test_that("html5: Basic text works as expected.", {

  fp <- file.path(base_path, "html/test5")

  txt <- create_text(cnt, width = 6, borders = "outside", align = "center") %>%
    titles("Text 1.0", "My Nice Text", borders = "outside", width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside")

  rpt <- create_report(fp, output_type = "HTML", font = "Courier",
                       font_size = 12) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "center") %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 1)

})



test_that("html6: Basic plot works as expected.", {


  library(ggplot2)

  fp <- file.path(base_path, "html/test6.html")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8, borders = c("outside")) %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "outside") %>%
    footnotes("* Motor Trend, 1974", borders = "outside")


  rpt <- create_report(fp, output_type = "HTML", font = fnt, font_size =fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})


test_that("remove_image_files works as expected.", {
  
  pth <- file.path(base_path, "html/test6.html")
  
  
  remove_image_files(pth)
  
  # Hard to test. Will just check for error.
  # And use this test interactively.
  expect_equal(TRUE, TRUE)
  
})


test_that("html7: Multi page table paper_size none works as expected.", {
  
  
  fp <- file.path(base_path, "html/test7.html")
  
  dat <- iris
  
  
  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = 1.5, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1.25, align = "centre") %>%
    define(Species, blank_after = TRUE)
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 12, orientation = "landscape",
                       paper_size = "none") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    footnotes("My footnote 1", "My footnote 2")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})
