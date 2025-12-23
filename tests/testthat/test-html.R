context("HTML Tests")

base_path <- paste0(getwd(),"/tests/testthat")
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
 # print(fp)
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "outside", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("outside"), 
           width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside", 
              align = "left", width = "content") %>% 
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
  
  tbl <- create_table(dat, borders = c("all")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>% 
    spanning_header(hp, wt, "Span 2", underline = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span", 
                    n = 11, level = 2) %>%
    titles("Table 1.0", "My Nice Table", borders = c("outside")) %>%
    footnotes("My footnote 1", "My footnote 2", borders = c("outside")) 
  
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
                       font_size = 12, paper_size = "letter") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "center") %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 1)

})


# Note that image may be deleted in next test case
test_that("html6: Basic plot works as expected.", {


  library(ggplot2)

  fp <- file.path(base_path, "html/test6.html")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8, borders = c("all")) %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "outside",
           font_size = 12) %>%
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
  
  if (dev & FALSE) {
    pth <- file.path(base_path, "html/test6.html")
    
    
    remove_image_files(pth)
    
    # Hard to test. Will just check for error.
    # And use this test interactively.
    expect_equal(TRUE, TRUE)
  } else 
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
  
  rpt <- create_report(fp, output_type = "HTML", font = "Times",
                       font_size = 9, orientation = "portrait",
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

# Good for testing
# Borders throw off line counts.  Made rudimentary adjustment.
test_that("html8: Page by works as expected.", {
  
  
  fp <- file.path(base_path, "html/test8")
  
  dat <- iris
  
  tbl <- create_table(dat, borders = "all") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title", 
           borders = "outside") %>%
    page_by(Species, label = "Species: ", borders = "outside", 
            align = "center") %>% 
    define(Sepal.Length, label = "Sepal Length", width = 1.5, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1.25, align = "centre")  %>%
    define(Species, visible = FALSE) %>% 
    footnotes("My footnote 1", "My footnote 2", borders = "outside")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 12, orientation = "portrait",
                       paper_size = "letter") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 6)
  
  
})


# Good for testing
test_that("html9: Page by on report works as expected.", {
  
  
  fp <- file.path(base_path, "html/test9.html")
  
  dat <- iris
  
  
  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = 1.5, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1.25, align = "centre") 
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 12, orientation = "landscape",
                       paper_size = "letter") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    page_by(Species, label = "Species: ") %>% 
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    footnotes("My footnote 1", "My footnote 2")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 9)
  
  
})

test_that("html10: Title Header and page header/footer wrapping work as expected.", {
  
  
  fp <- file.path(base_path, "html/test10.html")
  
  dat <- iris[1:10, ] 
  
  tbl <- create_table(dat, borders = "none") %>% 
    title_header("Table 1.0", "My Nice Report with Borders",
                 right = c("Right1", "Right2", 
                           "Right3 long enough to wrap around at least once"),
                 borders = "none",
                 blank_row = "none") %>%
    footnotes("My footnote 1", 
        paste("My footnote 2 Center1 here is a whole bunch of stuff to try and make it wrap", 
              "like more down here note 2 Center1 here is a whole bunch of stuff." ),
              valign = "top",
              borders = "none", 
              blank_row = "above") %>% 
    define(Sepal.Width, label = "Here is a rather long header label")
  
  rpt <- create_report(fp, output_type = "HTML", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_header(c("Left1", "Left2\nwrap"), "Right 1") %>% 
    page_footer("Left1", 
                "Center1 here is a whole bunch of stuff to try and make it wrap", 
                "Right1\nwrap\n and wrap again")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})


test_that("html11: Basic plot with titles on report works as expected.", {
  
  
  library(ggplot2)
  
  fp <- file.path(base_path, "html/test11.html")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8, borders = c("none"))
  
  
  rpt <- create_report(fp, output_type = "HTML", font = fnt, font_size =fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none") %>%
    footnotes("* Motor Trend, 1974", borders = "none")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})


test_that("html12: Text with titles on report works as expected.", {
  
  fp <- file.path(base_path, "html/test12")
  
  txt <- create_text(cnt, width = 6, borders = "none", align = "center") 
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier",
                       font_size = 12, paper_size = "letter") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "center") %>%
    page_footer("Left1", "Center1", "Right1")%>%
    titles("Text 1.0", "My Nice Text", borders = "none", width = "page") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 1)
  
})


# Works on all combinations on font and font size.  Needed adjustments to row height.
test_that("html13: Table with break between sections works as expected.", {
  
  
  fp <- file.path(base_path, "html/test13.html")
  
  
  # Setup
  subjid <- 100:109
  name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
            "al-Kazemi, Najwa", "Schaffer, Ashley", "Laner, Tahma", 
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
            "Smith, Shaileigh")
  sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
  age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
  arm <- c(rep("A", 5), rep("B", 5))
  
  # Create data frame
  df <- data.frame(subjid, name, sex, age, arm)
  
  
  tbl1 <- create_table(df, first_row_blank = TRUE) %>%
    define(subjid, label = "Subject ID", align = "left", width = 1) %>% 
    define(name, label = "Subject Name", width = 1) %>% 
    define(sex, label = "Sex") %>% 
    define(age, label = "Age") %>% 
    define(arm, label = "Arm", 
           blank_after = FALSE, 
           dedupe = TRUE, 
           align = "right") #%>% 
  # spanning_header(sex, arm, label = "Here is a spanning header")
  
  
  rpt <- create_report(fp, output_type = "HTML", font = fnt, font_size = fsz) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    # options_fixed(line_count = 46) %>% 
    titles("Table 1.0", "Analysis Data Subject Listing\n And more stuff", 
           "Safety Population", align = "center", bold = TRUE) %>%
    footnotes("Program Name: table1_0.R", 
              "Here is a big long footnote that is going to wrap\n at least once") %>%
    page_footer(left = "Time", center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  
  
  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)
  
  
})



test_that("html14: Plot with page by on plot works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "html/test14.html")
  
  
  fmt <- value(condition(x == 4, "4 Cylinder"),
               condition(x == 6, "6 Cylinder"),
               condition(x == 8, "8 Cylinder"))
  
  dat <- mtcars[order(mtcars$cyl), ]
  
  p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
  
  brdrs <- "outside"
  
  #dats <- split(p$data, p$data$grp)
  #tbl <- create_table(dat[1:3, ])
  
  plt <- create_plot(p, height = 4, width = 8, borders = brdrs) %>% 
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", 
           borders = brdrs, 
           blank_row = "none") %>%
    page_by(cyl, "Cylinders: ", borders = brdrs, format = fmt) %>% 
    footnotes("* Motor Trend, 1974", borders = brdrs) 
  
  rpt <- create_report(fp, output_type = "HTML", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt) %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
  
})

test_that("html15: Title bold and font size works as expected.", {
  
  
  fp <- file.path(base_path, "html/test15.html")
  
  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"
  
  tbl <- create_table(dat, borders = "outside") %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"), 
           width = "content", font_size = 14, bold = TRUE) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none", 
              align = "left", width = "content") %>% 
    define(wt, width = 1, label = "Weight", align = "center", 
           label_align = "right")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial",
                       font_size = 9, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"), 
                blank_row = "below") %>% 
    add_content(tbl, align = "center")  %>% 
    page_footer("Left1", "Center1", "Right1")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})


test_that("html16: 9 pt font inches works as expected.", {
  
  
  fp <- file.path(base_path, "html/test16.html")
  
  rpt <- create_report(fp, output_type = "HTML", font_size = 9, 
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})

test_that("html17: 9 pt font cm works as expected.", {
  
  
  fp <- file.path(base_path, "html/test17.html")
  
  rpt <- create_report(fp, output_type = "HTML", font_size = 9, 
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})

test_that("html18: 11 pt font inches works as expected.", {
  
  
  fp <- file.path(base_path, "html/test18.html")
  
  rpt <- create_report(fp, output_type = "HTML", font_size = 11, 
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})

test_that("html19: 11 pt font cm works as expected.", {
  
  
  fp <- file.path(base_path, "html/test19.html")
  
  rpt <- create_report(fp, output_type = "HTML", font_size = 11, 
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})


test_that("html20: RTF Image file works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "html/test20.html")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  pltpath <- file.path(base_path, "html/test20.jpg")
  ggsave(pltpath, width = 8, height = 4, 
         units = "in",
         dpi = 300)
  
  plt <- create_plot(pltpath, height = 4, width = 8)
  
  
  rpt <- create_report(fp, output_type = "HTML", font = "Arial") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


test_that("html21: Header_bold works as expected", {
  
  dat <- mtcars[1:10, 1:3]
  
  fp <- file.path(base_path, "html/test21.html")
  
  tbl <- create_table(dat, header_bold = TRUE, borders = "all") %>%
    column_defaults(width = 1) %>%
    titles("Report 1.0", "Simple Report", borders = "outside", 
           blank_row = "none", bold = TRUE) %>%
    footnotes("My footnote", blank_row = "none")
  
  rpt <- create_report(fp, orientation = "portrait",
                       output_type = "HTML", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})



test_that("html22: Label row is one cell.", {
  
  
  fp <- file.path(base_path, "html/test22.html")
  
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  ll <- "Here is a super long label to see if it can span the entire table."
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = "all", 
                      header_bold = TRUE) %>% 
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = ll, cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center", n = 19) %>% 
    define(B, label = "Group B", align = "center", n = 13)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "HTML",
                       font = "Times") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = Sys.time(), 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  
  
  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)
  
  
})


test_that("html23: Blank after on invisible column.", {
  
  fp <- file.path(base_path, "html/test23.html")
  
  tbl <- create_table(iris, borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "HTML") %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html24: Page header width works.", {
  
  fp <- file.path(base_path, "html/test24.html")
  
  tbl <- create_table(iris[1:10, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier") %>%
    page_header("Left and here is a really long left cell text to put it", 
                "Right", width = 8) %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html25: Carriage return in label row works.", {
  
  
  fp <- file.path(base_path, "html/test25.html")
  
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - \n21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  ll <- "Here is a super long label to \nsee if it can span the entire table."
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>% 
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = ll, cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center", n = 19) %>% 
    define(B, label = "Group B", align = "center", n = 13)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "HTML",
                       font = "Times") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  
  
  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)
  
  
})



test_that("html26: Title columns work 1 column.", {
  
  fp <- file.path(base_path, "html/test26.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all")  %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame2",
           blank_row = "both", columns =  1, align = "center",
           borders = c("outside")) 
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html27: Title columns work 2 columns.", {
  
  fp <- file.path(base_path, "html/test27.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame", "Left", "Right",
           blank_row = "both", columns =  2, borders = "all")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")  %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html28: Title columns work 3 columns.", {
  
  fp <- file.path(base_path, "html/test28.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  
  rght <- paste("Here is a big long text string to see how the automatic", 
                "wrapping is happening in a reduced size cell on the right.")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier", 
                       font_size = 10) %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame", 
           "      My right thing", "", "Center", rght,
           blank_row = "below", columns =  3, borders = "none") %>%
    footnotes("Here is a footnote", "And another", "A",
              "Here is a longer footnote to see if I can figure out the alignment pattern.",
              align = "right")
  
  
  res <- write_report(rpt)
  res
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html29: Multiple title blocks work as expected.", {
  
  fp <- file.path(base_path, "html/test29.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below", columns =  1, align = "center", width = 7,
           borders = "all") %>%
    titles("Table 2.0", "IRIS Data Frame2", "Left", "Right",
           blank_row = "below", columns =  2, borders = "all") %>%
    titles("Table 3.0", "IRIS Data Frame3", "My right thing", "", "Center",
           blank_row = "below", columns =  3, borders = "all") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("html30: Custom page size works as expected.", {
  
  fp <- file.path(base_path, "html/test30.html")
  
  tbl <- create_table(iris[1:15, ]) %>%
    define(Species, visible = FALSE)
  
  ttl <- c("Title1", "Title2", "Title3")
  
  rpt <- create_report(fp, output_type = "HTML", 
                       font = "Courier",
                       paper_size = c(6.5, 7.5),
                       orientation = "portrait") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles(ttl,
           blank_row = "below", columns =  1, align = "center",
           borders = "none") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})



test_that("html31: Basic cell style bold works as expected.", {
  
  if (dev) {
    
    fp <- file.path(base_path, "html/test31.html")
    
    dat <- mtcars[, 1:5]
    dat$hpflg <- ifelse(dat$hp > 100, TRUE, FALSE)
    
    
    tbl <- create_table(dat, width = 7) %>%
      titles("My title") %>%
      column_defaults(width = .75) %>%
      define(mpg, width = 1, style = cell_style(bold = TRUE)) %>%
      define(cyl, width = 1) %>%
      define(disp, style = cell_style(bold = TRUE)) %>%
      define(hp, style = cell_style(bold = TRUE, indicator = hpflg)) %>%
      define(hpflg, visible = FALSE) %>%
      footnotes("My footnotes", blank_row = "none")
    
    
    rpt <- create_report(fp, output_type = "HTML", 
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE)
    
    res <- write_report(rpt)
    
    
    # file.show(res$modified_path)
    
    res$column_widths
    
    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)
    
    
  } else {
    
    expect_equal(TRUE, TRUE)
    
  }
  
})

test_that("html32: Bolding works with stub.", {
  
  
  fp <- file.path(base_path, "html/test32.html")
  
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  df$cylflg <- ifelse(df$var == "cyl", TRUE, FALSE)
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE) %>% 
    column_defaults(vars = c("stub", "A"), 
                    style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    stub(c("var", "label"), 
         style = cell_style(bold = TRUE, indicator = "labelrow")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center", n = 19,
           style = cell_style(bold = TRUE, indicator = cylflg)) %>% 
    define(B, label = "Group B", align = "center", n = 13, 
           style = cell_style(bold = TRUE, indicator = "datarow")) %>%
    define(cylflg, visible = FALSE)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "HTML",
                       font = "Times") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  
  
  res <- write_report(rpt)
  
  #file.show(res$modified_path)
  res
  expect_equal(file.exists(fp), TRUE)
  
  
})


test_that("html33: Bold cell style with column defaults.", {
  
  if (dev) {
    
    fp <- file.path(base_path, "html/test33.html")
    
    dat <- mtcars[, 1:5]
    dat$hpflg <- ifelse(dat$hp > 100, TRUE, FALSE)
    
    
    tbl <- create_table(dat) %>%
      titles("My title") %>%
      column_defaults(width = .75, vars = c("cyl", "disp", "hp"),
                      style = cell_style(bold=TRUE, indicator = hpflg)) %>%
      define(mpg) %>%
      define(cyl) %>%
      define(disp) %>%
      define(hp) %>%
      define(hpflg, visible = FALSE) %>%
      footnotes("My footnotes", blank_row = "none")
    
    
    rpt <- create_report(fp, output_type = "HTML", 
                         font = "Arial", orientation = "portrait") %>%
      add_content(tbl) %>%
      footnotes("Here", footer = TRUE)
    
    res <- write_report(rpt)
    
    
    # file.show(res$modified_path)
    
    res$column_widths
    
    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)
    
    
  } else {
    
    expect_equal(TRUE, TRUE)
    
  }
  
})


test_that("html34: Bolding, column defaults, and stub works.", {
  
  
  fp <- file.path(base_path, "html/test34.html")
  
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  df$cylflg <- ifelse(df$var == "cyl", FALSE, TRUE)
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE) %>% 
    column_defaults(style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    stub(c("var", "label"), width = 2,
         style = cell_style(bold = TRUE, indicator = cylflg)) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center", n = 19) %>% 
    define(B, label = "Group B", align = "center", n = 13, 
           style = cell_style(bold = TRUE, indicator = "datarow")) %>%
    define(cylflg, visible = FALSE)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "HTML",
                       font = "Times") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  res <- write_report(rpt)
  
  # file.show(res$modified_path)
  res
  expect_equal(file.exists(fp), TRUE)
  
  
})


test_that("html35: Spanning header bold work as expected.", {
  
  
  fp <- file.path(base_path, "html/test35.html")
  
  dat <- mtcars[1:15, ]
  
  tbl <- create_table(dat, borders = c( "none")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>%
    spanning_header(hp, wt, "Span 2", underline = FALSE, bold = TRUE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span",
                    n = 11, level = 2, bold = TRUE) %>%
    titles("Table 1.0", "My Nice Table", blank_row = "none", 
           borders = c("top", "bottom")) %>%
    footnotes("My footnote 1", "My footnote 2", 
              blank_row = "none", borders = c("top", "bottom"))
  
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


test_that("html36: Italic footnotes work as expected.", {
  
  
  fp <- file.path(base_path, "html/test36.html")
  
  dat <- mtcars[1:15, ]
  
  tbl <- create_table(dat, borders = c( "outside")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>%
    spanning_header(hp, wt, "Span 2", underline = FALSE, bold = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span",
                    n = 11, level = 2, bold = FALSE) %>%
    titles("Table 1.0", "My Nice Table", blank_row = "none", 
           borders = c("top", "bottom")) %>%
    footnotes("My italic footnote1", "My italic footnote2", italics = TRUE, 
              blank_row = "none", borders = "top") %>%
    footnotes("My italic footnote", "My footnote 2", 
              blank_row = "none", borders = c("bottom"))
  
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


test_that("html37: Stub indent.", {
  
  
  fp <- file.path(base_path, "html/test37.html")
  
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder and more perhaps more" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  ll <- "Here is a super long label to see if it can span the entire table."
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>% 
    stub(c("var", "label"), width = .9) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = ll, cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center", n = 19) %>% 
    define(B, label = "Group B", align = "center", n = 13)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "HTML",
                       font = "Times") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1974") %>%
    page_footer(left = "Left", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  
  
  res <- write_report(rpt)
  res
  expect_equal(file.exists(fp), TRUE)
  
  
})



test_that("html38: Footnotes columns work 1 column.", {
  
  fp <- file.path(base_path, "html/test38.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all")  %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame2",
           blank_row = "both", columns =  1, align = "center",
           borders = c("outside")) %>%
    footnotes("Here is a footnote", "And another", 
              borders = "all", columns = 1, blank_row = "both")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Here is a footnote", "And another", columns = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html39: Footnote columns work 2 columns.", {
  
  fp <- file.path(base_path, "html/test39.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame", "Left", "Right",
           blank_row = "both", columns =  2, borders = "all") %>%
    footnotes("Here is a footnote", "And another", 
              borders = "all", columns = 2, blank_row = "both")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")  %>%
    footnotes("Here is a footnote", "And another", 
              "footnote left", "footnote right", columns = 2)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html40: Footnote columns work 3 columns.", {
  
  fp <- file.path(base_path, "html/test40.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = FALSE, visible = FALSE) %>%
    footnotes("Here is a footnote", "And another", "And more",
              "",  "centered",
              borders = "all", columns = 3, blank_row = "both")
  
  
  rght <- paste("Here is a big long text string to see how the automatic", 
                "wrapping is happening in a reduced size cell on the right.")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier", 
                       font_size = 10) %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame", 
           "      My right thing", "", "Center", rght,
           blank_row = "below", columns =  3, borders = "none") %>%
    footnotes("Here is a footnote", "And another", "A",
              "Here is a longer footnote",  "to see if I can figure",  
              "out the alignment pattern.", columns = 3)
  
  
  res <- write_report(rpt)
  res
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html41: Multiple footnote blocks work as expected.", {
  
  fp <- file.path(base_path, "html/test41.html")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = FALSE, visible = FALSE)  %>%
    footnotes("Here is a footnote", "And another", "And more",
              "",  "centered",
              borders = "all", columns = 3, blank_row = "both")
  
  rpt <- create_report(fp, output_type = "HTML", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Table 1.0", "IRIS Data Frame",
           blank_row = "below", columns =  1, align = "center", width = 7,
           borders = "all") %>%
    footnotes("Table 2.0", "IRIS Data Frame2", "Left", "Right",
           blank_row = "below", columns =  2, borders = "all") %>%
    footnotes("Table 3.0", "IRIS Data Frame3", "My right thing", "", "Center",
           blank_row = "below", columns =  3, borders = "all") %>%
    titles("Here is a title", "And another", borders = "all")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("html42: Page by with wrap works as expected.", {
  
  
  fp <- file.path(base_path, "html/test42.html")
  
  dat <- iris
  dat$Pgby <- as.character(dat$Species)
  dat$Pgby <- paste0("Flower Type\n", dat$Pgby)
  
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, label = "Species: ", align = "right", borders = "none") %>%
    define(Pgby, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "HTML", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_header("Left", "Right") %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    footnotes("My footnote 1", "My footnote 2", borders = "none")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 6)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})

test_that("html43: Page by with wrap works as expected.", {
  
  
  fp <- file.path(base_path, "html/test43.html")
  
  fmt1 <- c(setosa = 1, versicolor = 2, virginica = 3)
  fmt2 <- value(condition(x == 1, "Setosa"),
                condition(x == 2, "Versicolor"),
                condition(x == 3, "Virginica"))
  
  dat <- iris
  fmtval <- fmt1[dat$Species]
  names(fmtval) <- NULL
  dat$Pgby <- fmtval
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, align = "left", label = "Flower:", borders = "none", format = fmt2) %>%
    define(Pgby, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "HTML", 
                       orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_header("Left", "Right") %>% 
    page_footer("Left1", "Center1", "Right1") %>% 
    footnotes("My footnote 1", "My footnote 2", borders = "none")
  
  res <- write_report(rpt)
  res
  res$column_widths
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 6)
  expect_equal(length(res$column_widths[[1]]), 5)
  
  
})

test_that("html-44: Multi page table removes blank spaces.", {
  
  if (dev == TRUE) {
    
    
    fp <- file.path(base_path, "html/test44.html")
    
    dat1 <- iris[1:10, ]
    dat2 <- iris[11:20, ]
    dat3 <- iris[21:30, ]
    
    
    tbl1 <- create_table(dat1, borders = "none") %>%
      titles("Table 1.0", "My Nice Irises1") 
    
    tbl2 <- create_table(dat2, borders = "none") %>%
      titles("Table 1.0", "My Nice Irises2")
    
    tbl3 <- create_table(dat3, borders = "none") %>%
      titles("Table 1.0", "My Nice Irises3")
    
    rpt <- create_report(fp, output_type = "HTML", font = fnt,
                         font_size = 12, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl1, blank_row = "none") |> 
      add_content(tbl2, blank_row = "none") |> 
      add_content(tbl3, blank_row = "none") 
    
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 3)
    
  } else
    expect_equal(TRUE, TRUE)
})

test_that("html-45: Spanning header gap works as expected.", {
  
  if (dev == TRUE) {
    fp <- file.path(base_path, "html/test45.html")
    
    dat <- mtcars[1:15, ]
    
    tbl <- create_table(dat, borders = c("outside")) %>%
      spanning_header(cyl, disp, "Span 1", label_align = "left") %>%
      spanning_header(hp, wt, "Span 2", underline = TRUE) %>%
      spanning_header(qsec, vs, "Span 3", n = 10) %>%
      spanning_header(cyl, hp, "Super Span", n = 11, level = 2) |> 
      spanning_header(drat, gear, "Super Duper\nWrapped Span", n = 11, level = 2)
    
    rpt <- create_report(fp, output_type = "HTML", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
      titles("Table 1.0", "My Nice Table") %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2") %>%
      page_footer("Left1", "Center1", "Right1")
    
    res <- write_report(rpt)
    res
    res$column_widths
    
    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)
    
  } else {
    expect_true(TRUE)
  }
  
})

test_that("html-46: Table with blank_before works as expected.", {
  
  if (dev == TRUE) {
    fp <- file.path(base_path, "html/test46.html")
    
    
    # Setup
    subjid <- 100:109
    name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
              "al-Kazemi, Najwa", "Schaffer, Ashley", "Laner, Tahma", 
              "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
              "Smith, Shaileigh")
    sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
    age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
    arm <- c(rep("A", 5), rep("B", 5))
    
    # Create data frame
    df <- data.frame(subjid, name, sex, age, arm)
    
    
    tbl1 <- create_table(df, first_row_blank = FALSE) %>%
      define(subjid, label = "Subject ID", align = "left", width = 1) %>% 
      define(name, label = "Subject Name", width = 1) %>% 
      define(sex, label = "Sex") %>% 
      define(age, label = "Age") %>% 
      define(arm, label = "Arm", 
             blank_before = TRUE, 
             dedupe = TRUE, 
             align = "right") #%>% 
    # spanning_header(sex, arm, label = "Here is a spanning header")
    
    
    rpt <- create_report(fp, output_type = "HTML", font = fnt, font_size = fsz) %>%
      page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
      # options_fixed(line_count = 46) %>% 
      titles("Table 1.0", "Analysis Data Subject Listing\n And more stuff", 
             "Safety Population", align = "center", bold = TRUE) %>%
      footnotes("Program Name: table1_0.R", 
                "Here is a big long footnote that is going to wrap\n at least once") %>%
      page_footer(left = "Time", center = "Confidential", 
                  right = "Page [pg] of [tpg]") %>%
      add_content(tbl1) 
    
    
    res <- write_report(rpt)
    res
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_true(TRUE)
  }
})

test_that("html-47: Three level stub and indentation work as expected.", {
  
  if (dev == TRUE) {
    fp <- file.path(base_path, "html/test47.html")
    
    
    # Setup
    cat <- c(rep("Kaplan-Meier estimates", 6), 
             rep("Cox PH estimates and some more really long stuff", 6))
    grp <- c("25th percentile", "25th percentile", 
             "median (weeks)", "median (weeks)",
             "75th percentile", "75th percentile",
             "25th percentile", "25th percentile", 
             "median (weeks)", "median (weeks)",
             "75th percentile", "75th percentile")
    ci <- c(NA, "95% confidence interval",
            NA, "95% confidence interval",
            NA, "95% confidence interval",
            NA, "95% confidence interval",
            NA, "95% confidence interval",
            NA, "95% confidence interval")
    values <- c(41, 53, 43, 39, 47, 52, 38, 25, 37, 23, 78, 21)
    
    # Create data frame
    df <- data.frame(cat, grp, ci, values, stringsAsFactors = FALSE)
    
    tbl1 <- create_table(df) %>%
      stub(c(cat, grp, ci), "Estimates", width = 1.5) %>% 
      define(cat, label_row = TRUE, blank_after = TRUE, indent = 0.15) %>%
      define(grp, indent = .25) %>%
      define(ci, indent = .5) %>%
      define(values, label = "Values")
    
    rpt <- create_report(fp, output_type = "HTML", font = fnt,
                         font_size = fsz) %>%
      titles("Table 3.0", "Analysis of Time to Initial PSGA Success in Weeks") %>% 
      page_header("Sponsor", "Study") %>% 
      add_content(tbl1) %>% 
      page_footer("Time", "Confidential", "Page")
    
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_true(TRUE)
  }
})

test_that("html-48: Page footers with one assigned width work as expected.",{
  if (dev) {
    fp <- file.path(base_path, "html/test48.html")
    
    dat <- mtcars[,c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs")]
    
    tbl <- create_table(dat, borders = "outside") %>%
      footnotes("This is testing footnote", blank_row = "none")
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz) %>%
      set_margins(top = 1, bottom = 1) %>%
      titles("Table 1.0") %>%
      add_content(tbl) %>%
      page_footer(left = "this is a very long sentence whose length exceeds the limitation of the left part", 
                  center = "Center footer", right = "Right footer",
                  width = c(5.5))
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
}
)

test_that("html-49: Page footers with multiple assigned widths work as expected.",{
  if (dev) {
    fp <- file.path(base_path, "html/test49.html")
    
    dat <- mtcars[,c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs")]
    
    tbl <- create_table(dat, borders = "outside") %>%
      footnotes("This is testing footnote", blank_row = "none")
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz) %>%
      set_margins(top = 1, bottom = 1) %>%
      titles("Table 1.0") %>%
      add_content(tbl) %>%
      page_footer(left = "this is a very long sentence whose length exceeds the limitation of the left part", 
                  center = "", right = "Right Footer",
                  width = c(5.5, 0 , 2))
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
}
)

test_that("html-50: Page by with bold label and value works as expected.", {
  
  if (dev) {
    fp <- file.path(base_path, "html/test50.html")
    
    dat <- iris
    dat$Pgby <- as.character(dat$Species)
    dat <- dat[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Pgby")]
    
    tbl <- create_table(dat, borders = "outside") %>%
      titles("Table 1.0", "My Nice Report with a Page By") %>%
      page_by(Pgby, label = "Flower Type: ", bold = TRUE, blank_row = "none") %>%
      define(Pgby, visible = FALSE)
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2", borders = "none")
    
    res <- write_report(rpt)
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-51: Page by with bold label as expected.", {
  
  if (dev) {
    # Label and Value are bold
    fp <- file.path(base_path, "html/test51.html")
    
    dat <- iris
    dat$Pgby <- as.character(dat$Species)
    dat <- dat[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Pgby")]
    
    tbl <- create_table(dat, borders = "outside") %>%
      titles("Table 1.0", "My Nice Report with a Page By") %>%
      page_by(Pgby, label = "Flower Type: ", bold = "label", blank_row = "none") %>%
      define(Pgby, visible = FALSE)
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2", borders = "none")
    
    res <- write_report(rpt)
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-52: Page by with bold value as expected.", {
  
  if (dev) {
    # Label and Value are bold
    fp <- file.path(base_path, "html/test52.html")
    
    dat <- iris
    dat$Pgby <- as.character(dat$Species)
    dat <- dat[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Pgby")]
    
    tbl <- create_table(dat, borders = "outside") %>%
      titles("Table 1.0", "My Nice Report with a Page By") %>%
      page_by(Pgby, label = "Flower Type: ", bold = "value", blank_row = "none") %>%
      define(Pgby, visible = FALSE)
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2", borders = "none")
    
    res <- write_report(rpt)
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-53: Page by with bold long label and value works as expected.", {
  
  if (dev) {
    fp <- file.path(base_path, "html/test53.html")
    
    dat <- iris
    long_string <- "This is long page by value which should take more than one sentence - "
    dat$Pgby <- paste0(long_string, dat$Species)
    dat <- dat[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Pgby")]
    
    long_label <- "This is a long page by label which should take more than one sentence: "
    
    tbl <- create_table(dat, borders = "outside") %>%
      titles("Table 1.0", "My Nice Report with a Page By") %>%
      page_by(Pgby, label = long_label, bold = TRUE, blank_row = "none") %>%
      define(Pgby, visible = FALSE)
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2", borders = "none")
    
    res <- write_report(rpt)
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-54: Page by with bold long label works as expected.", {
  
  if (dev) {
    fp <- file.path(base_path, "html/test54.html")
    
    dat <- iris
    dat$Pgby <- as.character(dat$Species)
    dat <- dat[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Pgby")]
    
    long_label <- "This is a long page by label which should take more than one sentence: "
    
    tbl <- create_table(dat, borders = "outside") %>%
      titles("Table 1.0", "My Nice Report with a Page By") %>%
      page_by(Pgby, label = long_label, bold = "label", blank_row = "none") %>%
      define(Pgby, visible = FALSE)
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2", borders = "none")
    
    res <- write_report(rpt)
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-55: Page by with bold long value works as expected.", {
  
  if (dev) {
    fp <- file.path(base_path, "html/test55.html")
    
    dat <- iris
    long_string <- "This is long page by value which should take more than one sentence - "
    dat$Pgby <- paste0(long_string, dat$Species)
    dat <- dat[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Pgby")]
    
    long_label <- "This is a long page by label which should take more than one sentence:"
    
    tbl <- create_table(dat, borders = "outside") %>%
      titles("Table 1.0", "My Nice Report with a Page By") %>%
      page_by(Pgby, label = "Flower Type:", bold = "value", blank_row = "none") %>%
      define(Pgby, visible = FALSE)
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2", borders = "none")
    
    res <- write_report(rpt)
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-56: Page by with bold long label filling one line works as expected.", {
  
  if (dev) {
    fp <- file.path(base_path, "html/test56.html")
    
    dat <- iris
    dat$Pgby <- as.character(dat$Species)
    dat <- dat[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Pgby")]
    
    long_label <- "This is a long page by label which should take more- "
    
    tbl <- create_table(dat, borders = "outside") %>%
      titles("Table 1.0", "My Nice Report with a Page By") %>%
      page_by(Pgby, label = long_label, bold = "label", blank_row = "none") %>%
      define(Pgby, visible = FALSE)
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz, orientation = "landscape") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      footnotes("My footnote 1", "My footnote 2", borders = "none")
    
    res <- write_report(rpt)
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-57: Group border works as as expected.", {
  if (dev == TRUE) {
    fp <- file.path(base_path, "html/test57.html")
    
    # Setup
    arm <- c(rep("A", 3), rep("B", 2), rep("C", 3), rep("D", 2))
    subjid <- 100:109
    name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
              "al-Kazemi, Najwa \nand more and more", "Schaffer, Ashley", "Laner, Tahma",
              "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon",
              "Smith, Shaileigh")
    sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
    age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
    
    
    # Create data frame
    df <- data.frame(arm, subjid, name, sex, age, stringsAsFactors = FALSE)
    df <- rbind(df, df, df, df)
    
    tbl1 <- create_table(df, first_row_blank = FALSE, borders = "outside") %>%
      define(subjid, label = "Subject ID for a patient", n = 10, align = "left",
             width = 1) %>%
      define(name, label = "Subject Name", width = 1) %>%
      define(sex, label = "Sex", n = 10, align = "center") %>%
      define(age, label = "Age", n = 10) %>%
      define(arm, label = "Arm",
             dedupe = TRUE,
             group_border = TRUE) %>%
      footnotes("This is the footnote")
    
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz) %>%
      titles(c("Table 1.0", "This is a table with group border"), align = "center") %>%
      add_content(tbl1) %>%
      footnotes(c("This is the footnote 1")) %>%
      page_header(left = "Test header", right = "Test header") %>%
      page_footer(left = "Test footer", right = "Test footer") %>%
      set_margins(top = 1, bottom = 1)
    
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

test_that("html-58: Group border with blank after works as as expected.", {
  
  if (dev == TRUE) {
    fp <- file.path(base_path, "html/test58.html")
    
    # Setup
    arm <- c(rep("A", 3), rep("B", 2), rep("C", 3), rep("D", 2))
    subjid <- 100:109
    name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
              "al-Kazemi, Najwa \nand more and more", "Schaffer, Ashley", "Laner, Tahma",
              "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon",
              "Smith, Shaileigh")
    sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
    age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)    
    
    # Create data frame
    df <- data.frame(arm, subjid, name, sex, age, stringsAsFactors = FALSE)
    df <- rbind(df, df, df, df)
    
    tbl1 <- create_table(df, first_row_blank = FALSE, borders = "outside") %>%
      define(subjid, label = "Subject ID for a patient", n = 10, align = "left",
             width = 1) %>%
      define(name, label = "Subject Name", width = 1) %>%
      define(sex, label = "Sex", n = 10, align = "center") %>%
      define(age, label = "Age", n = 10) %>%
      define(arm, label = "Arm",
             dedupe = TRUE,
             blank_after = TRUE,
             group_border = TRUE) %>%
      footnotes("This is the footnote")
    
    
    rpt <- create_report(fp, output_type = "html", font = fnt,
                         font_size = fsz) %>%
      titles(c("Table 1.0", "This is a table with group border"), align = "center") %>%
      add_content(tbl1) %>%
      footnotes(c("This is the footnote 1")) %>%
      page_header(left = "Test header", right = "Test header") %>%
      page_footer(left = "Test footer", right = "Test footer") %>%
      set_margins(top = 1, bottom = 1)
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
  } else {
    expect_equal(TRUE, TRUE)
  }
})

# User Tests --------------------------------------------------------------


test_that("html-user1: demo table works.", {
  
  if (dev) {
    library(tidyr)
    library(dplyr)
    
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "html/user1.html")
    
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    
    data_demo <- subset(data_demo, data_demo$ARM != "SCREEN FAILURE")
    
    
    sex_decode <- c("M" = "Male",
                    "F" = "Female")
    
    race_decode <- c("WHITE" = "White",
                     "BLACK OR AFRICAN AMERICAN" = "Black or African American",
                     "ASIAN" = "Asian or Pacific Islander",
                     "NATIVE AMERICAN" = "Native American",
                     "UNKNOWN" = "Unknown")
    
    arm_pop <- table(data_demo$ARM) 
    
    
    demo_age <-
      data_demo %>%
      group_by(ARM) %>%
      summarise(across(.cols = AGE,
                       .fns = list(N      = ~ fmt_n(.),
                                   Mean   = ~ fmt_mean_sd(.),
                                   Median = ~ fmt_median(.),
                                   `Q1 - Q3` = ~ fmt_quantile_range(.),
                                   Range  = ~ fmt_range(.)
                       ))) %>%
      pivot_longer(-ARM,
                   names_to  = c("var", "label"),
                   names_sep = "_",
                   values_to = "value") %>%
      pivot_wider(names_from = ARM,
                  values_from = "value")
    
    
    
    demo_sex <-
      data_demo %>%
      add_count(ARM, SEX,  name = "n_SEX") %>%
      select(ARM, SEX, n_SEX) %>%
      distinct() %>%
      pivot_longer(cols = c(SEX),
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_SEX,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(sex_decode),
                            labels = sex_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"]))
    
    
    
    demo_race <-
      data_demo %>%
      add_count(ARM, RACE, name = "n_RACE") %>%
      select(ARM, RACE, n_RACE) %>%
      distinct() %>%
      pivot_longer(cols = RACE,
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_RACE,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(race_decode),
                            labels = race_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
      arrange(var, label)
    
    
    demo <- bind_rows(demo_age, demo_sex, demo_race)
    
    
    #View(demo)
    
    
    # Stub decode
    block_fmt <- c(AGE = "Age", SEX = "Sex", RACE = "Race")
    
    # Define table
    tbl <- create_table(demo, first_row_blank = TRUE) %>%
      column_defaults(from = "ARM A", to = "ARM D", width = 1.25) %>% 
      define(var, blank_after = TRUE, dedupe = TRUE,
             format = block_fmt, label = "") %>%
      define(label, label = "") %>%
      define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
      define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
      define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
      define(`ARM D`, align = "center", label = "Competitor", n = 38)
    
    # Define Report
    rpt <- create_report(fp, output_type = "HTML", font = fnt, font_size = fsz) %>%
      set_margins(top = 1, bottom = 1) %>% 
      options_fixed(font_size = 10) %>% 
      titles("Table 14.1/4",
             "Demographics and Baseline to Characteristics",
             "Specify Population Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼") %>%
      add_content(tbl) %>% 
      footnotes("Special symbols \U221e to mess things up: Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼") %>%   
      footnotes("Special symbols µ Ω £ there to mess things up: ", "Page [pg] of [tpg]") %>% 
      page_header("Left µ Ω £ ", "Right") %>% 
      page_footer("Time µ Ω £ ", right = "Page [pg] of [tpg]")
    
    # Write out report
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    
    
    
  } else 
    expect_equal(TRUE, TRUE)
  
  
})

test_that("html-user2: demo table with stub works.", {
  
  
  if (dev) {
    
    library(tidyr)
    library(dplyr)
    
    
    
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "html/user2.html")
    
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    data_demo <- subset(data_demo, data_demo$ARM != "SCREEN FAILURE")
    
    
    sex_decode <- c("M" = "Male",
                    "F" = "Female")
    
    race_decode <- c("WHITE" = "White",
                     "BLACK OR AFRICAN AMERICAN" = "Black or African American",
                     "ASIAN" = "Asian or Pacific Islander",
                     "NATIVE AMERICAN" = "Native American",
                     "UNKNOWN" = "Unknown")
    
    arm_pop <- table(data_demo$ARM) 
    
    
    demo_age <-
      data_demo %>%
      group_by(ARM) %>%
      summarise(across(.cols = AGE,
                       .fns = list(N      = ~ fmt_n(.),
                                   Mean   = ~ fmt_mean_sd(.),
                                   Median = ~ fmt_median(.),
                                   `Q1 - Q3` = ~ fmt_quantile_range(.),
                                   Range  = ~ fmt_range(.)
                       ))) %>%
      pivot_longer(-ARM,
                   names_to  = c("var", "label"),
                   names_sep = "_",
                   values_to = "value") %>%
      pivot_wider(names_from = ARM,
                  values_from = "value")
    
    
    
    demo_sex <-
      data_demo %>%
      add_count(ARM, SEX,  name = "n_SEX") %>%
      select(ARM, SEX, n_SEX) %>%
      distinct() %>%
      pivot_longer(cols = c(SEX),
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_SEX,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(sex_decode),
                            labels = sex_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"]))
    
    
    
    demo_race <-
      data_demo %>%
      add_count(ARM, RACE, name = "n_RACE") %>%
      select(ARM, RACE, n_RACE) %>%
      distinct() %>%
      pivot_longer(cols = RACE,
                   names_to  = "var",
                   values_to = "label") %>%
      pivot_wider(names_from  = ARM,
                  values_from = n_RACE,
                  values_fill = 0) %>%
      mutate(label = factor(label, levels = names(race_decode),
                            labels = race_decode),
             `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
             `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
             `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
             `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
      arrange(var, label)
    
    
    demo <- bind_rows(demo_age, demo_sex, demo_race)
    
    
    #View(demo)
    
    
    # Stub decode
    block_fmt <- c(AGE = "Age", SEX = "Sex", RACE2 = "Race")
    
    # Define table
    tbl <- create_table(demo, first_row_blank = TRUE, borders = "all") %>%
      stub(c("var", "label"), width = 2.5) %>% 
      column_defaults(width = 1) %>% 
      define(var, blank_after = TRUE, 
             format = block_fmt, label = "", label_row = TRUE) %>%
      define(label, label = "", indent = .25) %>%
      define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
      define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
      define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
      define(`ARM D`, align = "center", label = "Competitor", n = 38) %>% 
      titles("Table 14.1/4", bold = TRUE, blank_row = "above", 
             align = "center", borders = c("top", "left", "right")) %>% 
      titles( "Demographics and Baseline Characteristics",
             "Specify Population", borders = c("left", "right", "bottom"), 
             blank_row = "below",
             align = "center") %>%
      footnotes("Here is a footnote", "Here is another footnote", valign = "top",
                borders = "outside", blank_row = "both", align = "left")  
    
    # Define Report
    rpt <- create_report(fp, output_type = "HTML", 
                         font = "Arial", font_size = 10) %>%
      add_content(tbl, align = "center") %>% 
      page_header("Sponsor", "Drug") %>% 
      page_footer(left = "Time", right = "Page [pg] of [tpg]") #%>% 
    #page_by(var = "var", label = "Variable: ")
    
    # Write out report
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    
    
  } else
    expect_equal(TRUE, TRUE)
  
})

test_that("html-user3: listings works.", {
  if (dev == TRUE) {
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "html/user3.html")
    
    # Removing to make last page exactly equal to available rows on page.
    # In this case, any added blank rows should be skipped.
    fil <- c("ABC-14-124",
             "ABC-15-153",
             "ABC-15-154",
             "ABC-15-155",
             "ABC-15-156",
             "ABC-16-045",
             "ABC-16-046",
             "ABC-16-047",
             "ABC-16-157",
             "ABC-16-158",
             "ABC-16-159", 
             "ABC-16-160")
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    data_demo <- data_demo[!data_demo$USUBJID %in% fil, ]
    
    
    # Test that any assigned formats are applied
    attr(data_demo$SUBJID, "width") <- 1
    attr(data_demo$SUBJID, "justify") <- "left"
    attr(data_demo$SUBJID, "format") <- "S:%s"
    #print(widths(data_demo))
    names(data_demo)
    # Define table
    tbl <- create_table(data_demo) %>% 
      define(USUBJID, id_var = TRUE) 
    
    
    # Define Report
    rpt <- create_report(fp, font = "Arial", font_size = 12, 
                         orientation = "portrait") %>%
      titles("Listing 1.0",
             "Demographics Dataset") %>%
      add_content(tbl, align = "left") %>% 
      page_header("Sponsor", "Drug") %>% 
      page_footer(left = "Time", right = "Page [pg] of [tpg]") %>% 
      footnotes("My footnotes")
    
    #Write out report
    res <- write_report(rpt, output_type = "HTML")
    
    expect_equal(file.exists(fp), TRUE)
    
    
    
    # pdfpth <- file.path(base_path, "user/user3.pdf")
    # write_report(rpt, pdfpth, output_type = "PDF")
    # expect_equal(file.exists(pdfpth), TRUE)
  } else 
    expect_equal(TRUE, TRUE)
  
  
})


test_that("html-user4: listing in cm and times works.", {
  if (dev == TRUE) {
    # Data Filepath
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "html/user4.html")
    
    # Removing to make last page exactly equal to available rows on page.
    # In this case, any added blank rows should be skipped.
    fil <- c("ABC-14-124",
             "ABC-15-153",
             "ABC-15-154",
             "ABC-15-155",
             "ABC-15-156",
             "ABC-16-045",
             "ABC-16-046",
             "ABC-16-047",
             "ABC-16-157",
             "ABC-16-158",
             "ABC-16-159", 
             "ABC-16-160")
    
    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv() 
    
    data_demo <- data_demo[!data_demo$USUBJID %in% fil, ]
    
    
    # Test that any assigned formats are applied
    attr(data_demo$SUBJID, "width") <- 2.54
    attr(data_demo$SUBJID, "justify") <- "left"
    attr(data_demo$SUBJID, "format") <- "S:%s"
    #print(widths(data_demo))
    
    # Define table
    tbl <- create_table(data_demo) %>% 
      define(USUBJID, id_var = TRUE) 
    
    
    # Define Report
    rpt <- create_report(fp, font = "Times", font_size = 10, 
                         units = "cm", orientation = "landscape") %>%
      titles("Listing 1.0",
             "Demographics Dataset") %>%
      add_content(tbl, align = "left") %>% 
      page_header("Sponsor", "Drug") %>% 
      page_footer(left = "Time", right = "Page [pg] of [tpg]") %>% 
      footnotes("My footnote")
    
    #Write out report
    res <- write_report(rpt, output_type = "HTML")
    
    expect_equal(file.exists(fp), TRUE)
    
    #print(res$column_widths)
    
    # pdfpth <- file.path(base_path, "user/user3.pdf")
    # write_report(rpt, pdfpth, output_type = "PDF")
    # expect_equal(file.exists(pdfpth), TRUE)
  } else 
    expect_equal(TRUE, TRUE)
  
  
})

test_that("html-user5: Portrait in 12pt Arial works as expected.", {
  
  if (dev == TRUE) {
    
    dir_data <- file.path(data_dir, "data")
    
    fp <- file.path(base_path, "html/user5.html")
    
    # Read in prepared data
    df <- read.table(header = TRUE, text = '
        var     label        A             B          
        "ampg"   "N"          "19"          "13"         
        "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
        "ampg"   "Median"     "16.4"        "21.4"       
        "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
        "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
        "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
        "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
        "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')
    
    # Create table
    tbl <- create_table(df, first_row_blank = TRUE) %>% 
      define(var, label = "Variable", blank_after = TRUE, dedupe = TRUE,
             format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
      define(label, label = "") %>% 
      define(A, label = "Group A", align = "center", n = 19) %>% 
      define(B, label = "Group B", align = "center", n = 13)
    
    
    # Create report and add content
    rpt <- create_report(fp, orientation = "portrait", output_type = "HTML",
                         font = "Arial", font_size = 12) %>% 
      page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
      titles("Table 1.0", "MTCARS Summary Table") %>% 
      add_content(tbl) %>% 
      footnotes("* Motor Trend, 1974") %>%
      page_footer(left = Sys.time(), 
                  center = "Confidential", 
                  right = "Page [pg] of [tpg]")
    
    # Write out report
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)
  
})


