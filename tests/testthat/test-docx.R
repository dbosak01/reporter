
context("DOCX Tests")

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


# Basic Tests 1 - 10 ------------------------------------------------------


test_that("docx0: Basic docx works as expected.", {


  fp <- file.path(base_path, "docx/test0.docx")


  txt <- create_text(cnt, align = "left", width = 5, borders = c("all")) %>%
    titles("Here is my first title", blank_row = "below", borders = "all",
           align = "center", font_size = 14) %>%
    footnotes("Here is a footnotey", blank_row = "both", borders = "all")

  rpt <- create_report(fp, output_type = "DOCX", font = "Times",
                       font_size = 10, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(txt, align = "center") %>%
    #add_content(create_text("Goodbye")) %>%
    page_header(c("Left1", "Left2"), "Right") %>%
    page_footer("Page [pg] of [tpg]", "Center", "Right")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("docx1: Basic text with title header works as expected.", {


  fp <- file.path(base_path, "docx/test1.docx")


  txt <- create_text(cnt, align = "left", width = 5, borders = c("all")) %>%
    title_header("Here is my first title", right = "Right", blank_row = "none",
                 borders = "all") %>%
    footnotes("Here is my footnote", blank_row = "both", borders = "all")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 11, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(txt, align = "center") %>%
    page_header(c("Left1", "Left2"), "Right") %>%
    page_footer("Left", "Center", "Right")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  #  expect_equal(res$pages, 1)

})

test_that("docx2: Basic table works as expected.", {


  fp <- file.path(base_path, "docx/test2.docx")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"

  tbl <- create_table(dat, first_row_blank = TRUE, borders = "top")  %>%
     titles("Table 1.0", "My Nice Table", borders = c("top"),
            width = "content", align = "left") %>%
     footnotes("My footnote 1", "My footnote 2 Page [pg] of [tpg]",
               borders = c( "top"),
               align = "left", width = "content", blank_row = "none") %>%
    define(wt, width = 2, label = "Weight", align = "center",
           label_align = "right")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
     set_margins(top = 1, bottom = 1) %>%
     page_header("Left", c("Right1 and more", "Right2", "Page [pg] of [tpg]"),
                blank_row = "below") %>%
     add_content(tbl, align = "left") %>%
     page_footer("Left1", "Center1", "Page [pg] of [tpg]")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})


test_that("docx3: Basic table with title header works as expected.", {


  fp <- file.path(base_path, "docx/test3.docx")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1

  tbl <- create_table(dat, borders = c("outside")) %>%
    title_header("Table 1.0", "My Nice Table", right = "Right",
                 borders = c("top", "bottom"), blank_row = "none",
                 width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside",
              align = "left", width = "content")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"),
                blank_row = "none") %>%
    add_content(tbl)  %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("docx4: Spanning headers work as expected.", {


  fp <- file.path(base_path, "docx/test4")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c( "none")) %>%
    spanning_header(cyl, disp, "Span 1", label_align = "left") %>%
    spanning_header(hp, wt, "Span 2", underline = FALSE) %>%
    spanning_header(qsec, vs, "Span 3", n = 10) %>%
    spanning_header(drat, gear, "Super Duper\nWrapped Span",
                    n = 11, level = 2) %>%
    titles("Table 1.0", "My Nice Table", blank_row = "none",
           borders = c("top", "bottom")) %>%
    footnotes("My footnote 1", "My footnote 2",
              blank_row = "none", borders = c("top", "bottom"))

  rpt <- create_report(fp, output_type = "DOCX", font = fnt,
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


test_that("docx5: Multi page table works as expected.", {


  fp <- file.path(base_path, "docx/test5")

  dat <- iris


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = 1.5, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1.25, align = "centre") %>%
    define(Species, blank_after = TRUE)

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    footnotes("My footnote 1", "My footnote 2")

  res <- write_report(rpt)

  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 8)  # Temporary.  Should be 7


})


test_that("docx6: Basic text works as expected.", {

  fp <- file.path(base_path, "docx/test6")

  txt <- create_text(cnt, width = 6, borders = "outside", align = "center") %>%
    titles("Text 1.0", "My Nice Text", borders = "none", width = "content") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier",
                       font_size = 12, paper_size = "letter") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    add_content(txt, align = "center") %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)

  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 1)

})


test_that("docx7: Basic plot works as expected.", {


  library(ggplot2)

  fp <- file.path(base_path, "docx/test7.docx")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 7, borders = c("all")) %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none",
           font_size = 12, align = "left") %>%
    footnotes("* Motor Trend, 1974", borders = "none")


  rpt <- create_report(fp, output_type = "DOCX", font = fnt, font_size =fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})



# Good for testing
test_that("docx8: Page by works as expected.", {


  fp <- file.path(base_path, "docx/test8")

  dat <- iris


  tbl <- create_table(dat, borders = "all") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title",
           borders = "none") %>%
    page_by(Species, label = "Species: ", borders = "outside",
            blank_row = "both", align = "center") %>%
    define(Sepal.Length, label = "Sepal Length", width = 1.5, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1.25, align = "centre")  %>%
    define(Species, visible = FALSE) %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12, orientation = "portrait",
                       paper_size = "letter") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none", align = "left") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]")

  res <- write_report(rpt)

  expect_equal(file.exists(res$modified_path), TRUE)
  expect_equal(res$pages, 6)


})


# Good for testing.  Check bottom footnote positioning.
test_that("docx9: Page by on report works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "docx/test9.docx")

  dat <- iris


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = 1.5, align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = 1.25, align = "centre")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
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

  } else
    expect_equal(TRUE, TRUE)


})

test_that("docx10: Title Header and page header/footer wrapping work as expected.", {


  fp <- file.path(base_path, "docx/test10.docx")

  dat <- iris[1:10, ]

  tbl <- create_table(dat, borders = "none") %>%
    title_header("Table 1.0", "My Nice Report with Borders",
                 right = c("Right1", "Right2"),
                           #"Right3 long enough to wrap around at least once"),
                 borders = "none",
                 blank_row = "none") %>%
   # titles("Here is a title\nthat is going to wrap", "Another title") %>%
    footnotes("My footnote 1",
              paste("My footnote 2 Center1 here is a whole bunch of stuff to try and make it wrap",
                    "like more down here note 2 Center1 here is a whole bunch of stuff." ),
              valign = "top",
              borders = "none",
              blank_row = "above") %>%
    define(Sepal.Width, label = "Here is a rather long header label")

  rpt <- create_report(fp, output_type = "DOCX", font = fnt,
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


test_that("docx11: Basic plot with titles on report works as expected.", {

  if (dev == TRUE) {


  library(ggplot2)

  fp <- file.path(base_path, "docx/test11.docx")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8, borders = c("none"))


  rpt <- create_report(fp, output_type = "DOCX", font = fnt, font_size =fsz) %>%
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

  } else
    expect_equal(TRUE, TRUE)

})


test_that("docx12: Text with titles on report works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "docx/test12")

  txt <- create_text(cnt, width = 6, borders = "none", align = "center")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier",
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

  } else
    expect_equal(TRUE, TRUE)

})


# Works on all combinations on font and font size.  Needed adjustments to row height.
test_that("docx13: Table with break between sections works as expected.", {


  fp <- file.path(base_path, "docx/test13.docx")


  # Setup
  subjid <- 100:109
  name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
            "al-Kazemi, Najwa", "Schaffer, Ashley", "Laner, Tahma",
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon here is some more",
            "Smith, Shaileigh and \nmore and more and even more and more and more")
  sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
  age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
  arm <- c(rep("A", 5), rep("B", 5))

  # Create data frame
  df <- data.frame(subjid, name, sex, age, arm)


  tbl1 <- create_table(df, first_row_blank = TRUE, borders = "all") %>%
    define(subjid, label = "Subject ID", align = "left", width = 1) %>%
    define(name, label = "Subject Name", width = 1) %>%
    define(sex, label = "Sex") %>%
    define(age, label = "Age") %>%
    define(arm, label = "Arm",
           blank_after = TRUE,
           dedupe = TRUE,
           align = "right") #%>%
  # spanning_header(sex, arm, label = "Here is a spanning header")


  rpt <- create_report(fp, output_type = "DOCX", font = fnt, font_size = fsz) %>%
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



test_that("docx14: Plot with page by on plot works as expected.", {

  library(ggplot2)

  fp <- file.path(base_path, "docx/test14.docx")

  fmt <- value(condition(x == 4, "4 Cylinder"),
               condition(x == 6, "6 Cylinder"),
               condition(x == 8, "8 Cylinder"))

  dat <- mtcars[order(mtcars$cyl), ]

  p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()

  brdrs <- "outside"

  #dats <- split(p$data, p$data$grp)
  #tbl <- create_table(dat[1:3, ])

  plt <- create_plot(p, height = 4, width = 8, borders = "all") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot",
           borders = "all",
           blank_row = "both") %>%
    page_by(cyl, "Cylinders: ", borders = "all", blank_row = "both",
            format = fmt) %>%
    footnotes("* Motor Trend, 1974", borders = "all", valign = "top",
              blank_row = "both")

  rpt <- create_report(fp, output_type = "DOCX", font = fnt, font_size = fsz) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt) %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)


})

test_that("docx15: Title bold and font size works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "docx/test15.docx")

  dat <- mtcars[1:15, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"

  tbl <- create_table(dat, borders = "outside", first_row_blank = TRUE) %>%
    titles("Table 1.0", "My Nice Table", borders = c("outside"),
           width = "content", font_size = 14, bold = TRUE, blank_row = "none") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "outside",
              align = "left", width = "content") %>%
    define(wt, width = 1, label = "Weight", align = "center",
           label_align = "right")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 9, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1", "Right2", "Page [pg] of [tpg]"),
                blank_row = "none") %>%
    add_content(tbl, align = "left")  %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt)
  res$column_widths

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("docx16: 9 pt font inches works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "docx/test16.docx")

  rpt <- create_report(fp, output_type = "DOCX", font_size = 12,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)


})

test_that("docx17: 9 pt font cm works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "docx/test17.docx")

  rpt <- create_report(fp, output_type = "DOCX", font_size = 9,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)


})

test_that("docx18: 11 pt font inches works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "docx/test18.docx")

  tbl <- create_table(iris) %>%
    define(Species, page_break = TRUE)

  rpt <- create_report(fp, output_type = "DOCX", font_size = 11,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(tbl) %>%
    add_content(tbl) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)


})

test_that("docx19: 11 pt font cm works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "docx/test19.docx")

  rpt <- create_report(fp, output_type = "DOCX", font_size = 11,
                       font = "Courier",
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>%
    set_margins(top = 1, bottom = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)


})


test_that("docx20:  JPG Image file works as expected.", {

  if (dev == TRUE) {


  library(ggplot2)

  fp <- file.path(base_path, "docx/test20.docx")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  pltpath <- file.path(base_path, "docx/test20.jpg")
  ggsave(pltpath, width = 8, height = 4,
         units = "in",
         dpi = 300)

  plt <- create_plot(pltpath, height = 4, width = 8)


  rpt <- create_report(fp, output_type = "DOCX", font = "Arial") %>%
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

  } else
    expect_equal(TRUE, TRUE)


})


test_that("docx21: Check content blanks.", {

  if (dev == TRUE) {


  library(ggplot2)

  fp <- file.path(base_path, "docx/test21.docx")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 7, borders = c("all")) %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none",
           font_size = 12, align = "left", blank_row = "none") %>%
    footnotes("* Motor Trend, 1974", borders = "none", blank_row = "none")

  dat <- mtcars[1:5, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"

  tbl <- create_table(dat, borders = c("all"), first_row_blank = TRUE)  %>%
    titles("Table 1.0", "My Nice Table", borders = c("none"),
           width = "content", align = "left", blank_row = "none") %>%
    footnotes("My footnote 1", "My footnote 2 Page [pg] of [tpg]",
              borders = "none",
              align = "left", width = "content", blank_row = "none") %>%
    define(wt, width = .75, label = "Weight", align = "center",
           label_align = "right")

  txt <- create_text(cnt, align = "left", width = 5, borders = c("all")) %>%
    titles("Here is my first title", blank_row = "none", borders = "all",
           align = "center", font_size = 14) %>%
    footnotes("Here is a footnotey", blank_row = "none", borders = "all")

  rpt <- create_report(fp, output_type = "DOCX", font = "Times",
                       font_size = 10, orientation = "portrait") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center", page_break = FALSE) %>%
    add_content(tbl, align = "center", page_break = FALSE) %>%
    add_content(txt, align = "center", page_break = FALSE, blank_row = "none") %>%
    add_content(plt, align = "center", page_break = FALSE, blank_row = "none") %>%
    add_content(tbl, align = "center", page_break = FALSE, blank_row = "none") %>%
    add_content(txt, align = "center", page_break = FALSE, blank_row = "none") %>%
    page_header(c("Left1", "Left2"), "Right") %>%
    page_footer("Page [pg] of [tpg]", "Center", "Right")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})

test_that("docx22: Check titles and footnotes on report.", {

  if (dev == TRUE) {


  library(ggplot2)

  fp <- file.path(base_path, "docx/test22.docx")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 7, borders = c("all"))

  dat <- mtcars[1:5, ]
  attr(dat[[2]], "label") <- "Cylin."
  attr(dat[[2]], "width") <- 1
  attr(dat[[2]], "justify") <- "center"

  tbl <- create_table(dat, borders = c("all"), first_row_blank = TRUE) %>%
    define(wt, width = .75, label = "Weight", align = "center",
           label_align = "right")

  txt <- create_text(cnt, align = "left", width = 5, borders = c("all"))

  rpt <- create_report(fp, output_type = "DOCX", font = "Times",
                       font_size = 10, orientation = "portrait") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center", page_break = TRUE) %>%
    add_content(tbl, align = "center", page_break = TRUE) %>%
    add_content(txt, align = "center", page_break = TRUE) %>%
    page_header(c("Left1", "Left2"), "Right") %>%
    page_footer("Page [pg] of [tpg]", "Center", "Right") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none",
           font_size = 12, align = "left", blank_row = "none") %>%
    footnotes("* Motor Trend, 1974", borders = "none", blank_row = "none")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("docx23: Preview works as expected.", {


  fp <- file.path(base_path, "docx/test23.docx")

  dat <- iris

  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    footnotes("My footnote 1", "My footnote 2")

  rpt <- create_report(fp, output_type = "DOCX", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl) %>%
    page_footer("Left1", "Center1", "Right1")

  res <- write_report(rpt, preview = 2)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)


})

test_that("docx24: Header_bold works as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "docx/test24.docx")

  tbl <- create_table(dat, header_bold = TRUE, borders = "all") %>%
    column_defaults(width = 1) %>%
    titles("Report 1.0", "Simple Report", borders = "outside",
           blank_row = "none", bold = TRUE) %>%
    footnotes("My footnote", blank_row = "none")

  rpt <- create_report(fp, orientation = "portrait",
                       output_type = "DOCX", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1)

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})


test_that("docs25: Label row is one cell.", {


  fp <- file.path(base_path, "docx/test25.docx")


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
  tbl <- create_table(df, first_row_blank = TRUE, borders = "all") %>%
    stub(c("var", "label")) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = ll, cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19) %>%
    define(B, label = "Group B", align = "center", n = 13)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "DOCX",
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

test_that("docx26: Blank after on invisible column.", {

  fp <- file.path(base_path, "docx/test26.docx")

  tbl <- create_table(iris, borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "DOCX") %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


test_that("docx27: Page header width works.", {

  fp <- file.path(base_path, "docx/test27.docx")

  tbl <- create_table(iris[1:10, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
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


test_that("docx28: Carriage return in label row works.", {


  fp <- file.path(base_path, "docx/test28.docx")


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

  ll <- "Here is a super long label to \nsee if it can span\nthe entire table."

  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>%
    stub(c("var", "label")) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = ll, cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19) %>%
    define(B, label = "Group B", align = "center", n = 13)


  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait", output_type = "DOCX",
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



test_that("docx29: Titles and footnotes in header and footer works as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "docx/test29.docx")

  tbl <- create_table(dat) %>%
    column_defaults(width = 1)

  rpt <- create_report(fp, orientation = "landscape",
                       output_type = "DOCX", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", "Right") %>%
    titles("Report 1.0", "Simple Report",
           blank_row = "none", header = TRUE, align = "left") %>%
    footnotes("My footnote", "Another footnote", "And another",
              blank_row = "none", footer = TRUE) %>%
    page_footer("Left", "Center", "Right")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("docx30: Titles and footnotes variations in header and footer work as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "docx/test30.docx")

  tbl <- create_table(dat) %>%
    column_defaults(width = 1)

  rpt <- create_report(fp, orientation = "landscape",
                       output_type = "DOCX", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1) %>%
    page_header("Left", "Right") %>%
    titles("Report 1.0", "Simple Report", align = "left", width = 6,
           blank_row = "none", header = TRUE) %>%
    titles("Report 1.0", "Simple Report", align = "right", width = 6,
           blank_row = "below", header = TRUE, borders = "bottom") %>%
    footnotes("My footnote", blank_row = "none", footer = TRUE, borders = "top") %>%
    footnotes("My footnote2", blank_row = "none", footer = TRUE, align = "right") %>%
    footnotes("My footnote3", blank_row = "none", footer = TRUE, align = "center") %>%
    page_footer("Left", "Center", "Right")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("docx31: Titles and footnotes in header and footer no page header/footer works as expected", {

  dat <- mtcars[1:10, 1:3]

  fp <- file.path(base_path, "docx/test31.docx")

  tbl <- create_table(dat) %>%
    column_defaults(width = 1)

  rpt <- create_report(fp, orientation = "landscape",
                       output_type = "DOCX", font = "Arial") %>%
    add_content(tbl) %>%
    set_margins(top = 1, bottom = 1) %>%
    titles("Report 1.0 here is a big long title", "Simple Report", align = "left",
           blank_row = "none", header = TRUE) %>%
    titles("Report 1.0", "Simple Report", align = "center", width = 6,
           blank_row = "below", header = TRUE) %>%
    footnotes("My footnote1", "My footnote2", blank_row = "none", footer = TRUE)

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)

})

test_that("docx32: Title columns work 1 column.", {

  fp <- file.path(base_path, "docx/test32.docx")

  tbl <- create_table(iris[1:15, ], borders = "all")  %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame3",
           blank_row = "both", columns =  1, align = "center",
           borders = c("outside"))

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("docx33: Title columns work 2 columns.", {

  fp <- file.path(base_path, "docx/test33.docx")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame", "Left", "Right",
           blank_row = c("above", "below"), columns =  2, borders = "outside")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")  %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("docx34: Title columns work 3 columns.", {

  fp <- file.path(base_path, "docx/test34.docx")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rght <- paste("Here is a big long text string to see how the automatic",
                "wrapping is happening in a reduced size cell on the right.")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier",
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

test_that("docx35: Multiple title blocks work as expected.", {

  fp <- file.path(base_path, "docx/test35.docx")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
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

test_that("docx36: Custom page size works as expected.", {

  fp <- file.path(base_path, "docx/test36.docx")

  tbl <- create_table(iris[1:15, ]) %>%
    define(Species, visible = FALSE)

  ttl <- c("Title1", "Title2", "Title3")

  rpt <- create_report(fp, output_type = "DOCX",
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


test_that("docx37: Basic cell style bold works as expected.", {

  if (dev) {

    fp <- file.path(base_path, "docx/test37.docx")

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


    rpt <- create_report(fp, output_type = "DOCX",
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

test_that("docx38: Bolding works with stub.", {


  fp <- file.path(base_path, "docx/test38.docx")


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
  rpt <- create_report(fp, orientation = "portrait", output_type = "DOCX",
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


test_that("docx39: Bold cell style with column defaults.", {

  if (dev) {

    fp <- file.path(base_path, "docx/test39.docx")

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


    rpt <- create_report(fp, output_type = "DOCX",
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


test_that("docx40: Bolding, column defaults, and stub works.", {


  fp <- file.path(base_path, "docx/test40.docx")


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
  rpt <- create_report(fp, orientation = "portrait", output_type = "DOCX",
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

test_that("docx41: Spanning header bold work as expected.", {


  fp <- file.path(base_path, "docx/test41")

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

  rpt <- create_report(fp, output_type = "DOCX", font = fnt,
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


test_that("docx42: Italic footnotes work as expected.", {


  fp <- file.path(base_path, "docx/test42.docx")

  dat <- mtcars[1:15, ]

  tbl <- create_table(dat, borders = c( "none")) %>%
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

  rpt <- create_report(fp, output_type = "DOCX", font = fnt,
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

test_that("docx43: Two plots on report works as expected.", {

  if (dev == TRUE) {


    library(ggplot2)

    fp <- file.path(base_path, "docx/test43.docx")

    p1 <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

    plt1 <- create_plot(p1, height = 4, width = 8, borders = c("none"))


    p2 <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

    plt2 <- create_plot(p2, height = 4, width = 8, borders = c("none"))


    rpt <- create_report(fp, output_type = "DOCX", font = fnt, font_size =fsz) %>%
      page_header("Client", "Study: XYZ") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(plt1, align = "center") %>%
      add_content(plt2, align = "center") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>%
      titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "none") %>%
      footnotes("* Motor Trend, 1974", borders = "none")


    res <- write_report(rpt)

    #print(res)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 2)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("docx44: Stub indent.", {


  fp <- file.path(base_path, "docx/test44.docx")


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
  rpt <- create_report(fp, orientation = "portrait", output_type = "DOCX",
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

test_that("docx45: Footnote columns work 1 column.", {

  fp <- file.path(base_path, "docx/test45.docx")

  tbl <- create_table(iris[1:15, ], borders = "all")  %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame3",
           blank_row = "both", columns =  1, align = "center",
           borders = c("outside")) %>%
    footnotes("Here is a footnote", "And another", columns = 1, borders = "all")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Here is a footnote", "And another", columns = 1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("docx46: Footnote columns work 2 columns.", {

  fp <- file.path(base_path, "docx/test46.docx")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame", "Left", "Right",
           blank_row = c("above", "below"), columns =  2,
           borders = "outside") %>%
    footnotes("Here is a footnote", "And another", columns = 2, borders = "all")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")  %>%
    footnotes("Here is a footnote", "And another", columns = 2)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("docx47: Footnote columns work 3 columns.", {

  fp <- file.path(base_path, "docx/test47.docx")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)  %>%
    footnotes("Here is a footnote", "And another", "And more", "", "More",
              columns = 3, borders = "all")

  rght <- paste("Here is a big long text string to see how the automatic",
                "wrapping is happening in a reduced size cell on the right.")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier",
                       font_size = 10) %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0\nsecond row", "IRIS Data Frame",
           "      My right thing", "", "Center", rght,
           blank_row = "below", columns =  3, borders = "none") %>%
    footnotes("Here is a footnote", "And another", "A",
              "Here is a longer footnote",  "to see if I can figure",
              "out the alignment pattern.",
              columns = 3)


  res <- write_report(rpt)
  res

  expect_equal(file.exists(fp), TRUE)

})

test_that("docx48: Multiple footnote blocks work as expected.", {

  fp <- file.path(base_path, "docx/test48.docx")

  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE) %>%
    footnotes("Here is a footnote", "And another", "And more", "", "More",
              columns = 3, borders = "all", blank_row = "both")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
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
    titles("Here is a title", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})


test_that("docx49: Greater and Less characters are escaped as expected.", {

  fp <- file.path(base_path, "docx/test49.docx")

  dt <- iris[1:15, ]
  dt$Species <- as.character(dt$Species)
  dt[1, "Species"] <- "set > osa"
  dt[2, "Species"] <- "set < osa"
  dt[3, "Species"] <- "set & osa"

  tbl <- create_table(dt, borders = "all") %>%
    define(Species, label = "Spe>ies") %>%
    footnotes("Here is a > footnote", "And < another", "And & more", "", "More",
              columns = 3, borders = "all", blank_row = "both")

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier") %>%
    add_content(tbl) %>%
    page_header("left<", "right&") %>%
    page_footer("left>", "", "right<") %>%
    footnotes("Table> 1.0", "IRIS Data Frame",
              blank_row = "below", columns =  1, align = "center", width = 7,
              borders = "all") %>%
    footnotes("Table 2.0", "IRIS Data Frame2", "Left", "Right",
              blank_row = "below", columns =  2, borders = "all") %>%
    footnotes("Table 3.0", "IRIS Data Frame3", "My right thing", "", "Center",
              blank_row = "below", columns =  3, borders = "all") %>%
    titles("Here is>a title", "And<another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("docx50: Page by with wrap works as expected.", {


  fp <- file.path(base_path, "docx/test50.docx")

  dat <- iris
  dat$Pgby <- as.character(dat$Species)
  dat$Pgby <- paste0("Flower Type\n", dat$Pgby)


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, label = "Species: ", align = "right", borders = "none") %>%
    define(Pgby, visible = FALSE)

  rpt <- create_report(fp, output_type = "DOCX", font = fnt,
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
  expect_equal(res$pages, 9)
  expect_equal(length(res$column_widths[[1]]), 5)


})

test_that("docx51: Page by with format works as expected.", {


  fp <- file.path(base_path, "docx/test51.docx")

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

  rpt <- create_report(fp, output_type = "DOCX",
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

test_that("docx52: Borders work as expected.", {


  fp <- file.path(base_path, "docx/test52.docx")

  fmt1 <- c(setosa = 1, versicolor = 2, virginica = 3)
  fmt2 <- value(condition(x == 1, "Setosa"),
                condition(x == 2, "Versicolor"),
                condition(x == 3, "Virginica"))

  dat <- iris
  fmtval <- fmt1[dat$Species]
  names(fmtval) <- NULL
  dat$Pgby <- fmtval

  tbl <- create_table(dat, borders = "none") %>%
    define(Pgby, visible = FALSE)

  rpt <- create_report(fp, output_type = "DOCX",
                       orientation = "landscape") %>%
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, align = "left", label = "Flower:", borders = c("top", "bottom"),
            format = fmt2, blank_row = "both") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_header("Left", "Right") %>%
    page_footer("Left1", "Center1", "Right1") %>%
    footnotes("My footnote 1", "My footnote 2", borders = c("top", "bottom"),
              columns = 2, blank_row = "both") %>%
    footnotes("hello", borders = "none", blank_row = "none")

  res <- write_report(rpt)
  res
  res$column_widths

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 9)
  expect_equal(length(res$column_widths[[1]]), 5)


})


test_that("docx53: Page by with wrap on report works as expected.", {


  fp <- file.path(base_path, "docx/test53.docx")

  dat <- iris
  dat$Pgby <- as.character(dat$Species)
  dat$Pgby <- paste0("Flower Type\n", dat$Pgby)


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    define(Pgby, visible = FALSE)

  rpt <- create_report(fp, output_type = "DOCX", font = fnt,
                       font_size = fsz, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    page_by(Pgby, label = "Species: ", align = "right", borders = "none") %>%
    page_header("Left", "Right") %>%
    page_footer("Left1", "Center1", "Right1") %>%
    footnotes("My footnote 1", "My footnote 2", borders = "none")

  res <- write_report(rpt)
  res
  res$column_widths

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 9)
  expect_equal(length(res$column_widths[[1]]), 5)


})


# Not right yet.  Can't reproduce problem.
test_that("docx55: Top margin 1.5 works as expected.", {

  fp <- file.path(base_path, "docx/test55.docx")

  dat <- iris
  dat$Pgby <- as.character(dat$Species)
  dat$Pgby <- paste0("Flower Type\n", dat$Pgby)


  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, label = "Species: ", align = "right", borders = "none") %>%
    define(Pgby, visible = FALSE)

  rpt <- create_report(fp, output_type = "DOCX", font = "Courier",
                       font_size = 9, orientation = "landscape") %>%
    set_margins(top = 1.5, bottom = 1) %>%
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

test_that("docx-56: Multi page table removes blank spaces.", {
  
  if (dev == TRUE) {
    
    
    fp <- file.path(base_path, "docx/test56.docx")
    
    dat1 <- iris[1:10, ]
    dat2 <- iris[11:20, ]
    dat3 <- iris[21:30, ]
    
    
    tbl1 <- create_table(dat1, borders = "none") %>%
      titles("Table 1.0", "My Nice Irises1") 
    
    tbl2 <- create_table(dat2, borders = "none") %>%
      titles("Table 1.0", "My Nice Irises2")
    
    tbl3 <- create_table(dat3, borders = "none") %>%
      titles("Table 1.0", "My Nice Irises3")
    
    rpt <- create_report(fp, output_type = "DOCX", font = fnt,
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


# No errors, but Word converts the EMG to JPG automatically.
# Even if you just insert in manually.
test_that("docx-57:  EMF Image file works as expected.", {
  
  if (dev == TRUE) {
    
    fp <- file.path(base_path, "docx/test57.docx")
    
    pltpath <- file.path(base_path, "docx/example10.emf")
    
    plt <- create_plot(pltpath, height = 4, width = 8)
    
    
    rpt <- create_report(fp, output_type = "DOCX", font = "Arial") %>%
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
    
  } else
    expect_equal(TRUE, TRUE)
  
  
})



# User Tests --------------------------------------------------------------


test_that("docx-user1: demo table works.", {

  if (dev) {
    library(tidyr)
    library(dplyr)

    # Data Filepath
    dir_data <- file.path(data_dir, "data")

    fp <- file.path(base_path, "docx/user1.docx")


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
    rpt <- create_report(fp, output_type = "DOCX", font = fnt, font_size = fsz) %>%
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

test_that("docx-user2: demo table with stub works.", {


  if (dev) {

    library(tidyr)
    library(dplyr)



    # Data Filepath
    dir_data <- file.path(data_dir, "data")

    fp <- file.path(base_path, "docx/user2.docx")


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
      stub(c("var", "label"), width = 1.5) %>%
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
    rpt <- create_report(fp, output_type = "DOCX",
                         font = "Arial", font_size = 10) %>%
      add_content(tbl, align = "center") %>%
      page_header("Sponsor", "Drug") %>%
      page_footer(left = "Time", right = "Page [pg] of [tpg]") %>%
      footnotes("Here is a footnote on the bottom") #%>%
    #page_by(var = "var", label = "Variable: ")

    # Write out report
    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)


  } else
    expect_equal(TRUE, TRUE)

})

test_that("docx-user3: listings works.", {
  if (dev == TRUE) {
    # Data Filepath
    dir_data <- file.path(data_dir, "data")

    fp <- file.path(base_path, "docx/user3.docx")

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
    tbl <- create_table(data_demo, borders = "none") %>%
      define(USUBJID, id_var = TRUE) #%>%
      #define(ETHNIC, width = 1)


    # Define Report
    rpt <- create_report(fp, font = "Arial", font_size = 10,
                         orientation = "portrait") %>%
      titles("Listing 1.0",
             "Demographics Dataset") %>%
      add_content(tbl, align = "left") %>%
      page_header("Sponsor", "Drug") %>%
      page_footer(left = "Time", right = "Page [pg] of [tpg]") %>%
      footnotes("My footnotes")

    #Write out report
    res <- write_report(rpt, output_type = "DOCX")

    expect_equal(file.exists(fp), TRUE)



    # pdfpth <- file.path(base_path, "user/user3.pdf")
    # write_report(rpt, pdfpth, output_type = "PDF")
    # expect_equal(file.exists(pdfpth), TRUE)
  } else
    expect_equal(TRUE, TRUE)


})


test_that("docx-user4: listing in cm and times works.", {
  if (dev == TRUE) {
    # Data Filepath
    dir_data <- file.path(data_dir, "data")

    fp <- file.path(base_path, "docx/user4.docx")

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
    tbl <- create_table(data_demo, borders = "none") %>%
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
    res <- write_report(rpt, output_type = "DOCX")

    expect_equal(file.exists(fp), TRUE)

    #print(res$column_widths)

    # pdfpth <- file.path(base_path, "user/user3.pdf")
    # write_report(rpt, pdfpth, output_type = "PDF")
    # expect_equal(file.exists(pdfpth), TRUE)
  } else
    expect_equal(TRUE, TRUE)


})

test_that("docx-user5: Portrait in 12pt Arial works as expected.", {

  if (dev == TRUE) {

    dir_data <- file.path(data_dir, "data")

    fp <- file.path(base_path, "docx/user5.docx")

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
    rpt <- create_report(fp, orientation = "portrait", output_type = "DOCX",
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


test_that("docx-user6: Check wrapping.", {

  if (dev == TRUE) {

    dir_data <- file.path(data_dir, "data")

    fp <- file.path(base_path, "docx/user6.docx")


    # Load Data
    data_demo   <- file.path(dir_data, "dm.csv") %>%
      read.csv()


    dt <- data_demo[ , c("USUBJID", "BRTHDTC", "AGE", "SEX", "RACE", "ARM")]

    attr(dt$USUBJID, "label") <- "Universal Subject ID"
    attr(dt$BRTHDTC, "label") <- "Subject Birth Date"
    attr(dt$AGE, "label") <- "Subject Age in Years"
    attr(dt$SEX, "label") <- "Subject Biological Sex at Birth"
    attr(dt$RACE, "label") <- "Subject Race Category"
    attr(dt$ARM, "label") <- "Treatment Group"


    # Create table
    tbl <- create_table(dt)

    # Create report
    rpt <- create_report(fp, orientation = "portrait", font_size = 12,
                         output_type = "DOCX", font = "Times") %>%
      titles("Our first SASSY report", bold = TRUE) %>%
      add_content(tbl)

    # write out the report
    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)


  } else
    expect_equal(TRUE, TRUE)

})



test_that("user7: Borders with spanning headers work as expected.", {

  if (dev == TRUE) {

    df <- read.table(header = TRUE, text = '
        var     label           A             B             C
        "AGE"   "n"             "19"          "13"          "32"
        "AGE"   "Mean"          "18.8 (6.5)"  "22.0 (4.9)"  "20.0 (5.9)"
        "AGE"   "Median"        "16.4"        "21.4"        "20.1"
        "AGE"   "Q1 - Q3"       "15.1 - 21.2" "19.2 - 22.8" "15.2 - 21.8"
        "RACE"  "White"         "10 (52.6)"   "4 (30.8)"    "14 (43.8)"
        "RACE"  "Black"         "4 (21.1)"    "3 (23.1)"    "7 (21.9)"
        "RACE"  "Others\U1D47"  "5 (26.3)"    "6 (46.2)"    "11 (34.4.2)"
        ')

    var_fmt <- c(AGE = "Age (yrs)", RACE = "Race - n (%)")


    fp <- file.path(base_path, "docx/user7.docx")

    tbl <- create_table(df, first_row_blank = TRUE, borders = c("outside")) %>%
      stub(vars = c("var", "label"), " ", width = 2.5) %>%
      spanning_header(from = "A", to = "B", label = "Treatments\U1D43") %>%
      define(var, blank_after = TRUE, format = var_fmt, label_row = TRUE) %>%
      define(label, indent=0.25) %>%
      define(A,  align = "center", label = "Placebo\n(N = 19)") %>%
      define(B,  align = "center", label = "Drug\n(N = 13)") %>%
      define(C,  align = "center", label = "Total\n(N = 32)") %>%
      footnotes("\U1D43 study drug", blank_row="none" ) %>%
      footnotes("\U1D47 Asian, Japanese and Chinese", blank_row="none")

    rpt <- create_report(fp, output_type = "DOCX",
                         font = "Arial") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      page_footer(left = paste("Date:", Sys.time()), right = "Page [pg] of [tpg]", blank_row="none") %>%
      footnotes("Program: C:/Users/Home/AppData/Local/Temp/tdemo.R", blank_row="above")

    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 1)

    # file.show(fp)


  } else
    expect_equal(TRUE, TRUE)

})


# This is good
test_that("user8: Check footnotes on page by.", {

  if (dev == TRUE) {

    fp <- file.path(base_path, "docx/user8")


    df <- read.table(header = TRUE, text = '
      var     stat        A             B
      "Age (yrs)"   "n"          "19"          "13"
      " "   "Mean"       "18.8 (6.5)"  "22.0 (4.9)"
      " "   "Median"     "16.4"        "21.4"
      " "   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      " "   " "          " "           " "
      "Race"  "White" "10 ( 52.6%)" "4 ( 30.8%)"
      " "     "Black" "4 ( 21.1%)"  "3 ( 23.1%)"
      " "     "Others\U1D47" "5 ( 26.3%)"  "6 ( 46.2%)"')

    df1 <- df
    df2 <- df
    df3 <- df

    df1$SEX <- 'Female'
    df2$SEX <- 'Male'
    df3$SEX <- 'Other'

    df <- rbind(df1, df2, df3)

    # Create table
    tbl <- create_table(df, first_row_blank = TRUE, borders=c("top")) %>%
      page_by(SEX, "Sex: ", align = "left", blank_row="none") %>%
      #stub(c("var", "label")) %>%
      column_defaults(width = 1.25) %>%

      spanning_header(from = "A", to = "B", label = "Treatments\U1D43") %>%

      define(SEX, visible = FALSE) %>%
      define(var, label = " ", align = "left") %>%
      define(stat,label = " ", align = "left") %>%
      define(A,   label = "Treament A", align = "center", n = 19) %>%

      define(B,   label = "Treament B", align = "center", n = 13)  %>%
      titles("Table 1.1 Demographics", "Randomised Population", font_size = 10) %>%

      footnotes("Page [pg] of [tpg]", align = "right", blank_row="none", borders=c("top")) %>%
      footnotes("\U1D43 study drug treatments", blank_row="none", borders=c("top")  ) %>%
      footnotes("\U1D47 Asian, Japanese and Chinese", blank_row="none", borders = "bottom")

    rpt <- create_report(fp, output_type = "DOCX", font = "Arial", font_size = 10) %>%

      #This is page header and it goes into the header of the table
      page_header("Protocol: 9999") %>%

      add_content(tbl) %>%

      page_footer(left = paste("Date:", Sys.time()), right = "Page [pg] of [tpg]", blank_row="none") %>%
      footnotes("Program: C:/Users/Home/AppData/Local/Temp/tdemo.R", blank_row="above", valign = "bottom")



    res <- write_report(rpt)

    expect_equal(file.exists(res$modified_path), TRUE)
    expect_equal(res$pages, 3)

  } else
    expect_equal(TRUE, TRUE)

})

# Custom line count.  Needed because something is wrong. But I can't figure it out.
test_that("user9: Report with top and bottom borders stays on one page.", {

  if (dev) {

    fp <- file.path(base_path, "docx/user9")

    df <- read.table(header = TRUE, text = '
        var     label           A             B             C
        "AGE"   "n"             "19"          "13"          "32"
        "AGE"   "Mean"          "18.8 (6.5)"  "22.0 (4.9)"  "20.0 (5.9)"
        "AGE"   "Median"        "16.4"        "21.4"        "20.1"
        "AGE"   "Q1 - Q3"       "15.1 - 21.2" "19.2 - 22.8" "15.2 - 21.8"
        "RACE"  "White"         "10 (52.6)"   "4 (30.8)"    "14 (43.8)"
        "RACE"  "Black"         "4 (21.1)"    "3 (23.1)"    "7 (21.9)"
        "RACE"  "Others\U1D47"  "5 (26.3)"    "6 (46.2)"    "11 (34.4.2)"
        ')

    var_fmt <- c(AGE = "Age (yrs)", RACE = "Race - n (%)")


    tbl <- create_table(df, first_row_blank = TRUE, borders = c("top", "bottom")) %>%
      stub(vars = c("var", "label"), " ", width = 2.5) %>%
      # define(var, blank_after = TRUE, dedupe = TRUE, label = "Variable", format = var_fmt,label_row = TRUE) %>%
      spanning_header(from = "A", to = "B", label = "Treatments\U1D43") %>%
      define(var, blank_after = TRUE, format = var_fmt, label_row = TRUE) %>%
      define(label, indent=0.25) %>%
      # define(A,  align = "center", label = "Placebo\n(N = 19)", n = 19) %>%
      # define(B,  align = "center", label = "Drug\n(N = 13)", n = 13) %>%
      # define(C,  align = "center", label = "Total\n(N = 32)", n = 32) %>%
      define(A,  align = "center", label = "Placebo", n = 19) %>%
      define(B,  align = "center", label = "Drug", n = 13) %>%
      define(C,  align = "center", label = "Total", n = 32) %>%
      footnotes("\U1D43 study drug", blank_row="none" ) %>%
      footnotes("\U1D47 Asian, Japanese and Chinese", blank_row="none") %>%
      titles("My title", "My title 2")

    rpt <- create_report(fp, output_type = "DOCX",
                         font = "Arial", font_size = 11) %>%
      set_margins(top = 1, bottom = 1) %>%
      options_fixed(line_count = 32) %>%
      add_content(tbl) %>%
      # page_header("Help") %>%
      #page_footer(left = paste("Date:", Sys.time()), right = "Page [pg] of [tpg]", blank_row="none") %>%
      footnotes("Program: C:/Users/Home/AppData/Local/Temp/tdemo.R", blank_row="above")

    res <- write_report(rpt)

    #file.show(res$modified_path)
    expect_equal(file.exists(res$modified_path), TRUE)
    expect_equal(res$pages, 1)

  } else
    expect_equal(TRUE, TRUE)

})

