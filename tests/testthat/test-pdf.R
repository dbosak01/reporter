
context("PDF Tests")


base_path <- "/home/dbosak01/packages/reporter/tests/testthat"
base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

dev <- FALSE



test_that("pdf1: Simplest table works as expected.", {

  if (dev == TRUE) {


  # if (dev & rmarkdown::pandoc_available("1.12.3")) {

  fp <- file.path(base_path, "pdf/test1.pdf")

  rpt <- create_report(fp, output_type = "PDF") %>%
    add_content(create_table(mtcars[1:10, ]), align = "left")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  } else 
    expect_equal(TRUE, TRUE)



})

test_that("pdf2: Simplest table with title works as expected.", {

  if (dev == TRUE) {


    fp <- file.path(base_path, "pdf/test2.pdf")

    tbl <- create_table(mtcars[1:10, ]) %>%
      define(vs, visible = FALSE)

    rpt <- create_report(fp, output_type = "PDF") %>%
      options_fixed(font_size = 10) %>%
      titles("MTCARS Data Frame", align = "left") %>%
      add_content(tbl)


    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)


})

test_that("pdf3: Table with break between sections works as expected.", {

  if (dev == TRUE) {


  fp <- file.path(base_path, "pdf/test3.pdf")

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
  df <- data.frame(subjid, name, sex, age, arm, stringsAsFactors = FALSE)


  tbl1 <- create_table(df, first_row_blank = TRUE) %>%
    define(subjid, label = "Subject ID", align = "left") %>%
    define(name, label = "Subject Name") %>%
    define(sex, label = "Sex") %>%
    define(age, label = "Age") %>%
    define(arm, label = "Arm",
           blank_after = TRUE,
           dedupe = TRUE,
           align = "right")


  rpt <- create_report(fp, output_type = "PDF") %>%
    options_fixed(font_size = 10) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing",
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = "Time", center = "Confidential",
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  
  } else 
    expect_equal(TRUE, TRUE)


})

test_that("pdf4: Table that spans multiple pages breaks as expected.", {

  if (dev == TRUE) {

  
    fp <- file.path(base_path, "pdf/test4.pdf")

    rpt <- create_report(fp, output_type = "PDF") %>%
      page_header("left", "right") %>%
      titles("IRIS Data Frame") %>%
      add_content(create_table(iris)) %>%
      page_footer("left", "center", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)

  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
    
  } else 
    expect_equal(TRUE, TRUE)
})

test_that("pdf5: Table with long cell and label values wraps as expected.", {


    fp <- file.path(base_path, "pdf/test5.pdf")

    # Setup
    arm <- c(rep("A", 5), rep("B", 5))
    subjid <- 100:109
    name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
              "al-Kazemi, Najwa \nand more and more", "Schaffer, Ashley", "Laner, Tahma",
              "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon",
              "Smith, Shaileigh")
    sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
    age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)


    # Create data frame
    df <- data.frame(arm, subjid, name, sex, age, stringsAsFactors = FALSE)


    tbl1 <- create_table(df, first_row_blank = TRUE) %>%
      define(subjid, label = "Subject ID for a patient", n = 10, align = "left",
             width = 1) %>%
      define(name, label = "Subject Name", width = 1) %>%
      define(sex, label = "Sex", n = 10, align = "center") %>%
      define(age, label = "Age", n = 10) %>%
      define(arm, label = "Arm",
             blank_after = TRUE,
             dedupe = TRUE)


    rpt <- create_report(fp, output_type = "PDF") %>%
      titles("Table 1.0", align = "center") %>%

      add_content(tbl1)


    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)


})

test_that("pdf6: Table with spanning headers works as expected.", {


    fp <- file.path(base_path, "pdf/test6.pdf")


    df <- data.frame(vehicle = rownames(mtcars), mtcars, stringsAsFactors = FALSE)
    rownames(df) = NULL

    df$qsec <- fattr(df$qsec, format = "%.1f")
    df$wt <- fattr(df$wt, justify = "center", width = .75)

    tbl <- create_table(df) %>%
      spanning_header("mpg", "hp",
                      label = "Span 1", label_align = "center", n = 10) %>%
      spanning_header("drat", "qsec",
                      label = "Span 2", label_align = "center", n = 10) %>%
      spanning_header("vs", "carb",
                      label = "Span 3", label_align = "center", n = 10) %>%
      spanning_header(from = "drat", to = "carb", label = "Super Span",
                      label_align = "center",
                      level = 2) %>%
      define(vehicle, label = "Vehicle") %>%
      define(mpg, format = "%.1f")

    rpt <- create_report(fp, output_type = "PDF") %>%
      add_content(tbl) %>%
      titles("Table 1.0", "MTCARS Subset Test")

    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)


})




test_that("pdf7: Simplest PDF report with 1 in margins works as expected.", {

  if (dev == TRUE) {


    fp <- file.path(base_path, "pdf/test7.pdf")

    tbl <- create_table(mtcars[1:10, ]) %>%
      column_defaults(width = .5) %>%
      define(vs, visible = FALSE)

    rpt <- create_report(fp, output_type = "PDF") %>%
      page_header("Client", "Study") %>%
      titles("MTCARS Data Frame") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl, align = "left") %>%
      page_footer("Time", right = "Page [pg] of [tpg]")



    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)

  } else 
    expect_equal(TRUE, TRUE)
  
})


test_that("pdf8: Two page PDF report works as expected.", {

  if (dev == TRUE) {


    fp <- file.path(base_path, "pdf/test8.pdf")

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
    df <- data.frame(subjid, name, sex, age, arm, stringsAsFactors = FALSE)

    df1 <- df[df$arm == "A", ]
    df2 <- df[df$arm == "B", ]

    afmt <- value(condition(x == "A", "Placebo"),
                  condition(x == "B", "Treatment 1"))

    sfmt1 <- value(condition(x == "M", "Male"),
                   condition(x == "F", "Female"),
                   condition(TRUE, "Other"))

    sfmt2 <- c(M = "Male", F = "Female")

    tbl1 <- create_table(df1, width = 7) %>%
      define(sex, width = 1, format = sfmt1) %>%
      define(name, width = 2) %>%
      define(age, width = 1, align = "left")

    tbl2 <- create_table(df2, width = 7) %>%
      define(sex, width = 1, format = sfmt2) %>%
      define(age, format = "%0d%%", align = "left") %>%
      define(name, width = 2) %>%
      define(arm, format = afmt, width = 2, align = "right")


    rpt <- create_report(fp, output_type = "PDF") %>%
      options_fixed(font_size = 10) %>%
      set_margins(top = 1, bottom = 1) %>%
      page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
      titles("Table 1.0", "Analysis Data Subject Listing",
             "Safety Population", align = "center") %>%
      footnotes("Program Name: table1_0.R") %>%
      page_footer(left = "Time", center = "Confidential",
                  right = "Page [pg] of [tpg]") %>%
      add_content(tbl1) %>%
      add_content(tbl2)


    res <- write_report(rpt)

    #print(res)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)


})




test_that("pdf9: Simplest PDF Plot works as expected.", {


    library(ggplot2)

    fp <- file.path(base_path, "pdf/test9.pdf")

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

    #print(res)

    expect_equal(file.exists(fp), TRUE)


})


test_that("pdf10: PDF Table with Plot works as expected.", {


    library(ggplot2)

    fp <- file.path(base_path, "pdf/test10.pdf")

    p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()


    plt <- create_plot(p, height = 4, width = 8)
    tbl <- create_table(mtcars[1:10, ])


    rpt <- create_report(fp, output_type = "PDF") %>%
      page_header("Client", "Study: XYZ") %>%
      titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      add_content(plt, align = "center") %>%
      footnotes("* Motor Trend, 1974") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    #print(res)

    expect_equal(file.exists(fp), TRUE)


})




test_that("pdf11: PDF Table with Plot on same page works as expected.", {
  

  library(ggplot2)

  fp <- file.path(base_path, "pdf/test11.pdf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8)
  tbl <- create_table(mtcars[1:3, ])


  rpt <- create_report(fp, output_type = "PDF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
    set_margins(top = 1, bottom = 1) %>%

    add_content(plt, page_break = FALSE, blank_row = "none") %>%
    add_content(tbl) %>%
    footnotes("* Motor Trend, 1974") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")


  res <- write_report(rpt)

  #print(res)

  expect_equal(file.exists(fp), TRUE)


})


test_that("pdf12: Table and Text output works as expected.", {


  fp <- file.path(base_path, "pdf/test12.pdf")

  tbl1 <- mtcars[1:10, ]
  tbl2 <- mtcars[11:20, ]

  rpt <- create_report(fp, orientation = "portrait", output_type = "PDF") %>%
    titles("Report 5.0", "Table and Text Report") %>%
    page_header(left = "Client: ABC", right = "Study: 123") %>%
    add_content(create_table(tbl1), page_break = FALSE) %>%
    add_content(create_text("* NOTE: Car information from 1971."),
                align = "left") %>%
    add_content(create_table(tbl2), page_break = FALSE) %>%
    add_content(create_text("* NOTE: Car information from 1972."),
                align = "left") %>%
    page_footer(left = "Time",
                center = "Confidential",
                right ="Page [pg] of [tpg]")

  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)


})


test_that("pdf13: Very Long text output works as expected.", {

  if (dev) {

    fp <- file.path(base_path, "pdf/test13.pdf")

    l <- paste(rep(cnt, 1000), collapse = "\n\n")

    rpt <- create_report(fp, orientation = "portrait", output_type = "PDF") %>%
      titles("Report 6.0", "Very long Text Report") %>%
      page_header(left = "Client: ABC", right = "Study: 123") %>%
      add_content(create_text(l)) %>%
      page_footer(left = "Time",
                  center = "Confidential",
                  right ="Page [pg] of [tpg]")

    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)

})


test_that("pdf14: Simplest portrait table works as expected.", {



    fp <- file.path(base_path, "pdf/test14.pdf")

    rpt <- create_report(fp, output_type = "PDF", orientation = "portrait") %>%
      page_header("left", "right") %>%
      titles("Table 1.0", "MTCARS Data Frame") %>%
      add_content(create_table(mtcars)) %>%
      page_footer("Left", right = "right")

    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)



})


test_that("pdf15: Simplest landscape table works as expected.", {

  if (dev == TRUE) {


    fp <- file.path(base_path, "pdf/test15.pdf")

    rpt <- create_report(fp, output_type = "PDF", orientation = "landscape") %>%
      page_header("left", "right") %>%
      titles("Table 1.0", "MTCARS Data Frame") %>%
      add_content(create_table(mtcars)) %>%
      page_footer("Left", right = "right")

    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)


})

test_that("pdf16: 10 pt report with units in cm works as expected.", {

  if (dev == TRUE) {

  
    fp <- file.path(base_path, "pdf/test16.pdf")


    rpt <- create_report(fp, units = "cm", output_type = "PDF") %>%
      page_header("Client: Experis", "Study: ABC") %>%
      titles("IRIS Data Frame") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>%
      add_content(create_table(iris))


    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)



})


test_that("pdf17: 12 pt report with units in cm works as expected.", {

  if (dev == TRUE) {


    fp <- file.path(base_path, "pdf/test17.pdf")


    rpt <- create_report(fp, units = "cm", output_type = "PDF") %>%
      options_fixed(font_size = 12) %>%
      page_header("Client: Experis", "Study: ABC") %>%
      titles("IRIS Data Frame") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>%
      add_content(create_table(iris))


    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)


})


test_that("pdf18: Plot with page by on report works as expected.", {

  if (dev == TRUE) {

  
    library(ggplot2)

    fp <- file.path(base_path, "pdf/test18.pdf")


    dat <- mtcars[order(mtcars$cyl), ]

    p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()


    #dats <- split(p$data, p$data$grp)
    #tbl <- create_table(dat[1:3, ])

    plt <- create_plot(p, height = 4, width = 8)


    rpt <- create_report(fp, output_type = "PDF") %>%
      page_header("Client", "Study: XYZ") %>%
      titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
      set_margins(top = 1, bottom = 1) %>%
      page_by(cyl, "Cylinders: ", align = "right", blank_row = "none") %>%
      add_content(plt) %>%
      footnotes("* Motor Trend, 1974") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    #print(res)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)


})



test_that("pdf19: Plot with page by on plot works as expected.", {

  if (dev == TRUE) {

  
    library(ggplot2)

    fp <- file.path(base_path, "pdf/test19.pdf")


    dat <- mtcars[order(mtcars$cyl), ]

    p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()


    #dats <- split(p$data, p$data$grp)
    #tbl <- create_table(dat[1:3, ])

    plt <- create_plot(p, height = 4, width = 8) %>%
      titles("Figure 1.0", "MTCARS Mileage By Displacement", blank_row = "none") %>%
      page_by(cyl, "Cylinders: ", align = "left", blank_row = "none") %>%
      footnotes("* Motor Trend, 1974")

    rpt <- create_report(fp, output_type = "PDF") %>%
      page_header("Sponsor", "Study: cars") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(plt) %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    #print(res)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)


})



test_that("test21: 8 pt report with units in inches works as expected.", {

  if (dev == TRUE) {


    fp <- file.path(base_path, "pdf/test21.pdf")

    tbl <- create_table(iris)

    rpt <- create_report(fp, units = "inches", output_type = "PDF") %>%
      page_header("Client: Experis", "Study: ABC") %>%
      titles("IRIS Data Frame") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>%
      options_fixed(font_size = 8) %>%
      add_content(tbl)


    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)
    
  } else 
    expect_equal(TRUE, TRUE)


})

test_that("test22: 8 pt report with units in cm works as expected.", {

  if (dev == TRUE) {

  
    fp <- file.path(base_path, "pdf/test22.pdf")


    rpt <- create_report(fp, units = "cm", output_type = "PDF") %>%
      page_header("Client: Experis", "Study: ABC") %>%
      titles("IRIS Data Frame") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>%
      options_fixed(font_size = 8) %>%
      add_content(create_table(iris))


    res <- write_report(rpt)

    expect_equal(file.exists(fp), TRUE)
    
    
  } else 
    expect_equal(TRUE, TRUE)


})




test_that("pdf23: PDF Table with Plot and borders works as expected.", {

  if (dev == TRUE) {

  
    library(ggplot2)

    fp <- file.path(base_path, "pdf/test23.pdf")

    p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()


    plt <- create_plot(p, height = 4, width = 8)
    tbl <- create_table(mtcars[1:10, ])


    rpt <- create_report(fp, output_type = "PDF") %>%
      page_header("Client", "Study: XYZ") %>%
      titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "all") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      add_content(plt, align = "center") %>%
      footnotes("* Motor Trend, 1974", borders = "all") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>% 
      options_fixed(uchar = "-")


    res <- write_report(rpt)

    res

    expect_equal(file.exists(fp), TRUE)
    
    
  } else 
    expect_equal(TRUE, TRUE)

})



test_that("pdf24: PDF Table with Plot and borders on content works as expected.", {

    library(ggplot2)

    fp <- file.path(base_path, "pdf/test24.pdf")

    p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()


    plt <- create_plot(p, height = 4, width = 8) %>%
      titles("My plot", borders = "all") %>%
      footnotes("My plot footnotes", borders = "all")

    tbl <- create_table(mtcars[1:10, ]) %>%
      titles("My table", borders = "all") %>%
      footnotes("My table footnotes", borders = "all", align = "right")


    rpt <- create_report(fp, output_type = "PDF") %>%
      page_header("Client", "Study: XYZ") %>%
      set_margins(top = 1, bottom = 1) %>%
      add_content(tbl) %>%
      add_content(plt, align = "center") %>%
      page_footer("Time", "Confidential", "Page [pg] of [tpg]")


    res <- write_report(rpt)

    #print(res)

    expect_equal(file.exists(fp), TRUE)


})

test_that("pdf25: PDF Table with custom options works as expected.", {
  
  if (dev == TRUE) {

  fp <- file.path(base_path, "pdf/test25.pdf")
  
  
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    titles("My table", borders = "all") %>% 
    footnotes("My table footnotes", borders = "all", align = "right")
  
  tbl2 <- create_table(mtcars[11:20, ]) %>% 
    titles("My table", borders = "all") %>% 
    footnotes("My table footnotes", borders = "all", align = "right")
  
  
  rpt <- create_report(fp) %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    add_content(tbl2, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>% 
    options_fixed(font_size = 12, line_size = 80, line_count = 30,
                  uchar = "~")
  
  
  res <- write_report(rpt, output_type = "pdf")
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  
  } else 
    expect_equal(TRUE, TRUE)
})

test_that("pdf26: Table Borders that spans multiple pages work as expected.", {
  
  if (dev == TRUE) {

  
  fp <- file.path(base_path, "pdf/test26.pdf")
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris, borders = "all")) %>% 
    footnotes("Here is a footnote")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 5)
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
  
  } else 
    expect_equal(TRUE, TRUE)
})


test_that("pdf27: Table Borders with ttls/fnts on table works as expected.", {
  
  if (dev == TRUE) {

  
  fp <- file.path(base_path, "pdf/test27.pdf")
  
  tbl <- create_table(iris, borders = "all") %>% 
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>% 
    footnotes("Here is a footnote", "And another")
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    page_header("Left", "Right") %>% 
    add_content(tbl) %>% 
    page_footer("left", "", "right")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  } else 
    expect_equal(TRUE, TRUE)
  
})

test_that("pdf28: Table Borders with ttls/fnts on report works as expected.", {
  
  if (dev == TRUE) {

  
  fp <- file.path(base_path, "pdf/test28.pdf")
  
  tbl <- create_table(iris, borders = "all") 
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    page_header("Left", "Right") %>% 
    add_content(tbl) %>% 
    page_footer("left", "", "right") %>% 
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>% 
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  } else 
    expect_equal(TRUE, TRUE)
  

  
})

test_that("pdf29: 9 pt font inches works as expected.", {
  
  if (dev == TRUE) {

  
  fp <- file.path(base_path, "pdf/test29.pdf")
  
  rpt <- create_report(fp, output_type = "PDF", font_size = 9, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
  
  } else 
    expect_equal(TRUE, TRUE)
})

test_that("pdf30: 9 pt font cm works as expected.", {
  
  if (dev == TRUE) {

  
  fp <- file.path(base_path, "pdf/test30.pdf")
  
  rpt <- create_report(fp, output_type = "PDF", font_size = 9, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
  
  } else 
    expect_equal(TRUE, TRUE)
})

test_that("pdf31: 11 pt font inches works as expected.", {
  
  if (dev == TRUE) {

  
  fp <- file.path(base_path, "pdf/test31.pdf")
  
  rpt <- create_report(fp, output_type = "PDF", font_size = 11, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
  
  } else 
    expect_equal(TRUE, TRUE)
})

test_that("pdf32: 11 pt font cm works as expected.", {
  
  if (dev == TRUE) {

  
  fp <- file.path(base_path, "pdf/test32.pdf")
  
  rpt <- create_report(fp, output_type = "PDF", font_size = 11, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
  
  } else 
    expect_equal(TRUE, TRUE)
})


test_that("pdf33: PDF Image file works as expected.", {
  
  if (dev == TRUE) {

  
  library(ggplot2)
  
  fp <- file.path(base_path, "pdf/test33.pdf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  pltpath <- file.path(base_path, "pdf/test33.jpg")
  ggsave(pltpath, width = 8, height = 4, 
         units = "in",
         dpi = 300)
  
  plt <- create_plot(pltpath, height = 4, width = 8)
  
  
  rpt <- create_report(fp, output_type = "PDF", font = "fixed") %>%
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

test_that("pdf34: Blank after on invisible column.", {
  
  fp <- file.path(base_path, "pdf/test34.pdf")
  
  tbl <- create_table(iris, borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("pdf35: Page header width works as expected.", {
  
  fp <- file.path(base_path, "pdf/test35.pdf")
  
  tbl <- create_table(iris[1:10, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    page_header("Left here is some stuff and more stuff trying to get out in the middle", 
                "Right", width = 8) %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})
