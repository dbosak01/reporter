
context("RTF Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

test_that("rtf1: Simplest table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test1.rtf")
  
  rpt <- create_report(fp, output_type = "RTF") %>% 
    add_content(create_table(mtcars[1:10, ]), align = "left")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

test_that("rtf2: Simplest table with title works as expected.", {
  
  fp <- file.path(base_path, "rtf/test2.rtf")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "RTF") %>% 
    options_fixed(font_size = 10) %>% 
    titles("MTCARS Data Frame", align = "left") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


test_that("rtf3: Table with break between sections works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test3.rtf")
  
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
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
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
  expect_equal(res$pages, 1)
  
})

test_that("rtf4: Table that spans multiple pages breaks as expected.", {
  
  fp <- file.path(base_path, "rtf/test4.rtf")
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 4)
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("rtf5: Table with long cell and label values wraps as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test5.rtf")
  
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
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    titles("Table 1.0", align = "center") %>%
    
    add_content(tbl1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

test_that("rtf6: Table with spanning headers works as expected.", {
  
  fp <- file.path(base_path, "rtf/test6.rtf")
  
  
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
  
  rpt <- create_report(fp, output_type = "RTF") %>% 
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})




test_that("rtf7: Simplest RTF report with 1 in margins works as expected.", {

  fp <- file.path(base_path, "rtf/test7.rtf")

  tbl <- create_table(mtcars[1:10, ]) %>%
    column_defaults(width = .5) %>%
    define(vs, visible = FALSE)

  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study") %>%
    titles("MTCARS Data Frame") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl, align = "left") %>%
    page_footer("Time", right = "Page [pg] of [tpg]")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)


})


test_that("rtf8: Two page RTF report works as expected.", {


  fp <- file.path(base_path, "rtf/test8.rtf")

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


  rpt <- create_report(fp, output_type = "RTF") %>%
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
  expect_equal(res$pages, 2)


})




test_that("rtf9: Simplest RTF Plot works as expected.", {

  library(ggplot2)

  fp <- file.path(base_path, "rtf/test9.rtf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

  plt <- create_plot(p, height = 4, width = 8)


  rpt <- create_report(fp, output_type = "RTF") %>%
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




test_that("rtf10: RTF Table with Plot works as expected.", {

  library(ggplot2)

  fp <- file.path(base_path, "rtf/test10.rtf")

  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()


  plt <- create_plot(p, height = 4, width = 8)
  tbl <- create_table(mtcars[1:10, ])


  rpt <- create_report(fp, output_type = "RTF") %>%
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
  expect_equal(res$pages, 2)


})




test_that("rtf11: RTF Table with Plot on same page works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test11.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8)
  tbl <- create_table(mtcars[1:3, ])
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
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
  expect_equal(res$pages, 1)
  
  
})


test_that("rtf12: Table and Text output works as expected.", {
  
  fp <- file.path(base_path, "rtf/test12.rtf")
  
  tbl1 <- mtcars[1:10, ]
  tbl2 <- mtcars[11:20, ]
  
  rpt <- create_report(fp, orientation = "portrait", output_type = "RTF") %>%
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
  expect_equal(res$pages, 2)

})


test_that("rtf13: Very Long text output works as expected.", {
  
  debug <- TRUE
  
  if (debug) {
    fp <- file.path(base_path, "rtf/test13.rtf")
    
    l <- paste(rep(cnt, 1000), collapse = "\n\n")
    
    rpt <- create_report(fp, orientation = "portrait", output_type = "RTF") %>%
      titles("Report 6.0", "Very long Text Report") %>% 
      page_header(left = "Client: ABC", right = "Study: 123") %>% 
      add_content(create_text(l)) %>% 
      page_footer(left = "Time", 
                  center = "Confidential", 
                  right ="Page [pg] of [tpg]")
    
    res <- write_report(rpt)
    
    expect_equal(file.exists(fp), TRUE)
    expect_equal(res$pages, 123)

    
  } else {
    expect_equal(TRUE, TRUE) 
  }
  
})


test_that("rtf14: Simplest portrait table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test14.rtf")
  
  rpt <- create_report(fp, output_type = "RTF", orientation = "portrait") %>% 
    page_header("left", "right") %>% 
    titles("Table 1.0", "MTCARS Data Frame", borders = "all") %>% 
    add_content(create_table(mtcars)) %>% 
    page_footer("Left", right = "right")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
})


test_that("rtf15: Simplest landscape table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test15.rtf")
  
  rpt <- create_report(fp, output_type = "RTF", orientation = "landscape") %>% 
    page_header("left", "right") %>% 
    titles("Table 1.0", "MTCARS Data Frame", borders = "all") %>% 
    add_content(create_table(mtcars)) %>% 
    page_footer("Left", right = "right")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
})

test_that("test16: 10 pt report with units in cm works as expected.", {
  
  fp <- file.path(base_path, "rtf/test16.rtf")
  
  
  rpt <- create_report(fp, units = "cm", output_type = "RTF") %>%
    page_header("Client: Experis", "Study: ABC") %>% 
    titles("IRIS Data Frame") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>% 
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 4)
  
})


test_that("test17: 12 pt report with units in cm works as expected.", {
  
  fp <- file.path(base_path, "rtf/test17.rtf")
  
  
  rpt <- create_report(fp, units = "cm", output_type = "RTF") %>%
    options_fixed(font_size = 12) %>% 
    page_header("Client: Experis", "Study: ABC") %>% 
    titles("IRIS Data Frame") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>% 
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 5)
  
})



test_that("rtf18: Plot with page by on report works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test18.rtf")


  dat <- mtcars[order(mtcars$cyl), ]
  
  p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()

  
  #dats <- split(p$data, p$data$grp)
  #tbl <- create_table(dat[1:3, ])
  
  plt <- create_plot(p, height = 4, width = 8)
  

  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", 
           blank_row = "none", borders = "all") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_by(cyl, "Cylinders: ") %>% 
    add_content(plt) %>%
    footnotes("* Motor Trend, 1974", borders = "all") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
  
})




test_that("rtf19: Plot with page by on plot works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test19.rtf")
  
  
  dat <- mtcars[order(mtcars$cyl), ]
  
  p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
  
  
  #dats <- split(p$data, p$data$grp)
  #tbl <- create_table(dat[1:3, ])
  
  plt <- create_plot(p, height = 4, width = 8) %>% 
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
    page_by(cyl, "Cylinders: ") %>% 
    footnotes("* Motor Trend, 1974") 
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt) %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
  
})


test_that("test20: Title Header on Plot works as expected.", {
  
  fp <- file.path(base_path, "rtf/test20.rtf")
  
  
  p <- ggplot(mtcars, aes(x=disp, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 5, width = 7) %>% 
    title_header("Figure 1.0", "MTCARS Plot", 
                 right = c("Client", "Page", "More"), borders = "all") %>% 
    footnotes("* Motor Trend, 1974", borders = "all")
  
  rpt <- create_report(fp, units = "inches", output_type = "RTF") %>%
    options_fixed(font_size = 12) %>% 
    add_content(plt) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
})

# 
# test_that("rtf19: Plot and table with page by works as expected.", {
#   
#   library(ggplot2)
#   
#   fp <- file.path(base_path, "rtf/test19.rtf")
#   
#   
#   dat <- mtcars[order(mtcars$cyl), ]
#   
#   p <- ggplot(dat, aes(x=disp, y=mpg)) + geom_point()
#   
#   
#   #dats <- split(p$data, p$data$grp)
#   #tbl <- create_table(dat[1:3, ])
#   
#   plt <- create_plot(p, height = 4, width = 8)
#   
#   
#   
#   rpt <- create_report(fp, output_type = "RTF") %>%
#     page_header("Client", "Study: XYZ") %>%
#     titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", blank_row = "none") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     #page_by(grp, "Mileage Category: ") %>% 
#     add_content(plt) %>%
#     # add_content(tbl) %>%
#     footnotes("* Motor Trend, 1974") %>%
#     page_footer("Time", "Confidential", "Page [pg] of [tpg]")
#   
#   
#   res <- write_report(rpt)
#   
#   #print(res)
#   
#   expect_equal(file.exists(fp), TRUE)
#   
#   
#   
# })


test_that("test21: 8 pt report with units in inches works as expected.", {
  
  fp <- file.path(base_path, "rtf/test21.rtf")
  
  tbl <- create_table(iris) 
  
  rpt <- create_report(fp, units = "inches", output_type = "RTF") %>%
    page_header("Client: Experis", "Study: ABC") %>% 
    titles("IRIS Data Frame") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>% 
    options_fixed(font_size = 8) %>% 
    add_content(tbl) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
})

test_that("test22: 8 pt report with units in cm works as expected.", {
  
  fp <- file.path(base_path, "rtf/test22.rtf")
  
  
  rpt <- create_report(fp, units = "cm", output_type = "RTF") %>%
    page_header("Client: Experis", "Study: ABC") %>% 
    titles("IRIS Data Frame") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]") %>% 
    options_fixed(font_size = 8) %>% 
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 3)
  
})




test_that("rtf23: RTF Table with Plot and borders works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test23.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  
  plt <- create_plot(p, height = 4, width = 8)
  tbl <- create_table(mtcars[1:10, ])
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot", borders = "all") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    add_content(plt, align = "center") %>%
    footnotes("* Motor Trend, 1974", borders = "all") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
  
})



test_that("rtf24: RTF Table with Plot and borders works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test24.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  
  plt <- create_plot(p, height = 4, width = 8) %>% 
    titles("My plot", borders = "all") %>% 
    footnotes("My plot footnotes", borders = "all")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    titles("My table", borders = "all") %>% 
    footnotes("My table footnotes", borders = "all", align = "right")
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(tbl) %>%
    add_content(plt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
})


test_that("rtf25: RTF Table with custom options works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test25.rtf")
  

  
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
                  uchar = "-")
  
  
  res <- write_report(rpt, output_type = "RTF")
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 2)
})

test_that("rtf26: line_size and line_count overrides work as expected.", {
  
  fp <- file.path(base_path, "rtf/test26.rtf")
  
  
  rpt <- create_report(fp) %>%
    options_fixed(line_size = 40, line_count = 30) %>% 
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt, output_type = "RTF")
  res
  
  expect_equal(file.exists(fp), TRUE)
  

  
})


test_that("rtf27: Plot Borders work as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test27.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  
  plt <- create_plot(p, height = 4, width = 8, borders = "all") %>% 
    titles("My plot", borders = "none") %>% 
    footnotes("My plot footnotes", borders = "none")
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "right") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
})

test_that("rtf28: Table Borders that spans multiple pages work as expected.", {
  
  fp <- file.path(base_path, "rtf/test28.rtf")
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris, borders = "all")) %>% 
    footnotes("Here is a footnote")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 4)
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("rtf29: Simplest RTF Plot with valign top works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test29.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8)
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    footnotes("* Motor Trend, 1974", valign = "top") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("rtf30: Simplest RTF Plot with valign bottom works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test30.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  plt <- create_plot(p, height = 4, width = 8) %>%
    footnotes("* Motor Trend, 1974", valign = "bottom")
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(plt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("rtf31: Simplest RTF Text with valign top works as expected.", {
  

  fp <- file.path(base_path, "rtf/test31.rtf")

  txt <- create_text(cnt, width = 6)
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Text 1.0", "MTCARS Miles per Cylinder Text") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(txt, align = "center") %>%
    footnotes("* Motor Trend, 1974", valign = "top") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})

test_that("rtf32: Simplest RTF Text with valign bottom works as expected.", {
  

  fp <- file.path(base_path, "rtf/test32.rtf")
  
  txt <- create_text(cnt, width = 6) %>%
    footnotes("* Motor Trend, 1974", valign = "bottom")
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    page_header("Client", "Study: XYZ") %>%
    titles("Figure 1.0", "MTCARS Miles per Cylinder Plot") %>%
    set_margins(top = 1, bottom = 1) %>%
    add_content(txt, align = "center") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  expect_equal(res$pages, 1)
  
  
})


test_that("rtf33: use_attributes parameter table works as expected.", {
  
  
  fp1 <- file.path(base_path, "rtf/test33a.rtf")
  fp2 <- file.path(base_path, "rtf/test33b.rtf")
  fp3 <- file.path(base_path, "rtf/test33c.rtf")
  
  
  dat <- mtcars[1:10, ]
  attr(dat$mpg, "label") <- "Miles per gallon"
  attr(dat$cyl, "format") <- "%.1f"
  attr(dat$hp, "width") <- 2
  fattr(dat$vs) <- list(width = 2, justify = "center")
  
  tbl <- create_table(dat) 
  
  # Test default 
  rpt <- create_report(fp1, output_type = "RTF", font = "fixed") %>% 
    add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp1), TRUE)
  expect_equal(res$pages, 1)
  
  # Test none
  tbl <- create_table(dat, use_attributes = "none") 
  
  rpt <- create_report(fp2, output_type = "RTF", font = "fixed") %>% 
    add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp2), TRUE)
  
  
  # Test some
  tbl <- create_table(dat, use_attributes = c("format", "width")) 
  
  rpt <- create_report(fp3, output_type = "RTF", font = "fixed") %>% 
    add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp3), TRUE)
  expect_equal(res$pages, 1)
  
})



test_that("rtf34: 9 pt font inches works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test34.rtf")
  
  rpt <- create_report(fp, output_type = "RTF", font_size = 9, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("rtf35: 9 pt font cm works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test35.rtf")
  
  rpt <- create_report(fp, output_type = "RTF", font_size = 9, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("rtf36: 11 pt font inches works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test36.rtf")
  
  rpt <- create_report(fp, output_type = "RTF", font_size = 11, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("rtf37: 11 pt font cm works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test37.rtf")
  
  rpt <- create_report(fp, output_type = "RTF", font_size = 11, 
                       orientation = "portrait") %>%
    page_header("left", "right") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) %>%
    page_footer("left", "center", "Page [pg] of [tpg]") %>% 
    set_margins(top = 1, bottom = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("rtf38: RTF Image file works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "rtf/test38.rtf")
  
  p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()
  
  pltpath <- file.path(base_path, "rtf/test38.jpg")
  ggsave(pltpath, width = 8, height = 4, 
         units = "in",
         dpi = 300)
  
  plt <- create_plot(pltpath, height = 4, width = 8)
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
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
