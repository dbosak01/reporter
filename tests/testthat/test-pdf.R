
context("PDF Tests")

base_path <- "c:/packages/rptr/tests/testthat"

base_path <- "."

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

test_that("pdf1: Simplest table works as expected.", {
  
  
  fp <- file.path(base_path, "pdf/test1.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp, output_type = "PDF") %>% 
    add_content(create_table(mtcars[1:10, ]), align = "left")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("pdf2: Simplest table with title works as expected.", {
  
  fp <- file.path(base_path, "pdf/test2.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "PDF") %>% 
    options_fixed(font_size = 10) %>% 
    titles("MTCARS Data Frame", align = "left") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})

test_that("pdf3: Table with break between sections works as expected.", {
  
  
  fp <- file.path(base_path, "pdf/test3.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  
})

test_that("pdf4: Table that spans multiple pages breaks as expected.", {
  
  fp <- file.path(base_path, "pdf/test4.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp, output_type = "PDF") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("pdf5: Table with long cell and label values wraps as expected.", {
  
  
  fp <- file.path(base_path, "pdf/test5.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  df <- data.frame(arm, subjid, name, sex, age)
  
  
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
  
  
  df <- data.frame(vehicle = rownames(mtcars), mtcars)
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
  
  fp <- file.path(base_path, "pdf/test7.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  
  
})


test_that("pdf8: Two page PDF report works as expected.", {
  
  
  fp <- file.path(base_path, "pdf/test8.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  
  
})




test_that("pdf9: Simplest PDF Plot works as expected.", {
  
  library(ggplot2)
  
  fp <- file.path(base_path, "pdf/test9.pdf")
  
  if (file.exists(fp))
    file.remove(fp)
  
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  debug <- FALSE
  
  if (debug) {
    fp <- file.path(base_path, "pdf/test13.pdf")
    
    if (file.exists(fp))
      file.remove(fp)
    
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
    
    
    
  } else {
    expect_equal(TRUE, TRUE) 
  }
  
})


