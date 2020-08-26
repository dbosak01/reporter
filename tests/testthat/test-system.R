
context("System Tests")

base_path <- "c:/packages/rptr/tests/testthat"

base_path <- "."

test_that("test1: Simplest table works as expected.", {
  
  
  fp <- file.path(base_path, "output/test1.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp) %>% 
    add_content(create_table(mtcars[1:10, ]))
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test2: Simplest table with title works as expected.", {
  
  fp <- file.path(base_path, "output/test2.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp) %>% 
    titles("MTCARS Data Frame", align = "left") %>% 
    add_content(tbl)

  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test3: Simple table with formats works as expected.", {
  
  
  fp <- file.path(base_path, "output/test3.out")
  
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
  
  
  afmt <- value(condition(x == "A", "Placebo"),
                condition(x == "B", "Treatment 1"))

  
  sfmt2 <- c(M = "Male", F = "Female")
  
  tbl1 <- create_table(df, first_row_blank = TRUE) %>%
    define(subjid, align = "left") %>% 
    define(sex, width = 1, format = sfmt2) %>%
    define(age, width = .5) %>% 
    define(arm, format = afmt, width = 1.5, align = "right")
  

  
  rpt <- create_report(fp) %>%
    options_fixed(editor = "notepad++") %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", "Safety Population", 
           align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test4: Two page report works as expected.", {
  
  
  fp <- file.path(base_path, "output/test4.out")
  
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
  
  tbl1 <- create_table(df1, first_row_blank = TRUE) %>%
    define(sex, width = 2, format = sfmt1) %>%
    define(age, width = 2)
  
  tbl2 <- create_table(df2) %>%
    define(sex, width = .25, format = sfmt2) %>%
    define(age, format = "%0d%%") %>%
    define(arm, format = afmt, width = 2)
  

  rpt <- create_report(fp, uom = "inches", paper_size = "letter") %>%
    options_fixed(editor = "notepad++") %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) %>%
    add_content(tbl2)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test5: Table with break between sections works as expected.", {
  
  
  fp <- file.path(base_path, "output/test5.out")
  
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


  rpt <- create_report(fp) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  

  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test6: Table that spans multiple pages breaks as expected.", {

  fp <- file.path(base_path, "output/test6.out")

  if (file.exists(fp))
    file.remove(fp)

  rpt <- create_report(fp) %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) 


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)

})

test_that("test7: Table with long cell and label values wraps as expected.", {
  
  
  fp <- file.path(base_path, "output/test7.out")
  
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


  rpt <- create_report(fp) %>%
    titles("Table 1.0", align = "center") %>%

    add_content(tbl1)


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)

})

test_that("test8: Table with spanning headers works as expected.", {
  
  fp <- file.path(base_path, "output/test8.out")
  

  df <- data.frame(vehicle = rownames(mtcars), mtcars)
  rownames(df) = NULL
  
  df$qsec <- fattr(df$qsec, format = "%.1f")
  df$wt <- fattr(df$wt, justify = "center", width = .75)
  
  tbl <- create_table(df) %>% 
    spanning_header(span_cols = c("mpg", "cyl", "disp", "hp"),
                    label = "Span 1", label_align = "center", n = 10) %>% 
    spanning_header(span_cols = c("drat", "wt", "qsec"),
                    label = "Span 2", label_align = "center", n = 10) %>%
    spanning_header(span_cols = c("vs", "am", "gear", "carb"),
                    label = "Span 3", label_align = "center", n = 10) %>%
    spanning_header(span_cols = c(from = "drat", to = "carb"), label = "Super Span",
                    label_align = "center",
                    level = 2) %>%
    define(vehicle, label = "Vehicle") %>% 
    define(mpg, format = "%.1f")
  
  rpt <- create_report(fp) %>% 
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test9: Page wrap works as expected.", {
  
  fp <- file.path(base_path, "output/test9.out")
  
  
  df <- data.frame(vehicle = rownames(mtcars), mtcars)
  rownames(df) = NULL
  
  tbl <- create_table(df) %>% 
    # Need to fix bug when spanning header breaks
    # spanning_header(span_cols = c("mpg", "cyl", "disp", "hp"),
    #                 label = "Span 1", label_align = "center", n = 10) %>% 
    # spanning_header(span_cols = c("drat", "wt", "qsec"),
    #                 label = "Span 2", label_align = "center", n = 10) %>%
    # spanning_header(span_cols = c("vs", "am", "gear", "carb"),
    #                 label = "Span 3", label_align = "center", n = 10) %>%
    # spanning_header(span_cols = c(from = "drat", to = "carb"), label = "Super Span",
    #                 label_align = "center",
    #                 level = 2) %>% 
    define(vehicle, label = "Vehicle", id_var = TRUE) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl) 
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    options_fixed(editor = "wordpad") %>% 
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test")
  
  #print(rpt)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})

test_that("test10: Page wrap with spanning header works as expected.", {
  
  fp <- file.path(base_path, "output/test10.out")
  
  
  df <- data.frame(vehicle = rownames(mtcars), mtcars)
  rownames(df) = NULL
  
  tbl <- create_table(df) %>% 
    spanning_header(span_cols = c("mpg", "cyl", "disp", "hp"),
                    label = "Span 1", label_align = "center", n = 10) %>%
    spanning_header(span_cols = c("drat", "wt", "qsec"),
                    label = "Span 2", label_align = "center", n = 10) %>%
    spanning_header(span_cols = c("vs", "am", "gear", "carb"),
                    label = "Span 3", label_align = "center", n = 10) %>%
    # spanning_header(span_cols = c(from = "drat", to = "carb"), label = "Super Span",
    #                 label_align = "center",
    #                 level = 2) %>%
    define(vehicle, label = "Vehicle", id_var = TRUE) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl) 
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    options_fixed(editor = "wordpad") %>% 
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test")
  
  #print(rpt)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})

test_that("test11: Table with break between sections works as expected.", {
  
  
  fp <- file.path(base_path, "output/test11.out")
  
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
  
  
  rpt <- create_report(fp) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  
  
  res2 <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)

  expect_equal(length(lns), res2$pages * res2$line_count)
  expect_equal(nchar(lns[1]), res2$line_size)
  
})

test_that("test12: Headerless table with title works as expected.", {
  
  fp <- file.path(base_path, "output/test12.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ], headerless = TRUE) %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp) %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test13: Combination with Headerless table works as expected.", {
  
  fp <- file.path(base_path, "output/test13.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl1 <- create_table(mtcars[1:10, ],  headerless = FALSE) %>% 
    titles("MTCARS Combined Table 1.0")
  tbl2 <- create_table(mtcars[11:20, ], headerless = TRUE) 
  tbl3 <- create_table(mtcars, headerless =  TRUE) %>% 
    footnotes("Full cars table") 
  txt1 <- create_text("These tables are combined!")
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    page_footer(center = "Page [pg] of [tpg]") %>% 
    add_content(tbl1, page_break = FALSE, blank_row = "none") %>% 
    add_content(tbl2, page_break = FALSE) %>% 
    add_content(txt1) %>% 
    add_content(tbl1, page_break = FALSE, blank_row = "none") %>% 
    add_content(tbl3, page_break = FALSE) %>% 
    add_content(txt1, page_break = FALSE) # Also test open page at end
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test15: Multi-page table with Titles and Footnotes breaks as expected.", {
  
  fp <- file.path(base_path, "output/test15.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
                
  txt <- create_text(cnt, width = 6.25, align = "left") %>% 
    titles("Introduction to Irises")
  
  tbl <- create_table(iris) %>% 
    titles("My little Iris Table") %>% 
    footnotes("* Better Gardening, 1973") %>% 
    define(Species, blank_after = TRUE, dedupe = TRUE)
  
  rpt <- create_report(fp) %>%
    add_content(txt, page_break = FALSE) %>% 
    add_content(tbl, blank_row = "none")
  
  
  res2 <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res2$pages * res2$line_count)
  
})



test_that("test16: Simple regulatory listing works as expected.", {
  
  fp <- file.path(base_path, "output/test16.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  # Create mtcars listing
  rpt <- create_report(fp, orientation = "portrait") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Listing 1.0", "MTCARS Data Listing") %>% 
    add_content(create_table(mtcars)) %>% 
    footnotes("* Motor Trend, 1973") %>%
    page_footer(left = Sys.time(), 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

