
context("System Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

options("logr.output" = FALSE)

test_that("test1: Simplest table works as expected.", {
  
  
  fp <- file.path(base_path, "output/test1.out")
  
  
  rpt <- create_report(fp) %>% 
    add_content(create_table(mtcars[1:10, ]), align = "left")
  
  #print(rpt, verbose = TRUE)
  res <- write_report(rpt)
  
  res
  #print(res, verbose = TRUE)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test2: Simplest table with title works as expected.", {
  
  fp <- file.path(base_path, "output/test2.out")

  
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
    page_footer(left = "Time", center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test4: Two page report works as expected.", {
  
  
  fp <- file.path(base_path, "output/test4.out")
  
  
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
  
  tbl1 <- create_table(df1, first_row_blank = TRUE) %>%
    define(sex, width = 2, format = sfmt1) %>%
    define(age, width = 2)
  
  tbl2 <- create_table(df2) %>%
    define(sex, width = .25, format = sfmt2) %>%
    define(age, format = "%0d%%") %>%
    define(arm, format = afmt, width = 2)
  

  rpt <- create_report(fp, units = "inches", paper_size = "letter") %>%
    options_fixed(editor = "notepad++") %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = "Time", center = "Confidential", 
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

  
  # Setup
  subjid <- 100:109
  name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
            "al-Kazemi, Najwa", "Schaffer, Ashley", "Laner, Tahma", 
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
            "Smith, Shaileigh")
  sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
  age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
  arm <- c(rep("A", 5), rep("B", 3), "A", "A")
  
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


  rpt <- create_report(fp) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = "Time", center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  

  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test6: Table that spans multiple pages breaks as expected.", {

  fp <- file.path(base_path, "output/test6.out")


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
  
  dat <- mtcars[1:10, ]
  df <- data.frame(vehicle = rownames(dat), dat, stringsAsFactors = FALSE)
  
  tbl <- create_table(df) %>% 
    spanning_header(mpg, hp,
                    label = "Span 1", label_align = "center", n = 10) %>%
    spanning_header(drat, qsec,
                    label = "Span 2", label_align = "center", n = 10) %>%
    spanning_header(vs, carb,
                    label = "Span 3", label_align = "center", n = 10) %>%
    spanning_header("drat","carb", label = "Super Span",
                    label_align = "center",
                    level = 2) %>%
    define(vehicle, label = "Vehicle", id_var = TRUE) %>% 
    define(mpg, format = "%.1f") %>% 
    define(am, visible = FALSE) %>% 
    define(vs, page_wrap = TRUE)
  
  rpt <- create_report(fp, orientation = "portrait") %>%
    titles("Table 1.0", "MTCARS Spanning Header") %>% 
    add_content(tbl) 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})

test_that("test10: Page wrap with spanning header works as expected.", {
  
  fp <- file.path(base_path, "output/test10.out")
  
  
  df <- data.frame(vehicle = rownames(mtcars), mtcars, stringsAsFactors = FALSE)
  rownames(df) = NULL
  
  tbl <- create_table(df) %>% 
    spanning_header(2, 5,
                    label = "Span 1", label_align = "center", n = 10) %>%
    spanning_header(6, 8,
                    label = "Span 2", label_align = "center", n = 10) %>%
    spanning_header(9, 12,
                    label = "Span 3", label_align = "center", n = 10) %>%
    spanning_header(6, 12, label = "Super Span",
                    label_align = "center",
                    level = 2) %>%
    define(vehicle, label = "Vehicle", id_var = TRUE) %>% 
    define(mpg, format = "%.1f") %>% 
    define(wt, page_wrap = TRUE) %>% 
    define(vs, page_wrap = TRUE)
  
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
  
  
  rpt <- create_report(fp) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = "Time", center = "Confidential", 
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
  
  # Create mtcars listing
  rpt <- create_report(fp, orientation = "portrait") %>% 
    page_header(left = "Client: Motor Trend", right = c("Study: Cars", 
                                                        "Something else")) %>% 
    titles("Listing 1.0", "MTCARS Data Listing") %>% 
    add_content(create_table(mtcars)) %>% 
    footnotes("* Motor Trend, 1973") %>%
    page_footer(left = "Time", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  rpt
  res <- write_report(rpt)
  
  res
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test17: Simple regulatory table works as expected.", {
  
  library(tidyr)
  library(dplyr)
  
  fp <- file.path(base_path, "output/test17.out")
  
  dat <- mtcars
  
  # Hard coded this so report would come out the same every time.
  #dat$group <- replicate(nrow(dat), sample(c("A", "B"), 1), simplify = TRUE)
  dat$group <- c("B", "B", "B", "B", "B", "B", "A", "A", "B", "A", "A", 
                 "A", "A", "B", "A", "B", "B", "A", "B", "B", "B", "A",
                 "A", "A", "A", "A", "A", "B", "B", "A", "B", "B")
  dat$cyl <- factor(dat$cyl, levels = c(8, 6, 4), 
                    labels = c("8 Cylinder", "6 Cylinder", "4 Cylinder")) 
  group_pop <- table(dat$group)
  
  
  dat_mpg <-
    dat %>%
    group_by(group) %>%
    summarise(across(.cols = mpg,
                     .fns = list(N      = ~ fmt_n(.),
                                 Mean   = ~ fmt_mean_sd(.),
                                 Median = ~ fmt_median(.),
                                 `Q1 - Q3` = ~ fmt_quantile_range(.),
                                 Range  = ~ fmt_range(.)
                     ))) %>%
    pivot_longer(-group,
                 names_to  = c("var", "label"),
                 names_sep = "_",
                 values_to = "value") %>%
    pivot_wider(names_from = group,
                values_from = "value")
  
  
  dat_cyl <-
    dat %>%
    add_count(group, cyl,  name = "n_cyl") %>%
    select(group, cyl, n_cyl) %>%
    distinct() %>%
    pivot_longer(cols = c(cyl),
                 names_to  = "var",
                 values_to = "label") %>%
    pivot_wider(names_from  = group,
                values_from = n_cyl,
                values_fill = 0) %>%
    mutate(A = fmt_cnt_pct(A, group_pop["A"]),
           B = fmt_cnt_pct(B, group_pop["B"])) %>% 
    arrange(label)
  
  
  
  
  final <- bind_rows(dat_mpg, dat_cyl)
  #print(final)
  
  tbl <- create_table(final, first_row_blank = TRUE) %>% 
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(mpg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center") %>% 
    define(B, label = "Group B", align = "center")
  
  
  # Create mtcars table
  rpt <- create_report(fp, orientation = "portrait") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1973") %>%
    page_footer(left = "Time", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test18: Text and table with page breaks works as expected.", {
  
  fp <- file.path(base_path, "output/test18.out")
  
  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
  
  # Create text content
  txt <- create_text(cnt) %>% 
    titles("My Analysis of the previous table")
  
  # Prepare data
  dat <- mtcars
  dat$name <- rownames(dat)
  dat <- mtcars[1:10, ]
  
  # Create table content
  tbl <- create_table(dat) %>% 
    titles("Table 1.0", "MTCARS Sample Data") %>% 
    footnotes("* Motor Trend, 1973")
  
  
  # Create report and add both table and text content
  rpt <- create_report(fp, orientation = "portrait") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    add_content(tbl, page_break = FALSE) %>% 
    add_content(txt) %>% 
    add_content(tbl) %>% 
    add_content(txt) %>% 
    page_footer(left = "Time", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  # Write the report
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test19: show_cols 'none' parameter on table works as expected.", {
  
  fp <- file.path(base_path, "output/test19.out")
  
  tbl <- create_table(mtcars[1:10, ], show_cols = "none") %>%
    define(mpg) %>% 
    define(cyl) %>% 
    define(vs)
  
  rpt <- create_report(fp) %>% 
    titles("MTCARS Data Frame", align = "left") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test20: show_cols 'some' parameter on table works as expected.", {
  
  fp <- file.path(base_path, "output/test20.out")

  tbl <- create_table(mtcars[1:10, ], 
                      show_cols = c("vs", "mpg", "cyl", "disp")) %>% 
    define(mpg, label = "Miles Per Gallon") %>% 
    define(vs, label = "")
  
  
  rpt <- create_report(fp) %>% 
    titles("MTCARS Data Frame", align = "left") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test21: Multiple page headers and footers work as expected.", {
  
  fp <- file.path(base_path, "output/test21.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp) %>% 
    page_header(c("Line 1", "Line 2"), "Right Line") %>%  
    titles("MTCARS Data Frame", align = "left") %>% 
    page_footer(c("Footer Line 1", "Footer Line 2"), right = "Footer Right Line", 
                center = c("A", "B", "C")) %>%  
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test22: Multiple id_var parameters work as expected.", {
  
  fp <- file.path(base_path, "output/test22.out")
  
  dat <- data.frame(vehicle = rownames(mtcars), mtcars, stringsAsFactors = FALSE)
  
  tbl <- create_table(dat) %>%
    define(vehicle, id_var = TRUE, width = 3) %>% 
    define(mpg, id_var = TRUE, width = 2, align = "left") %>% 
    define(vs, id_var = TRUE, width = 2, align = "left")
  
  rpt <- create_report(fp) %>% 
    options_fixed(editor = "word") %>% 
    page_header(c("Line 1", "Line 2"), "Right Line") %>%  
    titles("MTCARS Data Frame", align = "left") %>% 
    page_footer(c("Footer Line 1", "Footer Line 2"), 
                right = "Footer Right Line") %>%  
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)


})

test_that("test23: Blank margins setting works as expected.", {
  
  # Compare output to test1
  
  fp <- file.path(base_path, "output/test23.out")
  
  rpt <- create_report(fp, output_type = "TXT") %>% 
    options_fixed( blank_margins = TRUE) %>% 
    set_margins(top = 1) %>% 
    add_content(create_table(mtcars[1:10, ]), align = "left")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), (res$pages * res$line_count) + res$blank_margin_top)
  
})


test_that("test24: Table width parameter works as expected for full width.", {
  
  
  fp <- file.path(base_path, "output/test24.out")
  
  tbl1 <- create_table(mtcars[1:10, 1:6], width = 9) 
  tbl2 <- create_table(mtcars[11:20, 1:6]) 
  
  rpt <- create_report(fp) %>% 
    page_header("Client", "Study") %>% 
    add_content(tbl1, align = "left", page_break = FALSE) %>% 
    add_content(tbl2, align = "left")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), 1 * res$line_count)
  
})

test_that("test25: page_break parameter simple case works as expected.", {
  
  
  fp <- file.path(base_path, "output/test25.out")
  
  dat <- mtcars
  rownames(dat) <- NULL
  
  dat$pg <- c(rep(1, 16), rep(2, 16))
  
  tbl <- create_table(dat) %>%
    define(pg, page_break = TRUE)
  
  rpt <- create_report(fp) %>% 
    page_header("Client", "Study") %>% 
    titles("MTCARS Sample Report") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), 2 * res$line_count)
  
})


test_that("test26: page_break parameter harder case works as expected.", {
  
  
  fp <- file.path(base_path, "output/test26.out")
 
  dat <- iris

  #dat$pg <- c(rep(1, 16), rep(2, 16))
  
  tbl <- create_table(dat) %>%
    define(Species, page_break = TRUE)
  
  rpt <- create_report(fp) %>% 
    page_header("Client", "Study") %>% 
    titles("IRIS Sample Report") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages  * res$line_count)
  
})



test_that("test27: page_break parameter even harder case works as expected.", {
  
  
  fp <- file.path(base_path, "output/test27.out")
  
  dat <- iris
  
  
  dat$pg <- c(rep(1, 20), rep(2, 20), rep(3, 20), rep(4, 20), rep(5, 10),
              rep(6, 20), rep(7, 20), rep(8, 20))
  
  tbl <- create_table(dat) %>%
    define(Species, blank_after = TRUE) %>% 
    define(pg, page_break = TRUE)
  
  rpt <- create_report(fp) %>% 
    page_header("Client", "Study") %>% 
    titles("IRIS Sample Report") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages  * res$line_count)
  
})



test_that("test28: use_attributes parameter table works as expected.", {
  
  
  fp1 <- file.path(base_path, "output/test28a.out")
  fp2 <- file.path(base_path, "output/test28b.out")
  fp3 <- file.path(base_path, "output/test28c.out")
  
  
  dat <- mtcars[1:10, ]
  attr(dat$mpg, "label") <- "Miles per gallon"
  attr(dat$cyl, "format") <- "%.1f"
  attr(dat$hp, "width") <- 2
  fattr(dat$vs) <- list(width = 2, justify = "center")
  
  tbl <- create_table(dat) 
  
  # Test default 
  rpt <- create_report(fp1) %>% 
    add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp1), TRUE)
  
  lns <- readLines(fp1)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
  # Test none
  tbl <- create_table(dat, use_attributes = "none") 
  
  rpt <- create_report(fp2) %>% 
    add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp2), TRUE)
  
  lns <- readLines(fp2)
  
  # Test some
  tbl <- create_table(dat, use_attributes = c("format", "width")) 
  
  rpt <- create_report(fp3) %>% 
    add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp3), TRUE)
  
  lns <- readLines(fp3)

  expect_equal(length(lns), res$pages * res$line_count)
  
})




test_that("test29: column_defaults work as expected.", {
  
  
  fp <- file.path(base_path, "output/test29.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    column_defaults(width = .5, align = "right", format = "%.1f",
                    n = 5) %>% 
    define(mpg, width = 2, format = "%.2f", align = "left",
           label_align = "right") %>% 
    define(wt, width = 2, format = "%.4f", align = "left", n = 6,
           label_align = "center")
  
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "left")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test30: multiple vars on define work as expected.", {


  fp <- file.path(base_path, "output/test30.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>%
    define(c(mpg, wt), width = 2, format = "%.2f", align = "left",
           label_align = "right")


  rpt <- create_report(fp) %>%
    add_content(tbl, align = "left")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  lns <- readLines(fp)

  expect_equal(length(lns), res$pages * res$line_count)

})





test_that("test31: Table width parameter works for less than full width.", {
  
  
  fp <- file.path(base_path, "output/test31.out")
  
  tbl1 <- create_table(mtcars[1:10, 1:6], width = 7) 
  tbl2 <- create_table(mtcars[11:20, 1:6]) 
  
  rpt <- create_report(fp) %>% 
    page_header("Client", "Study") %>% 
    add_content(tbl1, align = "left", page_break = FALSE) %>% 
    add_content(tbl2, align = "left")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), 1 * res$line_count)
  
})


test_that("test36: Report with NAs in data works as expected.", {
  
  
  fp <- file.path(base_path, "output/test36.txt")
  
  # Setup
  subjid <- 100:109
  name <- c("Quintana, Gabriel", NA, "Minniear, Presley",
            "al-Kazemi, Najwa", "Schaffer, Ashley", "Laner, Tahma", 
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
            "Smith, Shaileigh")
  sex <- c("M", "F", "F", "M", "M", NA, "M", "F", "F", "M")
  age <- c(41, 53, 43, NA, 47, 52, 21, 38, 62, 26)
  arm <- c(rep("A", 5), rep("B", 4), NA)
  
  # Create data frame
  df <- data.frame(subjid, name, sex, age, arm, stringsAsFactors = FALSE)
  
  
  afmt <- value(condition(x == "A", "Placebo"),
                condition(x == "B", "Treatment 1"))
  
  sfmt1 <- value(condition(x == "M", "Male"),
                 condition(x == "F", "Female"))
  
  
  tbl1 <- create_table(df, width = 7) %>%
    define(sex, width = 1, format = sfmt1) %>%
    define(name, width = 2) %>% 
    define(age)

  
  
  rpt <- create_report(fp, output_type = "TXT", missing = "-") %>%
    options_fixed(font_size = 12) %>% 
    set_margins(top = 1, bottom = 1) %>% 
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = "Time", center = "Confidential", 
                right = "Page [pg] of [tpg]") %>%
    add_content(tbl1) 
  
  
  res <- write_report(rpt)
  
  #print(res)
  
  expect_equal(file.exists(fp), TRUE)
  
  
  
})

test_that("test37: line_size and line_count overrides work as expected.", {
  
  fp <- file.path(base_path, "output/test37.out")
  
  
  rpt <- create_report(fp) %>%
    options_fixed(line_size = 40, line_count = 30) %>% 
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  res
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test38: column_defaults works as expected with multiple tables.", {
  
  fp <- file.path(base_path, "output/test38.out")
  
  tbl1 <- create_table(mtcars[1:5, ]) %>% 
    column_defaults(width = .5) 
  
  tbl2 <- create_table(mtcars[6:10, ], headerless=TRUE) %>% 
    column_defaults(width = .5) 
  
  # Create the report object
  rpt <- create_report(fp) %>%
    titles("MTCARS Sample Data", align = "left") %>%
    add_content(tbl1, page_break = FALSE, align = "left", blank_row = "none") %>%
    add_content(tbl2, page_break = FALSE, align = "left") %>%
    add_content(create_text("* NOTE: Above table is actually two tables stacked."))
  
  # Write the report to the file system
  res <- write_report(rpt)
  res
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)

})



test_that("test39: column_defaults works as expected with column positions.", {
  
  fp <- file.path(base_path, "output/test39.out")
  
  tbl1 <- create_table(mtcars[1:5, ]) %>% 
    column_defaults(vars = 1:5, width = .5) %>% 
    column_defaults(6:11, width = .6)
  
  
  # Create the report object
  rpt <- create_report(fp) %>%
    titles("MTCARS Sample Data", align = "left") %>%
    add_content(tbl1, align = "left", blank_row = "none")
  
  # Write the report to the file system
  res <- write_report(rpt)
  res
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


# 
# test_that("test28: Table width parameter more than sum of columns works.", {
#   
#   
#   fp <- file.path(base_path, "output/test28.out")
#   
#   if (file.exists(fp))
#     file.remove(fp)
#   
#   tbl1 <- create_table(mtcars[1:10, ], width = 9) %>% 
#     define(mpg, width = 2) %>% 
#     define(cyl, width = 2) %>% 
#     define(disp, width = 2) %>% 
#     define(hp, width = 2) %>% 
#     define(am, width = 2) %>% 
#     define(carb, width = 2) 
#   
#   rpt <- create_report(fp) %>% 
#     page_header("Client", "Study") %>% 
#     add_content(tbl1, align = "left") 
# 
#   
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   
#   lns <- readLines(fp)
#   
#   expect_equal(length(lns), res$pages * res$line_count)
#   
# })


test_that("test40: Report with units in cm works as expected.", {
  
  fp <- file.path(base_path, "output/test40.out")
  
  
  rpt <- create_report(fp, units = "cm") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test41: Title header on report works as expected.", {
  
  fp <- file.path(base_path, "output/test41.out")
  
  
  rpt <- create_report(fp) %>%
    title_header("Table 1.0", "IRIS Data Frame", 
                 right = c("Study ABC", "Client A", "Page [pg] of [tpg]")) %>%
    add_content(create_table(iris)) %>% 
    page_footer("DateTime", right = "Page")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test42: Title header on table works as expected.", {
  
  fp <- file.path(base_path, "output/test42.out")
  
  tbl <- create_table(iris) %>% 
    title_header("Table 1.0", "IRIS Data Frame",
                 right = c("Study ABC", "Client A", "Page [pg] of [tpg]"),
                 blank_row = "below")
  
  rpt <- create_report(fp) %>%
    add_content(tbl) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test43: Page by on table works as expected.", {
  
  fp <- file.path(base_path, "output/test43.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "IRIS Data Frame") %>% 
    page_by(Species, "Species: ", align = "left") %>% 
    define(Species, visible = FALSE) %>% 
    footnotes("Here is a footnote")
  
  rpt <- create_report(fp) %>%
    page_header("Client", "Study") %>%
    add_content(tbl) %>% 
    page_footer("Time", right = "Page [pg] of [tpg]") 
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test44: Page by on report works as expected.", {
  
  fp <- file.path(base_path, "output/test44.out")
  
  tbl <- create_table(iris) %>% 
    define(Species, visible = FALSE)
  
  rpt <- create_report(fp) %>% 
    page_header("Client", "Study") %>% 
    titles("Table 1.0", "IRIS Data Frame") %>% 
    page_by(Species, "Species: ") %>% 
    footnotes("Here is a footnote") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]") %>% 
    add_content(tbl) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test45: Report without page by works as expected.", {
  
  fp <- file.path(base_path, "output/test45.out")
  
  tbl <- create_table(iris) %>% 
    define(Species, page_break = TRUE)
  
  rpt <- create_report(fp, orientation = "portrait", units = "inches") %>% 
    options_fixed(editor = "word", uchar = "-") %>% 
    set_margins(top = 1, bottom = 1, left = 1, right = 1) %>% 
    # 2.54 1.27
    page_header("Client", "Study") %>% 
    titles("Table 1.0", "IRIS Data Frame") %>% 
    footnotes("Here is a footnote") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]") %>% 
    add_content(tbl) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test46: Another title_header test to check alignment.", {
  
  
  # Create a temporary file
  fp <- file.path(base_path, "output/test46.out")

  # Prepare data
  dat <- data.frame(category = rownames(USPersonalExpenditure),
                    USPersonalExpenditure, stringsAsFactors = FALSE)

  # Define table
  tbl <- create_table(dat) %>%
    title_header("Table 1.0", "US Personal Expenditures from 1940 - 1960",
                 right = c("Page [pg] of [tpg]", "World Almanac")) %>%
    column_defaults(from = X1940, to = X1960, format = "$%.2f") %>%
    define(category, label = "Category", width = 2) %>%
    define(X1940, label = "1940") %>%
    define(X1945, label = "1945") %>%
    define(X1950, label = "1950") %>%
    define(X1955, label = "1955") %>%
    define(X1960, label = "1960") %>%
    footnotes("* In billions of dollars")

  # Define report
  rpt <- create_report(fp, orientation="portrait") %>%
    add_content(tbl)

  # Write the report
  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

# Alignment is off on this one.
test_that("test47: CM Table with long cell and label values wraps as expected.", {
  
  
  fp <- file.path(base_path, "output/test47.out")
  
  
  # Setup
  arm <- c(rep("A", 5), rep("B", 5))
  subjid <- 100:109
  name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
            "al-Kazemi, Najwa and more and more and more and more and more and more", 
            "Schaffer, Ashley", "Laner, Tahma", 
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
            "Smith, Shaileigh")
  sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
  age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
  
  
  # Create data frame
  df <- data.frame(arm, subjid, name, sex, age, stringsAsFactors = FALSE)
  
  
  tbl1 <- create_table(df, first_row_blank = TRUE) %>%
    define(subjid, label = "Subject ID for a patient", n = 10, align = "left", 
           width = 2) %>%
    define(name, label = "Subject Name") %>%
    define(sex, label = "Sex", n = 10, align = "center") %>%
    define(age, label = "Age", n = 10) %>%
    define(arm, label = "Arm",
           blank_after = TRUE,
           dedupe = TRUE)
  
  
  rpt <- create_report(fp, units = "cm") %>%
    page_header("Sponsor", "Study") %>% 
    titles("Table 1.0", align = "right") %>%
    add_content(tbl1, align = "right") %>% 
    page_footer("Time", "Confidential", "Page")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test48: Three level stub works as expected.", {
  
  
  fp <- file.path(base_path, "output/test48.out")
  
  
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
    stub(c(cat, grp, ci), "Estimates", width = 2.5) %>% 
    define(cat, label_row = TRUE, blank_after = TRUE) %>%
    define(grp, indent = .25) %>%
    define(ci, indent = .5) %>%
    define(values, label = "Values")

  rpt <- create_report(fp) %>%
    titles("Table 3.0", "Analysis of Time to Initial PSGA Success in Weeks") %>% 
    page_header("Sponsor", "Study") %>% 
    add_content(tbl1) %>% 
    page_footer("Time", "Confidential", "Page")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test49: Stub and width settings works as expected.", {
  
  
  fp <- file.path(base_path, "output/test49.out")
  
  
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
  values1 <- c(41, 53, 43, 39, 47, 52, 38, 25, 37, 23, 78, 21)
  values2 <- c(32, 28, 94, 32, 09, 42, 67, 65, 56, 12, 91, 32)
  values3 <- c(41, 53, 43, 39, 47, 52, 38, 25, 37, 23, 78, 21)
  values4 <- c(32, 28, 94, 32, 09, 42, 67, 65, 56, 12, 91, 32)
  
  # Create data frame
  df <- data.frame(cat, grp, ci, values1, values2, values3, values4, 
                   stringsAsFactors = FALSE)
  
  tbl1 <- create_table(df, width = 9) %>%
    stub(c(cat, grp, ci), "Estimates", width = 3) %>% 
    define(cat, label_row = TRUE, blank_after = TRUE) %>%
    define(grp, indent = .25) %>%
    define(ci, indent = .5) %>%
    define(values1, label = "Values1", width = .70) %>% 
    define(values2, label = "Values2") %>% 
    define(values3, label = "Values3") %>% 
    define(values4, label = "Values4") 
  
  rpt <- create_report(fp) %>%
    titles("Table 3.0", "Analysis of Time to Initial PSGA Success in Weeks") %>% 
    page_header("Sponsor", "Study") %>% 
    add_content(tbl1) %>% 
    page_footer("Time", "Confidential", "Page")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

# This is still messed up, but not going to fix it for now.
test_that("test50: Table with long numeric values sizes as expected.", {
  
  fp <- file.path(base_path, "output/test50.out")
  
  # Setup
  arm <- c(rep("A", 5), rep("B", 5))
  subjid <- 100:109
  name <- c("Quintana, Gabriel", "Allison, Blas", "Minniear, Presley",
            "al-Kazemi, Najwa and more and more", "Schaffer, Ashley", "Laner, Tahma", 
            "Perry, Sean", "Crews, Deshawn Joseph", "Person, Ladon", 
            "Smith, Shaileigh")
  sex <- c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M")
  age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
  rnd <- c(0.7077520, 0.3180838, 0.9174493, 0.4395207, 0.5307518, 
           0.8501806, 0.4584808, 0.1016370, 0.7061505, 0.7819369)
  #rnd <- runif(10)
  calc <- age * rnd
  
  # Create data frame
  df <- data.frame(arm, subjid, name, sex, age, rnd, calc, 
                   stringsAsFactors = FALSE)
  
  
  tbl1 <- create_table(df, first_row_blank = TRUE) %>%
    define(subjid, label = "Subject ID for a patient", n = 10) %>%
    define(name, label = "Subject Name", width = 1) %>%
    define(sex, label = "Sex", n = 10, align = "left") %>%
    define(age, label = "Age", n = 10) %>%
    define(arm, label = "Arm",
           blank_after = TRUE,
           dedupe = TRUE) %>% 
    define(rnd, width = .5) %>% 
    define(calc, width = .6)
  
  
  rpt <- create_report(fp) %>%
    titles("Table 1.0", align = "center") %>%
    
    add_content(tbl1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test51: Page break on invisible columns works as expected.", {
  
  fp <- file.path(base_path, "output/test51.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    define(Species, visible = FALSE, page_break = TRUE) 

   
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test52: Column default values for label and n work as expected.", {
  
  fp <- file.path(base_path, "output/test52.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    column_defaults(label = "Data", n = "100") %>% 
    define("Species", label = "Species")
  
  
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test53: Multiple Column defaults work as expected for quoted vars.", {
  
  fp <- file.path(base_path, "output/test53.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    column_defaults(vars = c("Sepal.Length", "Petal.Length"), label = "Length") %>%
    column_defaults(vars = c("Sepal.Width", "Petal.Width"), label = "Width") %>%
    define("Species", label = "Species")
  
  
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test54: Multiple Column defaults work as expected for positions.", {
  
  fp <- file.path(base_path, "output/test54.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    column_defaults(vars = c(1, 3), label = "Length") %>% 
    column_defaults(vars = c(2, 4), label = "Width") %>% 
    define("Species", label = "Species")
  
  
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test55: Multiple Column defaults work as expected for unquoted vars.", {
  
  fp <- file.path(base_path, "output/test55.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    column_defaults(vars = c(Sepal.Length, Petal.Length), label = "Length") %>%
    column_defaults(vars = c(Sepal.Width, Petal.Width), label = "Width") %>%
    define("Species", label = "Species")
  
  
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test56: Multiple Column defaults work as expected for quoted from/to", {
  
  fp <- file.path(base_path, "output/test56.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    column_defaults(from = "Sepal.Length", to = "Sepal.Width", label = "Sepal") %>%
    column_defaults(from = "Petal.Length", to = "Petal.Width", label = "Petal") %>%
    define("Species", label = "Species")
  
  
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test57: Multiple Column defaults work as expected for unquoted from/to", {
  
  fp <- file.path(base_path, "output/test57.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    column_defaults(from = Sepal.Length, to = Sepal.Width, label = "Sepal") %>%
    column_defaults(from = Petal.Length, to = Petal.Width, label = "Petal") %>%
    define("Species", label = "Species")
  
  
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test58: Multiple Column defaults work as expected for positional from/to", {
  
  fp <- file.path(base_path, "output/test58.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "My Irises Data Sample") %>% 
    column_defaults(from = 1, to = 2, label = "Sepal") %>%
    column_defaults(from = 3, to = 4, label = "Petal") %>%
    define("Species", label = "Species")
  
  
  
  rpt <- create_report(fp) %>%
    add_content(tbl) %>% 
    page_header("Client", "Study") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test59: Formatting Attributes on data frame work as expected.", {
  
  fp <- file.path(base_path, "output/test59.out")
  
  
  dat <- mtcars
  
  attr(dat$mpg, "label") <- "Miles per gallon"
  dat$mpg <- fattr(dat$mpg, format = "%.2f", width = 2, justify = "left")
  attr(dat$cyl, "label") <- "Cylinders"
  dat$cyl <- fattr(dat$cyl, format = "%.1f", width = 1, justify = "center")
  attr(dat$disp, "label") <- "Displacement"
  dat$disp <- fattr(dat$disp, format = "%.2f", width = 1, justify = "right")
  
  tbl <- create_table(dat, show_cols = c(1:3))
  
  rpt <- create_report(fp) %>% add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test60: Attributes on data frame are overridden by column defaults.", {
  
  fp <- file.path(base_path, "output/test60.out")
  
  
  dat <- mtcars
  
  attr(dat$mpg, "label") <- "Miles per gallon"
  dat$mpg <- fattr(dat$mpg, format = "%.2f", width = 2, justify = "left")
  attr(dat$cyl, "label") <- "Cylinders"
  dat$cyl <- fattr(dat$cyl, format = "%.1f", width = 1, justify = "center")
  attr(dat$disp, "label") <- "Displacement"
  dat$disp <- fattr(dat$disp, format = "%.2f", width = 1, justify = "right")
  
  tbl <- create_table(dat, show_cols = c(1:3)) %>% 
    column_defaults(width = .75, align = "left", label = "Data", format = "%.0f")
  
  rpt <- create_report(fp) %>% add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test61: Parameters on column defaults are overridden by define vector.", {
  
  fp <- file.path(base_path, "output/test61.out")
  
  
  dat <- mtcars
  
  attr(dat$mpg, "label") <- "Miles per gallon"
  dat$mpg <- fattr(dat$mpg, format = "%.2f", width = 2, justify = "left")
  attr(dat$cyl, "label") <- "Cylinders"
  dat$cyl <- fattr(dat$cyl, format = "%.1f", width = 1, justify = "center")
  attr(dat$disp, "label") <- "Displacement"
  dat$disp <- fattr(dat$disp, format = "%.2f", width = 1, justify = "right")
  
  tbl <- create_table(dat, show_cols = c(1:3)) %>% 
    column_defaults(width = .75, align = "left", label = "Data",
                    format = "%.0f") %>% 
    define(c(mpg, cyl, disp), label = "Data1", width = 2, 
           align = "right", format = "%.2f")
  
  rpt <- create_report(fp) %>% add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test62: Parameters on column defaults are overridden by single defines.", {
  
  fp <- file.path(base_path, "output/test62.out")
  
  
  dat <- mtcars
  
  attr(dat$mpg, "label") <- "Miles per gallon"
  dat$mpg <- fattr(dat$mpg, format = "%.2f", width = 2, justify = "left")
  attr(dat$cyl, "label") <- "Cylinders"
  dat$cyl <- fattr(dat$cyl, format = "%.1f", width = 1, justify = "center")
  attr(dat$disp, "label") <- "Displacement"
  dat$disp <- fattr(dat$disp, format = "%.2f", width = 1, justify = "right")
  
  tbl <- create_table(dat, show_cols = c(1:3)) %>% 
    column_defaults(width = .75, align = "left", label = "Data",
                    format = "%.0f") %>% 
    define(mpg, label = "Data1", width = 2, 
           align = "right", format = "%.4f") %>% 
    define(cyl, label = "Data2", width = 1, 
           align = "left", format = "%.3f") %>% 
    define(disp, label = "Data3", width = .75, 
           align = "center", format = "%.1f")
  
  rpt <- create_report(fp) %>% add_content(tbl)
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})



test_that("test63: Simple regulatory table with borders works as expected.", {
  
  library(tidyr)
  library(dplyr)
  
  fp <- file.path(base_path, "output/test63.out")
  
  dat <- mtcars
  
  # Hard coded this so report would come out the same every time.
  #dat$group <- replicate(nrow(dat), sample(c("A", "B"), 1), simplify = TRUE)
  dat$group <- c("B", "B", "B", "B", "B", "B", "A", "A", "B", "A", "A", 
                 "A", "A", "B", "A", "B", "B", "A", "B", "B", "B", "A",
                 "A", "A", "A", "A", "A", "B", "B", "A", "B", "B")
  dat$cyl <- factor(dat$cyl, levels = c(8, 6, 4), 
                    labels = c("8 Cylinder", "6 Cylinder", "4 Cylinder")) 
  group_pop <- table(dat$group)
  
  
  dat_mpg <-
    dat %>%
    group_by(group) %>%
    summarise(across(.cols = mpg,
                     .fns = list(N      = ~ fmt_n(.),
                                 Mean   = ~ fmt_mean_sd(.),
                                 Median = ~ fmt_median(.),
                                 `Q1 - Q3` = ~ fmt_quantile_range(.),
                                 Range  = ~ fmt_range(.)
                     ))) %>%
    pivot_longer(-group,
                 names_to  = c("var", "label"),
                 names_sep = "_",
                 values_to = "value") %>%
    pivot_wider(names_from = group,
                values_from = "value")
  
  
  dat_cyl <-
    dat %>%
    add_count(group, cyl,  name = "n_cyl") %>%
    select(group, cyl, n_cyl) %>%
    distinct() %>%
    pivot_longer(cols = c(cyl),
                 names_to  = "var",
                 values_to = "label") %>%
    pivot_wider(names_from  = group,
                values_from = n_cyl,
                values_fill = 0) %>%
    mutate(A = fmt_cnt_pct(A, group_pop["A"]),
           B = fmt_cnt_pct(B, group_pop["B"])) %>% 
    arrange(label)
  
  
  
  
  final <- bind_rows(dat_mpg, dat_cyl)
  #print(final)
  
  tbl <- create_table(final, first_row_blank = TRUE) %>% 
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(mpg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", align = "center") %>% 
    define(B, label = "Group B", align = "center")
  
  
  # Create mtcars table
  rpt <- create_report(fp, orientation = "portrait") %>% 
    page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
    titles("Table 1.0", "MTCARS Summary Table", borders = "all") %>% 
    add_content(tbl) %>% 
    footnotes("* Motor Trend, 1973", borders = "all") %>%
    page_footer(left = "Time", 
                center = "Confidential", 
                right = "Page [pg] of [tpg]")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test64: Title header with border on report works as expected.", {
  
  fp <- file.path(base_path, "output/test64.out")
  
  
  rpt <- create_report(fp) %>%
    title_header("Table 1.0", "IRIS Data Frame", 
                 right = c("Study ABC", "Client A", "Page [pg] of [tpg]"),
                 borders = "all") %>%
    add_content(create_table(iris)) %>% 
    page_footer("DateTime", right = "Page")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test65: Title header with borders on table works as expected.", {
  
  fp <- file.path(base_path, "output/test65.out")
  
  tbl <- create_table(iris) %>% 
    title_header("Table 1.0", "IRIS Data Frame",
                 right = c("Study ABC", "Client A", "Page [pg] of [tpg]"),
                 blank_row = "below", borders = "all")
  
  rpt <- create_report(fp) %>%
    add_content(tbl) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test66: Titles and footnotes with border on table works as expected.", {
  
  fp <- file.path(base_path, "output/test66.out")
  
  tbl <- create_table(iris) %>% 
    titles("Table 1.0", "IRIS Data Frame",
                 blank_row = "below", borders = "all") %>% 
    footnotes("Here is a footnote", "And another",
              borders = "all")
  
  rpt <- create_report(fp) %>%
    page_header("Left", "Right") %>% 
    add_content(tbl) %>% 
    page_footer("left", "", "right")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test67: Title header with border on report works as expected.", {
  
  fp <- file.path(base_path, "output/test67.out")
  
  
  rpt <- create_report(fp) %>%
    page_header("Left", "Right") %>% 
    titles("Table 1.0", "IRIS Data Frame",
                 borders = "all") %>%
    add_content(create_table(iris)) %>% 
    footnotes("Something", borders = "all") %>% 
    page_footer("DateTime", right = "Page")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})




test_that("test68: show_cols 'some' parameter and label works as expected.", {
  
  fp <- file.path(base_path, "output/test68.out")
  
  mycols <- c("vs", "mpg", "cyl", "disp")
  myvar1 = "vs"
  myvar2 = "mpg"
  tbl <- create_table(mtcars[1:10, ], 
                      show_cols = mycols, use_attributes = "none") %>% 
    define({{myvar2}}, label = "n", width = 1.5) %>% 
    define({{myvar1}}, label = "(%)", width = 1.5)
  
  
  rpt <- create_report(fp, missing = NULL) %>% 
    titles("MTCARS Data Frame", align = "center") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test69: Table with spanning headers no underline works as expected.", {
  
  fp <- file.path(base_path, "output/test69.out")
  
  
  df <- data.frame(vehicle = rownames(mtcars), mtcars, stringsAsFactors = FALSE)
  rownames(df) = NULL
  
  df$qsec <- fattr(df$qsec, format = "%.1f")
  df$wt <- fattr(df$wt, justify = "center", width = .75)
  
  tbl <- create_table(df) %>% 
    spanning_header("mpg", "hp",
                    label = "Span 1", label_align = "center", n = 10) %>% 
    spanning_header("drat", "qsec",
                    label = "Span 2", label_align = "center", n = 10, 
                    underline = FALSE) %>%
    spanning_header("vs", "carb",
                    label = "Span 3", label_align = "center", n = 10) %>%
    spanning_header(from = "drat", to = "carb", label = "Super Span",
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

test_that("test70: Column widths work as expected.", {
  
  
  fp <- file.path(base_path, "output/test70.out")
  
  dat <- unlist(rep("AAAAAAAAA B", 10))
  
  df <- data.frame(col1 = dat, col2 = dat, col3 = dat, col4 = dat, col5 = dat,
                   col6 = dat, col7 = dat, col8 = dat, col9 = dat, 
                   stringsAsFactors = FALSE)
  
  tbl <- create_table(df) %>% 
    spanning_header(1, 2, "Span 1") %>% 
    spanning_header(3, 5, "Span 2") %>% 
    spanning_header(1, 5, "Super Span", level = 2) %>% 
    column_defaults(width = 1) %>% 
    footnotes("Here is a really long footnote that will need a few columns to contain.")
  
  rpt <- create_report(fp, output_type = "TXT") %>% 
    set_margins(left = 1, right = 1) %>% 
    add_content(tbl, align = "left") 
  

  res <- write_report(rpt)
  
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  expect_equal(res$pages, 1)
})


test_that("test71: Column widths work as expected with fractional widths.", {
  
  
  fp <- file.path(base_path, "output/test71.out")
  
  dat <- unlist(rep("AAA", 10))
  
  df <- data.frame(col1 = dat, col2 = dat, col3 = dat, col4 = dat, col5 = dat,
                   col6 = dat, col7 = dat, col8 = dat, col9 = dat, 
                   stringsAsFactors = FALSE)
  
  tbl <- create_table(df) %>% 
    spanning_header(1, 2, "Span 1") %>% 
    spanning_header(3, 5, "Span 2") %>% 
    spanning_header(1, 5, "Super Span", level = 2) %>% 
    column_defaults(width = .5) %>% 
    define(col1, width = 3.5) %>% 
    define(col2, width = 1) %>% 
    define(col7, width = .7) %>%
    define(col8, width = 1.3) %>% 
    footnotes("Here is a really long footnote that will need a few columns to contain.")
  
  rpt <- create_report(fp, output_type = "TXT") %>% 
    set_margins(left = 1, right = 1) %>% 
    add_content(tbl, align = "left")
  
  
  res <- write_report(rpt)
  
  
  # print(res, verbose = TRUE)

  lns <- readLines(fp)

  expect_equal(length(lns), res$pages * res$line_count)
  expect_equal(res$pages, 1)
})


test_that("test72: Check column alignment", {
  
  
  fp <- file.path(base_path, "output/test72.out")
  
  # Sample data
  df <- read.table(header = TRUE, text = '
  Country         N1   Mean1   SD1   N2   Mean2    SD2 
  Bangladesh      52   25.03   0.02  43   22.31   0.08
  Germany         42   95.01   1.02  53   9.31    0.09
  Italy            2   20.22   0.00  11   8.09    1.11')
  
  
  tbl <- create_table(df) %>% 
    column_defaults(width = .5) %>% 
    spanning_header(N1, SD1, label = "Treatment A") %>%
    spanning_header(N2, SD2, label = "Treatment B") %>%
    define(Country, width = 1) %>% 
    titles("Table 1.0", "Treatments by Country") %>% 
    footnotes("* Country of Site")
  
  rpt <- create_report(fp) %>% 
    options_fixed(uchar = "-") %>% 
    add_content(tbl)
  
  res <- write_report(rpt) 
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  expect_equal(res$pages, 1)
  
})



test_that("test73: Table Borders with ttls/fnts on table works as expected.", {
  
  fp <- file.path(base_path, "output/test73.out")
  
  tbl <- create_table(iris, borders = "all") %>% 
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>% 
    footnotes("Here is a footnote", "And another")
  
  rpt <- create_report(fp) %>%
    page_header("Left", "Right") %>% 
    add_content(tbl) %>% 
    page_footer("left", "", "right")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test74: Table Borders with ttls/fnts on report works as expected.", {
  
  fp <- file.path(base_path, "output/test74.out")
  
  tbl <- create_table(iris, borders = "all") 
  
  rpt <- create_report(fp) %>%
    page_header("Left", "Right") %>% 
    add_content(tbl) %>% 
    page_footer("left", "", "right") %>% 
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>% 
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})


test_that("test75: Label row is one cell.", {
  
  
  fp <- file.path(base_path, "output/test75.out")
  
  
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
  tbl <- create_table(df, first_row_blank = TRUE) %>% 
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = ll, cyl = "Cylinders")) %>% 
    define(label, indent = .25) %>% 
    define(A, label = "Group A", width = 1.25, align = "center", n = 19) %>% 
    define(B, label = "Group B",  width = 1.25,  align = "center", n = 13)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "landscape", output_type = "TXT") %>% 
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

test_that("test76: Page break on invisible column.", {
  
  fp <- file.path(base_path, "output/test76.out")
  
  dat <- iris
  dat$Sequence <- seq(1, nrow(iris))
  
  tbl <- create_table(dat, borders = "all") %>%
    define(Species, page_break = TRUE, visible = FALSE)
  
  rpt <- create_report(fp) %>%
    page_header("Left", "Right") %>% 
    add_content(tbl) %>% 
    page_footer("left", "", "right") %>% 
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>% 
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})

test_that("test77: Blank after on invisible column.", {

  fp <- file.path(base_path, "output/test77.out")

  tbl <- create_table(iris, borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)

  rpt <- create_report(fp) %>%
    page_header("Left", "Right") %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")


  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
})



test_that("test78: Blank nested stub works as expected.", {
  
  
  fp <- file.path(base_path, "output/test78.out")
  
  
  # Read in prepared data
  df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "Stats"    "19"          "13"         
      "ampg"   "Stats"    "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Stats"    "16.4"        "21.4"       
      "ampg"   "Stats"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Stats"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  ll <- "Here is a super long label to see if it can span the entire table."
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE, borders = c("all")) %>% 
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, label_row = TRUE, 
           format = c(ampg = ll, cyl = "Cylinders")) %>% 
    define(label, indent = .25, dedupe = TRUE) %>% 
    define(A, label = "Group A", align = "center", n = 19) %>% 
    define(B, label = "Group B", align = "center", n = 13)
  
  
  # Create report and add content
  rpt <- create_report(fp, orientation = "portrait") %>% 
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

test_that("test79: Page header width works.", {
  
  fp <- file.path(base_path, "output/test79.out")
  
  tbl <- create_table(iris[1:10, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rpt <- create_report(fp) %>%
    page_header(paste0("Left and here is a really long left ",
                       "cell text to put it and more and more"), 
                "Right", width = 8) %>%
    add_content(tbl) %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame",
           blank_row = "below") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("test80: Carriage return in label row works.", {
  
  
  fp <- file.path(base_path, "output/test80.out")
  
  
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
  rpt <- create_report(fp, orientation = "portrait") %>% 
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

test_that("test81: Glue feature works.", {
  
  library(common)
  
  fp <- file.path(base_path, "output/test81.out")
  
  tbl <- create_table(mtcars[1:10, ], borders = "all") %>%
    spanning_header(1, 4, label = "My span{subsc('4')}") %>%
    define(mpg, label = "Mpg{subsc('3')}")
  
  myvar <- "23"
  
  rpt <- create_report(fp) %>%
    page_header(c("Left {supsc('2')}really long left ",
                       "cell text to put it{supsc('3')} and more and more"), 
                "Right{supsc('x')}") %>%
    add_content(tbl) %>%
    page_footer(c("left1{supsc('5')}", "left2{supsc('6')}"), "", 
                 "right{supsc('7')}") %>%
     titles("Table 1.0{supsc('1')}", "IRIS Data Frame{{myvar}}",
            blank_row = "below") %>%
     footnotes("Here is a footnote{subsc('a')}", "And another{subsc('9')}")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  

    
})



test_that("test82: Title columns work 1 column.", {
  
  fp <- file.path(base_path, "output/test82.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all")  %>%
    titles("Table 1.0 second row", "IRIS Data Frame3",
           blank_row = "both", columns =  1, align = "center",
           borders = c("outside")) 
  
  rpt <- create_report(fp, output_type = "TXT") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test83: Title columns work 2 columns.", {
  
  fp <- file.path(base_path, "output/test83.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    titles("Table 1.0", "IRIS Data Frame", "Left", "Right",
           blank_row = c("below"), columns =  2, borders = "none")
  
  rpt <- create_report(fp, output_type = "TXT") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")  %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test84: Title columns work 3 columns.", {
  
  fp <- file.path(base_path, "output/test84.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rght <- paste("Here")
  
  rpt <- create_report(fp, output_type = "TXT", 
                       font_size = 10) %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame", 
           "My right thing", "", "Center", rght,
           blank_row = "below", columns =  3, borders = "top") %>%
    footnotes("Here is a footnote", "And another", "A",
              "Here is a longer footnote to see if I can figure out the alignment pattern.",
              align = "right")
  
  
  res <- write_report(rpt)
  res
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test85: Multiple title blocks work as expected.", {
  
  fp <- file.path(base_path, "output/test85.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "TXT") %>%
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

test_that("test86: Titles can be passed as a vector.", {
  
  fp <- file.path(base_path, "output/test86.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  ttl <- c("Title1", "Title2", "Title3")
  
  rpt <- create_report(fp, output_type = "TXT") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles(ttl,
           blank_row = "below", columns =  1, align = "center", width = 7,
           borders = "none") %>%
    footnotes("Here is a footnote", "And another")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test87: Custom page size works as expected.", {
  
  fp <- file.path(base_path, "output/test87.out")
  
  tbl <- create_table(iris[1:15, ]) %>%
    define(Species, visible = FALSE)
  
  ttl <- c("Title1", "Title2", "Title3")
  
  rpt <- create_report(fp, output_type = "TXT", paper_size = c(6.5, 7.5),
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



test_that("test88: Footnotes columns work 1 column.", {
  
  fp <- file.path(base_path, "output/test88.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all")  %>%
    titles("Table 1.0 second row", "IRIS Data Frame3",
           blank_row = "both", columns =  1, align = "center",
           borders = c("outside")) 
  
  rpt <- create_report(fp, output_type = "TXT") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Here is a footnote", "And another", columns = 1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test89: Footnotes columns work 2 columns.", {
  
  fp <- file.path(base_path, "output/test89.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    titles("Table 1.0", "IRIS Data Frame", "Left", "Right",
           blank_row = c("below"), columns =  2, borders = "none") %>%
    footnotes("Table left", "Table right", columns = 2)
  
  rpt <- create_report(fp, output_type = "TXT") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right")  %>%
    footnotes("Here is a footnote", "And another", columns = 2)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test90: Footnotes columns work 3 columns.", {
  
  fp <- file.path(base_path, "output/test90.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE)
  
  rght <- paste("Here")
  
  rpt <- create_report(fp, output_type = "TXT", 
                       font_size = 10) %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    titles("Table 1.0", "IRIS Data Frame", 
           "My right thing", "", "Center", rght,
           blank_row = "below", columns =  3, borders = "top") %>%
    footnotes("Here is a footnote", "And another", "A",
              "Here is a longer footnote", "to see if I can figure out",  "the alignment pattern",
              align = "right", columns = 3)
  
  
  res <- write_report(rpt)
  res
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test91: Multiple footnotes blocks work as expected.", {
  
  fp <- file.path(base_path, "output/test91.out")
  
  tbl <- create_table(iris[1:15, ], borders = "all") %>%
    define(Species, blank_after = TRUE, visible = FALSE) %>%
    footnotes("Left", "right", columns = 2)
  
  rpt <- create_report(fp, output_type = "TXT") %>%
    add_content(tbl) %>%
    page_header("left", "right") %>%
    page_footer("left", "", "right") %>%
    footnotes("Footnote1", "IRIS Data Frame",
           blank_row = "below", columns =  1, align = "center", width = 7,
           borders = "all") %>%
    footnotes("Footnotes2", "IRIS Data Frame2", "Left", "Right",
           blank_row = "below", columns =  2, borders = "all") %>%
    footnotes("Footnotes3", "IRIS Data Frame3", "My right thing", "", "Center",
           blank_row = "below", columns =  3, borders = "all") %>%
    titles("Table 1.0", "My little title")
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("test92: Page by with wrap works as expected.", {
  
  
  fp <- file.path(base_path, "output/test92.out")
  
  dat <- iris
  dat$Pgby <- as.character(dat$Species)
  dat$Pgby <- paste0("Flower Type\n", dat$Pgby)
  
  
  tbl <- create_table(dat, borders = "none") %>% 
    titles("Table 1.0", "My Nice Report with a Page By", borders = "none") %>%
    page_by(Pgby, align = "left", label = "", borders = "none") %>%
    define(Pgby, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "TXT", 
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


test_that("test93: Page by with wrap works as expected.", {
  
  
  fp <- file.path(base_path, "output/test93.out")
  
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
  
  rpt <- create_report(fp, output_type = "TXT", 
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

