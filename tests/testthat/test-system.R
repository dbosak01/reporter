
context("System Tests")

base_path <- "c:/packages/rptr/tests/testthat"

base_path <- "."

test_that("test1: Simplest table works as expected.", {
  
  
  fp <- file.path(base_path, "output/test1.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp) %>% 
    add_content(create_table(mtcars[1:10, ]))
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test2: Simplest table with title works as expected.", {
  
  fp <- file.path(base_path, "output/test2.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp) %>% 
    titles("MTCARS Data Frame") %>% 
    add_content(create_table(mtcars[1:10, ]))

  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
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
    options_text(editor = "notepad++") %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", "Safety Population", 
           align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", 
                right = "Page X of Y") %>%
    add_content(tbl1) 
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
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
    options_text(editor = "notepad++") %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", 
                right = "Page X of Y") %>%
    add_content(tbl1) %>%
    add_content(tbl2)
  
  
  res2 <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
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
    define(subjid, label = "Subject ID") %>% 
    define(name, label = "Subject Name") %>% 
    define(sex, label = "Sex") %>% 
    define(age, label = "Age") %>% 
    define(arm, label = "Arm", 
           blank_after = TRUE, 
           dedupe = TRUE)


  rpt <- create_report(fp) %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", 
           "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", 
                right = "Page X of Y") %>%
    add_content(tbl1) 
  

  res2 <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("test6: Table that spans multiple pages breaks as expected.", {

  fp <- file.path(base_path, "output/test6.out")

  if (file.exists(fp))
    file.remove(fp)

  rpt <- create_report(fp) %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris))


  write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

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


  res2 <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

})

test_that("test8: Table with spanning headers works as expected.", {
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    spanning_header(span_cols = c("mpg", "cyl", "disp", "hp"),
                    label = "Span 1", label_align = "center", n = 10) %>% 
    spanning_header(span_cols = c("drat", "wt", "qsec"),
                    label = "Span 2", label_align = "right", n = 10) %>%
    spanning_header(span_cols = c("vs", "am", "gear", "carb"),
                    label = "Span 3", label_align = "right", n = 10) %>%
    spanning_header(span_cols = c(5:11), label = "Super Span", label_align = "center",
                    level = 2) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  rpt <- create_report(file.path(base_path, "output/test8.out")) %>% 
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test")
  
  write_report(rpt)
  
})
  
