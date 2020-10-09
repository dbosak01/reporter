
context("System Tests")

base_path <- "c:/packages/rptr/tests/testthat"

base_path <- "."

test_that("rtf1: Simplest table works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test1.rtf")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp, output_type = "RTF") %>% 
    add_content(create_table(mtcars[1:10, ]), align = "left")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("rtf2: Simplest table with title works as expected.", {
  
  fp <- file.path(base_path, "rtf/test2.rtf")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp, output_type = "RTF") %>% 
    options_fixed(font_size = 10) %>% 
    titles("MTCARS Data Frame", align = "left") %>% 
    add_content(tbl)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})


test_that("rtf3: Table with break between sections works as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test3.rtf")
  
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
  
  
})

test_that("rtf4: Table that spans multiple pages breaks as expected.", {
  
  fp <- file.path(base_path, "rtf/test4.rtf")
  
  if (file.exists(fp))
    file.remove(fp)
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    titles("IRIS Data Frame") %>%
    add_content(create_table(iris)) 
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  #write_registration_file(file.path(base_path,"./rtf/reg.txt"))
})

test_that("rtf5: Table with long cell and label values wraps as expected.", {
  
  
  fp <- file.path(base_path, "rtf/test5.rtf")
  
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
  
  
  rpt <- create_report(fp, output_type = "RTF") %>%
    titles("Table 1.0", align = "center") %>%
    
    add_content(tbl1)
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})

test_that("rtf6: Table with spanning headers works as expected.", {
  
  fp <- file.path(base_path, "rtf/test6.rtf")
  
  
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
  
  rpt <- create_report(fp, output_type = "RTF") %>% 
    add_content(tbl) %>% 
    titles("Table 1.0", "MTCARS Subset Test")
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  
})
