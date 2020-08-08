
context("System Tests")


test_that("Simple table works as expected.", {
  
  
  
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
  df
  df1 <- df[df$arm == "A", ]
  df2 <- df[df$arm == "B", ]
  
  lbls <- c(subjid = "Subject ID",
            name = "Subject Name",
            sex = "Sex",
            age = "Age",
            arm = "Arm")
  
  for (nm in names(df)) {
    attr(df1[[nm]], "label") <- lbls[[nm]]
    attr(df2[[nm]], "label") <- lbls[[nm]]
  }

  
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
    define(arm, format = afmt)
  
  
  
  rpt <- create_report("./tests/testthat/output/test2.out", uom = "inches", paper_size = "letter") %>%
    #options_text(cpuom = 10.909, lpuom = 6.075) %>%
    options_text(editor = "notepadpp") %>%
    page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
    titles("Table 1.0", "Analysis Data Subject Listing", "Safety Population", align = "center") %>%
    footnotes("Program Name: table1_0.R") %>%
    page_footer(left = Sys.time(), center = "Confidential", right = "Page X of Y") %>%
    add_content(tbl1) %>%
    add_content(tbl2)
  
  
  res2 <- write_report(rpt)
  

})
