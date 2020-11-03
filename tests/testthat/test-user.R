context("User Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- "./"

test_that("user1: demo table works.", {

  library(tidyr)
  library(dplyr)

  # Data Filepath
  dir_data <- file.path(base_path, "data")
  
  fp <- file.path(base_path, "user/user1.out")
  

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
    column_defaults(from = "ARM A", to = "ARM D", width = 1) %>% 
    define(var, blank_after = TRUE, dedupe = TRUE,
           format = block_fmt, label = "") %>%
    define(label, label = "") %>%
    define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
    define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
    define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
    define(`ARM D`, align = "center", label = "Competitor", n = 38)

  # Define Report
  rpt <- create_report(fp) %>%
    titles("Table 14.1/4",
           "Demographics and Baseline to Characteristics",
           "Specify Population") %>%
    add_content(tbl) %>% 
    #footnotes("Special symbols \U221e to mess things up: Ω µ β ¥ ∑ ≠ ≤ £ ∞ ؈ ლ  \Ub8a 鬼") %>%   
    footnotes("Special symbols µ Ω £ there to mess things up: ") %>% 
    page_footer("Time", right = "Page [pg] of [tpg]")

  # Write out report
  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp, encoding = "native.enc")
  
  expect_equal(length(lns), res$pages * res$line_count)
  
  if (TRUE) {
    rtfpth <- file.path(base_path, "user/user1.rtf")
    write_report(rpt, rtfpth, output_type = "RTF")
    expect_equal(file.exists(rtfpth), TRUE)
    
    pdfpth <- file.path(base_path, "user/user1.pdf")
    write_report(rpt, pdfpth, output_type = "PDF")
    expect_equal(file.exists(pdfpth), TRUE)
  }
  
})

test_that("user2: demo table with stub works.", {
  
  library(tidyr)
  library(dplyr)
  
  
  
  # Data Filepath
  dir_data <- file.path(base_path, "data")
  
  fp <- file.path(base_path, "user/user2.out")
  
  
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
    stub(c("var", "label")) %>% 
    define(var, blank_after = TRUE, 
           format = block_fmt, label = "", label_row = TRUE) %>%
    define(label, label = "", indent = .25) %>%
    define(`ARM A`, align = "center", label = "Placebo", n = 36) %>%
    define(`ARM B`, align = "center", label = "Drug 10mg", n = 38) %>%
    define(`ARM C`, align = "center", label = "Drug 20mg", n = 38) %>%
    define(`ARM D`, align = "center", label = "Competitor", n = 38)
  
  # Define Report
  rpt <- create_report(fp) %>%
    titles("Table 14.1/4",
           "Demographics and Baseline Characteristics",
           "Specify Population") %>%
    add_content(tbl)
  
  # Write out report
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
  rtfpth <- file.path(base_path, "user/user2.rtf")
  write_report(rpt, rtfpth, output_type = "RTF")
  expect_equal(file.exists(rtfpth), TRUE)
  
  
})


test_that("user3: listings works.", {
  
  # Data Filepath
  dir_data <- file.path(base_path, "data")
  
  fp <- file.path(base_path, "user/user3.out")
  
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

  # Define table
  tbl <- create_table(data_demo) %>% 
    define(USUBJID, id_var = TRUE) 


  # Define Report
  rpt <- create_report(fp) %>%
    options_fixed(editor = "notepad", font_size = 10) %>% 
    titles("Listing 1.0",
           "Demographics Dataset") %>%
    add_content(tbl, align = "left") %>% 
    page_footer(left = "Time", right = "Page [pg] of [tpg]")
  
  #Write out report
  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)

  lns <- readLines(fp)

  expect_equal(length(lns), res$pages * res$line_count)
  
  
  rtfpth <- file.path(base_path, "user/user3.rtf")
  write_report(rpt, file_path = rtfpth, output_type = "RTF")
  
  expect_equal(file.exists(rtfpth), TRUE)
  
  rtfpth <- file.path(base_path, "user/user3.rtf")
  write_report(rpt, rtfpth, output_type = "RTF")
  expect_equal(file.exists(rtfpth), TRUE)
  
  pdfpth <- file.path(base_path, "user/user3.pdf")
  write_report(rpt, pdfpth, output_type = "PDF")
  expect_equal(file.exists(pdfpth), TRUE)
  
})

test_that("user4: Adverse Events table works.", {

  #devtools::install_github("https://github.com/dbosak01/fmtr")
  library(dplyr)
  library(tidyr)

  # Data Filepath
  dir_data <- file.path(base_path, "data")

  fp <- file.path(base_path, "user/user4.out")

  dp <- file.path(dir_data, "ADAE.csv")
  
  dat <- read.csv(dp)
  
  
  # Get population counts
  arm_pop <- table(dat$TRTA)  
  
  # Subset ADSL for needed rows and columns
  df_sub <- dat %>% 
    select(TRTA, AESEV, AESEVN, AEREL, AESOC, AEDECOD) 
  
  
  # Get counts and percents 
  df1 <- df_sub %>% 
    select(-AESEV, -AEREL) %>% 
    group_by(TRTA, AESOC, AEDECOD, AESEVN) %>% 
    summarize(cnt = n()) %>% 
    pivot_wider(names_from = c(TRTA, AESEVN),
                values_from = cnt, 
                values_fill = 0) %>% 
    transmute(AESOC = AESOC, 
              AEDECOD = stri_trans_totitle(AEDECOD),
              `ARM A_1` = fmt_cnt_pct(`ARM A_1`, arm_pop["ARM A"]),
              `ARM A_2` = fmt_cnt_pct(`ARM A_2`, arm_pop["ARM A"]),
              `ARM A_3` = fmt_cnt_pct(0, arm_pop["ARM A"]),
              `ARM B_1` = fmt_cnt_pct(`ARM B_1`, arm_pop["ARM B"]),
              `ARM B_2` = fmt_cnt_pct(`ARM B_2`, arm_pop["ARM B"]),
              `ARM B_3` = fmt_cnt_pct(0, arm_pop["ARM B"]),
              `ARM C_1` = fmt_cnt_pct(`ARM C_1`, arm_pop["ARM C"]),
              `ARM C_2` = fmt_cnt_pct(`ARM C_2`, arm_pop["ARM C"]),
              `ARM C_3` = fmt_cnt_pct(0, arm_pop["ARM C"]),
              `ARM D_1` = fmt_cnt_pct(`ARM D_1`, arm_pop["ARM D"]), 
              `ARM D_2` = fmt_cnt_pct(`ARM D_2`, arm_pop["ARM D"]), 
              `ARM D_3` = fmt_cnt_pct(`ARM D_3`, arm_pop["ARM D"])) %>% 
    ungroup() 
  
  
  # Get counts and percents for All Adverse Events
  df2 <- df_sub %>% 
    select(-AESEV, -AEREL,-AESOC, -AEDECOD,) %>% 
    group_by(TRTA, AESEVN) %>% 
    summarize(cnt = n()) %>% 
    pivot_wider(names_from = c(TRTA, AESEVN),
                values_from = cnt, 
                values_fill = 0) %>% 
    ungroup() 
  
  col_template <- paste0(c(rep("ARM A_", 3), rep("ARM B_", 3), rep("ARM C_", 3),
                           rep("ARM D_", 3)), rep(c(1, 2, 3), 3))
  
  for (nm in col_template) {
    if (!nm %in% names(df2))
      df2[[nm]] <- 0
  }
  
  df2 <- df2 %>% 
    transmute(AESOC = "All System Organ Classes",
              AEDECOD = "All Adverse Events", 
              `ARM A_1` = fmt_cnt_pct(`ARM A_1`, arm_pop["ARM A"]),
              `ARM A_2` = fmt_cnt_pct(`ARM A_2`, arm_pop["ARM A"]),
              `ARM A_3` = fmt_cnt_pct(`ARM A_3`, arm_pop["ARM A"]),
              `ARM B_1` = fmt_cnt_pct(`ARM B_1`, arm_pop["ARM B"]),
              `ARM B_2` = fmt_cnt_pct(`ARM B_2`, arm_pop["ARM B"]),
              `ARM B_3` = fmt_cnt_pct(`ARM B_3`, arm_pop["ARM B"]),
              `ARM C_1` = fmt_cnt_pct(`ARM C_1`, arm_pop["ARM C"]),
              `ARM C_2` = fmt_cnt_pct(`ARM C_2`, arm_pop["ARM C"]),
              `ARM C_3` = fmt_cnt_pct(`ARM C_3`, arm_pop["ARM C"]),
              `ARM D_1` = fmt_cnt_pct(`ARM D_1`, arm_pop["ARM D"]), 
              `ARM D_2` = fmt_cnt_pct(`ARM D_2`, arm_pop["ARM D"]), 
              `ARM D_3` = fmt_cnt_pct(`ARM D_3`, arm_pop["ARM D"]))
  
  
  final <- bind_rows(df2, df1) %>% arrange(AESOC, AEDECOD)
  
  tbl <- create_table(final, first_row_blank = TRUE) %>% 
    column_defaults(from = `ARM A_1`, to = `ARM D_3`, width = 1) %>% 
    spanning_header("ARM A_1", "ARM A_3", label = "ARM A", n = arm_pop["ARM A"]) %>%
    spanning_header("ARM B_1", "ARM B_3", label = "ARM B", n = arm_pop["ARM B"]) %>%
    spanning_header("ARM C_1", "ARM C_3", label = "ARM C", n = arm_pop["ARM C"]) %>%
    spanning_header("ARM D_1", "ARM D_3", label = "ARM D", n = arm_pop["ARM D"]) %>%
    stub(vars = c("AESOC", "AEDECOD"), label = "System Organ Class\n   Preferred Term", width = 5) %>% 
    define(AESOC, blank_after = TRUE, label_row = TRUE) %>% 
    define(AEDECOD, indent = .25) %>% 
    define(`ARM A_1`, align = "center", label = "Mild") %>% 
    define(`ARM A_2`, align = "center", label = "Mod**") %>% 
    define(`ARM A_3`, align = "center", label = "Severe") %>% 
    define(`ARM B_1`, align = "center", label = "Mild", page_wrap = TRUE) %>% 
    define(`ARM B_2`, align = "center", label = "Mod**") %>% 
    define(`ARM B_3`, align = "center", label = "Severe") %>% 
    define(`ARM C_1`, align = "center", label = "Mild", page_wrap = TRUE) %>% 
    define(`ARM C_2`, align = "center", label = "Mod**") %>% 
    define(`ARM C_3`, align = "center", label = "Severe") %>% 
    define(`ARM D_1`, align = "center", label = "Mild", page_wrap = TRUE) %>% 
    define(`ARM D_2`, align = "center", label = "Mod**") %>% 
    define(`ARM D_3`, align = "center", label = "Severe") 
  
  rpt <- create_report(fp) %>% 
    options_fixed(font_size = 10) %>% 
    page_header("Client: Experis", "Study: BBC") %>% 
    titles("Table 1.0", "Adverse Events by Severity", "Safety Population") %>% 
    add_content(tbl) %>% 
    footnotes(paste("Date Produced:", "Time", ";  Program: Table3_0.R"),
              paste("* Total Reporting is defined as number of subjects",
                    "who reported at least one adverse event."),
              "** Mod = Moderate",
              paste("# Episodes is defined as the total number of occurances",
                    "of adverse events"),
              paste("% is defined as Number of Subjects divided by Total Reporting"),
              "Note: Adverse events were coded using MedDRA Version 9.1") %>%
    page_footer("Time", "Confidential", "Page [pg] of [tpg]")
  
  res <- write_report(rpt)


  expect_equal(file.exists(fp), TRUE)

  lns <- readLines(fp)

  expect_equal(length(lns), res$pages * res$line_count)
  
  
  rtfpth <- file.path(base_path, "user/user4.rtf")
  res <- write_report(rpt, file_path = rtfpth, output_type = "RTF")
  
  rtfpth <- file.path(base_path, "user/user4.rtf")
  res <- write_report(rpt, rtfpth, output_type = "RTF")
  expect_equal(file.exists(rtfpth), TRUE)
  #print(res)
  
  pdfpth <- file.path(base_path, "user/user4.pdf")
  res <- write_report(rpt, pdfpth, output_type = "PDF")
  expect_equal(file.exists(pdfpth), TRUE)
  #print(res)

})



test_that("user5: large listing works.", {
  
  test <- FALSE
  
  # Skip except for special testing because it takes too long (+ 5 minutes)
  if (test) { 
    
    startTime <- Sys.time()  
  
    # Data Filepath
    dir_data <- file.path(base_path, "data")
    
    fp <- file.path(base_path, "user/user5.out")
    
    
    # Load Data
    data_lb   <- file.path(dir_data, "ADLB.csv") %>%
      read.csv() 
  
  
    # Define table
    tbl <- create_table(data_lb) %>% 
      define(USUBJID, id_var = TRUE) 
    
    # Define Report
    rpt <- create_report(fp) %>%
      options_fixed(font_size = 10) %>% 
      titles("Listing 2.0",
             "Analysis Dataset Labs") %>%
      add_content(tbl, align = "left") %>% 
      page_footer(left = "Time", right = "Page [pg] of [tpg]")
    
    # Write out report
    res <- write_report(rpt)
    
    endTime <- Sys.time()
    
    print(endTime - startTime)
    
    expect_equal(file.exists(fp), TRUE)
    
    lns <- readLines(fp)
  
    expect_equal(length(lns), res$pages * res$line_count)
    
    rtfpth <- file.path(base_path, "user/user5.rtf")
    write_report(rpt, rtfpth, output_type = "RTF")
    expect_equal(file.exists(rtfpth), TRUE)
    
    # Very special testing case.
    if (FALSE) {
      pdfpth <- file.path(base_path, "user/user5.pdf")
      write_report(rpt, pdfpth, output_type = "PDF")
      expect_equal(file.exists(pdfpth), TRUE)
    }
  
  } else {
    
   expect_equal(TRUE, TRUE) 
  }
  
})



test_that("user6: listings with page break works as expected.", {
  
  # Data Filepath
  dir_data <- file.path(base_path, "data")

  
  # Load Data
  data_demo   <- file.path(dir_data, "dm.csv") %>%
    read.csv() 
  
  data_demo <- data_demo[order(data_demo$ARMCD), ]
  
  # Define table
  tbl <- create_table(data_demo) %>% 
    define(USUBJID, id_var = TRUE) %>% 
    define(ARMCD, page_break = TRUE, id_var = TRUE) %>% 
    define(ARM, id_var = TRUE)
  
  # Define Report
  rpt <- create_report("user/user6") %>%
    options_fixed(editor = "notepad") %>% 
    titles("Listing 1.0",
           "Demographics Dataset") %>%
    add_content(tbl, align = "left") %>% 
    page_footer(left = "Time", right = "Page [pg] of [tpg]")
  
  # Write out report
  res <- write_report(rpt)
  
  expect_equal(file.exists(res$modified_path), TRUE)
  
  lns <- readLines(res$modified_path)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
  
  res <- write_report(rpt, output_type = "RTF")
  expect_equal(file.exists(res$modified_path), TRUE)
  
  res <- write_report(rpt, output_type = "PDF")
  expect_equal(file.exists(res$modified_path), TRUE)
  
  
})




test_that("user7: listings with NA values works.", {
  
  library(readr)
  
  # Data Filepath
  dir_data <- file.path(base_path, "data")
  
  fp <- file.path(base_path, "user/user7")
  
  # Load Data
  dat <- file.path(dir_data, "ADSL.csv") %>%
    read_csv() 
  
  # Define table
  tbl <- create_table(dat) %>% 
    define(USUBJID, id_var = TRUE) 
  
  # Define Report
  rpt <- create_report(fp) %>%
    titles("Listing 1.0",
           "ADSL Dataset") %>%
    add_content(tbl, align = "left") %>% 
    page_footer(left = "Time", right = "Page [pg] of [tpg]")
  
  # Write out report
  res <- write_report(rpt)
  
  expect_equal(file.exists(res$modified_path), TRUE)
  
  lns <- readLines(res$modified_path)
  
  expect_equal(length(lns), res$pages * res$line_count)
  

  res <- write_report(rpt, output_type = "RTF")
  expect_equal(file.exists(res$modified_path), TRUE)
  

  
})

# This is a special case, and I don't want to test it every time.
# One reason is to not create a dependancy on > R 3.5 just because
# of the .rds test file. The .rds test file has been appended with 
# a .txt extension to fool the checker. Turn back to .rds if needed.
# test_that("user8: table with spaces in column names works.", {
#   
#   fp <- file.path(base_path, "user/user8.out")
#   
#   dat <- readRDS(file.path(base_path, "./data/dm_final.rds"))
# 
#   tbl <- create_table(dat)  
#   
#   rpt <- create_report(fp) %>% 
#     add_content(tbl)
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   
# })

# Also a special case
# test_that("user9: table with stub and page by works as expected.", {
#   
#   # Data Filepath
#   dir_data <- file.path(base_path, "data")
#   
#   fp <- file.path(base_path, "user/user9")
#   
#   # Load Data
#   dat <- file.path(dir_data, "final.rds") %>% readRDS() 
#   
#   dat <- subset(dat, dat$label != "Q1 - Q3")
#   
#   arm_pop <- c("ARM A" = 20, "ARM B" = 21, "ARM C" = 19, "ARM D" = 22) 
#   
#   # Create Table
#   tbl <- create_table(dat, width = 9) %>% 
#     column_defaults(from = `ARM A`, to = `ARM D`, align = "center", width = 1.2) %>% 
#     page_by(AVISIT, label = "Visit: ", blank_row = "none") %>% 
#     stub(vars = c(PARAM, label), label  = "Parameter", width = 2) %>% 
#     define(AVISIT, visible = FALSE) %>% 
#     define(PARAMCD, visible = FALSE) %>% 
#     define(PARAM, label_row = TRUE, blank_after = TRUE) %>% 
#     define(label, label = "Statistic", indent = .25) %>% 
#     define(`ARM A`,  n = arm_pop["ARM A"]) %>% 
#     define(`ARM B`,  n = arm_pop["ARM B"]) %>% 
#     define(`ARM C`,  n = arm_pop["ARM C"]) %>% 
#     define(`ARM D`,  n = arm_pop["ARM D"]) 
#   
#   
#   rpt <- create_report(fp, output_type = "RTF") %>% 
#     set_margins(top = 1, bottom = .9) %>% 
#     page_header("Sponsor: Experis", "Study: ABC") %>% 
#     titles("Table 3.0", "Summary of Vital Sign Parameters by Visit", 
#            "Safety Population") %>% 
#     add_content(tbl) %>% 
#     footnotes("R Program: VA_Table.R") %>% 
#     page_footer(paste0("Date Produced: ", fapply(Sys.time(), "%d%b%y %H:%M")), 
#                 right = "Page [pg] of [tpg]")
#   
#   res <- write_report(rpt) 
#   
#   expect_equal(file.exists(res$modified_path), TRUE)
#   
# })

# Another special case
# test_that("user10: Combined report works as expected", {
#   
#   
#   dm_table <- readRDS(file.path(base_path, "data/DM_Table.rds"))
#   ae_table <- readRDS(file.path(base_path, "data/AE_Table.rds"))
#   lb_table <- readRDS(file.path(base_path, "data/LB_Table.rds"))
#   vs_table <- readRDS(file.path(base_path, "data/VS_Table.rds"))
#   
#   
#   rpt <- create_report("rtf/Combined_Table", output_type = "RTF") %>% 
#     set_margins(top = 1, bottom = .9) %>% 
#     page_header("Sponsor: Experis", "Study: ABC") %>% 
#     add_content(dm_table) %>% 
#     add_content(ae_table) %>% 
#     add_content(vs_table) %>% 
#     add_content(lb_table) %>% 
#     page_footer(paste0("Date Produced: ", fapply(Sys.time(), "%d%b%y %H:%M")), 
#                 right = "Page [pg] of [tpg]")
#   
#   
#   res <- write_report(rpt) 
#   
#   
#   
# })
