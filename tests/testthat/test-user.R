context("User Tests")

base_path <- "c:/packages/rptr/tests/testthat"

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
           "Demographics and Baseline Characteristics",
           "Specify Population") %>%
    add_content(tbl)

  # Write out report
  res <- write_report(rpt)

  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)

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
  
})


test_that("user3: listings works.", {
  

  
  # Data Filepath
  dir_data <- file.path(base_path, "data")
  
  fp <- file.path(base_path, "user/user3.out")
  
  
  # Load Data
  data_demo   <- file.path(dir_data, "dm.csv") %>%
    read.csv() 

  # Define table
  tbl <- create_table(data_demo) %>% 
    define(USUBJID, id_var = TRUE) 

  # Define Report
  rpt <- create_report(fp) %>%
    titles("Listing 1.0",
           "Demographics Dataset") %>%
    add_content(tbl, align = "left") %>% 
    page_footer(left = Sys.time(), right = "Page [pg] of [tpg]")
  
  # Write out report
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * res$line_count)
  
})
# 
# test_that("user4: Adverse Events table works.", {
#   
#   
#   
#   # Data Filepath
#   dir_data <- file.path(base_path, "data")
#   
#   fp <- file.path(base_path, "user/user4.out")
#   
#   
#   # Load Data
#   data_ae   <- file.path(dir_data, "adae.rds") %>%
#     readRDS()
#   
#   # Define table
#   tbl <- create_table(data_demo) %>% 
#     define(USUBJID, id_var = TRUE) 
#   
#   # Define Report
#   rpt <- create_report(fp) %>%
#     titles("Table 2.0",
#            "Demographics Dataset") %>%
#     add_content(tbl, align = "left") %>% 
#     page_footer(left = Sys.time(), right = "Page [pg] of [tpg]")
#   
#   # Write out report
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   
#   lns <- readLines(fp)
#   
#   expect_equal(length(lns), res$pages * res$line_count)
#   
# })
# 
# 
