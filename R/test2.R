# # Load Packages
# library(tidyverse)
# library(haven)
# 
# 
# dir_base <- "c:/Projects/Experis/Ideation/R Training/Clinical/code/"
# 
# source(file.path(dir_base, "formats.R"))
# 
# 
# # Data Filepath
# dir_data <- file.path(dir_base, "data")
# 
# 
# # Load Data
# data_demo   <- file.path(dir_data, "dm.sas7bdat") %>% 
#   read_sas() %>% filter(ARM != "SCREEN FAILURE") 
# 
# 
# sex_decode <- c("M" = "Male",
#                 "F" = "Female")
# 
# race_decode <- c("WHITE" = "White", 
#                  "BLACK OR AFRICAN AMERICAN" = "Black or African American", 
#                  "ASIAN" = "Asian", 
#                  "NATIVE AMERICAN" = "Native American",
#                  "UNKNOWN" = "Unknown")
# 
# # Data Validation ---------------------------------------------------------
# 
# data_demo %>% glimpse() 
# 
# arm_pop <- table(data_demo$ARM)
# 
# 
# # Summary Table -----------------------------------------------------------
# 
# demo_age <- 
#   data_demo %>%
#   group_by(ARM) %>%
#   summarise(across(.cols = AGE,
#                    .fns = list(N      = ~ n_fmt(.),
#                                Mean   = ~ mean_sd(mean(.), sd(.)),
#                                Median = ~ median_fmt(median(.)),
#                                `Q1 - Q3` = ~ quantile_range(quantile(., 0.25),
#                                                             quantile(., 0.75)),
#                                Range  = ~ range_fmt(range(.))
#                    ))) %>%
#   pivot_longer(-ARM,
#                names_to  = c("var", "label"),
#                names_sep = "_",
#                values_to = "value") %>%
#   pivot_wider(names_from = ARM,
#               values_from = "value") 
# 
# 
# demo_sex <- 
#   data_demo %>%
#   add_count(ARM, SEX,  name = "n_SEX") %>%
#   select(ARM, SEX, n_SEX) %>%
#   distinct() %>%
#   pivot_longer(cols = c(SEX),
#                names_to  = "var",
#                values_to = "label") %>%
#   pivot_wider(names_from  = ARM,
#               values_from = n_SEX,
#               values_fill = 0) %>%
#   mutate(label = factor(label, levels = names(sex_decode),
#                         labels = sex_decode),
#          `ARM A` = cnt_pct(`ARM A`, arm_pop["ARM A"]),
#          `ARM B` = cnt_pct(`ARM B`, arm_pop["ARM B"]),
#          `ARM C` = cnt_pct(`ARM C`, arm_pop["ARM C"]),
#          `ARM D` = cnt_pct(`ARM D`, arm_pop["ARM D"])) 
# 
# 
# 
# demo_race <- 
#   data_demo %>%
#   add_count(ARM, RACE, name = "n_RACE") %>%
#   select(ARM, RACE, n_RACE) %>%
#   distinct() %>%
#   pivot_longer(cols = RACE,
#                names_to  = "var",
#                values_to = "label") %>%
#   pivot_longer(cols = n_RACE,
#                names_to = "var_match",
#                names_prefix = "n_",
#                values_to = "N") %>%
#   filter(var == var_match) %>%
#   select(-var_match) %>%
#   distinct() %>%
#   pivot_wider(names_from  = ARM,
#               values_from = N,
#               values_fill = 0) %>%
#   mutate(label = factor(label, levels = names(race_decode),
#                         labels = race_decode),
#          `ARM A` = cnt_pct(`ARM A`, arm_pop["ARM A"]),
#          `ARM B` = cnt_pct(`ARM B`, arm_pop["ARM B"]),
#          `ARM C` = cnt_pct(`ARM C`, arm_pop["ARM C"]),
#          `ARM D` = cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%  
#   arrange(var, label) 
# 
# 
# 
# demo <- bind_rows(demo_age, demo_sex, demo_race) 
# 
# 
# View(demo)
# 
# 
# 


