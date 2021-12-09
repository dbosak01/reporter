## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)


## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(tidyverse)
#  library(sassy)
#  library(broom)
#  library(survival)
#  library(survminer)
#  
#  
#  options("logr.autolog" = TRUE,
#          "logr.notes" = FALSE)
#  
#  # Get temp location for log and report output
#  tmp <- tempdir()
#  
#  # Open Log
#  lf <- log_open(file.path(tmp, "example5.log"))
#  
#  
#  # Load and Filter Data  --------------------------------------------------
#  
#  sep("Load and Filter Data")
#  
#  # Get path to sample data
#  pkg <- system.file("extdata", package = "reporter")
#  
#  # Get adam data
#  libname(adam, pkg, "sas7bdat")
#  
#  # Load data into memory
#  lib_load(adam)
#  
#  # Filter data
#  adsl <- adam.adsl %>%
#    select(USUBJID, SEX, AGEGR1, AGE, ARM) %>%
#    filter(ARM != "SCREEN FAILURE") %>% put()
#  
#  adpsga <- adam.adpsga %>%
#    filter(PARAMCD =="PSGA" & TRTA != "" & !is.na(AVISITN)) %>%
#    select(USUBJID, TRTA, AVISIT, AVISITN, AVAL, CRIT1FL) %>% put()
#  
#  # Get population counts
#  arm_pop <- adsl %>% count(ARM) %>% deframe() %>% put()
#  
#  
#  # Prepare Data ------------------------------------------------------------
#  
#  sep("Prepare data for analysis")
#  
#  put("Determine minimum visit at which success was achieved")
#  adpsga_minvsuccess <-
#    adpsga %>%
#    filter(CRIT1FL == 'Y') %>%
#    group_by(USUBJID) %>%
#    summarize(minvisit = min(AVISITN))
#  
#  put("Get subjects which did not achieve success")
#  adpsga_nosuccess <-
#    anti_join(adpsga, adpsga_minvsuccess, by = ('USUBJID')) %>%
#    group_by(USUBJID) %>%
#    summarize(maxvisit = max(AVISITN))
#  
#  put("Combine subjects cured with subjects not cured")
#  adslpsga_final <-
#    inner_join(adsl, adpsga, by = c('USUBJID')) %>%
#    left_join(adpsga_minvsuccess, by = c('USUBJID')) %>%
#    left_join(adpsga_nosuccess, by = c('USUBJID')) %>%
#    filter((AVISITN == minvisit & !is.na(minvisit)) |
#             (AVISITN == maxvisit & !is.na(maxvisit))) %>%
#    mutate(cured    = case_when(CRIT1FL == "Y" ~ TRUE,
#                                TRUE ~ as.logical(FALSE))) %>%
#    select(-minvisit, -maxvisit)
#  
#  
#  # Counts  ---------------------------------------------------------------
#  
#  sep("Perform Counts and Statistical Tests")
#  
#  put("Count patients with PSGA <= 1")
#  success_counts <- adslpsga_final %>%
#    filter(cured == TRUE) %>%
#    count(TRTA) %>%
#    pivot_wider(names_from = TRTA,
#                values_from = n) %>%
#    transmute(block = "counts",
#              label = "Number of patients with PSGA <= 1",
#              "ARM A" = as.character(`ARM A`),
#              "ARM B" = as.character(`ARM B`),
#              "ARM C" = as.character(`ARM C`),
#              "ARM D" = as.character(`ARM D`)) %>%
#              put()
#  
#  put("Count patients with PSGA > 1")
#  failed_counts <- adslpsga_final %>%
#    filter(cured == FALSE) %>%
#    count(TRTA) %>%
#    pivot_wider(names_from = TRTA,
#                values_from = n) %>%
#    transmute(block = "counts",
#              label = "Number of Censored Subjects (PSGA > 1)",
#              "ARM A" = as.character(`ARM A`),
#              "ARM B" = as.character(`ARM B`),
#              "ARM C" = as.character(`ARM C`),
#              "ARM D" = as.character(`ARM D`)) %>%
#    put()
#  
#  count_block <- bind_rows(success_counts, failed_counts)
#  
#  
#  # Kaplan-Meier estimates ----------------------------------------------------
#  
#  
#  sep("Perform Kaplan-Meier Tests")
#  
#  put("Create survival vector")
#  surv_vct <- Surv(time = adslpsga_final$AVISITN, event = adslpsga_final$cured) %>%
#    put()
#  
#  put("Fit model on survival vector")
#  stats_survfit_trta <- survival::survfit(surv_vct ~ TRTA, data = adslpsga_final, ) %>%
#    put()
#  
#  put("Get model quantiles")
#  stats_survfit_quantiles <- quantile(stats_survfit_trta)
#  
#  put("Get lower confidence intervals")
#  ci_lower <-
#    as.data.frame(stats_survfit_quantiles$lower) %>%
#    rownames_to_column  %>%
#    mutate(block = "surv",
#           TRTA = substring(rowname,6)) %>%
#    pivot_longer(cols = c(`25`, `50`, `75`),
#                 names_to = "Q",
#                 values_to = "lower") %>%
#    put()
#  
#  put("Get upper confidence intervals")
#  ci_upper <-
#    as.data.frame(stats_survfit_quantiles$upper) %>%
#    rownames_to_column %>%
#    mutate(block = "surv",
#           TRTA = substring(rowname,6)) %>%
#    pivot_longer(cols = c(`25`, `50`, `75`),
#                 names_to = "Q",
#                 values_to = "upper") %>%
#    put()
#  
#  put("Get confidence intervals")
#  ci <-
#    inner_join(ci_lower, ci_upper)  %>%
#    mutate(ci = paste0("(", ifelse(is.na(lower), "-", lower)
#                       , ", ", ifelse(is.na(upper), "-", upper), ")")) %>%
#    pivot_wider(id_cols = c("block", "Q"),
#                names_from = TRTA,
#                values_from = ci) %>%
#    mutate(order=2,
#           label1   = case_when(Q == 25 ~ "25th  percentile (weeks)",
#                                Q == 50 ~ "Median (weeks)",
#                                Q == 75 ~ "75th  percentile (weeks)"),
#           label2   = "95% Confidence Interval**") %>%
#    select(block, Q, order, label1, label2,
#           `ARM A`, `ARM B`, `ARM C`, `ARM D`) %>%
#    put()
#  
#  
#  put("Get quantiles")
#  quants <-
#    as.data.frame(stats_survfit_quantiles$quantile) %>%
#    rownames_to_column %>%
#    mutate(block = "surv",
#           TRTA = substring(rowname,6)) %>%
#    pivot_longer(cols = c(`25`, `50`, `75`),
#                 names_to = "Q",
#                 values_to = "value") %>%
#    pivot_wider(id_cols = c("block", "Q"),
#                names_from = TRTA,
#                values_from = value) %>%
#    mutate(order=1,
#           label1   = case_when(Q == 25 ~ "25th  percentile (weeks)",
#                                Q == 50 ~ "Median (weeks)",
#                                Q == 75 ~ "75th  percentile (weeks)"),
#           label2   = "",
#           `ARM A`  = as.character(`ARM A`),
#           `ARM B`  = as.character(`ARM B`),
#           `ARM C`  = as.character(`ARM C`),
#           `ARM D`  = as.character(`ARM D`)) %>%
#    select(block, Q, order, label1, label2,
#           `ARM A`, `ARM B`, `ARM C`, `ARM D`) %>%
#    put()
#  
#  put("Final arrangement")
#  kaplan_block <-
#    bind_rows(quants, ci) %>%
#    arrange(block, Q, order) %>%
#    transmute(block,
#              label1,
#              label2 = ifelse(label2 == "", NA, label2),
#              `ARM A`, `ARM B`, `ARM C`, `ARM D`) %>%
#    put()
#  
#  
#  # Cox Proportional Hazards  -----------------------------------------------
#  
#  sep("Perform Cox Proportional Hazards Test")
#  
#  put("Run Cox tests")
#  stats_surv_cph   <- survival::coxph(surv_vct ~ TRTA, data = adslpsga_final) %>%
#    put()
#  
#  put("Create summary statistics on Cox results")
#  cph_summary <-
#    summary(stats_surv_cph) %>%
#    put()
#  
#  put("Extract coefficients")
#  cph_coef <-
#    as.data.frame(cph_summary$coefficients) %>%
#    rownames_to_column  %>%
#    mutate(block = "surv",
#           TRTA = substring(rowname,5)) %>%
#    put()
#  
#  put("Extract confidence intervals")
#  cph_ci <-
#    cph_summary$conf.int %>%
#    as.data.frame(cph_summary$conf) %>%
#    rownames_to_column  %>%
#    put()
#  
#  put("Create cox statistics block")
#  cox_block <-
#    bind_cols(cph_coef, cph_ci) %>%
#    rename(hazard = `exp(coef)...3`, pval = `Pr(>|z|)`,
#           lower = `lower .95`, upper = `upper .95`) %>%
#    select(TRTA, hazard, pval, lower, upper) %>%
#    mutate(block = "cox",
#           ci = paste0("(", ifelse(is.na(lower), "-", sprintf("%.2f", lower))
#                          , ", ", ifelse(is.na(upper), "-", sprintf("%.2f", upper)), ")"),
#           hazard = sprintf("%.2f", hazard),
#           pval   = sprintf("%.3f", pval)) %>%
#    pivot_longer(cols = c("hazard", "pval", "ci"),
#                 names_to = "stat",
#                 values_to = "value") %>%
#    pivot_wider(id_cols = c("block", "stat"),
#                names_from = TRTA,
#                values_from = value) %>%
#    mutate(label1 = case_when(stat == "hazard"
#                                ~ "Hazard Ratio (Each Treatment Group - ARM A)***",
#                              stat == "pval" ~ "P-value",
#                              TRUE ~ "95% CI of Hazard Ratio"),
#           label2 = as.character(NA),
#           `ARM A` = as.character(NA)) %>%
#    select(block, label1, label2, `ARM A`, `ARM B`, `ARM C`, `ARM D`) %>%
#    put()
#  
#  
#  
#  put("Combine statistics blocks")
#  stat_block <- bind_rows(kaplan_block, cox_block) %>% put()
#  
#  # Create Survival Plot -------------------------------------------------------
#  
#  sep("Create survival plot")
#  
#  put("Create data frame with zero values for each visit")
#  arms <- unique(adslpsga_final$ARM)
#  visits <- unique(adslpsga_final$AVISITN)
#  all_visits <- rep(arms, length(visits))
#  all_visits <- all_visits[order(all_visits)]
#  
#  put("Create visit template")
#  df <- data.frame(ARM = all_visits,
#                   AVISIT = paste("Week", visits),
#                   AVISITN = visits,
#                   cured = FALSE) %>% put()
#  
#  put("Calculate cummulative sum and percent")
#  adslpsga_plot <- adslpsga_final %>%
#    select(ARM, AVISIT, AVISITN, cured) %>%
#    bind_rows(df) %>%
#    group_by(ARM, AVISIT, AVISITN) %>%
#    summarize(sumc = sum(cured)) %>%
#    arrange(ARM, AVISITN) %>%
#    group_by(ARM) %>%
#    mutate(AVISIT = ifelse(AVISIT == "Week 0", "Day 1 Baseline", AVISIT),
#           csumc = cumsum(sumc)) %>%
#    distinct() %>%
#    mutate(pct = case_when(ARM == "ARM A" ~ csumc / arm_pop["ARM A"],
#                           ARM == "ARM B" ~ csumc / arm_pop["ARM B"],
#                           ARM == "ARM C" ~ csumc / arm_pop["ARM C"],
#                           ARM == "ARM D" ~ csumc / arm_pop["ARM D"])) %>%
#    put()
#  
#  # Add factor to ensure sort order is correct
#  adslpsga_plot$AVISIT <- factor(adslpsga_plot$AVISIT,
#                                 levels = c("Day 1 Baseline",
#                                            "Week 2",
#                                            "Week 4",
#                                            "Week 6",
#                                            "Week 8",
#                                            "Week 12",
#                                            "Week 16"))
#  
#  
#  put("Generate plot")
#  surv_gg <- adslpsga_plot %>%
#    ggplot(mapping = aes(y = pct, x = AVISIT , group = ARM)) +
#    geom_point(aes(shape = ARM, color = ARM)) +
#    geom_step(aes(linetype = ARM, color = ARM)) +
#    scale_x_discrete(name = "Study Week") +
#    scale_y_continuous(name = "Proportion of Subjects with Initial Success")
#  
#  
#  
#  
#  
#  # Print Report ------------------------------------------------------------
#  
#  sep("Create and print report")
#  
#  
#  # Create Table 1 with header
#  tbl1 <- create_table(count_block, width = 9) %>%
#    column_defaults(from = `ARM A`, to = `ARM D`, align = "center", width = 1.1) %>%
#    define(block, visible = FALSE) %>%
#    define(label, label = "", width = 4.25) %>%
#    define(`ARM A`,  n = arm_pop["ARM A"]) %>%
#    define(`ARM B`,  n = arm_pop["ARM B"]) %>%
#    define(`ARM C`,  n = arm_pop["ARM C"]) %>%
#    define(`ARM D`,  n = arm_pop["ARM D"]) %>%
#    titles("Table 5.0", bold = TRUE, blank_row = "above") %>%
#    titles("Analysis of Time to Initial PSGA Success* in Weeks",
#           "Safety Population")
#  
#    label_lookup <-  c(surv = "Kaplan-Meier estimates",
#                       cox = "Results of Proportional Hazards Regression Analysis")
#  
#  # Create table 2 for statistics with stub and without header
#  tbl2 <- create_table(stat_block, width = 9, headerless = TRUE) %>%
#    column_defaults(from = `ARM A`, to = `ARM D`, align = "center", width = 1.1) %>%
#    stub(c(block, label1, label2), width = 4.25) %>%
#    define(block, label_row = TRUE, format = label_lookup, blank_after = TRUE) %>%
#    define(label1, indent = .25) %>%
#    define(label2, indent = .5) %>%
#    define(`ARM A`,  n = arm_pop["ARM A"]) %>%
#    define(`ARM B`,  n = arm_pop["ARM B"]) %>%
#    define(`ARM C`,  n = arm_pop["ARM C"]) %>%
#    define(`ARM D`,  n = arm_pop["ARM D"])
#  
#  
#  # Create plot
#  plt <- create_plot(surv_gg, 3.5, 9) %>%
#    titles("Figure 5.0", bold = TRUE, blank_row = "above") %>%
#    titles("Kaplan-Meier Plot for Time to Initial PSGA Success (PSGA <= 1)",
#           "Safety Population", blank_row = "none")
#  
#  put("Create Report")
#  # Add table 1, table 2, and plot content to the same report.
#  # Plot will be on a separate page with it's own title.
#  rpt <- create_report(file.path(tmp, "output/example5.rtf"), output_type = "RTF",
#                       font = "Arial", missing = "-") %>%
#    set_margins(top = 1, bottom = 1) %>%
#    page_header("Sponsor: Client", "Study: ABC/BBC") %>%
#    add_content(tbl1, page_break = FALSE) %>%
#    add_content(tbl2) %>%
#    add_content(plt) %>%
#    footnotes("Program: Surv_Table.R",
#              "* Success: PSGA <= 1: PSGA > 1",
#              "** Based on R survival package survfit() function",
#              paste("*** Based on proportional hazards model with treatment",
#                    "indicator variables as explanatory variables"),
#              paste("Note: The end-point is cure of the disease (PSGA <= 1)."),
#              "  The probability of remaining diseased (PSGA > 1) defines the survival function",
#              paste("Note: A subject who is not cured by the end of 12 weeks or is lost",
#              "to follow provides a censored observation for the analysis."),
#              "\"-\" = Not Applicable") %>%
#    page_footer(paste0("Date Produced: ", fapply(Sys.time(), "%d%b%y %H:%M")),
#                right = "Page [pg] of [tpg]")
#  
#  put("Write out the report")
#  res <- write_report(rpt)
#  
#  
#  # Clean Up ----------------------------------------------------------------
#  
#  sep("Clean Up")
#  
#  lib_unload(adam)
#  
#  # Close log
#  log_close()
#  
#  # View log
#  writeLines(readLines(lf, encoding = "UTF-8"))
#  
#  # View report
#  # file.show(res$file_path)
#  

