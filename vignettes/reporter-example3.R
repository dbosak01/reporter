## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)


## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(tidyverse)
#  library(sassy)
#  
#  options("logr.autolog" = TRUE,
#          "logr.notes" = FALSE)
#  
#  # Get temp location for log and report output
#  tmp <- tempdir()
#  
#  
#  # Get Data ----------------------------------------------------------------
#  
#  
#  # Open log
#  lf <- log_open(file.path(tmp, "example3.log"))
#  
#  sep("Prepare Data")
#  
#  # Get path to sample data
#  pkg <- system.file("extdata", package = "reporter")
#  
#  # Create libname for csv data
#  libname(sdtm, pkg, "csv", quiet = TRUE)
#  
#  # Load data into workspace
#  lib_load(sdtm)
#  
#  
#  sep("Prepare table data")
#  dat <- sdtm.AE %>%
#    inner_join(select(sdtm.DM, USUBJID, ARM, ARMCD),
#               c("USUBJID" = "USUBJID"),
#               keep = FALSE) %>%
#    filter(ARM != "SCREEN FAILURE")
#  
#  put("Get population counts")
#  arm_pop <- dat %>%
#    select(USUBJID, ARM) %>%
#    distinct() %>%
#    count(ARM)  %>%
#    deframe() %>% put()
#  
#  put ("Create lookup for AE severity")
#  sevn <- c(MILD = 1, MODERATE = 2, SEVERE = 3) %>% put()
#  
#  put("Subset ADAE for needed rows and columns")
#  df_sub <- dat %>%
#    mutate(AESEVN = sevn[AESEV]) %>%
#    select(USUBJID, ARM, AESEV, AESEVN, AESOC, AEDECOD) %>%
#    distinct() %>%
#    put()
#  
#  
#  # Perform Calculations ----------------------------------------------------
#  sep("Perform Calculations")
#  
#  put("Create template for needed ARM columns")
#  col_template <- paste0(c(rep("ARM A_", 3), rep("ARM B_", 3), rep("ARM C_", 3),
#                           rep("ARM D_", 3)), rep(c(1, 2, 3), 3))
#  
#  
#  
#  put("Identify and count highest severity grade for each subject")
#  df1 <- df_sub %>% arrange(USUBJID, AESOC, AEDECOD, AESEVN) %>%
#    group_by(USUBJID, ARM, AESOC, AEDECOD) %>%
#    mutate(max_sev = ifelse(row_number() == n(), TRUE, FALSE)) %>%
#    filter(max_sev == TRUE) %>%
#    group_by(ARM, AESOC, AEDECOD, AESEVN) %>%
#    summarize(cnt = n()) %>%
#    pivot_wider(names_from = c(ARM, AESEVN),
#                values_from = cnt,
#                values_fill = 0) %>%
#    put()
#  
#  
#  put("Fill in missing columns where there were no events.")
#  for (nm in col_template) {
#    if (!nm %in% names(df1))
#      df1[[nm]] <- 0
#  }
#  put(df1)
#  
#  put("Format counts and percents for each column")
#  df_events <- df1 %>%
#    transmute(AESOC = AESOC,
#              AEDECOD = str_to_title(AEDECOD),
#              `ARM A_1` = fmt_cnt_pct(`ARM A_1`, arm_pop["ARM A"]),
#              `ARM A_2` = fmt_cnt_pct(`ARM A_2`, arm_pop["ARM A"]),
#              `ARM A_3` = fmt_cnt_pct(`ARM A_3`, arm_pop["ARM A"]),
#              `ARM B_1` = fmt_cnt_pct(`ARM B_1`, arm_pop["ARM B"]),
#              `ARM B_2` = fmt_cnt_pct(`ARM B_2`, arm_pop["ARM B"]),
#              `ARM B_3` = fmt_cnt_pct(`ARM B_3`, arm_pop["ARM B"]),
#              `ARM C_1` = fmt_cnt_pct(`ARM C_1`, arm_pop["ARM C"]),
#              `ARM C_2` = fmt_cnt_pct(`ARM C_2`, arm_pop["ARM C"]),
#              `ARM C_3` = fmt_cnt_pct(`ARM C_3`, arm_pop["ARM C"]),
#              `ARM D_1` = fmt_cnt_pct(`ARM D_1`, arm_pop["ARM D"]),
#              `ARM D_2` = fmt_cnt_pct(`ARM D_2`, arm_pop["ARM D"]),
#              `ARM D_3` = fmt_cnt_pct(`ARM D_3`, arm_pop["ARM D"])) %>%
#    arrange(AESOC, AEDECOD) %>%
#    ungroup() %>%
#    put()
#  
#  
#  put("Get counts for All Adverse Events")
#  df2 <- df_sub %>%
#    select(USUBJID, ARM, AESEVN) %>%
#    arrange(USUBJID, AESEVN) %>%
#    group_by(USUBJID) %>%
#    mutate(max_sev = ifelse(row_number() == n(), TRUE, FALSE)) %>%
#    filter(max_sev == TRUE) %>%
#    group_by(ARM, AESEVN) %>%
#    summarize(cnt = n()) %>%
#    pivot_wider(names_from = c(ARM, AESEVN),
#                values_from = cnt,
#                values_fill = 0) %>%
#    ungroup() %>%
#    put()
#  
#  put("Fill in missing columns where there were no events.")
#  for (nm in col_template) {
#    if (!nm %in% names(df2))
#      df2[[nm]] <- 0
#  }
#  put(df2)
#  
#  put("Format counts and percents for all adverse events.")
#  df_all <- df2 %>%
#    transmute(AESOC = "All System Organ Classes",
#              AEDECOD = "All Adverse Events",
#              `ARM A_1` = fmt_cnt_pct(`ARM A_1`, arm_pop["ARM A"]),
#              `ARM A_2` = fmt_cnt_pct(`ARM A_2`, arm_pop["ARM A"]),
#              `ARM A_3` = fmt_cnt_pct(`ARM A_3`, arm_pop["ARM A"]),
#              `ARM B_1` = fmt_cnt_pct(`ARM B_1`, arm_pop["ARM B"]),
#              `ARM B_2` = fmt_cnt_pct(`ARM B_2`, arm_pop["ARM B"]),
#              `ARM B_3` = fmt_cnt_pct(`ARM B_3`, arm_pop["ARM B"]),
#              `ARM C_1` = fmt_cnt_pct(`ARM C_1`, arm_pop["ARM C"]),
#              `ARM C_2` = fmt_cnt_pct(`ARM C_2`, arm_pop["ARM C"]),
#              `ARM C_3` = fmt_cnt_pct(`ARM C_3`, arm_pop["ARM C"]),
#              `ARM D_1` = fmt_cnt_pct(`ARM D_1`, arm_pop["ARM D"]),
#              `ARM D_2` = fmt_cnt_pct(`ARM D_2`, arm_pop["ARM D"]),
#              `ARM D_3` = fmt_cnt_pct(`ARM D_3`, arm_pop["ARM D"])) %>%
#    put()
#  
#  
#  # Final Data --------------------------------------------------------------
#  
#  sep("Create final data frame")
#  
#  final <- bind_rows(df_all, df_events) %>% put()
#  
#  
#  # Print Report ----------------------------------------------------------
#  
#  sep("Create and print report")
#  
#  put("Create table object")
#  tbl <- create_table(final, first_row_blank = TRUE, width = 9) %>%
#    column_defaults(from = `ARM A_1`, to = `ARM D_3`, width = 1) %>%
#    spanning_header("ARM A_1", "ARM A_3", label = "ARM A", n = arm_pop["ARM A"]) %>%
#    spanning_header("ARM B_1", "ARM B_3", label = "ARM B", n = arm_pop["ARM B"]) %>%
#    spanning_header("ARM C_1", "ARM C_3", label = "ARM C", n = arm_pop["ARM C"]) %>%
#    spanning_header("ARM D_1", "ARM D_3", label = "ARM D", n = arm_pop["ARM D"]) %>%
#    stub(vars = c("AESOC", "AEDECOD"), label = "System Organ Class\n   Preferred Term", width = 5) %>%
#    define(AESOC, blank_after = TRUE, label_row = TRUE) %>%
#    define(AEDECOD, indent = .25) %>%
#    define(`ARM A_1`, align = "center", label = "Mild") %>%
#    define(`ARM A_2`, align = "center", label = "Moderate") %>%
#    define(`ARM A_3`, align = "center", label = "Severe") %>%
#    define(`ARM B_1`, align = "center", label = "Mild", page_wrap = TRUE) %>%
#    define(`ARM B_2`, align = "center", label = "Moderate") %>%
#    define(`ARM B_3`, align = "center", label = "Severe") %>%
#    define(`ARM C_1`, align = "center", label = "Mild", page_wrap = TRUE) %>%
#    define(`ARM C_2`, align = "center", label = "Moderate") %>%
#    define(`ARM C_3`, align = "center", label = "Severe") %>%
#    define(`ARM D_1`, align = "center", label = "Mild", page_wrap = TRUE) %>%
#    define(`ARM D_2`, align = "center", label = "Moderate") %>%
#    define(`ARM D_3`, align = "center", label = "Severe")
#  
#  
#  put("Create report object")
#  rpt <- create_report(file.path(tmp, "output/example3.rtf"), output_type = "RTF",
#                       font = "Arial") %>%
#    options_fixed(font_size = 10) %>%
#    page_header("Sponsor: Company", "Study: ABC") %>%
#    titles("Table 5.0", "Adverse Events by Maximum Severity", bold = TRUE,
#           font_size = 12) %>%
#    add_content(tbl) %>%
#    footnotes("Program: AE_Table.R",
#              "Note: Adverse events were coded using MedDRA Version 9.1") %>%
#    page_footer(Sys.time(), "Confidential", "Page [pg] of [tpg]")
#  
#  put("Print report")
#  res <- write_report(rpt) %>% put()
#  
#  
#  
#  # Clean Up ----------------------------------------------------------------
#  sep("Clean Up")
#  
#  # Remove library from workspace
#  lib_unload(sdtm)
#  
#  # Close log
#  log_close()
#  
#  
#  # View log
#  writeLines(readLines(lf, encoding = "UTF-8"))
#  
#  # View report
#  # file.show(res$file_path)
#  

