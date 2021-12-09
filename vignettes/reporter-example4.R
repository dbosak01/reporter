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
#  
#  # Prepare Log -------------------------------------------------------------
#  
#  
#  options("logr.autolog" = TRUE,
#          "logr.notes" = FALSE)
#  
#  # Get temp location for log and report output
#  tmp <- tempdir()
#  
#  # Open log
#  lf <- log_open(file.path(tmp, "example4.log"))
#  
#  
#  # Load and Prepare Data ---------------------------------------------------
#  
#  sep("Prepare Data")
#  
#  # Get path to sample data
#  pkg <- system.file("extdata", package = "reporter")
#  
#  # Define data library
#  libname(sdtm, pkg, "csv", quiet = TRUE)
#  
#  # Loads data into workspace
#  lib_load(sdtm)
#  
#  # Prepare data
#  dm_mod <- sdtm.DM %>%
#    select(USUBJID, SEX, AGE, ARM) %>%
#    filter(ARM != "SCREEN FAILURE") %>%
#    datastep({
#  
#      if (AGE >= 18 & AGE <= 24)
#        AGECAT = "18 to 24"
#      else if (AGE >= 25 & AGE <= 44)
#        AGECAT = "25 to 44"
#      else if (AGE >= 45 & AGE <= 64)
#        AGECAT <- "45 to 64"
#      else if (AGE >= 65)
#        AGECAT <- ">= 65"
#  
#    }) %>% put()
#  
#  put("Get population counts")
#  arm_pop <- count(dm_mod, ARM) %>% put()
#  sex_pop <- count(dm_mod, SEX) %>% put()
#  agecat_pop <- count(dm_mod, AGECAT) %>% put()
#  
#  # Convert agecat to factor so rows will sort correctly
#  agecat_pop$AGECAT <- factor(agecat_pop$AGECAT, levels = c("18 to 24",
#                                                            "25 to 44",
#                                                            "45 to 64",
#                                                            ">= 65"))
#  # Sort agecat
#  agecat_pop <- agecat_pop %>% arrange(AGECAT)
#  
#  
#  # Create Plots ------------------------------------------------------------
#  
#  
#  plt1 <- ggplot(data = arm_pop, aes(x = ARM, y = n)) +
#    geom_col(fill = "#0000A0") +
#    geom_text(aes(label = n), vjust = 1.5, colour = "white") +
#    labs(x = "Treatment Group", y = "Number of Subjects (n)")
#  
#  plt2 <- ggplot(data = sex_pop, aes(x = SEX, y = n)) +
#    geom_col(fill = "#00A000") +
#    geom_text(aes(label = n), vjust = 1.5, colour = "white") +
#    labs(x = "Biological Sex", y = "Number of Subjects (n)")
#  
#  plt3 <- ggplot(data = agecat_pop, aes(x = AGECAT, y = n)) +
#    geom_col(fill = "#A00000") +
#    geom_text(aes(label = n), vjust = 1.5, colour = "white") +
#    labs(x = "Age Categories", y = "Number of Subjects (n)")
#  
#  
#  # Report ------------------------------------------------------------------
#  
#  
#  sep("Create and print report")
#  
#  
#  page1 <- create_plot(plt1, 4.5, 7) %>%
#    titles("Figure 1.1", "Distribution of Subjects by Treatment Group")
#  
#  page2 <- create_plot(plt2, 4.5, 7) %>%
#    titles("Figure 1.2", "Distribution of Subjects by Biological Sex")
#  
#  page3 <- create_plot(plt3, 4.5, 7) %>%
#    titles("Figure 1.2", "Distribution of Subjects by Age Category")
#  
#  rpt <- create_report(file.path(tmp, "./output/example4.rtf"), output_type = "RTF",
#                       font = "Arial") %>%
#    set_margins(top = 1, bottom = 1) %>%
#    page_header("Sponsor: Company", "Study: ABC") %>%
#    add_content(page1) %>%
#    add_content(page2) %>%
#    add_content(page3) %>%
#    footnotes("Program: DM_Figure.R") %>%
#    page_footer(paste0("Date Produced: ", fapply(Sys.time(), "%d%b%y %H:%M")),
#                right = "Page [pg] of [tpg]")
#  
#  res <- write_report(rpt)
#  
#  
#  # Clean Up ----------------------------------------------------------------
#  sep("Clean Up")
#  
#  # Unload library from workspace
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
#  

