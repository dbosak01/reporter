## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)


## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(sassy)
#  library(magrittr)
#  
#  options("logr.autolog" = TRUE,
#          "logr.notes" = FALSE)
#  
#  # Get temp location for log and report output
#  tmp <- tempdir()
#  
#  # Open log
#  lf <- log_open(file.path(tmp, "example1.log"))
#  
#  # Get Data ----------------------------------------------------------------
#  sep("Get Data")
#  
#  # Get path to sample data
#  pkg <- system.file("extdata", package = "reporter")
#  
#  # Define data library
#  libname(sdtm, pkg, "csv")
#  
#  # Load library into workspace
#  lib_load(sdtm)
#  
#  # Write Report ------------------------------------------------------------
#  sep("Write Report")
#  
#  # Define table object
#  tbl <- create_table(sdtm.DM) %>%
#    define(USUBJID, id_var = TRUE)
#  
#  
#  # Define report object
#  rpt <- create_report(file.path(tmp, "output/example1.pdf"), output_type = "PDF") %>%
#    page_header("Sponsor: Company", "Study: ABC") %>%
#    titles("Listing 1.0", "SDTM Demographics") %>%
#    add_content(tbl, align = "left") %>%
#    page_footer(Sys.time(), "CONFIDENTIAL", "Page [pg] of [tpg]")
#  
#  # Write report to file system
#  res <- write_report(rpt)
#  
#  
#  # Clean Up ----------------------------------------------------------------
#  sep("Clean Up")
#  
#  # Unload data
#  lib_unload(sdtm)
#  
#  # Close log
#  log_close()
#  
#  # View log
#  writeLines(readLines(lf, encoding = "UTF-8"))
#  
#  # View report
#  # file.show(res$modified_path)
#  

