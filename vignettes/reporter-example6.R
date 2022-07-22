## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)


## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(dplyr)
#  library(sassy)
#  library(magrittr)
#  
#  options("logr.notes" = FALSE,
#          "logr.autolog" = TRUE)
#  
#  # Get temp location for log and report output
#  tmp <- tempdir()
#  
#  lf <- log_open(file.path(tmp, "example6.log"))
#  
#  
#  # Get data ----------------------------------------------------------------
#  sep("Get data")
#  
#  
#  # Get sample data path
#  pth <- system.file("extdata", package = "reporter")
#  
#  
#  libname(sdtm, pth, "csv")
#  
#  
#  lib_load(sdtm)
#  
#  
#  
#  # Set labels --------------------------------------------------------------
#  sep("Set labels")
#  
#  put("DM labels")
#  labels(sdtm.DM) <- list(ARM = "Treatment Group",
#                          SITEID = "Centre",
#                          SUBJID = "Subject",
#                          SEX = "Sex",
#                          AGE = "Age (yrs)",
#                          RACE = "Race",
#                          BRTHDTC = "Birth Date",
#                          ARMCD = "Treatment Code"
#                          ) %>% put()
#  put("AE labels")
#  labels(sdtm.AE) <- list(AESTDTC = "Event Start Date",
#                          AEENDTC = "Event Stop Date",
#                          AESTDY = "Start",
#                          AEENDY = "End",
#                          AESOC = "System Organ Class",
#                          AESEV = "Severityᵃ",
#                          AESER = "Serious",
#                          AEREL = "Related") %>% put()
#  
#  
#  # Apply formats ----------------------------------------------------------
#  sep("Apply formats")
#  
#  sevfmt <- value(condition(x == "MODERATE", "Moderate"),
#                  condition(x == "SEVERE", "Severe"),
#                  condition(x == "MILD", "Mild")) %>% put()
#  
#  
#  relfmt <- value(condition(x == "RELATED", "Yes"),
#                  condition(x == "NOT RELATED", "No"),
#                  condition(x == "PROBABLY RELATED", "Probably"),
#                  condition(x == "POSSIBLY RELATED", "Possibly")) %>% put()
#  
#  serfmt <- value(condition(x == "N", "No"),
#                  condition(x == "Y", "Yes")) %>% put()
#  
#  
#  sexfmt <- value(condition(x == "M", "Male"),
#                  condition(x == "F", "Female"),
#                  condition(TRUE, "Unknown")) %>% put()
#  
#  racefmt <- value(condition(x == "WHITE", "White"),
#                   condition(x == "BLACK OR AFRICAN AMERICAN", "Black"),
#                   condition(x == "UNKNOWN", "Unknown"),
#                   condition(x == "ASIAN", "Asian")) %>% put()
#  
#  armfmt <- value(condition(x == "ARM A", "Placebo"),
#                  condition(x == "ARM B", "Dose 50mg"),
#                  condition(x == "ARM C", "Dose 100mg"),
#                  condition(x == "ARM D", "Competitor")) %>% put()
#  
#  
#  formats(sdtm.DM) <- list(SEX = sexfmt,
#                           RACE = racefmt,
#                           ARM = armfmt)
#  
#  formats(sdtm.AE) <- list(AESEV = sevfmt,
#                           AEREL = relfmt,
#                           AESER = serfmt)
#  
#  
#  # Prepare data ------------------------------------------------------------
#  
#  sep("Prepare data")
#  
#  dm <- sdtm.DM %>%
#    select(USUBJID, ARM, SITEID, SUBJID, SEX, AGE, RACE, BRTHDTC) %>% put()
#  
#  # Split dm data by subject id
#  dmlst <- split(dm, factor(dm$USUBJID))
#  
#  
#  ae <- sdtm.AE %>%
#    select(USUBJID, AESTDTC, AEENDTC, AESTDY,
#           AEENDY, AESOC, AESEV, AESER, AEREL) %>% put()
#  
#  # Split ae data by subject id
#  aelst <- split(ae, factor(ae$USUBJID))
#  
#  
#  # Create report -----------------------------------------------------------
#  sep("Create report")
#  
#  # Create report first, outside loop
#  rpt <- create_report(file.path(tmp, "output/example6.docx"),
#                       font = "Arial", output_type = "DOCX")
#  
#  # Loop on subjects
#  for (id in names(dmlst)) {
#  
#    dm_sub <- dmlst[[id]]
#    ae_sub <- aelst[[id]]
#  
#    tb1 <- create_table(dm_sub, width = 8, borders = "outside") %>%
#      titles("Listing 1.1 Subjects Narratives of Adverse Events",
#             paste0("Subject: ", dm_sub[[1, 1]]), bold = TRUE) %>%
#      define(AGE, align = "left")
#  
#    if (!is.null(ae_sub)) {
#      tb2 <- create_table(ae_sub, borders = "outside", width = 8) %>%
#        spanning_header("AESTDY", "AEENDY", label ="Study Day") %>%
#        define(USUBJID, visible = FALSE) %>%
#        define(AESOC, width = 2) %>%
#        define(AESTDY, align = "left") %>%
#        define(AEENDY, align = "left") %>%
#        footnotes("ᵃSeverity: 01=Mild, 02=Moderate, 03=Severe, 04=Life Threatening, 05=Fatal",
#                  paste0("ᵇAction Taken: 01=None, 02=Investigational product dose altered, ",
#                         "03=Medication taken, 04=Hospitalized, 05=Removed from study, ",
#                         "06=Investigational product discontinued, 07=Transfusion performed,",
#                         "88=Other"))
#    }
#  
#    # Append table content
#    rpt <- rpt %>%  add_content(tb1, page_break = FALSE) %>%
#            add_content(tb2)
#  
#  }
#  
#  put("Write out report")
#  res <- write_report(rpt)
#  
#  
#  # Clean Up ----------------------------------------------------------------
#  
#  
#  sep("Clean up")
#  
#  lib_unload(sdtm)
#  
#  log_close()
#  
#  
#  # View log
#  # file.show(lf)
#  
#  # View report
#  # file.show(res$modified_path)
#  

