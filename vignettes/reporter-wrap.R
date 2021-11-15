## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(reporter)
#  
#  # Create temp file name
#  tmp <- file.path(tempdir(), "example6.txt")
#  
#  # Prepare data
#  dat <- mtcars[1:10, ]
#  dat <- data.frame(vehicle = rownames(dat), dat)
#  
#  # Define table
#  tbl <- create_table(dat, show_cols = 1:8) %>%
#    define(vehicle, label = "Vehicle", width = 3, id_var = TRUE, align = "left") %>%
#    define(mpg, label = "Miles per Gallon", width = 1) %>%
#    define(cyl, label = "Cylinders", format = "%.1f") %>%
#    define(disp, label = "Displacement") %>%
#    define(hp, label = "Horsepower", page_wrap = TRUE) %>%
#    define(drat, visible = FALSE) %>%
#    define(wt, label = "Weight") %>%
#    define(qsec, label = "Quarter Mile Time", width = 1.5)
#  
#  
#  # Create the report
#  rpt <- create_report(tmp, orientation = "portrait") %>%
#    titles("Listing 2.0", "MTCARS Data Listing with Page Wrap") %>%
#    add_content(tbl, align = "left") %>%
#    page_footer(right = "Page [pg] of [tpg]")
#  
#  # Write the report
#  write_report(rpt)
#  
#  # Send report to console for viewing
#  writeLines(readLines(tmp, encoding = "UTF-8"))
#  
#  #                                  Listing 2.0
#  #                       MTCARS Data Listing with Page Wrap
#  #
#  #                                         Miles per
#  # Vehicle                                    Gallon Cylinders Displacement
#  # ------------------------------------------------------------------------
#  # Mazda RX4                                      21       6.0          160
#  # Mazda RX4 Wag                                  21       6.0          160
#  # Datsun 710                                   22.8       4.0          108
#  # Hornet 4 Drive                               21.4       6.0          258
#  # Hornet Sportabout                            18.7       8.0          360
#  # Valiant                                      18.1       6.0          225
#  # Duster 360                                   14.3       8.0          360
#  # Merc 240D                                    24.4       4.0        146.7
#  # Merc 230                                     22.8       4.0        140.8
#  # Merc 280                                     19.2       6.0        167.6
#  #
#  # ...
#  #
#  #                                                                    Page 1 of 2
#  #                                  Listing 2.0
#  #                       MTCARS Data Listing with Page Wrap
#  #
#  # Vehicle                              Horsepower Weight  Quarter Mile Time
#  # -------------------------------------------------------------------------
#  # Mazda RX4                                   110   2.62              16.46
#  # Mazda RX4 Wag                               110  2.875              17.02
#  # Datsun 710                                   93   2.32              18.61
#  # Hornet 4 Drive                              110  3.215              19.44
#  # Hornet Sportabout                           175   3.44              17.02
#  # Valiant                                     105   3.46              20.22
#  # Duster 360                                  245   3.57              15.84
#  # Merc 240D                                    62   3.19                 20
#  # Merc 230                                     95   3.15               22.9
#  # Merc 280                                    123   3.44               18.3
#  #
#  # ...
#  #
#  #                                                                    Page 2 of 2
#  

