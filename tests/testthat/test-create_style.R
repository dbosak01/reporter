context("Create Style Tests")

test_that("cell_style works as expected.", {
  
  df <- read.table(header = TRUE, text = '
      var     label        A             B
      "ampg"   "N and more and more"          "19"          "13"
      "ampg"   "Mean and more and more"       "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Median and more and more"     "16.4"        "21.4"
      "ampg"   "Q1 - Q3 and more and more"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range and more and more"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder and more and more" "10 ( 52.6%)" "4 ( 30.8%)"
      "cyl"    "6 Cylinder and more and more" "4 ( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "4 Cylinder and more and more" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  df$cylflg <- ifelse(df$var == "cyl", T, FALSE)
  
  expect_error(
    ts <- create_table(df, first_row_blank = TRUE) %>%
      stub(c("var", "label"), width = 2.06, style = cell_style(bold = "bold"))
  )
  expect_error(
    ts <- create_table(df, first_row_blank = TRUE) %>%
      stub(c("var", "label"), width = 2.06, style = cell_style(italic = 3))
  )
  expect_error(
    ts <- create_table(df, first_row_blank = TRUE) %>%
      stub(c("var", "label"), width = 2.06, style = cell_style(borders = c("up", "bottom")))
  )
})

test_that("get_cell_styles works as expected.", {
  
  # With fixed width, bold text should be wrapped correct. No overflow
  df <- read.table(header = TRUE, text = '
      var     label        A             B
      "ampg"   "N and more and more"          "19"          "13"
      "ampg"   "Mean and more and more"       "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Median and more and more"     "16.4"        "21.4"
      "ampg"   "Q1 - Q3 and more and more"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range and more and more"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder and more and more" "10 ( 52.6%)" "4 ( 30.8%)"
      "cyl"    "6 Cylinder and more and more" "4 ( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "4 Cylinder and more and more" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  df$cylflg <- ifelse(df$var == "cyl", T, FALSE)
  
  # Create table
  ts <- create_table(df, first_row_blank = TRUE) %>%
    stub(c("var", "label"), width = 2.06, style = cell_style(bold = TRUE)) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = "Miles Per Gallon and more bold text to be added to test function", 
                      cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19,
           style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    define(B, label = "Group B", align = "center", n = 13,
           style = cell_style(bold = TRUE, indicator = "datarow")) %>%
    define(cylflg, visible = FALSE)
  
  # Create report and add content
  rpt <- create_report("", orientation = "portrait", output_type = "RTF",
                       font = "Times")
  
  rs <- page_setup_rtf(rpt)
  
  dat <- as.data.frame(ts$data, stringsAsFactors = FALSE)  
  dat$..blank <- ""
  fdat <- prep_data(dat, ts, rs$char_width, rs$missing)
  names(fdat)[names(fdat) == "cylflg"] <- "..x.cylflg"
  
  styles <- get_styles(ts)
  
  flgs <- fdat$..blank
  
  stl <- get_cell_styles("stub", styles, flgs, 6, fdat)
  expect_equal(stl, list(bold = T, italic = F, 
                         font_color = NULL, cell_color = NULL,
                         borders = NULL))
  
  stl <- get_cell_styles("A", styles, flgs, 3, fdat)
  expect_equal(stl, list(bold = F, italic = F, 
                         font_color = NULL, cell_color = NULL,
                         borders = NULL))
  
  stl <- get_cell_styles("A", styles, flgs, 9, fdat)
  expect_equal(stl, list(bold = T, italic = F, 
                         font_color = NULL, cell_color = NULL,
                         borders = NULL))
  
  stl <- get_cell_styles("B", styles, flgs, 5, fdat)
  expect_equal(stl, list(bold = T, italic = F, 
                         font_color = NULL, cell_color = NULL,
                         borders = NULL))
})

# test_that("get_cell_styles works with multiple styles as expected.", {
#   
#   # With fixed width, bold text should be wrapped correct. No overflow
#   df <- read.table(header = TRUE, text = '
#       var      label                          A             B
#       "ampg"   "N and more and more"          "19"          "13"
#       "ampg"   "Mean and more and more"       "18.8 (6.5)"  "22.0 (4.9)"
#       "ampg"   "Median and more and more"     "16.4"        "21.4"
#       "ampg"   "Q1 - Q3 and more and more"    "15.1 - 21.2" "19.2 - 22.8"
#       "ampg"   "Range and more and more"      "10.4 - 33.9" "14.7 - 32.4"
#       "cyl"    "8 Cylinder and more and more" "10 ( 52.6%)" "4 ( 30.8%)"
#       "cyl"    "6 Cylinder and more and more" "4 ( 21.1%)"  "3 ( 23.1%)"
#       "cyl"    "4 Cylinder and more and more" "5 ( 26.3%)"  "6 ( 46.2%)"')
#   
#   df$cylflg <- ifelse(df$var == "cyl", T, F)
#   df$ampgflg <- ifelse(df$var == "ampg", T, F)
#   
#   # Create table
#   ts <- create_table(df, first_row_blank = TRUE) %>%
#     stub(c("var", "label"), width = 2.06, 
#          style = cell_style(bold = c(TRUE, FALSE), 
#                             italic = c(TRUE, FALSE),
#                             cell_color = c("white", "blue"),
#                             indicator = c("labelrow", "blankrow"))) %>%
#     define(var, blank_after = TRUE, label_row = TRUE,
#            format = c(ampg = "Miles Per Gallon and more bold text to be added to test function", 
#                       cyl = "Cylinders")) %>%
#     define(label, indent = .25) %>%
#     define(A, label = "Group A", align = "center", n = 19,
#            style = cell_style(italic = c(TRUE, FALSE), 
#                               bold = c(TRUE, FALSE), 
#                               font_color = c("blue", "red"),
#                               cell_color = c("Light Gray", "Yellow"),
#                               borders = list(c("top", "bottom"), "all"),
#                               indicator = c("ampgflg","cylflg"))) %>%
#     define(B, label = "Group B", align = "center", n = 13,
#            style = cell_style(bold = TRUE, 
#                               font_color = "red", 
#                               borders = "left",
#                               indicator = "datarow")) %>%
#     define(cylflg, visible = FALSE) 
#   
#   # Create report and add content
#   rpt <- create_report("", orientation = "portrait", output_type = "RTF",
#                        font = "Times")
#   
#   rs <- page_setup_rtf(rpt)
#   
#   dat <- as.data.frame(ts$data, stringsAsFactors = FALSE)  
#   dat$..blank <- ""
#   fdat <- prep_data(dat, ts, rs$char_width, rs$missing)
#   names(fdat)[names(fdat) == "cylflg"] <- "..x.cylflg"
# 
#   styles <- get_styles(ts)
#   
#   flgs <- fdat$..blank
#   
#   # Test single style
#   stl <- get_cell_styles("stub", styles, flgs, 6, fdat)
#   expect_equal(stl, list(bold = FALSE, italic = FALSE, 
#                          font_color = NULL, cell_color = NULL,
#                          borders = NULL))
#   
#   stl <- get_cell_styles("stub", styles, flgs, 7, fdat) # blank row
#   expect_equal(stl, list(bold = FALSE, italic = FALSE, 
#                          font_color = NULL, cell_color = "blue",
#                          borders = NULL))
#   
#   stl <- get_cell_styles("stub", styles, flgs, 8, fdat) # label row
#   expect_equal(stl, list(bold = TRUE, italic = TRUE, 
#                          font_color = NULL, cell_color = "white",
#                          borders = NULL))
#   
#   stl <- get_cell_styles("B", styles, flgs, 11, fdat) # datarow
#   expect_equal(stl, list(bold = TRUE, italic = FALSE, 
#                          font_color = "red", cell_color = NULL,
#                          borders = list("left")))
#   
#   # Test multiple styles in one column
#   stl <- get_cell_styles("A", styles, flgs, 3, fdat) # ampgflg
#   expect_equal(stl, list(bold = TRUE, italic = TRUE, 
#                          font_color = "blue", cell_color = "Light Gray",
#                          borders = list(c("top", "bottom"))))
#   
#   stl <- get_cell_styles("A", styles, flgs, 10, fdat) # cylflg
#   expect_equal(stl, list(bold = FALSE, italic = FALSE, 
#                          font_color = "red", cell_color = "Yellow",
#                          borders = list("all")))
# })

test_that("get_cell_styles works with multiple styles as expected.", {
  
  # With fixed width, bold text should be wrapped correct. No overflow
  df <- read.table(header = TRUE, text = '
      var      label                          A             B
      "ampg"   "N and more and more"          "19"          "13"
      "ampg"   "Mean and more and more"       "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Median and more and more"     "16.4"        "21.4"
      "ampg"   "Q1 - Q3 and more and more"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range and more and more"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder and more and more" "10 ( 52.6%)" "4 ( 30.8%)"
      "cyl"    "6 Cylinder and more and more" "4 ( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "4 Cylinder and more and more" "5 ( 26.3%)"  "6 ( 46.2%)"')
  
  df$cylflg <- ifelse(df$var == "cyl", T, F)
  df$ampgflg <- ifelse(df$var == "ampg", T, F)
  
  # Create table
  tbl <- create_table(df, first_row_blank = TRUE) %>%
    stub(c("var", "label"), width = 2.06, 
         style = list(
           cell_style(bold = TRUE, 
                      italic = TRUE,
                      cell_color = "white",
                      indicator = "labelrow"),
           cell_style(bold = FALSE, 
                      italic = FALSE,
                      cell_color = "cyan",
                      indicator = "blankrow"),
           cell_style(bold = FALSE, 
                      italic = FALSE,
                      cell_color = "yellow",
                      indicator = NULL)
         )) %>%
    
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = "Miles Per Gallon and more bold text to be added to test function",
                      cyl = "Cylinders")) %>%
    
    define(label, indent = .25) %>%
    
    define(A, label = "Group A", align = "center", n = 19,
           style = list(
             cell_style(italic = TRUE,
                        bold = TRUE,
                        font_color = "blue",
                        cell_color = "Light Gray",
                        borders = c("top", "bottom"),
                        indicator = "ampgflg"),
             cell_style(italic = FALSE,
                        bold = FALSE,
                        font_color = "white",
                        cell_color = "olive",
                        borders = "all",
                        indicator = "cylflg")
           )) %>%
    
    define(B, label = "Group B", align = "center", n = 13,
           style = list(
             cell_style(bold = FALSE,
                        font_color = "red",
                        borders = "left",
                        indicator = "datarow"),
             cell_style(bold = TRUE,
                        font_color = "blue",
                        borders = "left",
                        indicator = "ampgflg")
           )) %>%
    define(cylflg, visible = FALSE)
  
  ts <- tbl
  
  # Create report and add content
  rpt <- create_report("", orientation = "portrait", output_type = "RTF",
                       font = "Times")
  
  rs <- page_setup_rtf(rpt)
  
  dat <- as.data.frame(ts$data, stringsAsFactors = FALSE)  
  dat$..blank <- ""
  fdat <- prep_data(dat, ts, rs$char_width, rs$missing)
  names(fdat)[names(fdat) == "cylflg"] <- "..x.cylflg"
  
  styles <- get_styles(ts)
  
  flgs <- fdat$..blank
  
  # Test single style
  stl <- get_cell_styles("stub", styles, flgs, 6, fdat)
  expect_equal(stl, list(bold = FALSE, italic = FALSE, 
                         font_color = NULL, cell_color = "yellow",
                         borders = NULL))
  
  stl <- get_cell_styles("stub", styles, flgs, 7, fdat) # blank row
  expect_equal(stl, list(bold = FALSE, italic = FALSE, 
                         font_color = NULL, cell_color = "cyan",
                         borders = NULL))
  
  stl <- get_cell_styles("stub", styles, flgs, 8, fdat) # label row
  expect_equal(stl, list(bold = TRUE, italic = TRUE, 
                         font_color = NULL, cell_color = "white",
                         borders = NULL))
  
  stl <- get_cell_styles("B", styles, flgs, 3, fdat) # ampgflg
  expect_equal(stl, list(bold = TRUE, italic = FALSE, 
                         font_color = "blue", cell_color = NULL,
                         borders = "left"))
  
  stl <- get_cell_styles("B", styles, flgs, 11, fdat) # datarow
  expect_equal(stl, list(bold = FALSE, italic = FALSE, 
                         font_color = "red", cell_color = NULL,
                         borders = "left"))
  
  # Test multiple styles in one column
  stl <- get_cell_styles("A", styles, flgs, 3, fdat) # ampgflg
  expect_equal(stl, list(bold = TRUE, italic = TRUE, 
                         font_color = "blue", cell_color = "Light Gray",
                         borders = c("top", "bottom")))
  
  stl <- get_cell_styles("A", styles, flgs, 10, fdat) # cylflg
  expect_equal(stl, list(bold = FALSE, italic = FALSE, 
                         font_color = "white", cell_color = "olive",
                         borders = "all"))
})

test_that("get_color_rtf works as expected.", {
  
  ret <- get_color_rtf("red")
  expect_equal(ret, "\\cf6 ")
  
  ret <- get_color_rtf("green", type = "cell")
  expect_equal(ret, "\\clcbpat11")
  
  expect_warning(get_color_rtf("nono"))
})
