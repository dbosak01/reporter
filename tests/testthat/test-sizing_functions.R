context("Sizing Functions Tests")

options("logr.output" = FALSE)

test_that("get_table_cols() works as expected.", {
  
  control_cols <- c("..blank", "..page", "..row", "..page_by")
  
  tbl <- create_table(mtcars[1:10, ], show_cols = "none") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst1 <- get_table_cols(tbl, control_cols)
  
  expect_equal(lst1, c("mpg", "cyl", "hp", control_cols))
  
  tbl2 <- create_table(mtcars[1:10, ], show_cols = "all") %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst2 <- get_table_cols(tbl2, control_cols)
  
  expect_equal(length(lst2), 11 + length(control_cols))
  
  tbl3 <- create_table(mtcars[1:10, ], show_cols = c("mpg", "disp", "wt")) %>% 
    define(mpg, format = "%.1f") %>% 
    define(cyl, width = 1) %>% 
    define(hp)
  
  lst3 <- get_table_cols(tbl3, control_cols)
  
  expect_equal(lst3, c("mpg", "disp", "wt", "cyl", "hp", control_cols))
  
  
})



test_that("get_page_breaks works as expected", {
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  dat <- iris
  dat$..page <- NA
  mdm <- get_page_breaks(dat, 50, 0, off)
  
  expect_equal( unique(mdm$..page), c(1, 2, 3))
  
  
})

test_that("get_page_breaks with count_row_var works as expected", {
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  dat <- iris[1:10,]
  dat$..page <- NA
  dat$..row <- c(rep(1,9), 3)
  mdm <- get_page_breaks(dat, 10, 0, off, count_row_var = TRUE)
  
  expect_equal(mdm$..page, c(rep(1,9), 2))
})

test_that("get_page_breaks with group_cohesion works as expected", {
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  dat <- iris[1:20,]
  
  dat$..group_cohesion <- c(
    rep("A", 4),
    rep("B", 2),
    rep("C", 3),
    rep("D", 4),
    rep("E", 7)
  )
  
  dat$..row <- c(
    rep(2, 4),
    rep(2, 2),
    c(3, 3, 1),
    c(rep(2, 4)),
    c(1, rep(2, 6))
  )
  
  dat$..page <- NA
  
  mdm <- get_page_breaks(dat, 10, 0, off, count_row_var = TRUE,
                         group_cohesion = "..group_cohesion")
  
  # test results variable for review
  mdm$test_group_cnt <- get_group_count(mdm$..group_cohesion, mdm$..row)
  mdm$cum_lines <- unlist(tapply(mdm$..row, mdm$..page, cumsum))

  expect_equal(mdm$..page, c(1,1,1,1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5))
})

test_that("get_page_breaks with group_cohesion and min_page_prop works as expected", {
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  dat <- iris[1:20,]
  
  dat$..group_cohesion <- c(
    rep("A", 4),
    rep("B", 2),
    rep("C", 3),
    rep("D", 4),
    rep("E", 7)
  )
  
  dat$..row <- c(
    rep(2, 4),
    rep(2, 2),
    c(3, 3, 1),
    c(rep(2, 4)),
    c(1, rep(2, 6))
  )
  
  dat$..page <- NA
  
  mdm <- get_page_breaks(dat, 10, 0, off, count_row_var = TRUE,
                         group_cohesion = "..group_cohesion",
                         min_page_prop = 0.4)
  
  # test results variable for review
  mdm$test_group_cnt <- get_group_count(mdm$..group_cohesion, mdm$..row)
  mdm$cum_lines <- unlist(tapply(mdm$..row, mdm$..page, cumsum))
  
  expect_equal(mdm$..page, c(1,1,1,1,2,2,3,3,3,4,4,4,4,4,5,5,5,5,5,6))
})

test_that("get_page_breaks with userPage=TRUE works as expected", {
  
  dat <- iris[1:100, ]
  
  dat$Species <- as.character(dat$Species)
  dat$Species[1:25] <- rep("setosa1", 25)
  dat$Species[26:50] <- rep("setosa2", 25)
  
  dat$..page <- dat$Species
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)

  # Without userPage
  without_userPage <- get_page_breaks(dat, 24, 0, off)
  
  # With userPage
  with_userPage <- get_page_breaks(dat, 24, 0, off, userPage = TRUE)

  expect_equal( unique(without_userPage$..page), c(1, 2, 3, 4, 5, 6, 7))
  expect_equal( unique(with_userPage$..page), c(1, 2, 3))
  
})


test_that("get_splits_text works as expected", {
  
  
  off <- c(upper = 0, lower = 0, blank_upper = 0, blank_lower = 0)
  
  dat <- iris
  dat$..page <- NA
  dat$..blank <- ""
  w <- c(Sepal.Length = 1.5, Sepal.Width = 1.5, Petal.Length = 1.5,
         Petal.Width = 1.5, Species = 1.25)
  
  mdm <- get_splits_text(dat, w, 50, 0, off, list())
  
  expect_equal(length(mdm), 3)
  expect_equal(nrow(mdm[[1]]), 50)
  expect_equal(nrow(mdm[[2]]), 50)
  expect_equal(nrow(mdm[[3]]), 50)
  
  
  mdm2 <- get_splits_text(dat, w, 60, 0, off, list())
  expect_equal(length(mdm2), 3)
  expect_equal(nrow(mdm2[[1]]), 60)
  expect_equal(nrow(mdm2[[2]]), 60)
  expect_equal(nrow(mdm2[[3]]), 30)
  
  mdm <- get_splits_text(dat, w, 50, 10, off, list())
  
  expect_equal(length(mdm), 4)
  expect_equal(nrow(mdm[[1]]), 40)
  expect_equal(nrow(mdm[[2]]), 50)
  expect_equal(nrow(mdm[[3]]), 50)
  expect_equal(nrow(mdm[[4]]), 10)
    
})



test_that("prep_data works as expected", {
  
   mw <- max(nchar(as.character(iris$Species)))
  
  
  tbl <- create_table(iris) %>% 
    define(Species, blank_after = TRUE, blank_before = TRUE,
           dedupe = TRUE, indent = .25)

  
  d <- prep_data(iris, tbl, .0833333, "")

  
  expect_equal(sum(d$..blank != ""), 6)
  # Indendation is now processed after prep_data
  expect_equal(max(nchar(d$Species)) == mw, TRUE)
  
})


test_that("get_labels works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    stub(c("mpg_cat", "name"), label = "Stub") %>% 
    define(cyl, label = "Cylinders") %>% 
    define(disp, label = "Displacement")
  
  lbls <- get_labels(dat, tbl)
  
  expect_equal(lbls[["cyl"]], "Cylinders")
  expect_equal(lbls[["hp"]], "hp")
  expect_equal(lbls[["disp"]], "Displacement")
  expect_equal(lbls[["stub"]], "Stub")
  
})

test_that("get_aligns works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    stub(c("mpg_cat", "name"), label = "Stub") %>% 
    define(cyl, label = "Cylinders", align = "right") %>% 
    define(disp, label = "Displacement", align = "center")
  
  algn <- get_aligns(dat, tbl)
  
  expect_equal(algn[["cyl"]], "right")
  expect_equal(algn[["stub"]], "left")
  expect_equal(algn[["disp"]], "center")

  
})

test_that("get_label_aligns works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    stub(c("mpg_cat", "name"), label = "Stub", label_align = "center") %>% 
    define(cyl, label = "Cylinders", label_align = "left") %>% 
    define(disp, label = "Displacement", label_align = "center")
  
  ls <- rep("right", 13) 
  names(ls) <- names(dat)
  
  lbls <- get_label_aligns(tbl, ls)
  
  expect_equal(lbls[["cyl"]], "left")
  expect_equal(lbls[["hp"]], "right")
  expect_equal(lbls[["disp"]], "center")
  expect_equal(lbls[["stub"]], "center")
  
})

test_that("get_col_formats works as expected", {
  
  dat <- mtcars
  dat$name <- rownames(mtcars)
  
  fmt <- value(condition(x >= 20, "High"),
               condition(TRUE, "Low"))
  
  dat$mpg_cat <- fapply(dat$mpg, fmt)
  
  dat
  
  tbl <- create_table(dat) %>% 
    define(cyl, label = "Cylinders", format = "%.1f") %>% 
    define(disp, label = "Displacement", format = "%.2f")
  
  lbls <- get_col_formats(dat, tbl)
  
  
  expect_equal(lbls[["cyl"]], "%.1f")
  expect_equal(is.null(lbls[["hp"]]), TRUE)
  expect_equal(lbls[["disp"]], "%.2f")

  
})

test_that("get_page_wraps works as expected.", {
  
  
  tbl <- create_table(mtcars) %>% 
    define(mpg, width = 1) %>% 
    define(cyl, width = 1) %>% 
    define(disp, width = 1) %>% 
    define(hp, width = 1) %>% 
    define(drat, width = 1) %>% 
    define(wt, width = 1)
  
  wdths <- rep(1, 11)
  names(wdths) <- names(mtcars)

  res <- get_page_wraps(4, tbl, wdths, .2, c())
    
  expect_equal(length(res), 4)
  expect_equal(all(c("mpg", "cyl", "disp") %in% res[[1]]), TRUE)
  expect_equal(all(c("mpg", "cyl", "disp") %in% res[[2]]), FALSE)

  
})


test_that("get_col_widths_variable works as expected.", {
  
  # dat, ts, labels, font, 
  # font_size, uom, gutter_width
  
  df <- mtcars
  
  tbl <- create_table(df) %>% 
    define(mpg, label = "Miles Per Gallon") %>% 
    define(cyl, width = 1.5) %>% 
    define(disp, width = 2) 

  lbls <- get_labels(df, tbl)
    
  res <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2)
  res
  
  expect_equal(res[["mpg"]] < 1, TRUE)
  expect_equal(res[["cyl"]], 1.5)
  expect_equal(res[["disp"]], 2)
  expect_equal(res[["hp"]] < .5, TRUE)
  
  
})

test_that("get_col_widths_variable works with indentation as expected.", {
  
  # dat, ts, labels, font, 
  # font_size, uom, gutter_width
  
  df <- mtcars
  
  tbl <- create_table(df) %>% 
    define(mpg, label = "Miles Per Gallon", indent = 1) %>% 
    define(cyl, width = 1.5) %>% 
    define(disp, width = 2) 
  
  lbls <- get_labels(df, tbl)
  
  res <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2)
  res
  
  expect_equal(res[["mpg"]] > 1, TRUE)
  expect_equal(res[["cyl"]], 1.5)
  expect_equal(res[["disp"]], 2)
  expect_equal(res[["hp"]] < .5, TRUE)
  
  
})

test_that("get_col_widths_variable works with stub as expected.", {
  
  # dat, ts, labels, font, 
  # font_size, uom, gutter_width
  
  df <- mtcars[, c("mpg", "cyl", "disp")]
  
  tbl <- create_table(df) %>% 
    stub(c("mpg", "cyl")) %>%
    define(mpg, label = "", label_row = TRUE) %>% 
    define(cyl, label = "") %>% 
    define(disp, width = 2) 
  
  lbls <- get_labels(df, tbl)
  
  fdat <- prep_data(df, tbl, 0.11, "")
  
  res <- get_col_widths_variable(fdat, tbl, lbls, "Arial", 12, "inches", .2)
  res
  
  expect_equal(res[["stub"]] < 1, TRUE)
  expect_equal(res[["disp"]], 2)
  
})

test_that("get_col_widths_variable works with stub and indentation as expected.", {
  
  # dat, ts, labels, font, 
  # font_size, uom, gutter_width
  
  df <- mtcars[, c("mpg", "cyl", "disp")]
  
  tbl <- create_table(df) %>% 
    stub(c("mpg", "cyl")) %>%
    define(mpg, label = "", label_row = TRUE) %>% 
    define(cyl, label = "", indent = 1) %>% 
    define(disp, width = 2) 
  
  lbls <- get_labels(df, tbl)
  
  fdat <- prep_data(df, tbl, 0.11, "")
  
  res <- get_col_widths_variable(fdat, tbl, lbls, "Arial", 12, "inches", .2)
  res
  
  expect_equal(res[["stub"]] > 1, TRUE)
  expect_equal(res[["disp"]], 2)
  
})

test_that("get_col_widths_variable works with allow_rtf_code as expected.", {
  
  df <- mtcars[, c("mpg", "cyl", "disp")]
  
  df$disp <- paste0("\\clbrdrb\\brdrs\\clbrdrl\\brdrs\\clbrdrr\\brdrs\\cellx3413\\ql ", df$disp)
  
  tbl <- create_table(df) %>% 
    define(mpg, label = "Miles Per Gallon", indent = 1) %>% 
    define(cyl, width = 1.5) %>% 
    define(disp, width = 2) 
  
  lbls <- get_labels(df, tbl)
  
  res_old <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2)
  res_new <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2,
                                     allow_rtf_code = TRUE)
  
  expect_equal(res_old[["disp"]] > res_new[["disp"]], TRUE)
  expect_equal(res_new[["disp"]], 2)
})

test_that("get_col_widths_variable works with allow_html_code as expected.", {
  
  df <- data.frame(
    # 1. Standard HTML tags (mainstream format)
    "test_01" = "<p>This is a standard paragraph.</p>",
    "test_02" = "Hello <br/> World",                                # Self-closing tag
    "test_03" = "<div class='container' id='main'>Content</div>",   # Tags with attributes
    "test_04" = "<script type='text/javascript'>alert(1);</script>", # Malicious script tag
    "test_05" = "<!-- This is a comment -->",                        # HTML Comment
    
    # 2. HTML Character Entities Containing "&"
    "test_06" = "HTML space&nbsp;test",                              # Webpage whitespace
    "test_07" = "If A &lt; B and B &gt; C",                          # Mathematical symbol escaping
    "test_08" = "Copyright &copy; 2026",                             # Special symbols
    "test_09" = "Registered &#174; trademark",                       # Digital entity
    
    # 3. The & symbol in a URL
    "test_10" = "<a href='https://test.com'>Link</a>",   # URLs within HTML tags
    "test_11" = "https://test.com",                      # Plain-text URL (no HTML)
    
    # 4. "Plain text" and "mathematical symbols" prone to misinterpretation (no HTML)
    "test_12" = "This is normal text without any code.",
    "test_13" = "Formula: x < 5 and y > 10",                         # Arrows easily mistaken for labels
    "test_14" = "Research & Development Department",                 # Business Text-Only &
    "test_15" = "Vector assignment in R: x <- c(1, 2)",              # The assignment arrow in R
    "test_16" = "Email: person@company.com",                        # Contains special characters but is not HTML
    
    # 5. Extreme and ineffective conditions
    "test_17" = "",                                                  # Empty string
    "test_18" = "    \n    \t    ",                                  # Only line breaks and whitespace.
    "test_19" = "<invalid_tag>Is this HTML?</invalid_tag>",          # Custom/Invalid Tags
    "test_20" = "<p>Unclosed tag rendering test"                     # Unclosed tags
  )
  
  tbl <- create_table(df)
  
  lbls <- get_labels(df, tbl)
  
  # Only test 5 and 11-18 should be the same widths
  res_old <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2)
  res_new <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2,
                                     allow_html_code = TRUE)
  
  expect_equal(as.numeric(which(res_old == res_new)), c(5, 11:18))
  expect_equal(as.numeric(which(res_old > res_new)), c(1:4, 6:10, 19, 20))
})
 
test_that("get_col_widths_variable works with percentage width as expected.", {
  
  df <- mtcars
  
  tbl <- create_table(df) %>% 
    define(mpg, label = "Miles Per Gallon") %>% 
    define(cyl, width = 1.5) %>% 
    define(disp, width = "20%") 
  
  lbls <- get_labels(df, tbl)
  
  res <- get_col_widths_variable(df, tbl, lbls, "Arial", 12, "inches", .2, content_width = 8)
  res
  
  expect_equal(res[["disp"]], 1.6)
  
  # 100% doesn't exceed the content width
  dat <- iris
  
  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = "20.17%", align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = "20.17%", align = "centre") %>%
    define(Petal.Length, label = "Petal Length", width = "20.17%", align = "centre") %>%
    define(Petal.Width, label = "Petal.Width", width = "20.17%", align = "centre") %>%
    define(Species, width = "19.32%", blank_after = TRUE)
  
  total_width <- round(9*0.2017, 2) * 4 + round(9*0.1932, 2)
  # The total width is 9.02, which is greater than 9
  # The algorithm needs to prevent from total width exceeding 9
  lbls <- get_labels(dat, tbl)
  res <- get_col_widths_variable(dat, tbl, lbls, "Arial", 12, "inches", .2, content_width = 9)
  
  expect_equal(sum(res), 9)
})

test_that("get_col_widths_variable works with stub percentage width as expected.", {
  
  fp <- ""
  
  dat <- iris[1:50,]
  dat$Group <- c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10))
  dat$Species <- as.character(dat$Species)
  
  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    stub(c("Group", "Species"), width = "20%") %>%
    define(Group, label_row = T, blank_after = T) %>%
    define(Sepal.Length, label = "Sepal Length", width = "20%", align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = "20%", align = "centre") %>%
    define(Petal.Length, label = "Petal Length", width = "20%", align = "centre") %>%
    define(Petal.Width, label = "Petal.Width", width = "20%", align = "centre")
  
  rpt <- create_report(fp, output_type = "RTF", font = "Arial",
                       font_size = 12, orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1)
  
  lbls <- get_labels(dat, tbl)
  
  rs <- page_setup_rtf(rpt)
  
  fdat <- prep_data(dat, tbl, rs$char_width, rs$missing)
  
  res <- get_col_widths_variable(fdat, tbl, lbls, "Arial", 12, "inches", .2, content_width = 9)
  
  expect_equal(sum(res), 9)
})

test_that("get_col_widths_variable works with Chinese as expected.", {

  df <- read.table(header = TRUE, text = '
      group1   group2            trt1         trt2         subgroup
      "性别"   "男"              "75 (51.7)"  "91 (59.5)"  "≥65岁"
      "性别"   "女"              "70 (48.3)"  "62 (40.5)"  "≥65岁"
      "年龄"   "例数"            "145"        "153"        "≥65岁"
      "年龄"   "平均数"          "64.7"       "65.8"       "≥65岁"
      "年龄"   "中位数"          "65.0"       "66.0"       "≥65岁"
      "年龄"   "标准差"          "9.7"        "8.3"        "≥65岁"
      "年龄"   "最小值, 最大值"  "40, 83"     "43.85"      "≥65岁"')
  
  tbl <- create_table(df) %>% 
    define(trt1, label = "试验药物一")
  
  lbls <- get_labels(df, tbl)
  
  res <- get_col_widths_variable(df, tbl, lbls, "SimSun", 10, "inches", .2,
                                 content_width = 8)
  res
  
  expect_equal(res[["group2"]] == 1.15, TRUE)
})

test_that("get_col_widths_variable works with bold text as expected.", {
  
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
  tbl <- create_table(df, first_row_blank = TRUE) %>%
    column_defaults(vars = c("stub", "A"),
                    style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    stub(c("var", "label"),
         style = cell_style(bold = TRUE, indicator = "datarow")) %>%
    define(var, blank_after = TRUE, label_row = TRUE,
           format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>%
    define(label, indent = .25) %>%
    define(A, label = "Group A", align = "center", n = 19,
           style = cell_style(bold = TRUE, indicator = cylflg)) %>%
    define(B, label = "Group B", align = "center", n = 13,
           style = cell_style(bold = TRUE, indicator = "datarow")) %>%
    define(cylflg, visible = FALSE)
  
  rpt <- create_report("", orientation = "portrait", output_type = "RTF",
                       font = "Times")
  
  rs <- page_setup_rtf(rpt)
  
  fdat <- prep_data(df, tbl, rs$char_width, rs$missing)
  
  lbls <- get_labels(df, tbl)
  
  styles <- get_styles(tbl)
  
  names(fdat)[names(fdat) == "cylflg"] <- "..x.cylflg"
  
  # Width considering bold
  res <- get_col_widths_variable(fdat, tbl, lbls, 
                                 rs$font, rs$font_size, rs$units, 
                                 rs$gutter_width,
                                 allow_rtf_code = rs$allow_code,
                                 content_width = rs$content_size[["width"]],
                                 styles = styles)
  res
  
  # Width not considering bold
  res2 <- get_col_widths_variable(fdat, tbl, lbls, 
                                 rs$font, rs$font_size, rs$units, 
                                 rs$gutter_width,
                                 allow_rtf_code = rs$allow_code,
                                 content_width = rs$content_size[["width"]])
  res2
  
  expect_true(res[["stub"]] > res2[["stub"]])
  expect_true(res[["A"]] > res2[["A"]])
  expect_true(res[["B"]] > res2[["B"]])
})

test_that("get_col_widths_variable works with differnt bold styles in one column as expected.", {
  
  # The longest A is in ampg
  df <- read.table(header = TRUE, text = '
      var      label                          A             B
      "ampg"   "N and more and more"          "19"          "13"
      "ampg"   "Mean and more and more"       "18.8 (6.5)"  "22.0 (4.9)"
      "ampg"   "Median and more and more"     "16.4"        "21.4"
      "ampg"   "Q1 - Q3 and more and more"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range and more and more"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder and more and more" "10( 52.6%)" "4 ( 30.8%)"
      "cyl"    "6 Cylinder and more and more" "4( 21.1%)"  "3 ( 23.1%)"
      "cyl"    "4 Cylinder and more and more" "5( 26.3%)"  "6 ( 46.2%)"')
  
  df$cylflg <- ifelse(df$var == "cyl", T, F)
  df$ampgflg <- ifelse(df$var == "ampg", T, F)
  
  # Create table
  ts <- create_table(df, first_row_blank = TRUE) %>%
    stub(c("var", "label"), width = 2.06, 
         style = list(
           cell_style(bold = TRUE, 
                      italic = TRUE,
                      cell_color = "white",
                      indicator = "labelrow"),
           cell_style(bold = FALSE, 
                      italic = FALSE,
                      cell_color = "blue",
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
                        font_color = "red",
                        cell_color = "Yellow",
                        borders = "all",
                        indicator = "cylflg"),
             cell_style(italic = FALSE,
                        bold = FALSE),
             cell_style(italic = FALSE,
                        bold = FALSE,
                        indicator = "datarow")
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
  
  # Create report and add content
  rpt <- create_report("", orientation = "portrait", output_type = "RTF",
                       font = "Times")
  
  rs <- page_setup_rtf(rpt)
  
  tbl <- ts
  
  dat <- as.data.frame(ts$data, stringsAsFactors = FALSE)  
  dat$..blank <- ""
  fdat <- prep_data(dat, ts, rs$char_width, rs$missing)
  names(fdat)[names(fdat) == "cylflg"] <- "..x.cylflg"
  
  styles <- get_styles(ts)
  lbls <- get_labels(df, tbl)
  flgs <- fdat$..blank
  

  
  styles_orig <- styles
  
  # Only bold A when ampg
  res <- get_col_widths_variable(fdat, tbl, lbls, 
                                 rs$font, rs$font_size, rs$units, 
                                 rs$gutter_width,
                                 allow_rtf_code = rs$allow_code,
                                 content_width = rs$content_size[["width"]],
                                 styles = styles)
  res # A: 0.80
  expect_equal(as.numeric(res["A"]), 0.8)
  
  # Only bold A when cylflg
  styles$A[[1]]$bold <- F
  styles$A[[2]]$bold <- T
  res2 <- get_col_widths_variable(fdat, tbl, lbls, 
                                 rs$font, rs$font_size, rs$units, 
                                 rs$gutter_width,
                                 allow_rtf_code = rs$allow_code,
                                 content_width = rs$content_size[["width"]],
                                 styles = styles)
  res2 # A: 0.79
  expect_equal(as.numeric(res2["A"]), 0.79)
  
  # All bold A
  styles$A[[1]]$bold <- T
  styles$A[[2]]$bold <- T
  res3 <- get_col_widths_variable(fdat, tbl, lbls, 
                                  rs$font, rs$font_size, rs$units, 
                                  rs$gutter_width,
                                  allow_rtf_code = rs$allow_code,
                                  content_width = rs$content_size[["width"]],
                                  styles = styles)
  res3 # A: 0.80
  expect_equal(as.numeric(res3["A"]), 0.8)
  
  # All bold A
  styles$A[[1]]$bold <- T
  styles$A[[2]]$bold <- T
  styles$A[[3]]$bold <- F
  res3_2 <- get_col_widths_variable(fdat, tbl, lbls, 
                                  rs$font, rs$font_size, rs$units, 
                                  rs$gutter_width,
                                  allow_rtf_code = rs$allow_code,
                                  content_width = rs$content_size[["width"]],
                                  styles = styles)
  res3_2 # A: 0.80
  expect_equal(as.numeric(res3_2["A"]), 0.8)
  
  # All not bold A
  styles$A[[1]]$bold <- F
  styles$A[[2]]$bold <- F
  styles$A[[3]]$bold <- T
  res3_3 <- get_col_widths_variable(fdat, tbl, lbls, 
                                    rs$font, rs$font_size, rs$units, 
                                    rs$gutter_width,
                                    allow_rtf_code = rs$allow_code,
                                    content_width = rs$content_size[["width"]],
                                    styles = styles)
  res3_3 # A: 0.77
  expect_equal(as.numeric(res3_3["A"]), 0.77)
  
  # Width not considering bold
  res4 <- get_col_widths_variable(fdat, tbl, lbls, 
                                  rs$font, rs$font_size, rs$units, 
                                  rs$gutter_width,
                                  allow_rtf_code = rs$allow_code,
                                  content_width = rs$content_size[["width"]],
                                  styles = NULL)
  res4
  expect_equal(as.numeric(res4["A"]), 0.77)
  
  expect_true(res[["A"]] > res2[["A"]])
  expect_true(res[["A"]] == res3[["A"]])
  expect_true(res2[["A"]] > res4[["A"]])
})


test_that("get_col_widths works as expected.", {
  
  base_path <- tempdir()
  fp <- file.path(base_path, "output/test1.out")
  
  df <- read.table(header = TRUE, text = '
      group1   group2            trt1         trt2         subgroup
      "性别"   "男"              "75 (51.7)"  "91 (59.5)"  "≥65岁"
      "性别"   "女"              "70 (48.3)"  "62 (40.5)"  "≥65岁"
      "年龄"   "例数"            "145"        "153"        "≥65岁"
      "年龄"   "平均数"          "64.7"       "65.8"       "≥65岁"
      "年龄"   "中位数"          "65.0"       "66.0"       "≥65岁"
      "年龄"   "标准差"          "9.7"        "8.3"        "≥65岁"
      "年龄"   "最小值, 最大值"  "40, 83"     "43.85"      "≥65岁"')
  
  tbl <- create_table(df) %>% 
    define(trt1, label = "试验药物一")
  
  rs <- create_report(fp, output_type = "txt", font = "simsun", 
                      orientation = "portrait")
  
  lbls <- get_labels(df, tbl)
  
  res <- get_col_widths(df, tbl, lbls, rs$char_width, rs$units,
                        content_width = rs$content_size[["width"]])
  
  char_num <- round(res / rs$char_width) - 1
  
  expect_equal(as.numeric(char_num), c(6,13,9,9,8))
})

test_that("get_col_widths works as expected with 100%.", {
  
  fp <- ""
  
  dat <- iris
  
  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    define(Sepal.Length, label = "Sepal Length", width = "20%", align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = "20%", align = "centre") %>%
    define(Petal.Length, label = "Petal Length", width = "20%", align = "centre") %>%
    define(Petal.Width, label = "Petal.Width", width = "20%", align = "centre") %>%
    define(Species, width = "20%", blank_after = TRUE)
  
  rpt <- create_report(fp, output_type = "txt", font = "fixed",
                       orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1) %>%
    page_header("Left", c("Right1")) %>%
    add_content(tbl, blank_row = "none") %>%
    page_footer("Left1", "Center1", "Page [pg] of [tpg]") %>%
    footnotes("My footnote 1", "My footnote 2")
  
  lbls <- get_labels(dat, tbl)
  
  rs <- page_setup(rpt)
  
  res <- get_col_widths(dat, tbl, lbls, rs$char_width, rs$units,
                        content_width = rs$content_size[["width"]], rs = rs)
  
  char_num <- round(res / rs$char_width)
  
  expect_equal(as.numeric(char_num), c(22,22,22,22,20))
})

test_that("get_col_widths works with stub percentage width as expected.", {
  
  fp <- ""
  
  dat <- iris[1:50,]
  dat$Group <- c(rep("A", 10), rep("B", 10), rep("C", 10), rep("D", 10), rep("E", 10))
  dat$Species <- as.character(dat$Species)
  
  tbl <- create_table(dat, borders = "none") %>%
    titles("Table 1.0", "My Nice Irises", "Another Title") %>%
    stub(c("Group", "Species"), width = "20%") %>%
    define(Group, label_row = T, blank_after = T) %>%
    define(Sepal.Length, label = "Sepal Length", width = "20%", align = "center") %>%
    define(Sepal.Width, label = "Sepal Width", width = "20%", align = "centre") %>%
    define(Petal.Length, label = "Petal Length", width = "20%", align = "centre") %>%
    define(Petal.Width, label = "Petal.Width", width = "20%", align = "centre")
  
  rpt <- create_report(fp, output_type = "txt", font = "fixed",
                       orientation = "landscape") %>%
    set_margins(top = 1, bottom = 1)
  
  lbls <- get_labels(dat, tbl)
  
  rs <- page_setup(rpt)
  
  fdat <- prep_data(dat, tbl, rs$char_width, rs$missing)
  
  res <- get_col_widths(fdat, tbl, lbls, rs$char_width, rs$units,
                        content_width = rs$content_size[["width"]], rs = rs)
  
  char_num <- round(res / rs$char_width)
  expect_equal(as.numeric(char_num), c(22,22,22,22,20))
})

test_that("stub_dedupe works as expected", {
  
  df <- mtcars
  
  tbl <- create_table(df) %>% 
    stub(c("cyl", "mpg"),  width = 2, label = "Fork") %>%
    define(cyl,) %>% 
    define(mpg, dedupe = TRUE) %>% 
    define(disp, width = 2) 
  
  res <- stub_dedupe(tbl$stub, tbl$col_defs)
  
  expect_equal(res, TRUE)
  
  tbl <- create_table(df) %>% 
    stub(c("cyl", "mpg"),  width = 2, label = "Fork") %>%
    define(cyl,) %>% 
    define(mpg) %>% 
    define(disp, width = 2) 
  
  res <- stub_dedupe(tbl$stub, tbl$col_defs)
  
  expect_equal(res, FALSE)
  
})


test_that("get_pgby_cnt works as expected", {
  
  res1 <- get_pgby_cnt(NULL)
  
  expect_equal(res1, 0)
  
  res2 <- get_pgby_cnt(c("myval"))
  
  expect_equal(res2, 1)
  
  res3 <- get_pgby_cnt(c("myval", "my\nval"))
  
  expect_equal(res3, 2)
  
  res4 <- get_pgby_cnt(c("myval0", "myval1", "myval2"))
  
  expect_equal(res4, 1)
  
  
})

test_that("get_nchar works as expected", {
  
  res1 <- get_nchar("123b5")
  
  expect_equal(res1, 5)
  
  res2 <- get_nchar("最小值, 最大值")
  
  expect_equal(res2, 13)
  
  res3 <- get_nchar("试验药物一")
  
  expect_equal(res3, 9)
})
