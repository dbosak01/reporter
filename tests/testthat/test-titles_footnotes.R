context("Title and Footnote Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

test_that("ttfn1: single title right aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn1.out")
  
  tbl <- create_table(mtcars[1:10, ]) 
  
  rpt <- create_report(fp) %>% 
    titles("MTCARS Data Frame", align = "right") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})
  

test_that("ttfn2: two titles opposite aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn2.out")
  
  tbl <- create_table(mtcars[1:10, ]) 
  
  rpt <- create_report(fp) %>% 
    titles("MTCARS Data Frame 1", align = "right", blank_row = "above") %>% 
    titles("MTCARS Data Frame 2", align = "left", blank_row = "below") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn3: single footnote right aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn3.out")
  
  tbl <- create_table(mtcars[1:10, ]) 
  
  rpt <- create_report(fp) %>% 
    footnotes("MTCARS Data Frame", align = "right") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn4: two footnotes opposite aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn4.out")
  
  tbl <- create_table(mtcars[1:10, ]) 
  
  rpt <- create_report(fp) %>% 
    footnotes("MTCARS Data Frame 1", align = "right", blank_row = "above") %>% 
    footnotes("MTCARS Data Frame 2", align = "left", blank_row = "below") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn5: title and footnote assigned to table works.", {
  
  fp <- file.path(base_path, "titles/ttfn5.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    titles("MTCARS Data Frame 1", align = "center") %>% 
    footnotes("MTCARS Data Frame 2", align = "left") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn6: no blank_rows on header and footer work as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn6.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    footnotes("Table footnote", align = "left", blank_row = "none") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars", blank_row = "none") %>% 
    titles("MTCARS Data Frame 1", align = "left", blank_row = "none") %>% 
    footnotes("Footer footnote", align = "left", blank_row = "none") %>% 
    page_footer(left = "Footer", blank_row = "none") %>% 
    add_content(tbl, align = "left", blank_row = "none", page_break = FALSE) %>% 
    add_content(create_text("Text content", align = "left"))
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn7: blank_row on header and footer work as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn7.out")

  tbl <- create_table(mtcars[1:10, ]) %>% 
    footnotes("Table footnote", align = "left", blank_row = "both") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars", blank_row = "below") %>% 
    titles("MTCARS Data Frame 1", align = "left", blank_row = "below") %>% 
    footnotes("Footer footnote", align = "left", blank_row = "none") %>% 
    page_footer(left = "Footer", blank_row = "above") %>% 
    add_content(tbl, align = "left", page_break = FALSE) %>% 
    add_content(create_text("Here is some text content", align = "left"))
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * 45)
  
})


test_that("ttfn8: blank_row on content works as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn8.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    footnotes("Table footnote", align = "left", blank_row = "none") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars", blank_row = "none") %>% 
    titles("MTCARS Data Frame 1", align = "left", blank_row = "both") %>% 
    footnotes("Footer footnote", align = "left", blank_row = "both") %>% 
    page_footer(left = "Footer", blank_row = "none") %>% 
    add_content(tbl, align = "left", page_break = FALSE, blank_row = "none") %>% 
    add_content(create_text("Here is some text content", align = "left"))
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns), res$pages * 45)
  
})

test_that("ttfn9: default blank_rows on header and footer work as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn9.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    footnotes("Table footnote",
              align = "left") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars") %>% 
    titles("MTCARS Data Frame 1", align = "left") %>% 
    footnotes("Footer footnote", align = "left") %>% 
    page_footer(left = "Footer") %>% 
    add_content(tbl, align = "left", page_break = FALSE) %>% 
    add_content(create_text("Text content", align = "left"), 
                page_break = FALSE) %>% 
    add_content(create_text("Space check", align = "left"))
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  expect_equal(length(lns), 45)
  expect_equal(nchar(lns[1]), 108)
  
})

test_that("ttfn10: text blank rows work as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn10.out")
  
  cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
                "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
                "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                "qui officia deserunt mollit anim id est laborum.")
  
  tbl <- create_table(mtcars) %>% 
    footnotes("Table footnote", "T2", "T3", "T4", "T5", "T6", "T7", 
              align = "left", blank_row = "both") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars") %>% 
    titles("MTCARS Data Frame 1", align = "left") %>% 
    footnotes("Footer footnote", align = "left") %>% 
    page_footer(left = "Footer") %>% 
    add_content(tbl, align = "left", page_break = FALSE, blank_row = "below") %>% 
    add_content(create_text("Text content", align = "left"))
  
  
  rs <- write_report(rpt)
  
  expect_equal(rs$page_template_row_count, 7)
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn11: blank hitting page break works as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn11.out")
  
  tbl <- create_table(mtcars) %>% 
    footnotes("Table footnote",
              align = "left") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars") %>% 
    titles("MTCARS Data Frame 1", align = "left") %>% 
    footnotes("Footer footnote", align = "left") %>% 
    page_footer(left = "Footer") %>% 
    add_content(tbl, align = "left", page_break = FALSE) %>% 
    add_content(create_text("Text content", align = "left"))
  
  
  res <- write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
  lns <- readLines(fp)
  
  expect_equal(length(lns),  45)
  
})


test_that("ttfn12: title and footnote with bottom borders assigned to table works.", {
  
  fp <- file.path(base_path, "titles/ttfn12.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    titles("MTCARS Data Frame 1", align = "center", 
           borders = "bottom") %>% 
    footnotes("MTCARS Data Frame 2", align = "left", 
              borders = "bottom") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn13: title and footnote with top borders assigned to table works.", {
  
  fp <- file.path(base_path, "titles/ttfn13.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    titles("MTCARS Data Frame 1", align = "center", 
           borders = "top") %>% 
    footnotes("MTCARS Data Frame 2", align = "left", 
              borders = "top") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn14: title_header with bottom borders assigned to table works.", {
  
  fp <- file.path(base_path, "titles/ttfn14.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    title_header("MTCARS Data Frame 1", 
           borders = "bottom") %>% 
    footnotes("MTCARS Data Frame 2", align = "left", 
              borders = "bottom") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn15: title_header with top borders assigned to table works.", {
  
  fp <- file.path(base_path, "titles/ttfn15.out")
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    title_header("MTCARS Data Frame 1", 
           borders = "top") %>% 
    footnotes("MTCARS Data Frame 2", align = "left", 
              borders = "top") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn16: Long title generates warning not error.", {
  
  fp <- file.path(base_path, "titles/ttfn16.out")
  
  tbl <- create_table(mtcars[1:10, 1:3]) %>% 
    titles("MTCARS Data Frame 1 MTCARS Data Frame 1 ", align = "left") %>% 
    footnotes("MTCARS", align = "left") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  expect_warning(write_report(rpt))
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn17: Long footnote generates warning not error.", {
  
  fp <- file.path(base_path, "titles/ttfn17.out")
  
  tbl <- create_table(mtcars[1:10, 1:3]) %>% 
    titles("MTCARS Data", align = "left") %>% 
    footnotes("MTCARS Data Frame 1 MTCARS Data Frame 2", align = "left") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  expect_warning(write_report(rpt))
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn18: Long title_header generates warning not error.", {
  
  fp <- file.path(base_path, "titles/ttfn18.out")
  
  tbl <- create_table(mtcars[1:10, 1:3]) %>% 
    title_header("MTCARS Data Frame 1 MTCARS Data Frame 1 ", right = "Header") %>% 
    footnotes("MTCARS", align = "left") 
  
  rpt <- create_report(fp) %>% 
    add_content(tbl, align = "center")
  
  
  expect_warning(write_report(rpt))
  
  expect_equal(file.exists(fp), TRUE)
  
})
