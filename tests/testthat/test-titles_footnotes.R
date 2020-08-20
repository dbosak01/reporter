context("Title and Footnote Tests")

base_path <- "c:/packages/rptr/tests/testthat"


test_that("ttfn1: single title right aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn1.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ]) 
  
  rpt <- create_report(fp) %>% 
    titles("MTCARS Data Frame", align = "right") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})
  

test_that("ttfn2: two titles opposite aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn2.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ]) 
  
  rpt <- create_report(fp) %>% 
    footnotes("MTCARS Data Frame", align = "right") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn4: two footnotes opposite aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn4.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    footnotes("Table footnote", align = "left", blank_row = "both") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars", blank_row = "below") %>% 
    titles("MTCARS Data Frame 1", align = "left", blank_row = "below") %>% 
    footnotes("Footer footnote", align = "left", blank_row = "none") %>% 
    page_footer(left = "Footer", blank_row = "above") %>% 
    add_content(tbl, align = "left", page_break = FALSE) %>% 
    add_content(create_text("Here is some text content", align = "left"))
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn8: blank_row on content works as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn8.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ]) %>% 
    footnotes("Table footnote", align = "left", blank_row = "none") 
  
  rpt <- create_report(fp) %>% 
    page_header(left = "Study: Cars", blank_row = "none") %>% 
    titles("MTCARS Data Frame 1", align = "left", blank_row = "both") %>% 
    footnotes("Footer footnote", align = "left", blank_row = "both") %>% 
    page_footer(left = "Footer", blank_row = "none") %>% 
    add_content(tbl, align = "left", page_break = FALSE, blank_row = "none") %>% 
    add_content(create_text("Here is some text content", align = "left"))
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

test_that("ttfn9: default blank_rows on header and footer work as expected.", {
  
  fp <- file.path(base_path, "titles/ttfn9.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  if (file.exists(fp))
    file.remove(fp)
  
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
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})
