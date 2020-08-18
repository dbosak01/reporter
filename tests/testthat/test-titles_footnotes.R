context("Title and Footnote Tests")

base_path <- "c:/packages/rptr/tests/testthat"


test_that("ttfn1: single title right aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn1.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ], align = "right") %>% 
    define(vs, visible = FALSE)
  
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
  
  tbl <- create_table(mtcars[1:10, ], align = "right") %>% 
    define(vs, visible = FALSE)
  
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
  
  tbl <- create_table(mtcars[1:10, ], align = "right") %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp) %>% 
    footnotes("MTCARS Data Frame", align = "right") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})


test_that("ttfn4: two titles opposite aligned works.", {
  
  fp <- file.path(base_path, "titles/ttfn4.out")
  
  if (file.exists(fp))
    file.remove(fp)
  
  tbl <- create_table(mtcars[1:10, ], align = "right") %>% 
    define(vs, visible = FALSE)
  
  rpt <- create_report(fp) %>% 
    footnotes("MTCARS Data Frame 1", align = "right", blank_row = "above") %>% 
    footnotes("MTCARS Data Frame 2", align = "left", blank_row = "below") %>% 
    add_content(tbl)
  
  
  write_report(rpt)
  
  expect_equal(file.exists(fp), TRUE)
  
})

