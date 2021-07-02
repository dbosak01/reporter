context("Write Report Text Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

test_that("page_template_text() works as expected.", {
  
  
  rpt <- create_report("fork.out") %>% 
    titles("Here is a test title.") %>% 
    footnotes("Test footnote")
  
  rpt$line_size <- 140
  
  
  ptt <- page_template_text(rpt)
  
  expect_equal(length(ptt$titles), 2)
  expect_equal(length(ptt$footnotes), 2)
  expect_equal(length(ptt$page_header), 0)
  expect_equal(length(ptt$page_footer), 0)
  
  
  rpt1 <- create_report("fork.out") 
  
  rpt1$line_size <- 140
  
  
  ptt1 <- page_template_text(rpt1)
  
  expect_equal(length(ptt1$titles), 0)
  expect_equal(length(ptt1$footnotes), 0)
  expect_equal(length(ptt1$page_header), 0)
  expect_equal(length(ptt1$page_footer), 0)
  
  
  
  rpt2 <- create_report("fork.out") %>% 
    page_header(left = c("Something1", "Something2"), right = "Nothing") %>% 
    titles("Here is a test title.", "There is a title") %>% 
    footnotes("Test footnote", "Test footnote 2") %>% 
    page_footer(center = "confidential")
  
  rpt2$line_size <- 140
  
  ptt2 <- page_template_text(rpt2)
  
  expect_equal(length(ptt2$titles), 3)
  expect_equal(length(ptt2$footnotes), 3)
  expect_equal(length(ptt2$page_header), 2)
  expect_equal(length(ptt2$page_footer), 2)
  
  # No line size.  Should get error.
  rpt3 <- create_report("fork.out") 
  expect_error(page_template_text(rpt3))
  
  rpt4 <- create_report("fork.out") %>% 
    titles(paste0(rep("W", 200), collapse =""))
  rpt4$line_size <- 140
  expect_warning(page_template_text(rpt4))
  
})


test_that("write_report output_type parameter checks work as expected.", {
  
  fp <- file.path(base_path, "text/wreport.out")
  
  rpt2 <- create_report(fp) %>% 
    page_header(left = c("Something1", "Something2"), right = "Nothing") %>% 
    titles("Here is a test title.", "There is a title") %>% 
    add_content(create_text("Hello")) %>% 
    footnotes("Test footnote", "Test footnote 2") %>% 
    page_footer(center = "confidential")
  
  expect_error(write_report(rpt2, output_type = "text"))
  res <- write_report(rpt2, output_type = "txt")
  
  expect_equal(res$output_type, "TXT")
  
  expect_equal(file.exists(fp), TRUE)
  
  if (file.exists(fp))
    file.remove(fp)
  
  
})

# test_that("write_page_numbers() works as expected.", {
#   
#   fp <- file.path(base_path, "text/text4.out")
#   
#   rtp <- create_report(fp) %>% 
#     page_footer(right = "Page [pg] of [tpg]")
#   
#   res2 <- write_page_numbers(rtp)
#   
#   res2
#   
#   expect_equal(grepl("[pg]", 
#   
#   
# })

