
context("RTF2 Tests")

base_path <- "c:/packages/reporter/tests/testthat"

base_path <- tempdir()

cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

# test_that("rtf2-0a: Fixed report is correct.", {
# 
# 
#   fp <- file.path(base_path, "rtf2/test0a.rtf")
# 
#   rpt <- create_report(fp, output_type = "RTF", font = "fixed") %>%
#     titles("Table 0.0", "Baseline Characteristics") %>%
#     add_content(create_table(mtcars[1:10, ]))
# 
#   res <- write_report(rpt)
# 
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
# 
# 
# })
# 
# test_that("rtf2-0b: Fixed report with font_size is correct.", {
# 
# 
#   fp <- file.path(base_path, "rtf2/test0b.rtf")
# 
#   rpt <- create_report(fp, output_type = "RTF", font = "fixed",
#                        font_size = 12) %>%
#     titles("Table 0.0", "Baseline Characteristics") %>%
#     add_content(create_table(mtcars[1:10, ]))
# 
#   res <- write_report(rpt)
# 
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
# 
# 
# })
# 
# test_that("rtf2-0c: Fixed report with font_size options is correct.", {
# 
# 
#   fp <- file.path(base_path, "rtf2/test0c.rtf")
# 
#   rpt <- create_report(fp, output_type = "RTF", font = "fixed") %>%
#     options_fixed(font_size = 12) %>%
#     titles("Table 0.0", "Baseline Characteristics") %>%
#     add_content(create_table(mtcars[1:10, ]))
# 
#   res <- write_report(rpt)
# 
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
# 
# 
# })
# 
# test_that("rtf2-0d: Fixed report with conflicting font size is correct.", {
# 
# 
#   fp <- file.path(base_path, "rtf2/test0d.rtf")
# 
#   rpt <- create_report(fp, output_type = "RTF", font = "fixed",
#                        font_size = 8) %>%
#     options_fixed(font_size = 12) %>%
#     titles("Table 0.0", "Baseline Characteristics") %>%
#     add_content(create_table(mtcars[1:10, ]))
# 
#   res <- write_report(rpt)
# 
#   expect_equal(file.exists(fp), TRUE)
#   expect_equal(res$pages, 1)
# 
# 
# })
# 
# 
# test_that("rtf2-1: Simplest table works as expected.", {
# 
# 
#   fp <- file.path(base_path, "rtf2/test1.rtf")
# 
#   dat <- mtcars[1:15, ]
#   attr(dat[[2]], "label") <- "Cylin."
#   
#   rpt <- create_report(fp, output_type = "RTF", font = "Arial",
#                        font_size = 10, orientation = "portrait") %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", c("Right1", "Right2", "Right3"), blank_row = "below") %>%
#     titles("Table 1.0", "My Nice Table") %>%
#     add_content(create_table(dat)) %>%
#     footnotes("My footnote 1", "My footnote 2") %>%
#     page_footer("Left1", "Center1", "Right1")
# 
#   res <- write_report(rpt)
# 
#   expect_equal(file.exists(fp), TRUE)
#   #expect_equal(res$pages, 1)
# 
# 
# })
# 
# test_that("rtf2-2: get_cell_borders works as expected.", {
#   
#   
#   expect_equal(get_cell_borders(1, 1, 4, 4, c("all")), 
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs\\clbrdrr\\brdrs")
#   
#   expect_equal(get_cell_borders(4, 1, 4, 4, c("all")), 
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs\\clbrdrr\\brdrs")
#   
#   expect_equal(get_cell_borders(1, 1, 4, 4, c("outside")), 
#                "\\clbrdrt\\brdrs\\clbrdrl\\brdrs")
#   
#   expect_equal(get_cell_borders(4, 1, 4, 4, c("outside")), 
#                "\\clbrdrb\\brdrs\\clbrdrl\\brdrs")
#   
#   expect_equal(get_cell_borders(2, 2, 4, 4, c("outside")), 
#                "")
#   
#   expect_equal(get_cell_borders(2, 4, 4, 4, c("right")), 
#                "\\clbrdrr\\brdrs")
#   
#   expect_equal(get_cell_borders(2, 4, 4, 4, c("inside")), 
#                "\\clbrdrt\\brdrs\\clbrdrb\\brdrs\\clbrdrl\\brdrs")
#   
#   
# })
# 
# 
# test_that("rtf2-3: Text spec works as expected.", {
#   
#   
#   fp <- file.path(base_path, "rtf2/test3.rtf")
# 
#   txt <- create_text(cnt) %>% 
#     titles("Text 1.0", "My Nice Text") %>%
#     footnotes("My footnote 1", "My footnote 2")
#   
#   rpt <- create_report(fp, output_type = "RTF", font = "times",
#                        font_size = 10) %>%
#     set_margins(top = 1, bottom = 1) %>%
#     page_header("Left", c("Right1", "Right2")) %>%
#     add_content(txt) %>% 
#     page_footer("Left1", "Center1", "Right1")
#   
#   res <- write_report(rpt)
#   
#   expect_equal(file.exists(fp), TRUE)
#   #expect_equal(res$pages, 1)
#   
#   
#   get_page_header_rtf(rpt)
#   
#   str <- "Hello there here is my string and let's make it longer and longer"
#   get_lines_rtf(str, 2, "Courier", 12, "inches")
#   
#   get_excess_lines(str, 2, "Arial", 10, "inches")
#   
# })
# 
# 
# 
# test_that("rtf2-4: Page template functions work as expected.", {
#   
#   rpt <- create_report("") %>% 
#     titles("Hello") %>% 
#     footnotes("Goodbye")
#   
#    t <- get_titles_rtf(rpt$titles, 6, 1440)
#   
#   expect_equal(t$rtf[1], 
#     "\\trowd\\trgaph0\\cellx8640\\qc Hello\\cell\\row\n")
#   
#   f <- get_footnotes_rtf(rpt$footnotes, 6, 1440)
#   
#   expect_equal(f$rtf[2], 
#                "\\trowd\\trgaph0\\cellx8640\\ql Goodbye\\cell\\row\n\\pard")
#   
#   rpt2 <- create_report("") %>% 
#     title_header("Hello", right = "Right") 
#   
#   th <- get_title_header_rtf(rpt2$title_hdr, 6, 1440)
#   
#   expect_equal(th$rtf[1], 
#     "\\trowd\\trgaph0\\cellx8640\\ql Hello\\cell\\qr Right\\cell\\row\n")
#   
#   
#   expect_equal(get_lines_rtf(cnt, 9, "Arial", 10), 4)
#   
# })
# 
# 
