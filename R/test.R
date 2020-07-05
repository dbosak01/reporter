
library(magrittr)

as.list(tbl)

tbl <- create_table(mtcars)
tbl

rpt <- create_report("test.txt") %>%
  options_text(lpi = 6.5) %>%
  page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
  titles("Table 1.0", "Analysis Data Subject Listing", "Safety Population", align = "center") %>%
  footnotes("Program Name: table1_0.R") %>%
  page_footer(left = Sys.time(), center = "Confidential", right = "Page X of Y")
  #add_content(tbl)
  

rpt

write_report(rpt)

print(rpt, full = TRUE)



dt <- list(a = "1", b = 2)

print.listof(dt)


#  add_content(create_table(df))



#write_report(rpt)

library(stringi)
s <- stri_paste(
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Proin ",
  "nibh augue, suscipit a, scelerisque sed, lacinia in, mi. Cras vel ",
  "lorem. Etiam pellentesque aliquet tellus.")
cat(stri_wrap(s, 20, 0.0), sep="\n") # greedy
cat(stri_wrap(s, 20, 2.0), sep="\n") # dynamic
cat(stri_pad(stri_wrap(s), side='both'), sep="\n")


