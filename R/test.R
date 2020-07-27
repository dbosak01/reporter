
library(magrittr)
library(randomNames)

# How to Save an object ---------------------------------------------------

# Setup
subjid <- 100:109
name <- randomNames(10)
sex <- factor(c("M", "F", "F", "M", "M", "F", "M", "F", "F", "M"),
              levels = c("M", "F", "UNK"))
age <- c(41, 53, 43, 39, 47, 52, 21, 38, 62, 26)
arm <- c(rep("A", 5), rep("B", 5))

# Create data frame
df <- data.frame(subjid, name, sex, age, arm)
df

lbls <- c(subjid = "Subject ID",
            name = "Subject Name",
            sex = "Sex",
            age = "Age",
            arm = "Arm")

for (nm in names(df)) {
  attr(df[[nm]], "label") <- lbls[[nm]]
}
str(df)

tbl1 <- create_table(df[df$arm == "A", ])
tbl2 <- create_table(df[df$arm == "B", ])
tbl1
tbl2

rpt <- create_report("test2.out", uom = "inches", paper_size = "letter") %>%
  #options_text(cpuom = 10.909, lpuom = 6.075) %>%
  options_text(editor = "notepadpp") %>%
  page_header(left = "Experis", right = c("Study ABC", "Status: Closed")) %>%
  titles("Table 1.0", "Analysis Data Subject Listing", "Safety Population", align = "center") %>%
  footnotes("Program Name: table1_0.R") %>%
  page_footer(left = Sys.time(), center = "Confidential", right = "Page X of Y") %>%
  add_content(tbl1) %>%
  add_content(tbl2)


rpt

res <- write_report(rpt)
res

writeLines(readLines(rpt$file_path))


df[df$arm == "A", ]

df


#
# write_registration_file("reg.txt")
#
#
#
# df
#
# library(stringi)
# s <- stri_paste(
#   "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Proin ",
#   "nibh augue, suscipit a, scelerisque sed, lacinia in, mi. Cras vel ",
#   "lorem. Etiam pellentesque aliquet tellus.")
# cat(stri_wrap(s, 20, 0.0), sep="\n") # greedy
# cat(stri_wrap(s, 20, 2.0), sep="\n") # dynamic
# cat(stri_pad(stri_wrap(s), side='both'), sep="\n")


