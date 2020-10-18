# rptr <img src="man/images/reporter.svg" align="right" height="138" />

<!-- badges: start -->

[![rptr version](https://www.r-pkg.org/badges/version/rptr)](https://cran.r-project.org/package=rptr)
[![rptr lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=rptr)
[![rptr downloads](https://cranlogs.r-pkg.org/badges/grand-total/rptr)](https://cran.r-project.org/package=rptr)
[![Travis build status](https://travis-ci.com/dbosak01/rptr.svg?branch=master)](https://travis-ci.com/dbosak01/rptr)

<!-- badges: end -->

The **rptr** package creates regulatory-style, statistical reports. It was designed
to produce Tables, Listings, and Figures (TLFs) for the 
the pharmaceutical, biotechnology, and medical-device industries.
However, the functions are generalized enough to provide statistical reporting
for any industry.  The package is written in Base R, and has no dependencies on
any other reporting package.

The package is intended to give R programmers 
report layout capabilities such as those found in SAS® PROC REPORT, 
and a choice of output formats like SAS® ODS. The package will 
initially focus on printable, file-based 
output formats, as there are already numerous R packages that 
provide tabular reporting in HTML. 
The current version supports TXT, RTF, and PDF output types.   

## Key Features

The **rptr** package contains the following key features:

* Titles, footnotes, page header, and page footer are repeated on each page
* Supports header labels and spanning headers 
* Calculates default columns widths automatically
* Includes automatic wrapping and splitting of wide and long tables
* Integrates with the **fmtr** package to format numeric, date, and character data
* Plots from the popular **ggplot2** package can be added to RTF and PDF reports
* Allows appending multiple tables to a report, multiple tables to a page, 
and intermingling of text, tables, and plots
* Supports in-report date/time stamps and "Page X of Y" page numbering


## How to use **rptr**
There are four steps to creating a report:

* Create report 
* Create report content
* Add content to the report
* Write out the report 

You can create the report with the `create_report()` function.  Content is
created with the `create_table()`, `create_text()`, or `create_plot()`
functions.  Add content to the report with the `add_content()` function. 
Finally, the report can be written to a file with the `write_report()` function.  

In addition to these primary functions, there are several secondary functions
to help you formalize the report.  All available functions are shown in
the subsequent glossary.


## Glossary of Functions

Below are the functions available in the **rptr** package, and a brief
description of their use.  For additional information, see the help text:

### Report Functions

* `create_report()`: Define a report object  
* `options_fixed()`: Set options for fixed width (text) reports  
* `set_margins()`: Set margins for the report  
* `page_header()`: Define a page header  
* `page_footer()`: Define a page footer  
* `titles()`: Set titles for the report  
* `footnotes()`: Set footnotes for the report  
* `add_content()`: Add content to the report  
* `write_report()`: Write the report to a file  

### Table functions

* `create_table()`: Define a table object  
* `titles()`: Set titles for the table  
* `footnotes()`: Set footnotes for the table  
* `spanning_header()`: Define a spanning header  
* `stub()`: Define a stub column  
* `columns_defaults()`: Set default column attributes
* `define()`: Specify settings for a column  

### Text functions

* `create_text()`: Define a text object  
* `titles()`: Set titles for the text  
* `footnotes()`: Set footnotes for the text  

### Plot functions

* `create_plot()`: Define a plot object  
* `titles()`: Set titles for the plot  
* `footnotes()`: Set footnotes for the plot  


## Examples

### Example 1: Listing

Here is an example of 
a regulatory-style listing using **rptr** and the mtcars sample data frame:

```
# Create temp file name
tmp <- file.path(tempdir(), "example1.txt")

# Create the report
rpt <- create_report(tmp, orientation = "portrait") %>% 
  page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
  titles("Listing 1.0", "MTCARS Data Listing") %>% 
  add_content(create_table(mtcars)) %>% 
  footnotes("* Motor Trend, 1974") %>%
  page_footer(left = Sys.time(), 
              center = "Confidential", 
              right = "Page [pg] of [tpg]")

# Write the report
write_report(rpt)

# Send report to console for viewing
writeLines(readLines(tmp, encoding = "UTF-8"))

# Client: Motor Trend                                                Study: Cars
#                                  Listing 1.0
#                             MTCARS Data Listing
#
#            mpg cyl   disp   hp  drat     wt   qsec vs am gear carb
#          ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#             21   6    160  110   3.9   2.62  16.46  0  1    4    4
#             21   6    160  110   3.9  2.875  17.02  0  1    4    4
#           22.8   4    108   93  3.85   2.32  18.61  1  1    4    1
#           21.4   6    258  110  3.08  3.215  19.44  1  0    3    1
#           18.7   8    360  175  3.15   3.44  17.02  0  0    3    2
#           18.1   6    225  105  2.76   3.46  20.22  1  0    3    1
#           14.3   8    360  245  3.21   3.57  15.84  0  0    3    4
#           24.4   4  146.7   62  3.69   3.19     20  1  0    4    2
#           22.8   4  140.8   95  3.92   3.15   22.9  1  0    4    2
#           19.2   6  167.6  123  3.92   3.44   18.3  1  0    4    4
#           17.8   6  167.6  123  3.92   3.44   18.9  1  0    4    4
#           16.4   8  275.8  180  3.07   4.07   17.4  0  0    3    3
#           17.3   8  275.8  180  3.07   3.73   17.6  0  0    3    3
#           15.2   8  275.8  180  3.07   3.78     18  0  0    3    3
#           10.4   8    472  205  2.93   5.25  17.98  0  0    3    4
#           10.4   8    460  215     3  5.424  17.82  0  0    3    4
#           14.7   8    440  230  3.23  5.345  17.42  0  0    3    4
#           32.4   4   78.7   66  4.08    2.2  19.47  1  1    4    1
#           30.4   4   75.7   52  4.93  1.615  18.52  1  1    4    2
#           33.9   4   71.1   65  4.22  1.835   19.9  1  1    4    1
#           21.5   4  120.1   97   3.7  2.465  20.01  1  0    3    1
#           15.5   8    318  150  2.76   3.52  16.87  0  0    3    2
#           15.2   8    304  150  3.15  3.435   17.3  0  0    3    2
#           13.3   8    350  245  3.73   3.84  15.41  0  0    3    4
#           19.2   8    400  175  3.08  3.845  17.05  0  0    3    2
#           27.3   4     79   66  4.08  1.935   18.9  1  1    4    1
#             26   4  120.3   91  4.43   2.14   16.7  0  1    5    2
#           30.4   4   95.1  113  3.77  1.513   16.9  1  1    5    2
#           15.8   8    351  264  4.22   3.17   14.5  0  1    5    4
#           19.7   6    145  175  3.62   2.77   15.5  0  1    5    6
#             15   8    301  335  3.54   3.57   14.6  0  1    5    8
#           21.4   4    121  109  4.11   2.78   18.6  1  1    4    2
# 
# ...
# 
# * Motor Trend, 1974
# 
# 2020-08-25 23:57:30              Confidential                      Page 1 of 1


```
### Example 2: Summary table

Here is an example of a regulatory-style table of summary statistics:

```

# Create temporary path
tmp <- file.path(tempdir(), "example2.txt")

# Read in prepared data
df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')

# Create table
tbl <- create_table(df, first_row_blank = TRUE) %>% 
  define(var, label = "Variable", blank_after = TRUE, dedupe = TRUE,
         format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
  define(label, label = "") %>% 
  define(A, label = "Group A", align = "center", n = 19) %>% 
  define(B, label = "Group B", align = "center", n = 13)


# Create report and add content
rpt <- create_report(tmp, orientation = "portrait") %>% 
  page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
  titles("Table 1.0", "MTCARS Summary Table") %>% 
  add_content(tbl) %>% 
  footnotes("* Motor Trend, 1974") %>%
  page_footer(left = Sys.time(), 
              center = "Confidential", 
              right = "Page [pg] of [tpg]")

# Write out report
write_report(rpt)

# View report in console
writeLines(readLines(tmp, encoding = "UTF-8"))

# Client: Motor Trend                                                Study: Cars
#                                   Table 1.0
#                              MTCARS Summary Table
# 
#                                            Group A      Group B
#            Variable                         (N=19)       (N=13)
#            ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# 
#            Miles Per Gallon  N                19           13
#                              Mean         18.8 (6.5)   22.0 (4.9)
#                              Median          16.4         21.4
#                              Q1 - Q3     15.1 - 21.2  19.2 - 22.8
#                              Range       10.4 - 33.9  14.7 - 32.4
# 
#            Cylinders         8 Cylinder  10 ( 52.6%)   4 ( 30.8%)
#                              6 Cylinder   4 ( 21.1%)   3 ( 23.1%)
#                              4 Cylinder   5 ( 26.3%)   6 ( 46.2%)
# 
# 
# ...
# 
# 
# * Motor Trend, 1974
# 
# 2020-10-16 18:57:02              Confidential                      Page 1 of 1

```

### Example 3: Summary table with stub column

Here is an example of the same regulatory-style table of summary statistics,
but with row labels merged into a stub column:

```

# Create temporary path
tmp <- file.path(tempdir(), "example3.txt")

# Read in prepared data
df <- read.table(header = TRUE, text = '
      var     label        A             B          
      "ampg"   "N"          "19"          "13"         
      "ampg"   "Mean"       "18.8 (6.5)"  "22.0 (4.9)" 
      "ampg"   "Median"     "16.4"        "21.4"       
      "ampg"   "Q1 - Q3"    "15.1 - 21.2" "19.2 - 22.8"
      "ampg"   "Range"      "10.4 - 33.9" "14.7 - 32.4"
      "cyl"    "8 Cylinder" "10 ( 52.6%)" "4 ( 30.8%)" 
      "cyl"    "6 Cylinder" "4 ( 21.1%)"  "3 ( 23.1%)" 
      "cyl"    "4 Cylinder" "5 ( 26.3%)"  "6 ( 46.2%)"')

# Create table
tbl <- create_table(df, first_row_blank = TRUE) %>% 
  stub(c("var", "label")) %>% 
  define(var, blank_after = TRUE, label_row = TRUE, 
         format = c(ampg = "Miles Per Gallon", cyl = "Cylinders")) %>% 
  define(label, indent = .25) %>% 
  define(A, label = "Group A", align = "center", n = 19) %>% 
  define(B, label = "Group B", align = "center", n = 13)


# Create report and add content
rpt <- create_report(tmp, orientation = "portrait") %>% 
  page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
  titles("Table 1.0", "MTCARS Summary Table") %>% 
  add_content(tbl) %>% 
  footnotes("* Motor Trend, 1974") %>%
  page_footer(left = Sys.time(), 
              center = "Confidential", 
              right = "Page [pg] of [tpg]")

# Write out report
write_report(rpt)

# View report in console
writeLines(readLines(tmp, encoding = "UTF-8"))

# Client: Motor Trend                                                Study: Cars
#                                   Table 1.0
#                              MTCARS Summary Table
# 
#                                      Group A      Group B
#                                       (N=19)       (N=13)
#                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# 
#                  Miles Per Gallon
#                     N                   19           13
#                     Mean            18.8 (6.5)   22.0 (4.9)
#                     Median             16.4         21.4
#                     Q1 - Q3        15.1 - 21.2  19.2 - 22.8
#                     Range          10.4 - 33.9  14.7 - 32.4
# 
#                  Cylinders
#                     8 Cylinder     10 ( 52.6%)   4 ( 30.8%)
#                     6 Cylinder      4 ( 21.1%)   3 ( 23.1%)
#                     4 Cylinder      5 ( 26.3%)   6 ( 46.2%)
# 
# 
# ...
# 
# 
# * Motor Trend, 1974
# 
# 2020-10-16 18:57:02              Confidential                      Page 1 of 1

```

### Example 4: Intermingle Table and Text 

The below example demonstrates intermingling of table and text content.  This
functionality is enabled by the ability to append multiple pieces of content 
to the same report.  Appending content is useful when you want to create a 
report with multiple tables, or provide textual analysis of tabular data.

```
# Create temporary path
tmp <- file.path(tempdir(), "example4.txt")

# Dummy text
cnt <- paste0("Lorem ipsum dolor sit amet, consectetur adipiscing elit, ",
              "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
              "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
              "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ", 
              "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
              "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
              "qui officia deserunt mollit anim id est laborum.")

# Create text content
txt <- create_text(cnt) 

# Prepare data
dat <- mtcars[1:10, ]

# Create table content
tbl <- create_table(dat) %>% 
  titles("Table 1.0", "MTCARS Sample Data") %>% 
  footnotes("* Motor Trend, 1974")

# Create report and add both table and text content
rpt <- create_report(tmp, orientation = "portrait") %>% 
  page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
  add_content(tbl, page_break = FALSE) %>% 
  add_content(txt) %>% 
  page_footer(left = Sys.time(), 
              center = "Confidential", 
              right = "Page [pg] of [tpg]")

# Write the report
write_report(rpt)

# View in console
writeLines(readLines(tmp, encoding = "UTF-8"))

# Client: Motor Trend                                                Study: Cars
#                                   Table 1.0
#                               MTCARS Sample Data
# 
#             mpg cyl   disp   hp  drat     wt   qsec vs am gear carb
#           ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#              21   6    160  110   3.9   2.62  16.46  0  1    4    4
#              21   6    160  110   3.9  2.875  17.02  0  1    4    4
#            22.8   4    108   93  3.85   2.32  18.61  1  1    4    1
#            21.4   6    258  110  3.08  3.215  19.44  1  0    3    1
#            18.7   8    360  175  3.15   3.44  17.02  0  0    3    2
#            18.1   6    225  105  2.76   3.46  20.22  1  0    3    1
#            14.3   8    360  245  3.21   3.57  15.84  0  0    3    4
#            24.4   4  146.7   62  3.69   3.19     20  1  0    4    2
#            22.8   4  140.8   95  3.92   3.15   22.9  1  0    4    2
#            19.2   6  167.6  123  3.92   3.44   18.3  1  0    4    4
# 
#           * Motor Trend, 1974
# 
# Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
# incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
# nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
# Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
# eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt
# in culpa qui officia deserunt mollit anim id est laborum.
# 
# 
# ...
# 
# 2020-08-30 04:44:53              Confidential                      Page 1 of 1

```

### Example 5: Spanning Headers

Spanning headers are a common requirement of regulatory tables, yet few 
reporting packages support them.  **rptr** supports any number of spanning
levels, along with a flexible and simple syntax to define them.

```

# Create temporary path
tmp <- file.path(tempdir(), "example5.txt")

# Prepare Data
dat <- mtcars[1:10, ]
df <- data.frame(vehicle = rownames(dat), dat)

# Define Table with spanning headers
tbl <- create_table(df) %>% 
  titles("Table 1.0", "MTCARS Spanning Headers") %>% 
  spanning_header(from = "mpg", to = "hp", label = "Span 1", n = 10) %>%
  spanning_header(from = "drat", to = "qsec", label = "Span 2", n = 10) %>%
  spanning_header(from = "vs", to = "carb", label = "Span 3", n = 10) %>%
  spanning_header(from = "drat", to = "carb", 
                  label = "Super Span", level = 2) %>%
  define(vehicle, label = "Vehicle") %>% 
  define(mpg, format = "%.1f") %>% 
  define(disp, visible = FALSE) %>% 
  define(am, visible = FALSE) 

# Create Report and add table 
rpt <- create_report(tmp) %>%
  add_content(tbl, align = "left") 

# Write the report
res <- write_report(rpt)

# View in console
writeLines(readLines(tmp, encoding = "UTF-8"))
  
#                              Table 1.0
#                       MTCARS Spanning Headers
# 
#                                              Super Span
#                                   ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
#                        Span 1            Span 2          Span 3
#                        (N=10)            (N=10)          (N=10)
#                    ¯¯¯¯¯¯¯¯¯¯¯¯¯¯ ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯ ¯¯¯¯¯¯¯¯¯¯¯¯
# Vehicle              mpg cyl   hp  drat     wt   qsec vs gear carb
# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# Mazda RX4           21.0   6  110   3.9   2.62  16.46  0    4    4
# Mazda RX4 Wag       21.0   6  110   3.9  2.875  17.02  0    4    4
# Datsun 710          22.8   4   93  3.85   2.32  18.61  1    4    1
# Hornet 4 Drive      21.4   6  110  3.08  3.215  19.44  1    3    1
# Hornet Sportabout   18.7   8  175  3.15   3.44  17.02  0    3    2
# Valiant             18.1   6  105  2.76   3.46  20.22  1    3    1
# Duster 360          14.3   8  245  3.21   3.57  15.84  0    3    4
# Merc 240D           24.4   4   62  3.69   3.19     20  1    4    2
# Merc 230            22.8   4   95  3.92   3.15   22.9  1    4    2
# Merc 280            19.2   6  123  3.92   3.44   18.3  1    4    4


```


### Example 6: Page Wrap

Page wrapping will occur 
automatically if the table width exceeds the available page width.  But 
page wrapping can also be controlled using the `page_wrap` parameter on the 
`define()` function.  Below is an example.  Also note the use of the `id_var` 
option to cause the vehicle column to be retained on each wrapped page. 

```
# Create temp file name
tmp <- file.path(tempdir(), "example6.txt")

# Prepare data
dat <- mtcars[1:10, ]
dat <- data.frame(vehicle = rownames(dat), dat)

# Define table
tbl <- create_table(dat, show_cols = 1:8) %>% 
  define(vehicle, label = "Vehicle", width = 3, id_var = TRUE, align = "left") %>% 
  define(mpg, label = "Miles per Gallon", width = 1) %>% 
  define(cyl, label = "Cylinders", format = "%.1f") %>% 
  define(disp, label = "Displacement") %>% 
  define(hp, label = "Horsepower", page_wrap = TRUE) %>% 
  define(drat, visible = FALSE) %>% 
  define(wt, label = "Weight") %>% 
  define(qsec, label = "Quarter Mile Time", width = 1.5) 

  
# Create the report
rpt <- create_report(tmp, orientation = "portrait") %>% 
  titles("Listing 2.0", "MTCARS Data Listing with Page Wrap") %>% 
  add_content(tbl, align = "left") %>% 
  page_footer(right = "Page [pg] of [tpg]")

# Write the report
write_report(rpt)

# Send report to console for viewing
writeLines(readLines(tmp, encoding = "UTF-8"))

#                                  Listing 2.0
#                       MTCARS Data Listing with Page Wrap
# 
#                                         Miles per
# Vehicle                                    Gallon Cylinders Displacement
# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# Mazda RX4                                      21       6.0          160
# Mazda RX4 Wag                                  21       6.0          160
# Datsun 710                                   22.8       4.0          108
# Hornet 4 Drive                               21.4       6.0          258
# Hornet Sportabout                            18.7       8.0          360
# Valiant                                      18.1       6.0          225
# Duster 360                                   14.3       8.0          360
# Merc 240D                                    24.4       4.0        146.7
# Merc 230                                     22.8       4.0        140.8
# Merc 280                                     19.2       6.0        167.6
# 
# ...
# 
#                                                                    Page 1 of 2
#                                  Listing 2.0
#                       MTCARS Data Listing with Page Wrap
# 
# Vehicle                              Horsepower Weight  Quarter Mile Time
# ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
# Mazda RX4                                   110   2.62              16.46
# Mazda RX4 Wag                               110  2.875              17.02
# Datsun 710                                   93   2.32              18.61
# Hornet 4 Drive                              110  3.215              19.44
# Hornet Sportabout                           175   3.44              17.02
# Valiant                                     105   3.46              20.22
# Duster 360                                  245   3.57              15.84
# Merc 240D                                    62   3.19                 20
# Merc 230                                     95   3.15               22.9
# Merc 280                                    123   3.44               18.3
# 
# ...
# 
#                                                                    Page 2 of 2

```

