# rptr <img src="man/images/reporter.svg" align="right" height="138" />

<!-- badges: start -->

[![rptr version](https://www.r-pkg.org/badges/version/rptr)](https://cran.r-project.org/package=rptr)
[![rptr lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=rptr)
[![rptr downloads](https://cranlogs.r-pkg.org/badges/grand-total/rptr)](https://cran.r-project.org/package=rptr)
[![Travis build status](https://travis-ci.com/dbosak01/rptr.svg?branch=master)](https://travis-ci.com/dbosak01/rptr)

<!-- badges: end -->

The **rptr** package creates regulatory-style, tabular reports. It was designed
for use in the pharmaceutical, biotechnology, and medical-device industries.
However, the functions are generalized enough to provide statistical reporting
for any industry.  

The package is intended to give R programmers 
report layout capabilities such as those found in SAS® PROC REPORT, 
and a choice of output formats like SAS® ODS. The package will 
initially focus on printable, file-based 
output formats, as there are already numerous R packages that 
provide tabular reporting in HTML.  
The current version supports TEXT output.  Future releases will 
incorporate RTF, DOCX, and PDF file formats.  

Here is an example of 
a regulatory-style listing using `rptr` and the `mtcars` sample data frame:

```
# Create temp file name
tmp <- file.path(tempdir(), "listing_1_0.txt")

# Create the report
rpt <- create_report(tmp, orientation = "portrait") %>% 
  page_header(left = "Client: Motor Trend", right = "Study: Cars") %>% 
  titles("Listing 1.0", "MTCARS Data Listing") %>% 
  add_content(create_table(mtcars)) %>% 
  footnotes("* Motor Trend, 1973") %>%
  page_footer(left = Sys.time(), 
              center = "Confidential", 
              right = "Page [pg] of [tpg]")

# Write the report
write_report(rpt)

# Send report to console for viewing
writeLines(readLines(tmp))


# Client: Motor Trend                                                Study: Cars
#                                  Listing 1.0
#                              MTCARS Data Listing
# 
#    mpg    cyl    disp     hp   drat      wt    qsec     vs     am   gear   carb
# -------------------------------------------------------------------------------
#     21      6     160    110    3.9    2.62   16.46      0      1      4      4
#     21      6     160    110    3.9   2.875   17.02      0      1      4      4
#   22.8      4     108     93   3.85    2.32   18.61      1      1      4      1
#   21.4      6     258    110   3.08   3.215   19.44      1      0      3      1
#   18.7      8     360    175   3.15    3.44   17.02      0      0      3      2
#   18.1      6     225    105   2.76    3.46   20.22      1      0      3      1
#   14.3      8     360    245   3.21    3.57   15.84      0      0      3      4
#   24.4      4   146.7     62   3.69    3.19      20      1      0      4      2
#   22.8      4   140.8     95   3.92    3.15    22.9      1      0      4      2
#   19.2      6   167.6    123   3.92    3.44    18.3      1      0      4      4
#   17.8      6   167.6    123   3.92    3.44    18.9      1      0      4      4
#   16.4      8   275.8    180   3.07    4.07    17.4      0      0      3      3
#   17.3      8   275.8    180   3.07    3.73    17.6      0      0      3      3
#   15.2      8   275.8    180   3.07    3.78      18      0      0      3      3
#   10.4      8     472    205   2.93    5.25   17.98      0      0      3      4
#   10.4      8     460    215      3   5.424   17.82      0      0      3      4
#   14.7      8     440    230   3.23   5.345   17.42      0      0      3      4
#   32.4      4    78.7     66   4.08     2.2   19.47      1      1      4      1
#   30.4      4    75.7     52   4.93   1.615   18.52      1      1      4      2
#   33.9      4    71.1     65   4.22   1.835    19.9      1      1      4      1
#   21.5      4   120.1     97    3.7   2.465   20.01      1      0      3      1
#   15.5      8     318    150   2.76    3.52   16.87      0      0      3      2
#   15.2      8     304    150   3.15   3.435    17.3      0      0      3      2
#   13.3      8     350    245   3.73    3.84   15.41      0      0      3      4
#   19.2      8     400    175   3.08   3.845   17.05      0      0      3      2
#   27.3      4      79     66   4.08   1.935    18.9      1      1      4      1
#     26      4   120.3     91   4.43    2.14    16.7      0      1      5      2
#   30.4      4    95.1    113   3.77   1.513    16.9      1      1      5      2
#   15.8      8     351    264   4.22    3.17    14.5      0      1      5      4
#   19.7      6     145    175   3.62    2.77    15.5      0      1      5      6
#     15      8     301    335   3.54    3.57    14.6      0      1      5      8
#   21.4      4     121    109   4.11    2.78    18.6      1      1      4      2
# 
# ...
# 
# * Motor Trend, 1973
# 
# 2020-08-25 23:57:30              Confidential                      Page 1 of 1


```
Here is an example of a regulatory-style table:

```

```

## Key Features

**rptr** contains the following key features:

* Titles, footnotes, page header, and page footer are repeated on each page
* Supports header labels and spanning headers 
* Calculates default columns widths automatically
* Includes automatic wrapping and splitting of wide and long tables
* Integrates with the **fmtr** package to format numeric, date, and character data
* Allows appending multiple tables to a report, multiple tables to a page, 
and intermingling of text and tables
* Supports in-report date/time stamps and "Page X of Y" page numbering

## How to use **rptr**


...
