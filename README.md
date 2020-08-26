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
and a choice of output formats like  
SAS® ODS. The package will initially focus on printable, file-based 
output formats, as there are already numerous R packages that 
provide tabular reporting in HTML.  
The current version supports TEXT output.  Future releases will 
incorporate RTF, DOCX, and PDF file formats.  

Here is an example of 
a regulatory-style listing using `rptr` and the `mtcars` sample data frame:

```
# Construct data frame from state vectors
df <- data.frame(state = state.abb, area = state.area)[1:10, ]
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
# Supports in-report date/time stamps and "Page X of Y" page numbering

## How to use **rptr**


...
