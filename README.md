<!-- badges: start -->

[![reporter version](https://www.r-pkg.org/badges/version/reporter)](https://cran.r-project.org/package=reporter)
[![reporter lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=reporter)
[![reporter downloads](https://cranlogs.r-pkg.org/badges/grand-total/reporter)](https://cran.r-project.org/package=reporter)
[![Travis build status](https://travis-ci.com/dbosak01/reporter.svg?branch=master)](https://travis-ci.com/dbosak01/reporter)
<!-- badges: end -->

# Introduction to **reporter**
<img src="man/images/reporter4.png" align="left" height="138" style="margin-right:10px;"/>

Historically, R has not been very strong on reporting.  The **reporter**
package aims to fill that gap.  

Using **reporter**, you can create a report in just a few lines of code. 
Not only is it easy to create a report, but the **reporter** package can handle 
all sorts of situations that other packages struggle with.

For example, unlike other packages, the **reporter** package creates the *entire
report*: page header and footer, titles, footnotes, tables - everything.
The end result of a reporter call is a complete, printable report. 

In addition, **reporter** can handle page breaking, page wrapping, and 
automatic sizing of column widths.  The package
offers a choice of output file types.  And it supports the inclusion of 
tables, text and graphics into a report.

What is more, the package does not expect you to know R Markdown, knitr, or pandoc.
You do not need to learn Latex, HTML, or any other intermediate language.
With **reporter**, you send your data into a create function, assign 
titles and footnotes, and write the report.  That's it!

If you are familiar with SAS® software, you may notice some 
similarity between **reporter** functions and `proc report`.  This similarity,
however, is only on the surface.  The implementation of **reporter** is
done entirely in R, and, internally, is modeled in a different way. 
However, SAS® users will find the **reporter** 
functions very convenient and easy to understand compared to the
alternatives.

### Installation

The **reporter** package can be installed from the console.  Simply run 
the following command: 

    install.packages("reporter")
    
Or if you want the latest development version, you can install it directly
from github:

    devtools::install_github("https://github.com/dbosak01/reporter")


Then put the following line at the top of your program or script:

    library(reporter)

The **reporter** package will give you access to a number of functions
to help create, lay out, and write your report to the file system. 
For examples and usage information, visit the **reporter** documentation
site [here](http://reporter.r-sassy.org/articles/reporter.html).

### Getting Help

If you need help, the first place 
to turn to is the [reporter](http://reporter.r-sassy.org) web site. The web site
has full documentation on all **reporter** functions.

If you need additional help, please consult 
[stackoverflow.com](https://stackoverflow.com).  The stackoverflow 
community will be very willing to answer your questions.  

If you want to look at the code for the **reporter** package, visit the
github page [here](https://github.com/dbosak01/reporter).

If you encounter a bug or have a feature request, please submit an issue 
[here](https://github.com/dbosak01/reporter/issues).

### See Also

The **reporter** package is part of the **sassy** meta-package. 
The **sassy** meta-package includes several packages that help make R
easier for SAS® programmers.  You can read more about the **sassy** package
[here](http://sassy.r-sassy.org).
