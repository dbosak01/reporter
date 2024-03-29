% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_spec.r
\name{title_header}
\alias{title_header}
\title{Adds a title header block}
\usage{
title_header(
  x,
  ...,
  right = "",
  blank_row = "below",
  borders = "none",
  width = NULL
)
}
\arguments{
\item{x}{The object to assign titles to.  Valid objects are a report,
table, text, or plot specification.}

\item{...}{A set of title strings.}

\item{right}{A set of header strings to be shown on the right side of the
title header.  Pass the header strings as a vector of strings.}

\item{blank_row}{Where to place a blank row.  Valid values are 'above',
'below', 'both', or 'none'.  Default is 'below'.}

\item{borders}{Whether and where to place a border. Valid values are 'top',
'bottom', 'left', 'right', 'outside', 'inside', 'all', or 'none'.  
Default is 'none'.   The 
'left', 'right', 'outside', and 'inside' border specifications only 
apply to RTF, HTML, PDF and DOCX reports.}

\item{width}{The width of the title header.  If the title header is attached
to the report, valid values are 'page' or a numeric width, and the default
is 'page'. If the title header is attached to the
table, plot, or text content, the valid values are 'page', 'content' or a 
numeric value, and the default is 'content'.  The value 'content' means the 
footnotes will be aligned to the width of the table, plot, or text
content.  The value
'page' means the footnotes will be aligned to the width of the page. 
In addition to these two convenience settings, you 
may also specify a specific width in the current units of measure.  The
units of measure is determined by the 'units' parameter on 
\code{\link{create_report}}.}
}
\value{
The modified report.
}
\description{
This function adds a title header to an object.  A title header is a special
type of title layout that has titles on the left and header information
on the right.
}
\details{
The \code{title_header} function accepts a set of strings of the desired 
title text, and a vector of header strings. The titles will appear on the 
left of the title header, and the header strings on the right. To
specify multiple titles for the block, pass them to the function 
as separate strings.

Title headers may be assigned to a report, a table, a text specification, 
or a plot. If assigned to the report, the title header will appear 
at the top of the page, and be repeated for every page of the report.  
If the title header is assigned to  
content, the titles will appear above the content, and be repeated if the 
content breaks to the next page.  

One title header function accepts up to 10 titles. Blank rows above or below 
the title block may be controlled using the 
\code{blank_row} parameter.

Each title string must fit within the available space.  The \strong{reporter}
package will not wrap titles on fixed-width reports.  
If a title does not fit within the 
available space, an error will be generated.  In these situations, either
shorten the title or split it into multiple titles that each fit within the
available space.
}
\examples{
library(reporter)
library(magrittr)

# Create a temporary file
tmp <- file.path(tempdir(), "expenses.txt")

# Prepare data
dat <- data.frame(category = rownames(USPersonalExpenditure),
                  USPersonalExpenditure, stringsAsFactors = FALSE)

# Define table
tbl <- create_table(dat) \%>\% 
  title_header("Table 1.0", "US Personal Expenditures from 1940 - 1960",
               right = c("Page [pg] of [tpg]", "World Almanac")) \%>\% 
  column_defaults(from = X1940, to = X1960, width = .6, format = "$\%.2f") \%>\%
  define(category, label = "Category") \%>\% 
  define(X1940, label = "1940") \%>\% 
  define(X1945, label = "1945") \%>\% 
  define(X1950, label = "1950") \%>\% 
  define(X1955, label = "1955") \%>\% 
  define(X1960, label = "1960") \%>\% 
  footnotes("* In billions of dollars")

# Define report
rpt <- create_report(tmp, orientation="portrait") \%>\%
  add_content(tbl) 

# Write the report
write_report(rpt)

# Display in console
writeLines(readLines(tmp, encoding = "UTF-8"))

#     Table 1.0                                        Page 1 of 1
#     US Personal Expenditures from 1940 - 1960      World Almanac
# 
#     Category                1940    1945    1950    1955    1960
#     ------------------------------------------------------------
#     Food and Tobacco      $22.20  $44.50  $59.60  $73.20  $86.80
#     Household Operation   $10.50  $15.50  $29.00  $36.50  $46.20
#     Medical and Health     $3.53   $5.76   $9.71  $14.00  $21.10
#     Personal Care          $1.04   $1.98   $2.45   $3.40   $5.40
#     Private Education      $0.34   $0.97   $1.80   $2.60   $3.64
# 
#     * In billions of dollars
}
\seealso{
Other report: 
\code{\link{add_content}()},
\code{\link{create_report}()},
\code{\link{footnotes}()},
\code{\link{options_fixed}()},
\code{\link{page_by}()},
\code{\link{page_footer}()},
\code{\link{page_header}()},
\code{\link{print.report_spec}()},
\code{\link{set_margins}()},
\code{\link{titles}()},
\code{\link{write_report}()}
}
\concept{report}
