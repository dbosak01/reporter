% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_style.R
\name{cell_style}
\alias{cell_style}
\title{A style specification for a table cell}
\usage{
cell_style(indicator = NULL, bold = FALSE)
}
\arguments{
\item{indicator}{A keyword or column name to indicate which rows the cell
style should be applied to. Valid keywords are "labelrow", "blankrow", or
"datarow". To use an indicator column, create a column on the input dataset
that has a TRUE value for each row that you want the cell style applied.
Then pass the name of the column to the indicator parameter. If you do not
want to see the indicator column on the report, set the "visible" property 
on the \code{\link{define}} function to FALSE for that column. 
The default value of the indicator parameter is NULL, 
meaning to apply the style to all rows.}

\item{bold}{Whether to bold the text in the cell.  Valid values are TRUE and
FALSE.  The default is FALSE.}
}
\description{
A class to define the style for a cell in a table.  This
class can be assigned to the "style" parameter of a \code{\link{define}}
function, a \code{\link{stub}} function, or a \code{\link{column_defaults}}  
function.  When assigned, the cell style will apply to some or all of the cells 
in the relevant columns.  The 
"indicator" parameter identifies a column in the table to trigger the style.
Alternatively, the "labelrow", "blankrow", or "datarow" shortcuts may be used
to identify cells for styling.
}
\examples{
library(reporter)
library(magrittr)

# Create temporary path
tmp <- file.path(tempdir(), "table1.rtf")

# Prepare data
df <- data.frame(names = rownames(mtcars), mtcars[, 1:3])

# Set indicator variable
df$mpgind <- ifelse(df$mpg > 20, TRUE, FALSE)

# Create table
tbl <- create_table(df, first_row_blank = TRUE, 
                    header_bold = TRUE, borders = c("top", "bottom")) \%>\% 
  column_defaults(style = cell_style(bold = TRUE, indicator = "mpgind")) \%>\%
  define(names, label = "Car Name") \%>\%
  define(mpg, label = "Miles Per Gallon") \%>\% 
  define(cyl, label = "Cylinders") \%>\% 
  define(disp, label = "Displacement")  \%>\% 
  define(mpgind, visible = FALSE) \%>\%
  titles("Table 1.0", "MTCARS with Indicator Variable", 
         borders = "none", bold = TRUE, font_size = 11) \%>\% 
  footnotes("* Motor Trend, 1974", borders = "none", blank_row = "none")

# Create report and add custom style
rpt <- create_report(tmp, output_type = "RTF", font = "Arial") \%>\% 
  add_content(tbl) 

# Write out report
write_report(rpt)

# View report
# file.show(tmp)
}
