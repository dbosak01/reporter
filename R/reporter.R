#' @title reporter: A package for creating statistical reports
#'
#' @description The \strong{reporter} package creates statistical reports in TXT, 
#' RTF, PDF, HTML, and DOCX file formats.  Features include automatic page wrapping and 
#' breaking for wide and long tables, spanning headers, titles, footnotes, 
#' page headers, page footers, and page numbering.  The package allows mixing
#' of multiple tables, text, and plots in a single report, or even on a single
#' page.     
#'     
#' @details 
#' The \strong{reporter} package creates regulatory-style, statistical reports. It 
#' was designed to create tables, listings, and figures (TLFs) 
#' for use in the pharmaceutical, biotechnology, and medical-device industries.
#' However, the functions are generalized enough to provide statistical reporting
#' for any industry.  The package is written primarily in Base R, and has 
#' few dependencies on other packages.
#' 
#' The package is intended to give R programmers 
#' flexible report layout capabilities, and a choice of output formats. 
#' The package will 
#' initially focus on printable, file-based 
#' output formats, as there are already numerous R packages that 
#' provide tabular reporting in HTML. 
#' 
#' PDF output may have limitations not associated with other output types.  
#' See \link{NotesOnPDF}
#' for more information.
#'     
#' @section Key Features:
#' The \strong{reporter} package contains the following key features:
#' \itemize{
#'   \item Titles, footnotes, page header, and page footer are repeated on each page
#'   \item Supports header labels and spanning headers 
#'   \item Calculates default columns widths automatically
#'   \item Includes automatic wrapping and splitting of wide and long tables
#'   \item Integrates with the \strong{\link[fmtr]{fmtr}} package to format 
#'         numeric, date, and character data
#'   \item Plots from the popular \strong{\link[ggplot2]{ggplot2}} package can 
#'         be added to RTF, PDF, HTML, and DOCX reports
#'   \item Allows appending multiple tables to a report, multiple tables to a page, 
#'         and intermingling of text, tables, and plots
#'   \item Supports in-report date/time stamps and "Page X of Y" page numbering
#' }
#'
#' @section Key Functions:
#' \itemize{
#'   \item \code{\link{create_report}} to define a report
#'   \item \code{\link{create_table}} to define a table
#'   \item \code{\link{create_text}} to define text content
#'   \item \code{\link{create_plot}} to define a plot 
#'   \item \code{\link{write_report}} to write out the report
#' }
#' 
#' @section Package Assumptions and Limitations:
#' Note that the \strong{reporter} package is built on several assumptions, and 
#' has some limitations.  Those
#' assumptions and limitations are as follows:
#' \itemize{
#'   \item The current version supports both a monospace, fixed-width style 
#'   report and a variable-width style report.  The monospace report is the 
#'   default.  To create a variable width report, set the font parameter on
#'   the \code{create_report} function to 'Arial', 'Courier', or 'Times'.
#'   \item If a font is selected, it will be applied uniformly to the entire
#'   report.  The package has no capabilities to mix different fonts in the 
#'   same report.
#'   \item The package assumes that, except for formatting and layout, 
#'   the incoming data is ready for 
#'   printing. The \strong{reporter} package has no capabilities to perform 
#'   calculations, summaries, grouping, or derivations.  Use the many 
#'   capabilities in R to perform these operations prior to sending data
#'   to the \strong{reporter} package.
#'   \item It is assumed that the incoming data is sorted as desired.  
#'   The \strong{reporter} package
#'   has no capabilities for sorting.  Use the sorting functionality in 
#'   Base R or supplemental packages to sort the data prior to sending to
#'   \strong{reporter}.
#'   \item For monospace reports, titles, footnotes, page headers, and page 
#'   footers must fit in the available space. 
#'   If they don't, the \strong{reporter} package will 
#'   generate a warning.  In these situations, the recommended course of 
#'   action is to split the offending string into two or more strings that 
#'   fit within the available width.
#'   \item The \strong{reporter} package will never break a word. That means you 
#'   cannot set a column width that is less than the length of the longest 
#'   word.  If you wish to break words, add the breaks with
#'   \code{\link[base]{strwrap}} or an equivalent function before reporting.  
#'   \item The package will make a best guess on column widths based on the 
#'   width of the data and column headers.  To override the best guess,
#'   set the column width directly by placing a width attribute on the variable
#'   or by using the \code{\link{column_defaults}} or 
#'   \code{\link{define}} functions. 
#'   \item The max automatic column width
#'   is 5 inches.  Longer data values will be wrapped.
#'   \item The package support plots from \strong{ggplot2}. These plots
#'   can be added to RTF, PDF, HTML, and DOCX output types.  The package does not 
#'   support Base R plots.
#'   \item The package currently supports styling for HTML reports.  
#'   This styling allows setting background colors, font colors, border
#'   colors, and some font sizing and bolding.  Styling for additional
#'   output types will be available in future versions of the package.  
#' }
#' 
#' @docType package
#' @name reporter
NULL

#' @title Notes on PDF output type
#' @description 
#' PDF output supports a limited character set.  Some Unicode characters
#' can cause the PDF rendering to fail.  All Latin characters and other 
#' common characters like 
#' mathematical operators and Greek letters are supported.  However, 
#' other special characters and characters from Asian languages may not 
#' be supported, depending on the operating system and installed operating 
#' system languages. If a character is not supported, it will be replaced
#' with a question mark (?).
#' 
#' As of \strong{reporter} v1.1.3, the package will 
#' generate PDF files directly, without using \strong{rmarkdown} as an 
#' intermediary. This technique eliminates dependencies, makes the package 
#' easier to install, and greatly improves performance.  It is recommended
#' to use the most recent version of the package if possible.
#' 
#' For versions of \strong{reporter} prior to v1.1.3, the package has
#' additional dependencies and installation requirements.  See details below.
#' 
#' @details 
#' For \strong{reporter} v1.1.2 and below, 
#' PDF output type requires the \strong{rmarkdown} package and a 
#' LaTeX renderer like \strong{MiKTeX}.  
#' \strong{MiKTeX} must be installed separately, and will
#' not be installed as part of the \strong{reporter} install.  This program must
#' furthermore be added to the \code{path} environment variable on the system
#' where \strong{reporter} will run, so that \strong{reporter} can find it.
#' 
#' @name NotesOnPDF
NULL
