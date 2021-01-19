#' @title reporter: A package for creating statistical reports
#'
#' @description The \strong{reporter} package creates statistical reports in TXT, 
#' RTF, and PDF file formats.  Features include automatic page wrapping and 
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
#' PDF output has special installation requirements.  See \link{NotesOnPDF}
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
#'         be added to RTF and PDF reports
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
#'   \item The package assumes that displaying the data in the proper
#'   layout is more important than aesthetic considerations like colors,
#'   borders, fonts, striping, etc.  
#'   \item The current version supports a monospace, fixed-width font only.
#'   Variable width fonts will be supported in future versions.
#'   \item RTF and PDF output are actually text reports with an RTF
#'   or PDF wrapper.  Future versions will support native RTF and PDF file 
#'   formats.
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
#'   \item Titles, footnotes, page headers, and page footers must fit in the 
#'   available space. If they don't, the \strong{reporter} package will 
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
#'   can be added to RTF and PDF output types.  The package does not support
#'   Base R plots.
#' }
#' 
#' @docType package
#' @name reporter
NULL

#' @title Notes on PDF output type
#' @description The PDF output type has some limitations not associated with 
#' TXT and RTF output.  PDF has special installation requirements and package
#' dependencies.  
#' 
#' @details 
#' The PDF output type requires the \strong{rmarkdown} package and a 
#' LaTeX renderer like \strong{MiKTeX}.  
#' \strong{MiKTeX} must be installed separately, and will
#' not be installed as part of the \strong{reporter} install.  This program must
#' furthermore be added to the \code{path} environment variable on the system
#' where \strong{reporter} will run, so that \strong{reporter} can find it.
#' 
#' PDF output also supports a limited character set.  Some Unicode characters
#' can cause the LaTeX rendering to fail.  Common Unicode characters like 
#' mathematical operators and Greek letters are supported.  However, 
#' other special characters and characters from Asian languages may not 
#' be supported, depending on the operating system and installed operating 
#' system languages. 
#' 
#' Future versions of \strong{reporter} will eliminate the LaTeX 
#' intermediary and broaden the allowed character set.
#' @name NotesOnPDF
NULL
