#' @title rptr: A package for creating statistical reports
#'
#' @description The \strong{rptr} package creates statistical reports in TXT, 
#' RTF, and PDF file formats.  Features include automatic page wrapping and 
#' breaking for wide and long tables, spanning headers, titles, footnotes, 
#' page headers, page footers, and page numbering.  The package allows mixing
#' of multiple tables, text, and plots in a single report, or even on a single
#' page.     
#'     
#' @details 
#' The \strong{rptr} package creates regulatory-style, statistical reports. It 
#' was designed
#' for use in the pharmaceutical, biotechnology, and medical-device industries.
#' However, the functions are generalized enough to provide statistical reporting
#' for any industry.  The package is written primarily in Base R, and has 
#' few dependencies on other packages.
#' 
#' The package is intended to give R programmers 
#' report layout capabilities such as those found in SAS® PROC REPORT, 
#' and a choice of output formats like SAS® ODS. The package will 
#' initially focus on printable, file-based 
#' output formats, as there are already numerous R packages that 
#' provide tabular reporting in HTML. 
#' The current version supports TXT, RTF, and PDF output types. 
#' 
#' PDF output has special installation requirements.  See \link{NotesOnPDF}
#' for more information.
#'     
#' @section Key Features:
#' The \strong{rptr} package contains the following key features:
#' \itemize{
#'   \item Titles, footnotes, page header, and page footer are repeated on each page
#'   \item Supports header labels and spanning headers 
#'   \item Calculates default columns widths automatically
#'   \item Includes automatic wrapping and splitting of wide and long tables
#'   \item Integrates with the \strong{fmtr} package to format numeric, date, 
#'         and character data
#'   \item Plots from the popular \strong{ggplot2} package can be added to RTF 
#'         and PDF reports
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
#' @docType package
#' @name rptr
NULL

#' @title Notes on PDF output type
#' @description The PDF output type has some limitations not associated with 
#' TXT and RTF output.  PDF has special installation requirements, package
#' dependencies, and performance limitations.  
#' 
#' @details 
#' The PDF output type requires the \strong{rmarkdown} package, a LaTeX renderer 
#' like \strong{MiKTeX}, and the PDF manipulation program \strong{qpdf}.  
#' \strong{MiKTeX} and \strong{qpdf} must be installed separately, and will
#' not be installed as part of the \strong{rptr} install.  These programs must
#' furthermore be added to the _path_ environment variable on the system
#' where \strong{rptr} will run, so that \strong{rptr} can find them.
#' 
#' Also note that PDF generation is extremely slow compared to TXT and RTF.  
#' During development of a PDF report, use the \code{preview} parameter of the 
#' \code{\link{write_report}} function to temporarily limit the number of 
#' pages produced.  Once development is complete, remove the preview parameter 
#' and run the entire report.  
#' 
#' Another strategy to improve performance is to output the report to
#' RTF, and then convert to PDF with a document editor or other
#' conversion program. 
#' 
#' PDF output also supports a limited character set.  Some Unicode characters
#' can cause the LaTeX rendering to fail.  Common Unicode characters like 
#' mathematical operators and Greek letters are supported.  However, 
#' other special characters and characters from Asian languages may not 
#' be supported, depending on the operating system and installed operating 
#' system languages. 
#' 
#' Lastly, note that alignment on some styles of PDF report is not rendered
#' accurately.  This misalignment is due to LaTeX rendering of plain text
#' content.  
#' 
#' Future versions of \strong{rptr} will eliminate the LaTeX 
#' intermediary, increase performance, broaden the allowed character set, 
#' and improve alignment.
#' @name NotesOnPDF
NULL
