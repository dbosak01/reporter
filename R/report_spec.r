

# Report Spec Assembly Functions -----------------------------------------------


#' @title
#' Create a report
#'
#' @description
#' Creates a report shell to which you may add content.
#' 
#' @details
#' This function is the constructor for the report_spec object.  The report_spec
#' object contains information needed to construct a report. The object is
#' defined as an S3 object, and is inherited from a list.
#' 
#' The report_spec object hold information concerning report page size, font, 
#' titles, footnotes, page header, page footer, margins, and other options.  
#' Use the \code{add_content()} function to add content to the report.  
#' The report may be written using the \code{write_report()} function. 
#'
#' @param file_path The output path of the desired report. Either a full path or
#' a relative path is acceptable.  This parameter is not required to create the
#' report_spec object, but will be required to print the report.
#' @param output_type The report output type.  Valid values are "text" and 
#' "docx".  Default is "text".
#' @param orientation The page orientation of the desired report.  Valid values
#' are "landscape" or "portrait".  The default page orientation is "landscape".
#' @return A new report_spec S3 object.
#' @seealso 
#' \itemize{
#'   \item \code{\link{titles}} to add titles to the report.
#'   \item \code{\link{footnotes}} to add footnotes to the report.
#'   \item \code{\link{page_header}} to add a page header to the report. 
#'   \item \code{\link{page_footer}} to add a page_footer to the report. 
#'   \item \code{\link{add_content}} to add content to the report.
#'   \item \code{\link{options_text}} to set options for text output.
#'   \item \code{\link{options_docx}} to set options for docx output.
#'   \item \code{\link{add_content}} to add content to the report.
#'   \item \code{\link{write_report}} to write the report to the file system.
#' }
#' @examples
#' # Create the report object
#' rpt <- create_report("output/mtcars.docx", orientation="portrait") 
#' 
#' # Add content to the report
#' rpt <- add_content(rpt, create_table(mtcars)) 
#' 
#' # Write the report to the file system
#' #write_report(rpt)
#' @export
create_report <- function(file_path = "", output_type = "text", 
                          orientation="landscape") {

  x <- structure(list(), class = c("report_spec", "list"))

  # Trap missing or invalid output_type parameter
  if (!output_type %in% c("text", "docx")) {
    
    stop(paste0("ERROR: output_type parameter on create_report() ",
                "function is invalid: '", output_type,
                "'\n\tValid values are: 'text', 'docx'."))
  }
  
  # Trap missing or invalid orientation parameter.
  if (!orientation %in% c("landscape", "portrait")) {

    stop(paste0("ERROR: orientation parameter on ",
                "create_report() function is invalid: '", orientation,
                "'\n\tValid values are: 'landscape' or 'portrait'."))
  }

  # Populate report_spec fields
  x$file_path <- file_path
  x$output_type <- output_type
  x$orientation <- orientation
  x$content <- list()
  
  if (output_type == "text") {
    
    # Set default options for text
    x <- options_text(x)
    
  } else if (output_type == "docx") {
    
    # Set default options for docx
    x <- options_docx(x)
    
    # Set default margins
    x <- set_margins(x)
  }



  return(x)

}

#' @title
#' Set options for a docx report
#'
#' @description
#' This function sets the options for a report of output type docx.
#'
#' @param x The report spec.
#' @param font_name The font name to use on the report.  The specified font
#' will be used on the entire report.  Valid values are "Courier New", "Arial",
#' "Calibri", or "Times New Roman".  The default font is "Courier New".
#' @param font_size The font size.  Valid value is 10.
#' @return The updated report spec.
#' @examples
#' # Here is an example
#' @export
options_docx <- function(x, font_name="Courier New", font_size=10) {
  
  # Trap missing or invalid font_name parameter.
  if (!font_name %in% c("Courier New", "Times New Roman", "Arial", "Calibri")) {
    
    stop(paste0("ERROR: font_name parameter on create_report() ",
                "function is invalid: '", font_name,
                "'\n\tValid values are: 'Arial', 'Calibri', 'Courier New', ",
                "and 'Times New Roman'."))
  }
  
  x$font_size <-font_size
  x$font_name <- font_name
  x$font_family <- get_font_family(font_name)
  
  return(x)
}

#' @title
#' Set options for a docx report
#'
#' @description
#' This function sets the options for a report of output type docx.
#'
#' @param x The report spec.
#' @param cpi Characters per inch.  Valid values are between 8 and 14.  Default
#' value is 12.
#' @return The updated report spec.
#' @examples
#' # Here is an example
#' @export
options_text <- function(x, cpi = 12) {
  
  # Trap missing or invalid font_name parameter.
  if (!(cpi >= 8 & cpi <= 14)) {
    
    stop(paste0("ERROR: cpi parameter on create_report() ",
                "function is invalid: '", cpi,
                "'\n\tValue must be between 8 and 14."))
  }
  
  x$cpi <- cpi
  
  return(x)
  
}

#' @title
#' Set page margins.
#' @description Set the page margins on the report spec object.
#' @details
#' The margins will be used for the entire report.  Units for the margins
#' are inches.  The default margins are 1 inch on the left and right, and
#' .5 inch on the top and bottom.
#' @param x The report spec object.
#' @param margin_top The top margin.
#' @param margin_bottom The bottom margin.
#' @param margin_left The left margin.
#' @param margin_right The right margin.
#' @return The report_spec with margins set as desired.
#' @examples
#' # Here is an example
# create_report("mtcars.docx", orientation="portrait") %>%
# set_margins(margin_top = 1, margin_bottom =1)  %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# file.show("mtcars.docx")
#' @export
set_margins <- function(x, margin_top=.5, margin_bottom=.5,
                           margin_left=1, margin_right=1) {


  if (is.na(margin_top) | margin_top < 0 | !is.numeric(margin_top)){
    stop("ERROR: invalid value for margin_top.")
  }
  if (is.na(margin_bottom) | margin_bottom < 0| !is.numeric(margin_bottom)){
    stop("ERROR: invalid value for margin_bottom.")
  }
  if (is.na(margin_left) | margin_left < 0| !is.numeric(margin_left)){
    stop("ERROR: invalid value for margin_left.")
  }
  if (is.na(margin_right) | margin_right < 0| !is.numeric(margin_right)){
    stop("ERROR: invalid value for margin_right.")
  }

  # Populate margin value fields
  x$margin_top = margin_top
  x$margin_bottom = margin_bottom
  x$margin_left = margin_left
  x$margin_right = margin_right


  return(x)
}

#' @title
#' Add a page header to the report
#'
#' @description
#' This function adds a page header to the report.  The page header will appear
#' on each page of the report.
#'
#' @details
#' The page header may contain text on the left or right. Use the appropriate
#' parameters to specify the desired text.  The page header may also contain
#' titles for the report, if the location of the
#' the titles has been specified as "header".  See \code{titles()} function
#' for additional details.
#' @param x The report spec object.
#' @param left The left page header text.  May be a single string or a vector
#' of strings.
#' @param right The right page header text.  May be a single string or a vector
#' of strings.
#' @examples
#' # Here is an example
# create_report("mtcars.docx", orientation="portrait") %>%
# page_header(left = "Cars Data", right = "Study ABC")  %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# #file.show("mtcars.docx")
#' @export
page_header <- function(x, left="", right=""){

  x$page_header_left <- left
  x$page_header_right <- right

  return(x)
}


#' @title
#' Add titles to the report
#'
#' @description
#' This function adds one or more to the report.  The titles will be added to
#' the page template, and thus appear on each page of the report.
#'
#' @details
#' The titles function accepts a set of strings of the desired title text.
#' The titles may be aligned center, left or right using the align parameter.
#' The titles may be located in the header, page body, or table according to
#' the location parameter.
#'
#' @param x The report specification object.
#' @param ... A set of title strings.
#' @param location The location to place the titles in the report.  Valid
#' @param align The position to align the titles.  Valid values are: "left"
#' @examples
#' # Here is a comment
# create_report("mtcars.docx", orientation="portrait") %>%
# titles("Cars Table Title", "All Cars")  %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# #file.show("mtcars.docx")
#' @export
titles <- function(x, ..., location = "header", align = "center"){


  tl <- c(...)

  if (length(tl) > 5){
    stop("ERROR: titles function is limited to a maximum of five (5) titles.")
  }

  x$titles <- tl
  x$titles_location <- location
  x$titles_align <- align

  return(x)

}


#' @title
#' Add footnotes to the report
#'
#' @description
#' This function adds one or more footnotes to the report.  The footnotes will
#' be added to the page template, and thus appear on each page of the report.
#'
#' @details
#' The footnotes function accepts a set of strings of the desired footnote text.
#' The footnotes may be aligned center, left or right using the align parameter.
#' The footnotes may be located in the footer, page body, or table according to
#' the location parameter. The user is responsible for adding desired symbols.
#' Footnote symbols will not be generated automatically.
#' @param x The report specification object.
#' @param ... A set of footnotes strings.
#' @param location The location to place the footnotes in the report.
#' Valid values are "header", "body", or "table".
#' @param align The position to align the titles.  Valid values are: "left",
#' "right", or "center".
#' @examples
#' # Here are some examples.
# create_report("mtcars.docx", orientation="portrait") %>%
# titles("Cars Table Title", "All Cars")  %>%
# footnotes("Source: 1974 Motor Trend US magazine") %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# #file.show("mtcars.docx")
#' @export
footnotes <- function(x, ..., location = "footer", align = "left"){

  ft <- c(...)

  x$footnotes <- ft
  x$footnotes_location <- location
  x$footnotes_align <- align

  return(x)

}

#' @title
#' Add a page footer to the report
#'
#' @description
#' This function adds a page footer to the report.  The page footer will appear
#' on each page of the report.
#'
#' @details
#' The page footer may contain text on the left, right, or center.
#' Use the appropriate parameters to specify the desired text.  The page footer
#' may also contain footnotes for the report, if the location of the
#' the footnotes has been specified as "footer".  See \code{footnotes()}
#' function for additional details.
#'
#' @param x The report spec object.
#' @param left The left page footer text.  May be a single string or a vector
#' of strings.
#' @param right The right page footer text.  May be a single string or a vector
#' of strings.
#' @param center The center page footer text.  May be a single string or a
#' vector of strings.
#' @examples
#' # Here is an example.
# create_report("mtcars.docx", orientation="portrait") %>%
# page_header(left = "Cars Data", right = "Study ABC")  %>%
# page_footer(left = Sys.time())  %>%
# add_content(create_table(mtcars)) %>%
# write_report()
# #file.show("mtcars.docx")
#' @export
page_footer <- function(x, left="", right="", center=""){

  x$page_footer_left <- left
  x$page_footer_right <- right
  x$page_footer_center <- center

  return(x)
}



#' @title
#' Print the report parameters.
#'
#' @description
#' A function to print the report parameters.  This function is an S3 generic
#' function.  The class printed is "report_spec".
#'
#' @param x The report spec to print.
#' @param ... Generic arguments
#' @param full Whether to print the spec or an abbreviated version.
#' @examples
#' # Here is an example
# create_report("mtcars.docx", orientation="portrait") %>%
# page_header(left = "Cars Data", right = "Study ABC")  %>%
# page_footer(left = Sys.time())  %>%
# add_content(create_table(mtcars)) %>%
# print()
#' @export
print.report_spec <- function(x, ..., full=FALSE){

  if (full)
    print.listof(x, ...)
  else
    print.simple.list(x, ...)

  invisible(x)
}

#' @title
#' Add content to a report
#'
#' @description
#' This function adds an object to the report content list. Valid objects
#' are a table_spec, a flextable, or a plot from ggplot.  Objects will be
#' appended to the report in order they are added.  By default, a page break
#' is added after the content.
#'
#' @param x a report_spec to append content to
#' @param object the object to append
#' @param page_break whether to add a page break. Value values are "before",
#' "after", or "none"
#' @return The modified report_spec
#' @examples
#' #library(magrittr)
#' #create_report("listing_3_0.docx") %>%
#' #add_content(create_table(mtcars)) %>%
#' #write_report()
#'
#' @export
add_content <- function(x, object, page_break="after") {

  # Add page break before if requested
  if (page_break == "before")
    x$content[[length(x$content) + 1]] <- "page_break"

  # Add object to the content list
  x$content[[length(x$content) + 1]] <- object

  # Add page break after if requested, and by default
  if (page_break == "after")
    x$content[[length(x$content) + 1]] <- "page_break"

  return(x)
}


#' @title
#' Write a report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @param ... Any parameters passed to the output-specific write functions.
#' @return The report spec.
#' @export
write_report <- function(x, ...) {
  
  ret <- ""
  
  if (x$output_type == "text") {
    ret <- write_report_text(x, ...)
  } else if (x$output_type == "docx") {
    ret <- write_report_docx(x, ...)
  }
  
  invisible(ret)
}






