
# Flextable and Officer Extended Functions ------------------------------------

#
#' @title
#' Normal theme for flextable.
#'
#' @description
#' A theme for flextable with bold, italic, and row bands turned off.  Just
#' display the data and don't try to pretty it up.
#' (Needs more work to eliminate defaults.)
#' @param x The flextable object.
#' @param fontname The name of the font.  Valid values are "Courier New",
#' "Arial", "Calibri", and "Time New Roman".  Default is "Courier New".
#' @param fontsize The size of the font in points.
#' @return The flextable object modified.
#' @import flextable
#' @import magrittr
#' @export
theme_normal <- function(x, fontname = "Courier New", fontsize=10){

  b_black = fp_border(color = "black")

  ret <- x %>% valign(valign = "top", part = "body") %>%
    font(fontname = fontname, part="all") %>%
    fontsize(size = fontsize, part = "all") %>%
    bold(bold = FALSE, part= "all") %>%
    italic(italic = FALSE, part="all")  %>%
    valign(valign = "bottom", part = "header") %>%
    hline_bottom(border = b_black, part = "header")

  return(ret)
}





#'@title
#'Add a ggplot to the document as an SVG graphic
#'
#'@description
#'This function will add a SVG format ggplot object to the report body.  Note
#'that most versions of Word do not support embedded SVG graphics. In that case,
#'the image will be imbedded as a png.
#'@param x The officer object to add content to.
#'@param value The ggplot object.
#'@param width The desired width of the graphic.
#'@param height The desired height of the graphic.
#'@param res The desired resolution of the graphic.
#'@param style The style used on the ggplot.
#'@param ... Other parameters passed to ggsave.
#'@return None
#'@import ggplot2
#'@import officer
#'@export
body_add_gg_svg <- function( x, value, width = 6, height = 5, res = 300,
                             style = "Normal", ... ) {

  if( !requireNamespace("ggplot2") )
    stop("package ggplot2 is required to use this function")

  stopifnot(inherits(value, "gg") )
  file_path <- tempfile(fileext = ".svg")
  print(file_path)

  ggsave(filename=file_path, plot=value, width=width,
         height=height, dpi=res, ...)

  body_add_img(x, src = file_path, style = style,
               width = width, height = height)

  unlink(file_path)
}

#' @title
#' Add a list of flextables to a document body
#'
#' @description
#' Multi-table add function for flextable.  This function avoids the
#' memory problems associated with adding many multiple tables and
#' creating large documents.
#'
#'
#' @param x officer document to add the flextable to
#' @param flextable_list A list of flextables to add to the document
#' @param align How to align the table within the document.
#' @param pos Where to insert the table within the document.
#' @param split Whether to split the table.
#' @param page_break Whether to add a page break after each table.
#' @return The original officer document with the tables from the list
#' added to the document.
#' @import officer
#' @import flextable
#' @examples
#' # Create sample data
# rowsize <- 160
# col1 <- 1:rowsize
# col2 <- runif(rowsize, 0, 1000)
# col3 <- rep(1:(rowsize /40), each=40)
# data <- data.frame(col1, col2, col3)
# dfs  <- split(data, col3)
#
# # Create flextable objects
# fts <- list()
# b_black = fp_border(color = "black")
# for(pg in dfs){
#   fts[[length(fts) + 1]] <- flextable(pg, theme_fun=NULL)  %>%
#     width(j=c(1, 2, 3), c(2,2,3)) %>%
#     valign(valign = "top", part = "body") %>%
#     font(fontname = "Courier New", part="all") %>%
#     fontsize(size = 10, part = "all") %>%
#     bold(bold = FALSE, part= "all") %>%
#     italic(italic = FALSE, part="all")  %>%
#     hline_bottom(border = b_black, part = "header")
# }
# # Add flextables to document
# my_doc <- read_docx()
# my_doc <- cursor_begin(my_doc)
# my_doc <- body_remove(my_doc)
# my_doc <- body_add_flextables(my_doc, fts)
# print(my_doc, "test.docx")
#' @seealso [body_add_flextable()]
#' @export
body_add_flextables <- function(x, flextable_list, align = "center",
                                pos = "after", split = FALSE, page_break=TRUE) {


  # Initialize variables
  counter <- 1
  length_list <- length(flextable_list)

  # Create temp file
  temp_path <- tempfile(pattern = "", fileext = ".docx")


  for(tbl in flextable_list){

    # Create new document
    temp_doc <- read_docx()

    # temp_doc <- cursor_begin(temp_doc) %>% body_remove()


    # Add flextable to new document
    temp_doc <- body_add_flextable(temp_doc, tbl, align = align, pos = pos, split = split)

    # Print document to temp file
    print(temp_doc, target = temp_path)

    # Add the temp file to the main document
    # This method avoids the memory problems
    # associated with adding the flextables
    # directly to the main document.


    x <- body_add_docx(x, src= temp_path)

    # Add a page break after all but the last flextable
    if (page_break == TRUE & counter < length_list){
      x <- body_add_break(x)
    }

    # Counter for tracking place in list
    counter <- counter + 1
  }

  # Remove temp file
  unlink(temp_path)

  return(x)
}


#' @title
#' Fit a flextable to a specified width
#'
#' @description
#' Fit a flex table to take up the amount of space specified by the
#' \code{pgwidth} parameter.  Default is 9 inches, which will take up
#' the available body on a landscape report with 1 inch margins.
#'
#' @param ft The flextable to fit.
#' @param page_width The width of the available space in inches.
#' @return The flextable returned with column widths resized.
#' @import flextable
#' @examples
#' # Here is an example
# flextable(mtcars)  %>%
#   fit_to_page(pgwidth=8)
#' @export
fit_to_page <- function(ft, page_width = 9) {

  # First run autofit to get proportions
  ft_out <- autofit(ft)

  # Adjust based on pgwidth parameter
  ft_out <- width(ft_out,
                  width = dim(ft_out)$widths*page_width /
                    (flextable_dim(ft_out)$widths))

  return(ft_out)
}

#' @title
#' Use dataframe labels as column headers in flextable
#'
#' @description
#' Assigns the labels associated with a dataframe to the headers in
#' the flextable.
#'
#' @param x The flextable to modify.
#' @return The flextable returned with header labels assigned.
#' @import flextable
#' @export
use_data_labels <- function(x) {

  # Extract the dataframe from the flextable body
  ds <- x$body$dataset

  # Get the column names from the dataframe
  v1 <- names(ds)

  # Get the labels from the dataframe
  # Not so easy because not all columns have labels.
  # If a column has no labels, use the column name
  # as the header string.
  v2 <- c()
  counter <- 1
  for(col in ds){
    if (!is.null(attr(col, "label"))){
      v2 <- c(v2, attr(col, "label"))
    } else {
      if (is.null( names(col))){
        v2 <- c(v2, v1[counter])
      } else {
        v2 <- c(v2, names(col))
      }

    }
    counter <- counter + 1
  }

  # Convert label vector to a list
  ls <- as.list(v2)

  # Assign names to list
  names(ls) <- v1

  # Assign the labels to the flextable using list constructed above
  x <- set_header_labels(x, values = ls)

  return(x)
}




