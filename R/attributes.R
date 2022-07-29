

# Attribute Classes --------------------------------------------------------


#' @title Defines a spanning header attribute
#' @description Create a spanning header attribute object that can be
#' attached to a data frame and passed to the \code{\link{create_table}}
#' function.  This attribute is used internally by other packages in the 
#' \strong{r-sassy} system.  
#' @details 
#' A spanning header is a label and underline that spans one or more 
#' columns.  A spanning header is defined minimally by identifying the
#' column range to be spanned, and a label.  A label alignment 
#' may also be supplied.
#' 
#' The spanning column range is defined by the \code{from} and \code{to} 
#' parameters.  The range identifies a contiguous set of variables on the data.
#' Variables can be identified by position, a quoted variable name, or an 
#' unquoted variable name.
#' @param from The starting column to span.  Spanning columns are defined as
#' range of columns 'from' and 'to'. The columns may be identified by position, 
#' or by quoted or unquoted variable names. 
#' The \code{from} parameter is required.  
#' @param to The ending column to span.  Spanning columns are defined as
#' range of columns 'from' and 'to'. The columns may be identified by position,
#' or by quoted or unquoted variable names.  
#' The \code{to} parameter is required. 
#' @param label The label to apply to the spanning header.
#' @param label_align The alignment to use for the label. Valid values are 
#' "left", "right", "center", and "centre".  The default for spanning columns
#' is "center".
#' @param level The level to use for the spanning header.  The lowest
#' spanning level is level 1, the next level above is level 2, and so on.  
#' By default, the level is set to 1.
#' @param underline A TRUE or FALSE value indicating whether the spanning
#' header should be underlined.  Default is TRUE.  
#' @return The spanning header attribute object.
#' @family attributes
#' @export
span <- function(from, to, label = NULL, label_align = 'center',
                      level = 1, underline = TRUE) {
  
  # Create new structure of class "span_attr"
  x <- structure(list(), class = c("span_attr", "list"))
  x$label <- label
  x$from <- from
  x$to <-  to
  x$label_align <- label_align
  x$level <- level
  x$underline <- underline
  
  return(x)
}


#' @title
#' Creates a title attribute 
#' @description
#' This function creates a titles attribute, which can be used to
#' define titles by attaching as an attribute to a data frame.  This function is 
#' used internally to the system.
#' @param titles A vector of title strings.
#' @param align The position to align the titles.  Valid values are 'left', 
#' 'right', 'center' or 'centre'.  For titles, the default is 'center'.
#' @param blank_row Where to place a blank row.  Valid values are 'above',
#' 'below', 'both', or 'none'.  Default is "below".
#' @param borders Whether and where to place a border. Valid values are 'top',
#' 'bottom', 'left', 'right', 'outside', 'inside', 'all', or 'none'.  
#' Default is "none".  
#' @param width The width of the titles block.  
#' @param bold A parameter to bold the titles.  Valid values are TRUE and FALSE.
#' Default is FALSE.  
#' @param font_size The font size to use for the title block.  The font size
#' of the report will be used by default.  Valid values are 8, 9, 10, 11, 12,
#' 13, and 14.  
#' @return A title attribute object.
#' @seealso \code{\link{titles}} function.
#' @family attributes
#' @export
ttl <- function(titles, align = 'center',
                       blank_row = "below", borders = NULL, width = NULL,
                       bold = FALSE, font_size = NULL) {
  
  # Create new structure of class "title_attr"
  x <- structure(list(), class = c("ttl_attr", "list"))
  
  x$titles <- titles
  x$align <- align
  x$blank_row <- blank_row
  x$borders <- borders
  x$width <- width
  x$bold <- bold
  x$font_size <- font_size
  
  return(x)
}

#' @title
#' Creates a footnote attribute
#' @description
#' The \code{ftn} function creates a footnote attribute,
#' which may be attached as an attribute to a data frame and passed
#' into \code{\link{create_table}}. This function is used internally to
#' the system.
#' @param footnotes A vector of footnote strings.
#' @param align The position to align the footnotes.  Valid values are: 'left',
#' 'right', 'center', or 'centre'.
#' @param blank_row Whether to print a blank row above or below the footnote.
#' Valid values are 'above', 'below', 'both', or 'none'.  Default is 'above'.
#' @param borders Whether to print a border above or below the footnote. Valid
#' values are 'top', 'bottom', 'outside', 'inside', 'all',  or 'none'.  
#' Default is 'none'.  
#' @param valign The vertical position to align the footnotes.  Valid
#' values are: 'top' and 'bottom'.  
#' @param width The width of the footnotes block.  
#' @return The footnote attribute object.
#' @seealso \code{\link{footnotes}} to create a footnote.
#' @family attributes
#' @export
ftn <- function(footnotes, align = 'left',
                          blank_row = "above", borders = "none",
                          valign = NULL, width = NULL) {
  
  # Create new structure of class "ftn_attr"
  x <- structure(list(), class = c("ftn_attr", "list"))
  
  x$footnotes <- footnotes
  x$align <- align
  x$blank_row <- blank_row
  x$borders <- borders
  x$valign <- valign
  x$width <- width
  
  return(x)
}


# Process Attributes ------------------------------------------------------

apply_attributes <- function(x, obj) {
  
  
  ttls <- attr(obj, "titles", exact = TRUE)
  if (!is.null(ttls)) {
    if (is.list(ttls)) {
      for (i in seq_along(ttls)) {
        ttl <- ttls[[i]]
        if ("ttl_attr" %in% class(ttl)) {
          
          x <- titles(x, ttl$titles, align = ttl$align, borders = ttl$borders,
                      bold = ttl$bold, font_size = ttl$font_size, width = ttl$width,
                      blank_row = ttl$blank_row)
          
        }
      }
    }
  }
  
  ftns <- attr(obj, "footnotes", exact = TRUE)
  if (!is.null(ftns)) {
    if (is.list(ftns)) {
      for (i in seq_along(ftns)) {
        ftn <- ftns[[i]]
        if ("ftn_attr" %in% class(ftn)) {

          x <- footnotes(x, ftn$footnotes, align = ftn$align, borders = ftn$borders,
                      valign = ftn$valign, width = ftn$width,
                      blank_row = ftn$blank_row)

        }
      }
    }
  }

  spns <- attr(obj, "spans", exact = TRUE)
  if (!is.null(spns)) {
    if (is.list(spns)) {
      for (i in seq_along(spns)) {
        spn <- spns[[i]]
        if ("span_attr" %in% class(spn)) {

          x <- spanning_header(x, from = spn$from, to = spn$to, 
                               label_align = spn$label_align, 
                               label = spn$label,
                         level = spn$level, underline = spn$underline,
                         standard_eval = TRUE)

        }
      }
    }
  }
  
  return(x)
  
}
