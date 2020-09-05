

# Utility Functions -------------------------------------------------------

#' @title
#' Simple Concatenation operator
#'
#' @description
#' A simple operator for concatenating strings.  This operator
#' is based on \code{paste0()}, and performs the same function, but with a
#' more compact syntax.
#' @param x The left string to concatenate.
#' @param y The right string to concatenate.
#' @examples
#' "Today is " %+% weekdays(Sys.Date()) %+% "."
#' @seealso [paste0()]
#' @noRd
`%+%` <- function(x, y)paste0(x,y)


#' gen_groups
#'
#' Creates group values based on a number of items and a vector of item
#' counts per group.
#'
#' This function is used to create a vector of group values.  It can use used to
#' dynamically subset a dataframe based on row counts rather than data values.
#' The last_indices parameter can be used to return the indexes of the 
#' last item in each group.
#'
#' @param tot Total number of items in return vector.
#' @param group_cnt Number of items in each group.  This can be a single value
#' or vector of values.  Will recycle if needed.
#' @param last_indices TRUE or FALSE value indicating whether to return the
#' vector of values, or a vector of row indices where the breaks occur.
#' @return A vector of length \code{tot} broken into groups specified by
#' \code{group_cnt}.  Groups will be identified by integers from 1 to n.
#' @examples
#' gen_groups(10, 3)
#' #>[1] 1 1 1 2 2 2 3 3 3 4
#'
#' gen_groups(12, c(3, 2, 5, 2))
#' #>[1] 1 1 1 2 2 3 3 3 3 3 4 4
#'
#' gen_groups(10, 3, last_indices = TRUE)
#' #>[1] 3 6 9 10
#'
#' gen_groups(12, c(3, 2, 5, 2), last_indices = TRUE)
#' #>[1] 3 5 10 12
#' @noRd
gen_groups <- function(tot, group_cnt, last_indices = FALSE) {
  
  # Create empty return vector
  ret <- c()
  
  # Initialize cursors
  cnt <- 0
  ind <- 1
  
  # Populate return vector with group values or last indices
  while(cnt <= tot){
    for(i in seq_along(group_cnt)){
      for(j in 1:group_cnt[i]){
        
        cnt <- cnt + 1
        
        if (cnt <= tot){
          if (last_indices){
            ret[ind] <- cnt
          }else{
            ret[cnt] <- ind
          }
        }
        
      }
      ind <- ind + 1
    }
  }
  
  return(ret)
}



#' split_df_pages
#'
#' A function to split a dataframe into pages according to vectors rows and cols
#'
#' @param df A dataframe to split
#' @param rows A vector of row counts on which to split.  Will recycle
#' if needed.
#' @param cols A vector of column counts on which to split.  Will recycle
#' if needed.
#' @param idcols A vector of id columns to include on each page.
#' @return A list of dataframes sized according to the specifications
#' in \code{rows} and \code{cols}
#' @examples
#' # With row labels and no identity column
# split_df_pages(mtcars, 16, c(5, 6))
#
# # With identity column
# split_df_pages(starwars,10, 5, 1)
#' @noRd
split_df_pages <- function(df, rows, cols, idcols = NULL) {
  
  # Initialize list of dataframe to return
  ret <- list()
  
  # Get the row indicies for each target dataframe
  row_indices <- gen_groups(nrow(df), rows)
  
  # Split the incoming dataframe according to indicies
  split_data <- split(df, row_indices)
  
  # Reapply the labels lost during the split
  data_labeled <- list()
  for(sds in seq_along(split_data)){
    data_labeled[[sds]] <- copy_labels(split_data[[1]], df)
  }
  
  # Generate the column indices to split data vertically
  col_indices <- gen_groups(ncol(df), cols, last_indices = TRUE)
  
  # Split data vertically and add each to return list
  counter <- 1
  for(i in data_labeled){
    startpos <- 1
    for(j in col_indices){
      if (is.null(idcols)){
        ret[[counter]] <- i[ , startpos:j]
      } else {
        ret[[counter]] <- i[ , unique(c(idcols, startpos:j))]
      }
      startpos <- j + 1
      counter <- counter + 1
    }
  }
  
  return(ret)
}

#' Copy labels from one data frame to another.
#' Written to avoid creating dependencies on labeling packages.
#' @noRd
copy_labels <- function(x, y) {
  
  for (i in names(x)) {
    
    attr(x[[i]], "label") <- attr(y[[i]], "label")
    
  }
  
  return(x)
  
}


#' Get the font family from the font name
#' @noRd
get_font_family <- function(font_name) {
  
  # Trap missing or invalid font_name parameter
  if (!font_name %in% c("Arial", "Courier New", "Times New Roman", "Calibri")) {
    
    stop(paste0("ERROR: font_name parameter on get_font_family() ",
                "function is invalid: '", font_name,
      "'\n\tValid values are: 'Arial', 'Calibri', 'Times New Roman', 'Courier'."
      ))
  }
  
  fam <- ""
  # mono, serif, sans
  if(font_name == "Courier New") {
    fam <- "mono"
  } else if (font_name == "Arial") {
    fam <- "sans"
  } else if (font_name == "Times New Roman") {
    fam <- "serif"
  } else if (font_name == "Calibri") {
    fam <- "sans"
  }
  
  return(fam)
  
}




#' @title
#' Add a blank row to a data frame
#'
#' @description
#' The purpose of this function is to add a blank row to the top or bottom
#' of a dataframe.  Character columns will be set to an empty string.  Numeric
#' and date columns will be set to an NA. The function allows the user to pass
#' in values for specified columns.  This feature is useful for setting key
#' values.  Note that a blank value will be added to factor levels that do not
#' contain blanks.
#' @param x The dataframe to add blanks to.
#' @param ... Column names and non-blank values to assign.
#' @param location The location to add the blank row.  Valid values are "above",
#' "below", and "both".  The default value is "below".
#' @return  The input dataset with the blank row added at the specified
#' location.
#' @examples
#' #s <- filter(iris, Species == "setosa")
#' #b <- add_blank_row(s)
#' @noRd
add_blank_row <- function(x, location="below", vars = NULL){


  # Create a blank row with the same structure as the incoming dataframe.
  rw <- x[0, ]
  
  # Get group values
  if (!is.null(vars)) {
    gv <- x[1, vars]
    names(gv) <- vars
    gn <- names(x)
  }

  # For character columns, add a blank.
  # For numeric columns, NA is generated automatically.
  # For factors, cast to vector if blank is not in level list.
  for (i in seq_along(x)) {
    rv <- NA
    if ("character" %in% class(rw[[i]])) {
      
      rv <- ""
      
    } else if("factor" %in% class(rw[[i]])) {

      if (!"" %in% levels(rw[[i]])) {
        levels(x[[i]]) <- c(levels(x[[i]]), "")
        levels(rw[[i]]) <- c(levels(rw[[i]]), "")
      }

      rv <- ""
    }
    
    if (is.null(vars))
      rw[1, i] <- rv
    else if (gn[i] %in% vars) {
      if (!is.data.frame(gv))
        rw[1, i] <- gv[1]
      else
        rw[1, i] <- gv[1, gn[[i]]]
    }
    else
      rw[1, i] <- rv
  }

  
  # Add the blank row to the specified location.
  ret <- x
  if ("..blank" %in% names(ret))
    ret$..blank <- ifelse(is.na(ret$..blank), "", ret$..blank)
  else 
    ret$..blank <- ""
  
  if (location == "below") {
    rw$..blank <- "B"
    ret <- rbind(ret, rw)
  } else if (location == "above") {
    rw$..blank <- "A"
    ret <- rbind(rw, ret)
  } else if (location == "label") {
    rw$..blank <- "L"
    ret <- rbind(rw, ret)
  } else if (location == "both") {
    rw2 <- rw
    rw$..blank <- "A"
    rw2$..blank <- "B"
    
    ret <- rbind(rw, ret, rw2)
  }

  return(ret)
}




#' @title
#' Add blank rows to a data frame after each by-group
#'
#' @description
#' The purpose of this function is to add a blank rows to a dataframe for each
#' by-group.  Character columns will be set to an empty string.  Numeric
#' and date columns will be set to an NA. The function allows the user to pass
#' in column names to group by.  Note that a blank value will be added to factor
#' levels that do not contain blanks.
#' @param x The dataframe to add blanks to.
#' @param ... Column names for group variables.
#' @param .var_list A character vector or list of column names to split by.
#' @return  The input dataset with the blank row added after each by-group.
#' @examples
#' b <- add_blank_rows(iris, Species)
#' @noRd
add_blank_rows <- function(x, location = "below", vars = NULL) {

  
  for (nm in vars)
    x[[nm]] <- factor(x[[nm]], levels=unique(x[[nm]]))

  # Alternate to get rid of tidyverse dependency
  if (is.null(vars))
    lst <- list(x)
  else
    lst <- split(x, x[vars])

  # print("Split list")
  # print(lst)

  # Create a new list to avoid complaints
  # from tidyverse
  ret <- list()

  # Add blank row for each split
  for (i in seq_along(lst)) {

    ret[[i]] <- add_blank_row(lst[[i]], location = location, vars = vars)

  }


  # Combine splits
  ret <- do.call("rbind", ret)
  
  rownames(ret) <- NULL

  return(ret)

}


#' @noRd
get_page_size <- function(paper_size, uom) {

  if (uom == "inches") {
    if (paper_size == "letter")
      ret <- c(8.5, 11)
    else if (paper_size == "legal")
      ret <- c(8.5, 14)
    else if (paper_size == "A4")
      ret <- c(8.27, 11.69)
    else if (paper_size == "RD4")
      ret <- c(7.7, 10.7)
    
  } else if (uom == "cm") {
    if (paper_size == "letter")
      ret <- c(21.59, 27.94)
    else if (paper_size == "legal")
      ret <- c(21.59, 35.56)
    else if (paper_size == "A4")
      ret <- c(21, 29.7)
    else if (paper_size == "RD4")
      ret <- c(19.6, 27.3)
  }
  
  return(ret)
}


#' Split data frame cells into multiple rows based on expected column width.
#' This is necessary to wrap long values onto multiple lines, and have other
#' columns stay aligned horizontally.
#' @param x A data frame
#' @param col_widths A named vector of columns widths in number of characters
#' @return The data frame with long values split and added to their own
#' rows.
#' @noRd
split_cells <- function(x, col_widths) {
  
  dat <- NULL           # Resulting data frame
  row_values <- list()  # A list to hold cell values for one row 
  max_length <- 0       # The maximum number of splits of a cell in that row

  for (i in seq_len(nrow(x))) {
    for (nm in names(x)) {

      if (any(typeof(x[[nm]]) == "character") & !is.control(nm)) {

          cell <- stri_wrap(unlist(
            strsplit(x[[i, nm]], split = "\n", fixed = TRUE)), 
            width = col_widths[[nm]], normalize = FALSE)
        
      
      } else {
        cell <- x[i, nm]
      }
      # print(paste("cell: ", cell))
      
      if (length(cell) > max_length)
        max_length <- length(cell)
      
      if (identical(cell, character(0)))
          cell <- ""
    
      row_values[[length(row_values) + 1]] <- cell
      # print(paste("Row:", row_values))
    }

    names(row_values) <- names(x)

    a <- align_cells(row_values, max_length)
    a$..row <- i

    if (is.null(dat))
      dat <- a
    else
      dat <- rbind(dat, a)
    max_length <- 0
    row_values <- list()
    
  }
  
  # Reset names
  if ("..row" %in% names(x)) 
    names(dat) <- c(names(x))
  else
    names(dat) <- c(names(x), "..row")


  
  return(dat)
}


#' Given a jagged set of vectors, align to the longest by filling with 
#' empty strings
#' @param x A list of vectors of varying lengths
#' @param len The length of the longest vector
#' @return A data frame with the number of rows equal to len. Missing cell
#' values are filled with empty strings.
#' @noRd
align_cells <- function(x, len) {
  
  ret <- list() 
  
  for(nm in names(x)) {
    
    t <- len - length(x[[nm]])
    
    if (t > 0) {
      if (any(typeof(x[[nm]]) == "character")) 
        v <- c(rep("", t))
      else
        v <- c(rep(NA, t))
      
      ret[[nm]] <- c(x[[nm]], v)
    } else {
      
      ret[[nm]] <-  x[[nm]]
    }
  }
  
  names(ret)  <- names(x)
  ret <- as.data.frame(ret)
  
  return(ret)
  
}

#' For any remaining non-character columns, convert to character and
#' clear the missing values.
#' @noRd
clear_missing <- function(x) {
  
  for (nm in names(x)) {
    if (!is.control(nm) & typeof(x[[nm]]) != "character") {    
      x[[nm]] <- as.character(x[[nm]])
      x[[nm]] <- ifelse(is.na(x[[nm]]), "", x[[nm]])
    }
  }
  
  return(x)
}

#' Push string values down to lowest row in data frame
#' @noRd
push_down <- function(x) {

  tot <- nrow(x)

  for (nm in names(x)) {
    tmp <- x[[nm]][x[[nm]] != ""]
    x[[nm]] <- c(rep("", tot - length(tmp)), tmp) 
  }
  
  return(x)
}

#' @description Dedupe requested columns
#' @details This function is performed in the page splitting routine
#' so that groups which span multiple pages retain a label at the top 
#' of the page.
#' @noRd
dedupe_pages <- function(pgs, defs) {
  
  ret <- list()

  for (dat in pgs) {
    nms <- names(dat)
    for (def in defs) {
      if (def$dedupe) {
        
        if(def$var_c %in% nms) {
        
          # Convert to character if necessary
          if (all(dat[[def$var_c]] != "character"))
            dat[[def$var_c]] <- as.character(dat[[def$var_c]])
          
          # Fill with blanks as appropriate
          w <- nchar(dat[[def$var_c]][1])
          v <- paste0(rep(" ", times = w), collapse = "")
          
          dat[[def$var_c]] <- ifelse(!duplicated(dat[[def$var_c]]), 
                                     dat[[def$var_c]], v) 
        }
      }
    }
    ret[[length(ret) + 1]] <- dat
  }
  
  return(ret)
}


# Sizing utilities --------------------------------------------------------

#' @noRd
get_content_size <- function(rs) {
  
  
  # Assume landscape
  pg_h <- rs$page_size[1]
  pg_w <- rs$page_size[2]
  
  # Change to portrait
  if(rs$orientation == "portrait") {
    pg_w <- rs$page_size[1]
    pg_h <- rs$page_size[2]
  }
  
  # Calculate available space for page body
  ret <- c(height = pg_h - rs$margin_top - rs$margin_bottom ,
           width = pg_w - rs$margin_right - rs$margin_left)

  
  return(ret)
  
}


#' @param rs Report spec
#' @param pt Page Template
#' @noRd
get_body_size <- function(rs) {
  

  # Calculate header and footer heights
  h_h <- get_header_height(rs)
  f_h <- get_footer_height(rs)
  
  # Calculate available space for page body
  ret <- c(height = rs$content_size[["height"]] - h_h - f_h,
           width = rs$content_size[["width"]])
  
  return(ret)
}

#' @noRd
get_header_height <- function(rs) {
  
  # Get height of page header
  phdr <- rs$page_header_left
  if(length(rs$page_header_left) < length(rs$page_header_right))
    phdr <- rs$page_header_right
  
  if (rs$output_type == "docx") { 
    
    # DOCX not available
    
    # hh <- sum(strheight(phdr, units = "inches", family = rs$font_family))
    # 
    # # Get height of titles
    # th <- sum(strheight(rs$titles, units = "inches", family = rs$font_family))
    # 
    # # Add buffer for table margins, etc.
    # buff <- .1  # Will need to adjust this
    # 
    # if (rs$uom == "cm") {
    #   hh <- ccm(hh)
    #   th <- ccm(th)
    #   buff <- ccm(buff)
    # }
    
  } else {
    
    hh <- length(phdr) * rs$line_height
    th <- length(rs$titles) * rs$line_height
    buff <- 0
    
  }
  
  # Add all heights
  ret <- hh + th + buff
  
  return(ret)
}

#' @noRd
get_footer_height <- function(rs) {
  
  # Get height of page header
  pftr <- rs$page_footer_left
  if(length(rs$page_footer_left) < length(rs$page_footer_right))
    pftr <- rs$page_footer_right
  if(length(pftr) < length(rs$page_footer_center))
    pftr <- rs$page_footer_center
  
  if (rs$output_type == "docx") {
  
    # DOCX not available
    
    # fh <- sum(strheight(pftr, units = "inches", family = rs$font_family))
    # 
    # # Get height of footnotes
    # fth <- sum(strheight(rs$footnotes, units = "inches", family = rs$font_family))
    # 
    # # Add buffer for table margins, etc.
    # buff <- .1  # Need to adjust
    # 
    # if (rs$uom == "cm") {
    #   fh <- ccm(fh)
    #   fth <- ccm(fth)
    #   buff <- ccm(buff)
    # }
    
  } else {
    
    fh <- length(pftr) * rs$line_height
    fth <- length(rs$footnotes) * rs$line_height
    buff <- rs$line_height # Space between footnotes and page footer
    
  }
  
  # Add all heights
  ret <- fh + fth + buff
  
  return(ret)
}

#' @noRd
ccm <- function(x) {
  
  return(2.54 * x)
}



