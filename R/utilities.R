

# Utility Functions -------------------------------------------------------


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



#' Get the font family from the font name
#' @noRd
get_font_family <- function(font_name) {
  
  # Trap missing or invalid font_name parameter
  if (!tolower(font_name) %in% c("arial", "courier", "times")) {
    
    stop(paste0("ERROR: font_name parameter on get_font_family() ",
                "function is invalid: '", font_name,
      "'\n\tValid values are: 'Arial', 'Courier', and 'Times'."
      ))
  }
  
  fam <- "mono"
  # mono, serif, sans
  if (tolower(font_name) == "arial") {
    fam <- "sans"
  } else if (tolower(font_name) == "times") {
    fam <- "serif"
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
  
  # Change copy position depending on location
  cpos <- 1
  if (location == "below")
    cpos <- nrow(x)

  # Set page and page by values
  if ("..page_by" %in% names(x))
    rw[1, "..page_by"] <-  x[1, "..page_by"]
  if ("..page" %in% names(x)) {
    # rw[1, "..page"] <-  x[1, "..page"]
    rw[1, "..page"] <-  x[cpos, "..page"]
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
    
    if ("..page" %in% names(x)) 
      rw2[1, "..page"] <- x[nrow(x), "..page"]
    
    ret <- rbind(rw, ret, rw2)
  }

  return(ret)
}

#' @noRd
compare_rows <- function(row1, row2) {
 
  ret <- TRUE
  for(i in seq_along(row1)) {
    if (!strong_eq(row1[[i]], row2[[i]])) {
      ret <- FALSE
      break
    }
  }
  
  return(ret)
  
}

#' @noRd
strong_eq <- Vectorize(function(x1, x2) {
  
  ret <- TRUE
  if (is.null(x1) & is.null(x2))
    ret <- TRUE
  else if (is.null(x1) & !is.null(x2))
    ret <- FALSE
  else if (!is.null(x1) & is.null(x2))
    ret <- FALSE
  else if (is.na(x1) & is.na(x2))
    ret <- TRUE
  else if (is.na(x1) & !is.na(x2))
    ret <- FALSE
  else if (!is.na(x1) & is.na(x2))
    ret <- FALSE
  else {
    ret <- x1 == x2
    
  }
  
  return(ret)
  
})

#' @noRd
get_breaks <- function(x) {
  
  counter <- 1
  lastrow <- x[1, ]
  ret <- c()
  for(i in seq_len(nrow(x))) {
    row <- x[i, ]
    if (!compare_rows(row, lastrow)) {
      counter <- counter + 1
      lastrow <- row
      
    }
    ret[length(ret) + 1] <- counter
    
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

  # Seems like this is not needed any more
  # for (nm in vars)
  #   x[[nm]] <- factor(x[[nm]], levels=unique(x[[nm]]))

  # Alternate to get rid of tidyverse dependency
  if (is.null(vars))
    lst <- list(x)
  else {
    
    lst <- split(x, get_breaks(x[vars]))
    
  }

  # print("Split list")
  # print(lst)

  # Create a new list to avoid complaints
  ret <- list()

  # Add blank row for each split
  for (i in seq_along(lst)) {
    
    # Don't append line for blank rows
    if ("..blank" %in% names(lst[[i]]) & all(lst[[i]][["..blank"]] == "B")) {
      
      ret[[i]] <- lst[[i]]
    } else {

      ret[[i]] <- add_blank_row(lst[[i]], location = location, vars = vars)
    }

  }

  # print("After blank row")
  # print(ret)

  # Combine splits
  ret <- do.call("rbind", ret)
  
  rownames(ret) <- NULL

  return(ret)

}


#' @noRd
get_page_size <- function(paper_size, units) {

  # If no paper size specified, 
  # make it essentially infinite.
  ret <- c(1000000, 1000000)
  
  if (units == "inches") {
    if (paper_size == "letter")
      ret <- c(8.5, 11)
    else if (paper_size == "legal")
      ret <- c(8.5, 14)
    else if (paper_size == "A4")
      ret <- c(8.27, 11.69)
    else if (paper_size == "RD4")
      ret <- c(7.7, 10.7)
    
  } else if (units == "cm") {
    if (paper_size == "letter")
      ret <- c(21.59, 27.94)
    else if (paper_size == "legal")
      ret <- c(21.59, 35.56)
    else if (paper_size == "A4")
      ret <- c(21, 29.7)
    else if (paper_size == "RD4")
      ret <- c(19.6, 27.3)
    
     # For character units, 
     # use inches and convert
  }  else if (units == "char") {
    if (paper_size == "letter")
      ret <- c(8.5, 11)
    else if (paper_size == "legal")
      ret <- c(8.5, 14)
    else if (paper_size == "A4")
      ret <- c(8.27, 11.69)
    else if (paper_size == "RD4")
      ret <- c(7.7, 10.7)
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
  wdths <- col_widths[!is.controlv(names(x))]

  for (i in seq_len(nrow(x))) {
    for (nm in names(x)) {

      if (any(typeof(x[[nm]]) == "character") & 
          !is.control(nm) ) {

        if ("..blank" %in% names(x) && x[[i, "..blank"]] == "B") {
          
          cell <- substr(x[[i, nm]], 1, col_widths[[nm]])
          
        } else if ("..blank" %in% names(x) && x[[i, "..blank"]] == "L") {
          
          cell <- stri_wrap(unlist(
            strsplit(x[[i, nm]], split = "\n", fixed = TRUE)), 
            width = sum(wdths), normalize = FALSE)
          
        } else {
          
          cell <- stri_wrap(unlist(
            strsplit(x[[i, nm]], split = "\n", fixed = TRUE)), 
            width = col_widths[[nm]], normalize = FALSE)
        }
        
      
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

#' @noRd
split_strings <- function(strng, width, units, multiplier = 1.03) {
 
  lnlngth <- 0
  ln <- c()
  lns <- c()
  wdths <- c()
  
  un <- "inches"
  w <- width
  if (units == "cm")
    w <- cin(width)
  
  
  if (!is.na(strng)) {
    
    splits <- unlist(stri_split_fixed(strng, "\n"))
    
    for (split in splits) {
      
      wrds <- strsplit(split, " ", fixed = TRUE)[[1]]
      
      # Old code
      # lngths <- (suppressWarnings(strwidth(wrds, units = un)) + 
      #              suppressWarnings(strwidth(" ", units = un))) * multiplier

      if (length(wrds) > 0) {
        lngths <- (strwdth(wrds, un) + strwdth(" ", un)) * multiplier
      } 
      
      # Loop through words and add up lines
      for (i in seq_along(wrds)) {
        
        lnlngth <- lnlngth + lngths[i] 
        if (lnlngth <= w)
          ln <- append(ln, wrds[i])
        else {
          
          if (length(ln) == 0) {
            
            lns[length(lns) + 1] <- wrds[i]
            if (units == "cm")
              wdths[length(wdths) + 1] <- ccm(lnlngth)
            else
              wdths[length(wdths) + 1] <- lnlngth
            
            lnlngth <- 0
            
          } else {
            # Assign current lines and counts
            lns[length(lns) + 1] <- paste(ln, collapse = " ")
            if (units == "cm")
              wdths[length(wdths) + 1] <- ccm(lnlngth - lngths[i])
            else
              wdths[length(wdths) + 1] <- lnlngth - lngths[i]
            
            # Assign overflow to next line
            ln <- wrds[i]
            lnlngth <- lngths[i]
          }
        }
        
        
      }
      
      # Deal with last line
      if (length(ln) > 0) {
        
        lns[length(lns) + 1] <- paste(ln, collapse = " ")
        if (units == "cm")
          wdths[length(wdths) + 1] <- ccm(lnlngth)
        else
          wdths[length(wdths) + 1] <- lnlngth
        
      }
      
      # Reset ln and lnlngth
      ln <- c()
      lnlngth <- 0
      
    }
    
    
  } else {
    
    lns <- ""
    wdths <- 0
  } 
  
  
  ret <- list(text = lns, 
              widths = wdths)
  
  return(ret)
}



strwdth <- Vectorize(function(wrd, un) {
  

    tryCatch({
    
      if (is.na(wrd)) 
        nwrd <- " "
      else 
        nwrd <- wrd
      
      ret <- suppressWarnings(strwidth(nwrd, units = un))
    
    }, error = function(cond) {
      
      if (is.na(wrd)) {
        nwrd <- " "
      } else {
        
        nwrd <- paste0(rep("a", nchar(wrd)), collapse = "") 
      }
  
      ret <- suppressWarnings(strwidth(nwrd, units = un)) 
  
    })
  
  return(ret)
}, USE.NAMES = FALSE, SIMPLIFY = TRUE)

#' @description Calling function is responsible for opening the 
#' device context and assigning the font.  This function will use 
#' strwidth to determine number of wraps of a string within a particular
#' width.  Lines are returned as a single rtf string separated by an rtf
#' line ending.  
#' @noRd
split_string_rtf <- function(strng, width, units, font = "Arial", nm = "", 
                             char_width = 1) {
  
  
  if (tolower(font) == "courier")
    mp <- 1.01
  else 
    mp <- 1.02
  
  # Deal with indents
  blnks <- ""
  indnt <- 0
  cstrng <- strng
  indntw <- 0
  if (nm == "stub") {
    
    bpos <- regexpr("^\\s+", strng)
    if (bpos > 0) {
      
      indnt <-  attr(bpos, "match.length")
      blnks <- paste0(rep(" ", indnt), sep = "", collapse = "")
      cstrng <- substr(strng, indnt + 1, nchar(strng))
      #indntw <- indnt * char_width / mp
    }
  }
  
  res <- split_strings(cstrng, width - indntw, units, multiplier = mp)
  
  
  # Concat lines and add line ending to all but last line.
  # Also translate any special characters to a unicode rtf token
  # Doing it here handles for the entire report, as every piece runs
  # through here.
  ret <- list(rtf = paste0(blnks, encodeRTF(res$text), collapse = "\\line "),
              lines = length(res$text),
              widths = res$widths + indntw)
  
  return(ret)
}

#' @noRd
split_string_html <- function(strng, width, units, nm = "", char_width = 1) {
  
  
  # Deal with indents
  blnks <- ""
  indnt <- 0
  cstrng <- strng
  indntw <- 0
  if (nm == "stub") {
    
    bpos <- regexpr("^\\s+", strng)
    if (bpos > 0) {
      
      indnt <-  attr(bpos, "match.length")
      blnks <- paste0(rep(" ", indnt), sep = "", collapse = "")
      cstrng <- substr(strng, indnt + 1, nchar(strng))
      indntw <- indnt * char_width 
    }
  }
  
  
  res <- split_strings(cstrng, width - indntw, units, multiplier = 1)
  
  ret <- list(html = paste0(blnks, res$text, collapse = "\n"),
              lines = length(res$text),
              widths = res$widths + indntw)
  
  return(ret)
}


#' @noRd
split_string_text <- function(strng, width, units, nm = "", char_width = 1) {
  
  
  # Deal with indents
  blnks <- ""
  indnt <- 0
  cstrng <- strng
  indntw <- 0
  if (nm == "stub") {
    
    bpos <- regexpr("^\\s+", strng)
    if (bpos > 0) {
      
      indnt <-  attr(bpos, "match.length")
      blnks <- paste0(rep(" ", indnt), sep = "", collapse = "")
      cstrng <- substr(strng, indnt + 1, nchar(strng))
      indntw <- indnt * char_width 
    }
  }
  
  res <- split_strings(cstrng, width - indntw, units, multiplier = 1)
  
  ret <- list(text = paste0(blnks, res$text),
              lines = length(res$text),
              widths = res$widths + indntw)
  
  return(ret)
}

#' Split data frame cells based on expected column width.
#' For variable width reports, will just insert a carriage return at the 
#' split points.
#' @param x A data frame
#' @param col_widths A named vector of columns widths in the unit of measure
#' @return The data frame with long values split by carriage returns.
#' @import stringi
#' @import grDevices
#' @noRd
split_cells_variable <- function(x, col_widths, font, font_size, units, 
                                 output_type, char_width) {
  
  dat <- NULL           # Resulting data frame
  wdths <- list()       # Resulting list of widths
  row_values <- list()  # A list to hold cell values for one row 
  row_widths <- list()  # A list to hold text widths for one row
  max_length <- 1       # The maximum number of splits of a cell in that row
  
  fnt <- "mono"
  if (tolower(font) == "arial")
    fnt <- "sans"
  else if (tolower(font) == "times")
    fnt <- "serif"
  
  
  pdf(NULL)
  par(family = fnt, ps = font_size)
  
  
  for (i in seq_len(nrow(x))) {
    for (nm in names(x)) {
      
      nch <- 1
      res <- list(widths = 0)
      
      if (any(typeof(x[[nm]]) == "character") & 
          !is.control(nm) ) {
        
        if ("..blank" %in% names(x) && x[[i, "..blank"]] == "B") {
          
          cell <- ""
          
        } else if ("..blank" %in% names(x) && x[[i, "..blank"]] == "L") {
          
          if (output_type %in% c("HTML", "DOCX")) {
            res <- split_string_html(x[[i, nm]], sum(col_widths), units)
            
            cell <- res$html
            
          } else if (output_type == "RTF") {
            res <- split_string_rtf(x[[i, nm]], sum(col_widths), units, font)
            
            cell <- res$rtf
          } else if (output_type == "PDF") {
            
            res <- split_string_text(x[[i, nm]], sum(col_widths), units)
            
            cell <- paste0(res$text, collapse = "\n")
            
          }
          
        } else {
          
          if (output_type %in% c("HTML", "DOCX")) {
            res <- split_string_html(x[[i, nm]], col_widths[[nm]], units, nm, char_width)
            
            cell <- res$html
          
          } else if (output_type == "RTF") {
            res <- split_string_rtf(x[[i, nm]], col_widths[[nm]], units, font, nm, char_width)
          
            cell <- res$rtf
          } else if (output_type == "PDF") {
            
            res <- split_string_text(x[[i, nm]], col_widths[[nm]], units, nm, char_width)
            
            cell <- paste0(res$text, collapse = "\n")
            
          }
          nch <- res$lines
        }
        
        
      } else {
        cell <- x[i, nm]
      }
      # print(paste("cell: ", cell))
      
      if (identical(cell, character(0)))
        cell <- ""
      
      # print(nch)
      # print(cell)
      # print(max_length)
      
      if (nch > max_length)
        max_length <- nch
      
      
      row_values[[length(row_values) + 1]] <- cell
      if (is.null(res$widths)) 
        row_widths[[length(row_widths) + 1]] <- 0
      else
        row_widths[[length(row_widths) + 1]] <- res$widths
      # print(paste("Row:", row_values))
    }
    
    # print(names(x))
    names(row_values) <- names(x)
    names(row_widths) <- names(x)
    
    row_values$..row <- max_length
    wdths[[length(wdths) + 1]] <- row_widths
    
    if (is.null(dat)) {
      dat <- as.data.frame(row_values, stringsAsFactors = FALSE, 
                           check.names = FALSE)
    } else {
      # names(dat)
      # names(row_values)
      dat <- rbind(dat, row_values)
    }
    
    max_length <- 1
    row_values <- list()
    row_widths <- list()
  }
  
  dev.off()
  
  rownames(dat) <- NULL
  # # Reset names
  # if ("..row" %in% names(x)) 
  #   names(dat) <- c(names(x))
  # else
  #   names(dat) <- c(names(x), "..row")

  ret <- list(data = dat,
              widths = wdths)
  
  return(ret)
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
      if (nm == "..blank") {
        if (is.na(x[[nm]])) 
          v <- c(rep(NA, t))
        else
          v <- c(rep(x[[nm]], t))
      } else if (any(typeof(x[[nm]]) == "character")) 
        v <- c(rep("", t))
      else
        v <- c(rep(NA, t))
      
      ret[[nm]] <- c(x[[nm]], v)
    } else {
      
      ret[[nm]] <-  x[[nm]]
    }
  }
  
  names(ret)  <- names(x)
  ret <- as.data.frame(ret, stringsAsFactors = FALSE)
  
  return(ret)
  
}

#' For any remaining non-character columns, convert to character and
#' clear the missing values.
#' @noRd
clear_missing <- function(x, missing_val) {
  
  for (nm in names(x)) {
    if (!is.control(nm)) { 
      
      if (typeof(x[[nm]]) != "character")
        x[[nm]] <- as.character(x[[nm]])
      
      x[[nm]] <- ifelse(is.na(x[[nm]]), missing_val, x[[nm]])
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
#' @import common
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
          w <- min(nchar(dat[[def$var_c]])) # Take min to exclude label row
          v <- paste0(rep(" ", times = w), collapse = "")
          
          dat[[def$var_c]] <- ifelse(changed(dat[[def$var_c]]),
                                     dat[[def$var_c]], v)
          
          # dat[[def$var_c]] <- ifelse(!duplicated(dat[[def$var_c]]), 
          #                            dat[[def$var_c]], v) 
        }
      }
    }
    ret[[length(ret) + 1]] <- dat
  }
  
  return(ret)
}


clear_formats <- function(x) {
  
  for (nm in names(x)) {
   attr(x[[nm]], "format") <- NULL 
  }
  
  return(x)
}

quote_names <- function(x) {
 
  
  if (typeof(substitute(x, env = environment())) == "language") 
    v <- substitute(x, env = environment())
  else 
    v <- substitute(list(x), env = environment())
  
  vars <- c()
  if (length(v) > 1) {
    for (i in 2:length(v)) {
      vars[[length(vars) + 1]] <- as.character(v[[i]]) 
    }
    
  }
  
  ret <- unlist(vars)
  
  return(ret)
  
}

set_column_defaults <- function(ts, keys) {
  
  ret <- ts$col_defs
  dflts <- ts$col_dflts
  if (!is.null(dflts)) {
    for (i in seq_along(dflts)) {
      
      # A vector of columns names for the defaults
      v <- c()
      
      # An individual default
      dflt <- dflts[[i]]
      
      # Need to get vector of column names whether it is 
      # passed to the default as a vector or a range
      if (!is.null(dflt$vars))
          v <- dflt$vars
      else if (!is.null(dflt$from) & !is.null(dflt$to)) {
         startpos <- which(keys == dflt$from)
         endpos <- which(keys == dflt$to)
         if (startpos > endpos) {
           stop(paste("'from' and 'to' parameters on column_defaults",
                      "must be specified from left to right"))
           
         }
         
         # Get vector of column names from range
         v <- keys[seq(from = startpos, to = endpos)]
        
      } else 
        v <- keys[!is.controlv(keys)]
      
      
      
      for (nm in v) {
        #print(nm)
        
        # if definition doesn't exist, create it
        if (nm %in% names(ret))
          def <- ret[[nm]]
        else 
          def <- define_c(nm)
        
        if (is.null(def$width))
          def$width <- dflt$width
        if (is.null(def$align))
          def$align <- dflt$align
        # Have to deal with $ partial matching
        if (is.null(def[["label"]]))
          def[["label"]] <- dflt[["label"]]
        if (is.null(def$format))
          def$format <- dflt$format
        if (is.null(def$label_align))
          def$label_align <- dflt$label_align
        if (is.null(def$n))
          def$n <- dflt$n
        if (is.null(def$style))
          def$style <- dflt$style
        
        ret[[nm]] <- def
      }
      

    }
  
  }
  
  return(ret)
}

# Create a list of style specs for all columns that have styles assigned.
get_styles <- function(ts) {
  
  ret <- list()
  if (!is.null(ts$col_defs)) {
    for (def in ts$col_defs) {
      
      if (!is.null(def$style)) {
        if (!is.null(def$style$indicator)) {
          icol <- ts$col_defs[[def$style$indicator]] 
          if (!is.null(icol)) {
            if (icol$visible == FALSE) {
              def$style$indicator <- paste0("..x.", def$style$indicator)
            }
          }
        }
        ret[[def$var_c]] <- def$style 
      }
      if (!is.null(ts$stub$style)) {
        if (!"stub" %in% names(styles)) { 
          if (!is.null(ts$stub$style$indicator)) {
            icol <- ts$col_defs[[ts$stub$style$indicator]] 
            if (!is.null(icol)) {
              if (icol$visible == FALSE) {
                ts$stub$style$indicator <- paste0("..x.", ts$stub$style$indicator)
              }
            }
          }
          
          ret[["stub"]] <- ts$stub$style
        }
      }
    }
  } 
  
  return(ret)
}


#' @noRd
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 


#' @noRd
log_logr <- function(x) {
 
  if (length(find.package('logr', quiet=TRUE)) > 0) {
    if (utils::packageVersion("logr") >= "1.2.0") {
      logr::log_hook(x)
    }
  }
}

#' @noRd
has_bottom_footnotes <- function(rs) {
  
  ret <- FALSE
  
  if (!is.null(rs$footnotes)) {
    
    for (ftn in rs$footnotes) {
      if (ftn$valign == "bottom") { 
        ret <- TRUE
        break()
      }
    }
    
  }
  
  return(ret)
}


#' @noRd
has_top_footnotes <- function(rs) {
  
  ret <- FALSE
  
  if (!is.null(rs$footnotes)) {
    
    for (ftn in rs$footnotes) {
      if (ftn$valign == "top") { 
        ret <- TRUE
        break()
      }
    }
    
  }
  
  return(ret)
}

#' @description  Translate border spec to a vector of border positions
#' @noRd
get_outer_borders <- function(brdr_spec) {
  
  ret <- c()
  if (any(brdr_spec == "none")) {
    ret <- c()
  } else {
    
    if (any(brdr_spec == "outside") | any(brdr_spec == "all")) {
      
      ret <- append(ret, c("top", "bottom", "left", "right")) 
    } else {
    
      if (any(brdr_spec == "top"))
        ret <- append(ret, "top")
      if (any(brdr_spec == "bottom"))
        ret <- append(ret, "bottom")
      if (any(brdr_spec == "left"))
        ret <- append(ret, "left")
      if (any(brdr_spec == "right"))
        ret <- append(ret, "right")
    }

  } 
  
  return(ret)
}

#' @noRd
has_page_footer <- function(rs) {
  
  ret <- FALSE
  
  if (!is.null(rs$page_footer_left)) {
    
        ret <- TRUE
  }
  if (!is.null(rs$page_footer_right)) {
    ret <- TRUE
  }
  if (!is.null(rs$page_footer_center)) {
    ret <- TRUE
  }
  
  return(ret)
}

get_spanning_info <- function(rs, ts, pi, widths, gutter = 1) {
  
  spns <- ts$col_spans
  cols <- pi$keys
  cols <- cols[!is.controlv(cols)]
  w <- widths
  w <- w[cols]
  
  # print("Cols:")
  # print(cols)
  #print(w)
  
  # Figure out how many levels there are, 
  # and organize spans by level
  lvls <- c()     # Unique levels
  slvl <- list()  # Will be a list of lists of spans
  for (spn in spns) {
    
    if (!spn$level %in% lvls) {
      lvls[length(lvls) + 1] <- spn$level
      slvl[[spn$level]] <- list()
    }
    slvl[[spn$level]][[length(slvl[[spn$level]]) + 1]] <- spn
  }
  
  # Get unique levels and sort in decreasing order so we go from top down
  lvls <- sort(unique(lvls), decreasing = TRUE)
  # print("Levels:")
  # print(lvls)
  # 
  # print("Spanning levels:")
  # print(slvl)
  
  # Create data structure to map spans to columns and columns widths by level
  # - Seed span_num with negative index numbers to identify unspanned columns
  # - Also add gutter to each column width for the space between columns 
  d <- data.frame(colname = cols, colwidth = w + gutter, 
                  span_num = seq(from = -1, to = -length(cols), by = -1), 
                  stringsAsFactors = FALSE)
  
  wlvl <- list()  # Create one data structure for each level
  for (l in lvls) {
    
    t <- d  # Copy to temporary variable
    # print(t)
    # print(slvl)
    col_span <- c()
    # if column is in spanning column list, populate structure with index.
    # Otherwise, leave as negative value.
    for (i in 1:length(slvl[[l]])) {
      cl <- slvl[[l]][[i]]$span_cols
      
      # Span specifications can be a vector of column names or numbers
      if (typeof(cl) == "character")
        t$span_num <- ifelse(t$colname %in% cl, i, t$span_num)
      else 
        t$span_num <- ifelse(t$colname %in% cols[cl], i, t$span_num)
      
      col_span[i] <- length(cl)

    }
    
    # Aggregate data structures to get span widths for each span
    s <- aggregate(x = list(width = t$colwidth), by = list(span = t$span_num), FUN = sum)
    
    # Then put back in original column order
    s$span <- factor(s$span, levels = unique(t$span_num))
    s <- s[order(s$span), ]
    rownames(s) <- NULL
    
    # Prep data structure
    s$span <- unique(t$span_num)
    s$label <- ""
    s$align <- ""
    s$n <- NA
    s$name <- ""
    s$underline <- TRUE
    s$col_span <- 1
    s$bold <- FALSE
    
    # Populate data structure with labels, alignments, and n values from 
    # spanning column objects
    counter <- 1
    for (index in s$span) {
      if (index > 0) {
        s$label[counter] <- slvl[[l]][[index]]["label"]
        s$align[counter] <- slvl[[l]][[index]]$label_align
        s$underline[counter] <- slvl[[l]][[index]]$underline
        s$bold[counter] <- slvl[[l]][[index]]$bold
        if (!is.null(slvl[[l]][[index]]$n))
          s$n[counter] <- slvl[[l]][[index]]$n
        
      }
      s$name[counter] <- paste0("Span", counter)
      if (index > 0) {
        if (!is.na(col_span[index]))
          s$col_span[counter] <- col_span[index]
      }
      counter <- counter + 1
    }
    
    # Apply n counts to labels
    if (!is.null(ts$n_format)) {
      s$label <- ifelse(is.na(s$n), s$label, paste0(s$label, ts$n_format(s$n))) 
    }
    
    wlvl[[l]] <- s
    
  }
  
  
  return(wlvl)
  
}


#' @import stringi
apply_widths <- function(dat, wdths, algns) {
  
  w <- wdths[!is.controlv(names(wdths))]
  ret <- dat
  
  for (nm in names(ret)) {
    if (!is.control(nm)) {
      
      cw <- wdths[[nm]]
      
      # If column width is miscalculated, fix it.
      if ("..blank" %in% names(dat)) {
        mxw <- max(ifelse(ret[["..blank"]] == "L", 0, nchar(ret[[nm]])), na.rm = TRUE)
        if (mxw > cw)
          cw <- mxw
      }
        
      if (algns[nm] == "left") {
        
        ret[[nm]] <- stri_pad_right(ret[[nm]], cw)
      } else if (algns[nm] == "right") {
        
        ret[[nm]] <- stri_pad_left(ret[[nm]], cw)
      } else if (algns[nm] == "center") {
        
        ret[[nm]] <- stri_pad_both(ret[[nm]], cw) 
      }
      
      # Clear out label rows
      ret[[nm]] <- clear_labels(ret[[nm]], ret[["..blank"]], sum(w)) 

    }
    
    if (any(class(ret[[nm]]) == "factor"))
      ret[[nm]] <- as.character(ret[[nm]])
    
  }
  

   
  return(ret)
}

#' @import stringi
clear_labels <- Vectorize(function(vect, bvect, tw) {
  
  ret <- vect
  if (bvect == "L") {
    if (trimws(vect) == "") {
      ret <- ""
    
    } else {
      
      ret <- stri_pad_right(vect, tw)
    }
  }
  
  return(ret)
}, USE.NAMES = FALSE)

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


#' @noRd
ccm <- function(x) {
  
  return(2.54 * x)
}

#' @noRd
cin <- function(x) {
  
  return(x / 2.54)
}


units_html <- function(u) {
  
 ret <- u  
 if (ret == "inches")
   ret <- "in"
 
 
 return(ret)
  
}

# RTF Functions -----------------------------------------------------------



#' @description Estimate number of wraps based on text, width, and a font.
#' @import stringi
#' @noRd
get_lines_rtf <- function(txt, width, font, font_size = 10, units = "inches") {
  
  
  f <- "mono"
  if (tolower(font) == "arial")
    f <- "sans"
  else if (tolower(font) == "times")
    f <- "serif"
  
  names(width) <- NULL
  
  lns <- unlist(stri_split_fixed(txt, "\n"))
  
  val <- get_text_width(lns, units = units, 
                        font = font, font_size = font_size) * .975/width
  
  # print(val)
  ret <- sum(ceiling(val))

  
  return(ret)
}


#' @description Estimate number of wraps based on text, width, and a font.
#' @import graphics
#' @noRd
get_text_width <- function(txt, font, font_size = 10, units = "inches", 
                           multiplier = .975) {
  
  
  f <- "mono"
  if (tolower(font) == "arial")
    f <- "sans"
  else if (tolower(font) == "times")
    f <- "serif"
  
  un <- "inches"

  
  #R.devices::devEval("nulldev", {
    pdf(NULL)
    par(family = f, ps = font_size)
    if (length(txt) > 0) {
      #ret <- suppressWarnings(strwidth(txt, units = un)) * multiplier 
      ret <- strwdth(txt, un) * multiplier 
    } else {
      
      ret <- 0 
    }
    dev.off()
  #})
  

  if (units == "cm")
    ret <- ccm(ret)
    
  return(ret)
}



# PDF Functions -----------------------------------------------------------

# Convert units to points for PDF x/y placement
cpoints <- function(vals, units) {
  
  if (units == "inches")
    ret <- round(vals * 72, 3)
  else if (units == "cm")
    ret <- round((vals / 2.54) * 72, 3)
  
  names(ret) <- NULL
  
  return(ret)
}

get_points_left <- function(left_bound, right_bound, widths, units) {
  
  # Three points added to the left function to prevent text from touching borders
  ret <- cpoints(rep(left_bound, length(widths)), units) + 3
  
  names(ret) <- NULL
  
  return(ret)
  
} 

get_points_right <- function(left_bound, right_bound, widths, units) {
  
  ret <- cpoints(right_bound - widths, units)
  
  names(ret) <- NULL
  
  return(ret)
} 

get_points_center <- function(left_bound, right_bound, widths, units) {
  
  # One point added to the center function to prevent text from touching borders
  # and center the text more accurately.
  ret <- cpoints(left_bound + ((right_bound - left_bound)/ 2) - (widths/ 2), units) + 1
  
  names(ret) <- NULL
  
  return(ret)
}  

get_points <- function(left_bound, right_bound, widths, units, align) {
  
  ret <- NULL
  if (tolower(align) == "left")
    ret <- get_points_left(left_bound, right_bound, widths, units)
  else if (tolower(align) == "right")
    ret <- get_points_right(left_bound, right_bound, widths, units)
  else if (tolower(align) %in% c("center", "centre"))
    ret <- get_points_center(left_bound, right_bound, widths, units)
  
  
  return(ret)
}


gluev <- function(str) {
 
  ret <- str
  
  if (!is.null(ret)) {
    if (!all(is.na(ret))) {
      if (typeof(ret) == "character") { 
        ret <- glueint(str)
      }
    }
  } 
  
  return(ret)
}

#' @import glue
glueint <- Vectorize(function(str) {
  
  ret <- glue(str, .trim = FALSE)
  
  return(ret) 
}, USE.NAMES = FALSE, SIMPLIFY = TRUE)


#' @noRd
has_glue <- function() {
  
 ret <- TRUE
 
 opts <- options("reporter.glue")[[1]]
 if (!is.null(opts)) {
   if (opts == FALSE) {
     ret <- FALSE 
   }
 }
 
 return(ret)
  
}
