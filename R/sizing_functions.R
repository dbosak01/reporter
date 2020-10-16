


# Sizing Functions --------------------------------------------------------

# @description Splits the data according to height
# @details Logic is to try and estimate the vertical height of each cell
# in a row to get the overall row height.  Then break rows into pages based
# on available body height.  This function is only used for non-text output.
# For text output, get_splits_text is used.
# @import graphics
# @import qlcMatrix
# @import stringi
# @noRd
# get_splits <- function(dat, col_widths, data_size, font_family) {
#   
#   # Row height fixed for now
#   # Ultimately needs to be dynamic based on font size and letting
#   rh <- .3 #strheight("Test String", units = "inches", family = font_family)
#   
#   ws <- list()
#   for (i in seq_along(dat)) {
#     
#     w_all <- strwidth(dat[[i]], units="inches", family=font_family)
#     ws[[length(ws) + 1]] <- w_all
#   }
#   
#   
#   m <- matrix(unlist(ws), ncol=length(ws), byrow=FALSE)
#   # print(m)
#   
#   #lin <- ceiling(m /col_widths)
#   lin <- ceiling(t(t(m) / col_widths))
#   
#   # print(lin)
#   
#   # print(col_widths)
#   
#   # Get counts of carriage returns per cell
#   
#   rs <- list()
#   for (col in seq_along(dat)) {
#     rs[[length(rs)+1]] <- stri_count(dat[[col]], fixed = "\n")
#   }
#   
#   r <- matrix(unlist(rs), ncol=length(rs), byrow=FALSE)
#   
#   # Add carriage returns to line counts
#   lr <- lin + r
#   # print(lr)
#   
#   lr[is.na(lr) | lr == 0] <- 1
#   
#   # print(lr)
#   # print(rowMax(lr)@x)
#   
#   row_heights <- rowMax(lr, ignore.zero = FALSE)@x * rh
#   
#   # print(row_heights)
#   # print(length(row_heights))
#   
#   row_pages <- get_pages(row_heights, data_size["height"])
#   
#   # print(nrow(dat))
#   # print(length(row_pages))
#   # print(row_pages)
#   
#   ret <- split(dat, row_pages)
#   
#   
#   return(ret)
#   
# }

#' @description Subset the data by row and column. Depends on whether
#' the user supplied a preview page number.
#' @noRd
get_data_subset <- function(dat, keys, pages) {
 
  if (!is.null(pages)) {
    est <- 60 * pages
    if (nrow(dat) > est) {
      ret <- dat[seq(1, est ), keys]
    } else {
      ret <- dat[ , keys]
    }
    
  } else {
    
    ret <- dat[ , keys]
  }
   
  return(ret)
}


#' Gets the page wraps
#' @noRd
get_page_wraps <- function(line_size, ts, widths) {
  
  defs <- ts$col_defs
  
  # Get ID variable from definitions
  # These need to be shown on each page
  # Also get page wraps
  if (!is.null(ts$stub)) 
    id_vars <- c("stub")
  else
    id_vars <- c()
  
  wraps <- c()
  nms <- c()
  for (def in defs) {
    if (!is.null(def$id_var) && def$id_var)
      id_vars[length(id_vars) + 1] <- def$var_c
    
    wraps[length(wraps) + 1] <- def$page_wrap
    nms[length(nms) + 1] <- def$var_c
  }
  names(wraps) <- nms

  #print(id_vars)
  
  ret <- list() # list of columns for each page
  pg <- c()     # columns on a page
  tw <- line_size  # width of the page
  
  for (nm in names(widths)) {

    if (!is.control(nm)) {
      #If ID vars exist, add them to list
      if (length(pg) == 0 && length(id_vars) > 0) {

        # Plus 1 for blank space after
        pg <- widths[id_vars] + 1

        if (any(is.na(pg)))
          stop(paste0("ID column width for '", 
                      paste(id_vars[is.na(pg)], sep = " ", collapse = ""),
                      " not found."))

        names(pg) <- id_vars

      }
      
      # Force a page wrap if requested in definition
      force_wrap <- FALSE
      if (is.null(wraps) == FALSE) 
        if (is.na(wraps[nm]) == FALSE) 
          if (wraps[nm] == TRUE)
            force_wrap <- TRUE
      
      # print(paste("Name:", nm))
      # print(paste("Force wrap:", force_wrap))
      # print(paste("Sum of widths:", sum(pg, widths[nm] + 1)))
      # print(paste("Page:", pg))
      # print(paste("Widths:", widths[nm]))
      # print(paste("Total width:", tw))
      # print("")
      

      if ((sum(pg, widths[nm]) >  tw | force_wrap) & (!nm %in% id_vars)) {
        
        # If sum of widths exceed page size, add page to list and reset pg
        # Also add control cols so downstream functions can use them
        ret[[length(ret) + 1]] <- c(names(pg), control_cols)
        pg <- c()
        
        # Add widths for ID vars
        if (length(id_vars) > 0) {
          pg <- widths[id_vars] + 1
          names(pg) <- id_vars
        }
        
        # Add width for current column
        pg[nm] <- widths[nm] + 1
        
      } else {
        
        # If sum of widths does not exceed page size, add to pg and keep going
        pg[nm] <- widths[nm] + 1

      } 
    }
    
  }
  
  # Pick any remaining columns
  if (length(pg) > 0) {
    # Add page to list
    ret[[length(ret) + 1]] <- c(names(pg), control_cols)
  }

  
  return(ret)
  
}


#' @description Preps the data
#' @details 
#' This function performs a variety of tasks needed to prepare the data
#' for printing.  This function must be called before page wrapping and 
#' splitting because it will add rows and columns which must be taken into
#' account during those operations.  Tasks performed include several 
#' options on the variable define function.  These include blank_after, 
#' label_row, indenting, and creating stub columns. 
#' @noRd
prep_data <- function(dat, ts, char_width, missing_val) {
  
  defs <- ts$col_defs
  # print("Before prep data")
  # print(dat)
  
  # Get vector of columns for blank rows
  ls <- c()
  for (def in defs) {

    if (def$blank_after)
      ls[length(ls) + 1] <- def$var_c
  }

  # Add blanks on requested columns
  if (!is.null(ls)) {
    if (length(ls) > 0) {
      
      # Reverse order so groups turn out correct
      ls <- ls[order(ls, decreasing = TRUE)]
      
      # Add blanks
      if (length(ls) > 0) {
        dat <- add_blank_rows(dat, location = "below", vars = ls)
      }
    }
  }
  
   # print("Blanks")
   # print(dat)
  
  # Set up label rows
  for (def in defs) {
    if (def$label_row) {
      
      # Convert to character if necessary
      if (all(dat[[def$var_c]] != "character"))
        dat[[def$var_c]] <- as.character(dat[[def$var_c]])
      
      dat <- add_blank_rows(dat, "label", vars = def$var_c)
    }
  }
  
  
  # print("Labels")
  # print(dat)
  
  # Indent and Dedupe variables as requested
  # Do this after adding blanks
  # So any group values in blank rows are removed
  for (def in defs) {
    if (!is.null(def$indent) | def$dedupe) {
      
      # Convert to character if necessary
      if (all(dat[[def$var_c]] != "character"))
        dat[[def$var_c]] <- as.character(dat[[def$var_c]])
          
      # Actual deduping now takes place in get_splits_text, so 
      # label appears at top of each page

    }
    
    # Perform Indenting of requested variables
    if (!is.null(def$indent)) {
      ind <- floor(def$indent / char_width)
      blnks <- paste0(rep(" ", ind), sep = "", collapse = "")
      dat[[def$var_c]] <- paste0(blnks, dat[[def$var_c]])
    }
  }
  
  # print("Before stub")
  # print(dat)

  # Create stub
  dat <- create_stub(dat, ts)
  
  # print("After stub")
  # print(dat)
    
  # Clear out missing values 
  dat <- clear_missing(dat, missing_val)

  
  return(dat)
  
}


create_stub <- function(dat, ts) {
 
  if (!is.null(ts$stub)) {
    
    s <- ts$stub 
    v <- s$vars
    
    # Initialize with first column 
    st <- dat[[v[1]]]
    #print(st)
    
    # For each subsequent column, if the value is not NA,
    # replace first column value.
    for (i in seq(from = 2, to = length(v), by = 1)) {
      
      st <- ifelse(is.na(dat[[v[i]]]) | trimws(dat[[v[i]]]) == "", st, dat[[v[i]]])    
      
    }

    # Remove stub variables from data frame
    d <-   dat[ , -which(names(dat) %in% v)]
    
    # print("Names before")
    # print(names(d))
    
    # Combine with stub column
    dat <- data.frame(stub = st, d)
    
    # Have to restore names, otherwise data.frame will mess them up
    names(dat) <- c("stub", names(d))
    
    # print("Names after")
    # print(names(dat))
  }
  
  return(dat)
  
}

#' Get the column widths
# @import graphics
#' @import stringi
#' @noRd
get_col_widths <- function(dat, ts, labels, char_width) {
  
  defs <- ts$col_defs
  max_col_width = 5
  min_col_width = .1
  padding_buffer = .05
  nms <- names(labels)
  #print(nms)
  dwidths <- c()
  
  # Set default widths based on length of data
  for (nm in nms) {
  
    
    #w <- max(strwidth(dat[[nm]], units="inches", family=font_family))
    if (is.control(nm) | all(is.na(dat[[nm]]) == TRUE))
      w <- 0
    else
      w <- max(nchar(as.character(dat[[nm]])), na.rm = TRUE) * char_width
     
    if (w > max_col_width)
      w <- max_col_width
    else if (w < min_col_width)
      w <- min_col_width
    else
      w <- (ceiling(w * 100)/100) + padding_buffer
    
    # print(paste("w:", w))
    # print(paste("Label:", labels[[nm]]))
    
    # Determine width of words in label for this column
    s <- stri_split(labels[[nm]], regex=" |\n|\r|\t", simplify = TRUE)
    #l <- strwidth(s[[1]], units="inches", family=font_family)
    l <- max(nchar(as.character(s)), na.rm = TRUE) * char_width
    
    # print(paste("s:", s))
    # print(paste("l:", l))
    
    # If the max word width is greater than the data width,
    # set column width to max label word width
    # so as not to break any words in the label
    if (max(l) > w)
      dwidths[[nm]] <- max(l)
    else
      dwidths[[nm]] <- w
    
  }
  
  # Set names for easy access
  #names(dwidths) <- names(dat)
  
  # Set default widths
  ret = dwidths
  #print("Default Widths")
  #print(ret)
  
  # Let widths on orig df override defaults
  orig <- widths(dat)
  for (nm in names(orig))
    ret[[nm]] <- orig[[nm]]

  
  # Let user definitions override everything
  for (def in defs) {
    
    if (def$var_c %in% names(dat) & !is.null(def$width) && def$width > 0) {
      ret[[def$var_c]] <- def$width
    }
    
  }
  
  # Deal with stub
  if (!is.null(ts$stub)) {
 
    # Add stub width if exists   
    if (!is.null(ts$stub$width))
      ret[["stub"]] <- ts$stub$width
    
  }
  
  # Turn into vector if needed
  ret <- unlist(ret)
  
  # Remove control columns
  ret <- ret[!sapply(names(ret), is.control)]
  
  # Deal with table width

  if (!is.null(ts$width)) {
     # print(paste("Table width:", ts$width))
    
     blnkw <- (length(ret) - 1) * char_width
     # print(paste("Blank width:", blnkw))
     # print(paste("Before:", sum(ret)))
     # print(ret)
     
    if (sum(ret) + blnkw < ts$width) {
      ret <- ret * ((ts$width - blnkw) / sum(ret))
    } else {
      # Come back to this later
      # print("Here")
      # ret2 <- c()
      # pg <- c() 
      # for (i in seq_along(ret)) {
      #   print(ret[i])
      #   print(paste("sumpg:", sum(pg)))
      #   print(paste("lengthpg:", length(pg)))
      #   print(paste("char_width:", char_width))
      #   blnkw <- (length(pg) - 1) * char_width
      #   tot <- sum(pg) + blnkw
      #   print(paste("tot:", tot))
      #   if (tot + ret[i] > ts$width) {
      # 
      #     pg <- pg * ((ts$width - blnkw) / sum(pg))
      #     ret2 <- c(ret2, pg)
      #     print(ret2)
      #     pg <- c(ret[[i]])
      #   } else {
      #     pg[length(pg) + 1] <- ret[[i]]
      #     print(paste("pg:", pg))
      #   }
      #   
      # }
      # 
      # if (length(pg) > 0) {
      #   blnkw <- (length(pg) - 1) * char_width
      #   pg <- pg * ((ts$width - blnkw) / sum(pg))
      #   ret2 <- c(ret2, pg)
      #   
      # }
      # 
      # names(ret2) <- names(ret)
      # print(paste("ret2:", ret2))
      # ret <- ret2
    }
    
    # print(paste("After:", sum(ret)))
    # print(ret)
  }
  

  
  return(ret)
}


# widths not incorporated yet
# Gets the data size
# @import graphics
# @noRd
# get_data_size <- function(body_size, widths, labels, font_family) {
#   
#   #ppi = 72
#   blank_row <- .3
#   
#   sz <- c()
#   for (n in labels) {
#     sz[length(sz) + 1] <- strheight(n, units="inches", family=font_family)
#     
#   }
#   
#   ret <- body_size
#   
#   ret["height"] <- ret["height"] - max(sz) - blank_row
#   
#   return(ret)
#   
# }


get_label_aligns <- function(ts, aligns) {
  
  defs <- ts$col_defs
  
  # Default to column aligns
  ret <- aligns

  # Let any defined value override the default
  for (d in defs) {
    if (!is.null(d$label_align) & d$var_c %in% names(aligns))
      ret[[d$var_c]] <-  d$label_align
    
  }
  
  # Deal with stub
  if (!is.null(ts$stub)) {
  
    # Assing label align if exists
    if (is.null(ts$stub$label_align))
      ret[["stub"]] <- ts$stub[["align"]] 
    else
      ret[["stub"]] <- ts$stub[["label_align"]] 
    
  }
  
  return(ret)
}

#' Gets column formats from definitions
#' @noRd
get_col_formats <- function(dat, ts) {
  
  defs <- ts$col_defs
  ret <- c()
  
  
  # Get any existing formats
  if ("format" %in% ts$use_attributes) {
    ret <- formats(dat)
  }

  for (d in defs) {
    if (!is.null(d$format))
      ret[[d$var_c]] <-  d$format
  }
  

  
  # Deal with stub
  if (!is.null(ts$stub)) {
    
    
    # Add stub format
    if (!is.null(ts$stub$format))
      ret$stub <- ts$stub$format
    
    
  }
  
  return(ret)
}


#' Gets the alignments
#' @noRd
get_aligns <- function(dat, ts) {
  
  defs <- ts$col_defs
  nms <- names(dat)
  ret <- c()
  
  # Get default alignments
  # based on data type
  # Character will go to left
  # Others goes to right
  for (nm in nms) {
    
    if (is.character(dat[[nm]]))
      ret[length(ret) + 1] <- "left"
    else
      ret[length(ret) + 1] <- "right"
    
  }
  
  # Assign names to vector for easy access to alignment values
  names(ret) <- nms

  # Get any justification from original data frame
  if ("justify" %in% ts$use_attributes) {
    orig <- justification(dat)
    for (nm in names(orig))
      ret[[nm]] <- orig[[nm]]
  }
  
  # Assign alignments from column definitions
  for (d in defs) {
    if (!is.null(d$align) & d$var_c %in% nms)
      ret[d$var_c] <- d$align
  }
  
  # Deal with stub
  if (!is.null(ts$stub)) {
    
    # Remove variables associated with stub
    ret <- ret[-which(names(ret) %in% ts$stub$vars)]  
    
    # Assign alignment from stub definition
    ret[["stub"]] <- ts$stub$align 
    
  }
  
  return(ret)
  
}


#' Gets the labels
#' @noRd
get_labels <- function(dat, ts){
  
  defs <- ts$col_defs
  nfmt <- ts$n_format
  
  # Get the column names from the dataframe
  #print("Names in label function")
  v1 <- names(dat)
  #print(v1)
  
  # Get the labels from the dataframe
  # Not so easy because not all columns have labels.
  # If a column has no labels, use the column name
  # as the header string.
  v2 <- c()
  counter <- 1
  
  for (col in dat) {
    if (!is.null(attr(col, "label")) & "label" %in% ts$use_attributes) {
      v2 <- c(v2, attr(col, "label"))
    } else {
      if (is.null( names(col))) {
        v2 <- c(v2, v1[counter])
      } else {
        
        # Not actually sure what the situation is when this would be viable.
        # Consider taking this whole else condition out.
        # Only time I've seen names directly on the column, 
        # something is messsed up.
        if (length(names(col)) == 1)
          v2 <- c(v2, names(col))
        else
          v2 <- c(v2, v1[counter])
      }
      
    }
    counter <- counter + 1
  }
  
  # Convert label vector to a list
  #ls <- as.list(v2)
  ls <- v2


  # Assign names to list
  names(ls) <- v1
  #print(ls)
  
  # Let any defined labels override any attribute labels
  for (def in defs) {

    if (!is.null(def[["label"]]))
      ls[[def$var]] <- def[["label"]]
    
    if (!is.null(def$n) ) {
      ls[[def$var]] <- paste0(ls[[def$var]],  nfmt(def$n))
    }
  }
  
  # Deal with stub
  if (!is.null(ts$stub)) {
    
    # Remove variables associated with stub
    ls <- ls[-which(names(ls) %in% ts$stub$vars)]  
    
    # Assign label from stub definition
    ls <- c(stub = ts$stub$label, ls)
    
  }
  
  
  return(ls)
}


#' @title Get the columns for the table
#'
#' @description Get the columns for the table
#'
#' @param x The Table spec object
#' @noRd
get_table_cols <- function(x) {
  
  dat <- x$data
  
  ret <- c()
  show_all <- FALSE
  if (length(x$show_cols) == 1 && x$show_cols == "all") {
    ret <- names(dat)
    show_all <- TRUE
  }
  else if (length(x$show_cols) == 1 && x$show_cols == "none") {
    show_all <- FALSE
  }
  else if (all(x$show_cols %in% names(dat)))
    ret <- x$show_cols
  
  # Deal with visible options
  if (!is.null(x$col_defs)) {
    for (def in x$col_defs) {
      
      if (show_all == FALSE & def$visible)
        ret[length(ret) + 1] <- def$var_c
      else if (show_all == TRUE & def$visible == FALSE)
        ret <- ret[!ret %in% def$var_c]
    }
    
    ret <- unique(ret)
  }
  
  ret <- unique(c(ret, control_cols))
  
  return(ret)
  
}



# Text Sizing Functions ---------------------------------------------------



#' Split the data horizontally by available page size
#' @param x The data frame to split
#' @param widths The column widths
#' @param page_size The size of the available space in rows
#' @noRd
get_splits_text <- function(x, widths, page_size, lpg_rows, 
                            content_offsets, ts) {
  
  defs <- ts$col_defs
  

  # Calculate where page breaks should occur
  # Based on available height, content size, and offsets
  # Function adds a ..page variable with page indicator
  pgs <- get_page_breaks(x, page_size, lpg_rows, content_offsets)

  
  # Eliminate pages that have only blank lines
  sb <- subset(pgs, trimws(pgs$..blank) == "")
  non_blank_pages <- unique(sb$..page)
  sbst <- subset(pgs, pgs$..page %in% non_blank_pages)
  
  # Split the data frame at the page indicators
  ret <- split(sbst, sbst$..page)
  
  # Perform column deduping
  ret <- dedupe_pages(ret, defs)
  

  
  return(ret)
  
}


#' Function to calculate page breaks
#' @param x Data frame to page.
#' @param page_size Available data height in number of rows.
#' @param lpg_rows Last page rows.  Subtracted from available height.
#' @param content_offsets Blank rows requested above or below content
#' @return Data frame with ..page column populated with page numbers.
#' @noRd
get_page_breaks <- function(x, page_size, lpg_rows, content_offsets){
  
  pg <- 1
  counter <- 0
  offset <- lpg_rows + content_offsets["blank_upper"]
  ttfl <- content_offsets["upper"] + content_offsets["lower"]
  
  # User Paging variable
  currentPage <- NA
  lastPage <- NA
  userForce <- FALSE
  
  # print(paste("Content Upper:", content_offsets["upper"]))
  # print(paste("Content Lower:", content_offsets["lower"]))
  # print(paste("Page size:", page_size))
  # print(paste("Last Page Rows:", lpg_rows))
  # print(paste("Content offset:", offset))

  
  for (i in seq_len(nrow(x))){
    
    counter <- counter + 1
    
    if (i == nrow(x)) {
      # Exception where last line is equal to number of available lines
      # Don't put the blank row in this case.
      if (counter < (page_size - ttfl)) {  
        offset <- offset + content_offsets["blank_lower"]
        #print(paste("Lower Blank:", offset))
      }
    }
    # print(paste("Counter:", counter))
    # print(paste("Condition:", (page_size - offset - ttfl)))
    

    # Get current page value
    currentPage <- x$..page[i]
    
    # If last page is not equal to current page, and neither is NA,
    # force a page break
    if (!is.na(lastPage) & !is.na(currentPage) & 
        trimws(lastPage) != "NA" & trimws(currentPage) != "NA" & 
        trimws(lastPage) != "" & trimws(currentPage) != "" & 
        currentPage != lastPage) {
      userForce <- TRUE

    } else
      userForce <- FALSE
    
    # After comparison, set last page value
    lastPage <- currentPage

    if (counter > (page_size  - offset - ttfl) | userForce) {
      #print(paste("Page count:", counter))
      counter <- 0
      
      # Don't understand why this adjustment is needed.  But it is.
      # Have to figure it out, because it seems wrong.
      if (pg == 1)
        page_size <- page_size - 1
      
      pg <- pg + 1
      offset <- 0
    }
  
    x$..page[i] <- pg
    # print(pg)
  }

  
  # Convert ..page back to number if necessary
  if (class(x$..page) != "numeric")
    x$..page <- as.numeric(x$..page)
  
  #print(paste("Page count:", counter))
  
  return(x)
}









