


# Sizing Functions --------------------------------------------------------

#' Splits the data according to height
#' @import graphics
#' @import qlcMatrix
#' @noRd
get_splits <- function(dat, col_widths, data_size, font_family) {
  
  
  rh <- .3 #strheight("Test String", units = "inches", family = font_family)
  
  ws <- list()
  for (i in seq_along(dat)) {
    
    w_all <- strwidth(dat[[i]], units="inches", family=font_family)
    ws[[length(ws) + 1]] <- w_all
  }
  
  
  m <- matrix(unlist(ws), ncol=length(ws), byrow=FALSE)
  
  # print(m)
  
  #lin <- ceiling(m /col_widths)
  lin <- ceiling(t(t(m) / col_widths))
  
  # print(lin)
  
  # print(col_widths)
  
  # Get counts of carriage returns per cell
  
  rs <- list()
  for (col in seq_along(dat)) {
    rs[[length(rs)+1]] <- stri_count(dat[[col]], fixed = "\n")
  }
  
  r <- matrix(unlist(rs), ncol=length(rs), byrow=FALSE)
  
  # Add carriage returns to line counts
  lr <- lin + r
  # print(lr)
  
  lr[is.na(lr) | lr == 0] <- 1
  
  # print(lr)
  # print(rowMax(lr)@x)
  
  row_heights <- rowMax(lr, ignore.zero = FALSE)@x * rh
  
  # print(row_heights)
  # print(length(row_heights))
  
  row_pages <- get_pages(row_heights, data_size["height"])
  
  # print(nrow(dat))
  # print(length(row_pages))
  # print(row_pages)
  
  ret <- split(dat, row_pages)
  
  
  return(ret)
  
}



# b <- get_body_size(rpt)
# l <- get_labels(tb$data, tb$col_defs, tb$n_format)
# d <- get_data_size(b, NULL, l, "mono" )
# w <- get_col_widths(tb$data, tb$col_defs, l, "mono")
# p <- get_splits(tb$data, w, d, "mono")


#' Gets the page wraps
#' @noRd
get_page_wraps <- function(data_size, defs, widths) {
  
  id_vars <- c()
  for (def in defs) {
    if (!is.null(def$id_var) && def$id_var)
      id_vars[length(id_vars) + 1] <- def$var_c
    
  }
  
  
  ret <- list()
  pg <- c()
  tw <- data_size["width"]
  
  for (nm in names(widths)) {
    if (length(pg) == 0 && length(id_vars) > 0) {
      pg <- widths[id_vars]
      names(pg) <- id_vars
    }
    
    if (sum(pg, widths[nm]) < tw) {
      pg[nm] <- widths[nm]
      #names(pg[length(pg)]) <- nm
    } else {
      
      ret[[length(ret) + 1]] <- names(pg)
      pg <- c()
    }
  }
  
  
  if (length(pg) > 0) {
    ret[[length(ret) + 1]] <- names(pg)
  }

  
  return(ret)
  
}

# b <- get_body_size(rpt)
# l <- get_labels(tb$data, tb$col_defs, tb$n_format)
# d <- get_data_size(b, NULL, l, "mono" )
# w <- get_col_widths(tb$data, tb$col_defs, l, "mono")
# p <- get_page_wraps(d, tb$col_defs, w)
# p

#' Preps the data
#' @noRd
prep_data <- function(dat, defs) {
  
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
  
  # Dedupe variables as requested
  # Do this after adding blanks
  # So any group values in blank rows are removed
  for (def in defs) {
    if (def$dedupe) {
      
      # Fill with blanks as appropriate
      w <- nchar(dat[[def$var_c]][1])
      v <- paste0(rep(" ", times = w), collapse = "")
      
      dat[[def$var_c]] <- ifelse(!duplicated(dat[[def$var_c]]), 
                                 dat[[def$var_c]], v) 

    }
  }
  
  return(dat)
  
}


#' Get the column widths
#' @import graphics
#' @noRd
get_col_widths <- function(dat, defs, labels, font_family) {
  
  max_col_width = 5
  min_col_width = .5
  padding_buffer = .05
  
  dwidths <- c()
  
  # Set default widths based on length of data
  for (i in seq_along(dat)) {
    
    w <- max(strwidth(dat[[i]], units="inches", family=font_family))
    if (w > max_col_width)
      w <- max_col_width
    else if (w < min_col_width)
      w <- min_col_width
    else
      w <- (ceiling(w * 100)/100) + padding_buffer
    
    # Determine width of words in label for this column
    s <- stri_split(labels[[i]], fixed=" ")
    l <- strwidth(s[[1]], units="inches", family=font_family)
    
    # If the max word width is greater than the data width,
    # set column width to max label word width
    # so as not to break any words in the label
    if (max(l) > w)
      dwidths[length(dwidths) + 1] <- max(l)
    else
      dwidths[length(dwidths) + 1] <- w
    
  }
  
  # Set names for easy access
  names(dwidths) <- names(dat)
  
  # Set default widths
  ret = dwidths
  
  # Let user settings override defaults
  for (def in defs) {
    
    if (def$var_c %in% names(dat) & !is.null(def$width) && def$width > 0) {
      ret[def$var_c] <- def$width
    }
    
  }
  
  return(ret)
}


# widths not incorporated yet
#' Gets the data size
#' @import graphics
#' @noRd
get_data_size <- function(body_size, widths, labels, font_family) {
  
  ppi = 72
  blank_row <- .3
  
  sz <- c()
  for (n in labels) {
    sz[length(sz) + 1] <- strheight(n, units="inches", family=font_family)
    
  }
  
  ret <- body_size
  
  ret["height"] <- ret["height"] - max(sz) - blank_row
  
  return(ret)
  
}


get_label_aligns <- function(defs, aligns) {
  
  
  ret <- aligns
  
  for (d in defs) {
    if (!is.null(d$label_align) & d$var_c %in% names(aligns))
      ret[d$var_c] <-  d$label_align
    
  }
  
  return(ret)
}


get_col_formats <- function(defs) {
  
  
  ret <- list()
  
  for (d in defs) {
    if (!is.null(d$format))
      ret[[d$var_c]] <-  d$format
  }
  
  return(ret)
}


#' Gets the alignments
#' @noRd
get_aligns <- function(dat, defs) {
  
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
  
  # Assign alignments from column definitions
  for (d in defs) {
    if (!is.null(d$align) & d$var_c %in% nms)
      ret[d$var_c] <- d$align
  }
  
  return(ret)
  
}


#' Gets the labels
#' @noRd
get_labels <- function(dat, defs, nfmt){
  
  # Get the column names from the dataframe
  v1 <- names(dat)
  
  # Get the labels from the dataframe
  # Not so easy because not all columns have labels.
  # If a column has no labels, use the column name
  # as the header string.
  v2 <- c()
  counter <- 1
  
  for (col in dat) {
    if (!is.null(attr(col, "label"))) {
      v2 <- c(v2, attr(col, "label"))
    } else {
      if (is.null( names(col))) {
        v2 <- c(v2, v1[counter])
      } else {
        v2 <- c(v2, names(col))
      }
      
    }
    counter <- counter + 1
  }
  
  # Convert label vector to a list
  #ls <- as.list(v2)
  ls <- v2
  
  # Assign names to list
  names(ls) <- v1
  
  for (def in defs) {
    
    if (!is.null(def$label))
      ls[[def$var]] <- def$label
    
    if (!is.null(def$n) ) {
      ls[[def$var]] <- paste0(ls[[def$var]],  nfmt(def$n))
    }
  }
  
  return(ls)
}

# l <- get_labels(final, t$col_defs, t$n_format)


#' Declare function to calculate pages
#' @noRd
get_pages <- function(x, page_size){


  running_sum <- 0
  page <- 1

  get_pages_int <- Vectorize(function(x){

    if (running_sum + x > page_size) {
      page <<- page + 1
      running_sum <<- x
    } else {
      running_sum <<- running_sum + x
    }

    return(page)
  })

  return(get_pages_int(x))
}


# 
# get_page_breaks <- function(x, page_size){
#   
#   
#   running_sum <- 0
#   page_breaks <- c(1)
#   counter <- 0
#   
#   get_pages_int <- Vectorize(function(x){
#     
#     counter <<- counter + 1
#     
#     if (running_sum + x > page_size) {
#       page_breaks[length(page_breaks) + 1] <<- counter
#       running_sum <<- x
#     } else {
#       running_sum <<- running_sum + x
#     }
#     
#     
#   })
#   
#   
#   get_pages_int(x)
#   
#   page_breaks[length(page_breaks) + 1] <- length(x) + 1
#   
#   return(page_breaks)
# }




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
get_splits_text <- function(x, widths, page_size) {
  

  pgs <- get_page_breaks(x, page_size)
  
  
  ret <- split(pgs, pgs$..page)
  
  
  return(ret)
  
}


#' Function to calculate page breaks
#' @param x Data frame to page.
#' @param page_size Available data height in number of rows.
#' @return Data frame with ..page column populated with page numbers.
#' @noRd
get_page_breaks <- function(x, page_size){
  
  pg <- 1
  counter <- 0
  
  for (i in seq_len(nrow(x))){
    
    counter <- counter + 1
    
    if (counter > page_size) {
      counter <- 1
      pg <- pg + 1
    }
    
    x$..page[i] <- pg
    
  }
  
  return(x)
}


get_data_size_text <- function(rs, widths, labels) {
  
  
  sz <- c()
  for (n in names(labels)) {
    sz[length(sz) + 1] <- length(strwrap(labels[n], 
                                 width = floor(widths[n]/ rs$char_width)))
    
  }
  
  ret <-c(width = rs$line_size)
  
  # Available body height minus label height, and underline
  ret["height"] <- rs$body_line_count - max(sz) - 1
  
  return(ret)
  
}


# Page Info ---------------------------------------------------------------



page_info <- function(data, keys, font_name, col_width, col_align,
                      label, label_align) {
  
  ret <- structure(list(), class = c("page_info", "list"))
  
  ret$data <- data
  ret$keys <- keys
  ret$font_name <- font_name
  ret$col_width <- col_width
  ret$col_align <- col_align
  ret$label <- label
  ret$label_align <- label_align
  
  
  return(ret)
  
}





