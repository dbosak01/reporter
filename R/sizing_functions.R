


# Text Sizing Functions --------------------------------------------------------

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

#' Gets the page wraps for both text and variable width reports
#' @noRd
get_page_wraps <- function(content_width, ts, widths, gutter, control_cols) {
  
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
  tw <- content_width  # width of the page
  
  for (nm in names(widths)) {
    
    if (!is.control(nm)) {
      #If ID vars exist, add them to list
      if (length(pg) == 0 && length(id_vars) > 0) {
        
        # Plus gutter in between columns
        pg <- widths[id_vars] + gutter
        
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
          pg <- widths[id_vars] + gutter
          names(pg) <- id_vars
        }
        
        # Add width for current column
        pg[nm] <- widths[nm] + gutter

      } else {
        
        # If sum of widths does not exceed page size, add to pg and keep going
        pg[nm] <- widths[nm] + gutter

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
  ls_after <- c()
  ls_before <- c()
  for (def in defs) {

    if (def$blank_after) {
      ls_after[length(ls_after) + 1] <- translate_invisible(def$var_c, names(dat))
    }
    
    if (def$blank_before) {
      ls_before[length(ls_before) + 1] <- translate_invisible(def$var_c, names(dat))
    }
  }

  # Add after blanks on requested columns
  if (!is.null(ls_after)) {
    if (length(ls_after) > 0) {
      
      # Reverse order so groups turn out correct
      ls_after <- ls_after[order(ls_after, decreasing = TRUE)]
      
      # Add blanks
      if (length(ls_after) > 0) {
        dat <- add_blank_rows(dat, location = "below", vars = ls_after)
      }
    }
  }

   # print("Blanks")
    #print(dat)
  
  # Set up label rows
  for (def in defs) {
    if (def$label_row) {
      
      # Convert to character if necessary
      if (all(class(dat[[def$var_c]]) != "character"))
        dat[[def$var_c]] <- as.character(dat[[def$var_c]])
      
      dat <- add_blank_rows(dat, "label", vars = def$var_c)
    }
  }
  
  # Add before blanks on requested columns
  # This step should be after label rows so that the blank rows' order are correct 
  if (!is.null(ls_before)) {
    if (length(ls_before) > 0) {
      
      # Reverse order so groups turn out correct
      ls_before <- ls_before[order(ls_before, decreasing = TRUE)]
      
      # Add blanks
      if (length(ls_before) > 0) {
        dat <- add_blank_rows(dat, location = "above", vars = ls_before)
      }
    }
  }
  
  # Add cell border indicator
  group_border_vars <- c()
  for (def in defs) {
    if (def$group_border) {
      group_border_vars <- c(group_border_vars, def$var_c)
    }
  }
  if (length(group_border_vars) > 0) {
    dat <- add_group_border_ind(dat, group_border_vars)
  }

   # print("Labels")
   # print(dat)
  
  # Indent and Dedupe variables as requested
  # Do this after adding blanks
  # So any group values in blank rows are removed
  # if (blank_indent) {
  #   for (def in defs) {
  #     if (!is.null(def$indent) | def$dedupe) {
  #       
  #       # Convert to character if necessary
  #       if (all(class(dat[[def$var_c]]) != "character"))
  #         dat[[def$var_c]] <- as.character(dat[[def$var_c]])
  #       
  #       # Actual deduping now takes place in get_splits_text, so
  #       # label appears at top of each page
  #       
  #     }
  #     
  #     # Perform Indenting of requested variables
  #     if (!is.null(def$indent)) {
  #       ind <- floor(def$indent / char_width)
  #       blnks <- paste0(rep(" ", ind), sep = "", collapse = "")
  #       dat[[def$var_c]] <- ifelse(is.na(dat[[def$var_c]]), NA, paste0(blnks, dat[[def$var_c]]))
  #     }
  #   }
  # }
   # print("Before stub")
   # print(dat)

  # Create stub
  dat <- create_stub(dat, ts)
  
   #print("After stub")
   #print(dat)
    
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
    stv <- rep(names(dat)[1], nrow(dat))
    #print(st)
    
    nms <- names(dat)
    
    # For each subsequent column, if the value is not NA,
    # replace first column value.
    for (i in seq(from = 2, to = length(v), by = 1)) {
      
      if ("..blank" %in% nms) {
        st <- ifelse(is.na(dat[[v[i]]]) | (trimws(dat[[v[i]]]) == "" & dat[["..blank"]] == "L"), 
                   st, dat[[v[i]]])
        # stub var to indicate the value source
        stv <- ifelse(is.na(dat[[v[i]]]) | (trimws(dat[[v[i]]]) == "" & dat[["..blank"]] == "L"), 
                      stv, v[i])
      } else {
        st <- ifelse(is.na(dat[[v[i]]]), st, dat[[v[i]]])  
        stv <- ifelse(is.na(dat[[v[i]]]), stv, v[i])  
      }
      
      # Allow empty to remain.  Sure I put this in for a reason.  Need to test.
      #st <- ifelse(is.na(dat[[v[i]]]) | trimws(dat[[v[i]]]) == "", st, dat[[v[i]]])    
      
    }
    
    dat$..stub_var <- stv

    # Remove stub variables from data frame
    d <-   dat[ , -which(names(dat) %in% v)]
    
    # print("Names before")
    # print(names(d))
    
    # Combine with stub column
    dat <- data.frame(stub = st, d, stringsAsFactors = FALSE)
    
    # Have to restore names, otherwise data.frame will mess them up
    names(dat) <- c("stub", names(d))
    
    # Issue 373: Remove row label when it's NA. If it's "", will consider it as
    # a real value
    if ("..blank" %in% names(dat)) {
      dat <- dat[!(is.na(dat$stub) & dat$..blank == "L"),]
      # dat <- dat[!(dat$stub == "" & dat$..blank == "L"),]
    }
    
    # print("Names after")
    # print(names(dat))
  }
  
  return(dat)
  
}

#' Get the column widths
# @import graphics
#' @import stringi
#' @noRd
get_col_widths <- function(dat, ts, labels, char_width, uom, 
                           merge_label_row = TRUE,
                           content_width = NULL, rs = NULL) {

  defs <- ts$col_defs
  if (uom == "cm") {
    #12.7
    max_col_width = 12.7  
    min_col_width = .254
  } else if (uom == "inches") {
    max_col_width = 5  
    min_col_width = .1
  } else if (uom == "char") {
    max_col_width = 60
    min_col_width = 2
  }
  

  nms <- names(labels)
  #print(nms)
  dwidths <- c()
  mwidths <- c()
  
  # Set default widths based on length of data
  for (nm in nms) {
    
    #w <- max(strwidth(dat[[nm]], units="inches", family=font_family))
    if (is.control(nm) | all(is.na(dat[[nm]]) == TRUE))
      w <- 0
    else {
      # Clear out label rows, as these can mess up column width calculations.
      # Label row widths are dealt with later.
      if ("..blank" %in% names(dat) & merge_label_row) {
        colattr <- attributes(dat[[nm]])
        dat[[nm]] <- ifelse(dat[["..blank"]] %in% c("L", "B", "A"), " ", dat[[nm]])
        attributes(dat[[nm]]) <- colattr
      }
      
      w <- max(get_nchar(as.character(dat[[nm]])), na.rm = TRUE) * char_width
      
      sd <- stri_split(as.character(dat[[nm]]), regex=" |\n|\r|\t", simplify = TRUE)
      mwidths[[nm]]  <- max(get_nchar(as.character(sd)), na.rm = TRUE) * char_width 

    }
     
    if (w > max_col_width)
      w <- max_col_width
    else if (w < min_col_width)
      w <- min_col_width
    else
      w <- (ceiling(w * 100)/100) 
    
    # print(paste("w:", w))
    # print(paste("Label:", labels[[nm]]))
    
    # Determine width of words in label for this column
    s <- stri_split(labels[[nm]], regex=" |\n|\r|\t", simplify = TRUE)
    #l <- strwidth(s[[1]], units="inches", family=font_family)
    l <- max(get_nchar(as.character(s)), na.rm = TRUE) * char_width
    
    # print(paste("s:", s))
    # print(paste("l:", l))
    
    # If the max word width is greater than the data width,
    # set column width to max label word width
    # so as not to break any words in the label
    if (max(l) > w)
      dwidths[[nm]] <- max(l) + char_width
    else
      dwidths[[nm]] <- w + char_width
  }
  
  # Set names for easy access
  #names(dwidths) <- names(dat)
  
  # Set default widths
  ret <-  dwidths
  #print("Default Widths")
  #print(ret)
  
  # Let widths on orig df override defaults
  orig <- widths(dat)
  for (nm in names(orig)) {
    ret[[nm]] <- orig[[nm]]
  }

  defnms <- c()
  # Let user definitions override everything
  
  # Process the user width:
  # If all the widths are percentage, then...
  # -- If total percentage <= 100%, the last width should be min(remaining width, total width*percentage)
  # -- to make sure total length won't exceed total width
  # -- If total percentage > 100%, let it just overrides. It would go to page wrap next in next step
  # If some widths are percentage and others are absolute values, just override.
  
  width_vars <- c()
  width_lst <- c()
  width_per_lst <- c()
  
  # Stub width
  if (!is.null(ts$stub)) {
    if (!is.null(ts$stub$width)) {
      width_vars <- c(width_vars, "stub")
      
      if (grepl("^[0-9]+(\\.[0-9]+)?%$", ts$stub$width)) {
        width_per_lst <- c(width_per_lst, as.numeric(sub("%", "", ts$stub$width)) / 100)
        fraction <- as.numeric(sub("%", "", ts$stub$width)) / 100
        width_lst <- c(width_lst, round(content_width * fraction, 2))
      } else {
        width_lst <- c(width_lst, ts$stub$width)
      }
    }
  }
  
  # Column width
  for (def in defs) {
    if (!is.null(def$width)) {
      width_vars <- c(width_vars, def$var_c)
      
      if (grepl("^[0-9]+(\\.[0-9]+)?%$", def$width)) {
        width_per_lst <- c(width_per_lst, as.numeric(sub("%", "", def$width)) / 100)
        fraction <- as.numeric(sub("%", "", def$width)) / 100
        width_lst <- c(width_lst, round(content_width * fraction, 2))
      } else {
        width_lst <- c(width_lst, def$width)
      }
    }
  }
  
  # If all the widths are percentage, then...
  # -- If total percentage <= 100%, the last width should be min(remaining width, total width*percentage)
  # -- to make sure total length won't exceed total width
  if (length(width_per_lst) > 1 & length(width_per_lst) == length(width_lst)) {
    if (sum(width_per_lst) <= 1) {
      remain_width <- content_width - sum(width_lst[1:length(width_lst)-1])
      last_width <- min(remain_width, width_lst[length(width_lst)])
      width_lst[length(width_lst)] <- last_width
      
      # Make sure total words don't exceed line_size
      total_words <- sum(round(width_lst / char_width))
      while (total_words > rs$line_size) {
        width_lst[length(width_lst)] <- width_lst[length(width_lst)] - char_width
        total_words <- sum(round(width_lst / char_width))
      }
    }
  }
  if (length(width_vars) > 0) {
    for (i in seq_len(length(width_vars))) {
      width_var <- width_vars[i]
      user_width <- width_lst[i]
      if (width_var %in% names(dat) & !is.null(user_width) && user_width > 0) {
        if (user_width >= mwidths[[width_var]]) {
          ret[[width_var]] <- user_width
        }
        else {
          ret[[width_var]] <- mwidths[[width_var]] + char_width
        }
        
        defnms[length(defnms) + 1] <- width_var
      }
    }
  }
  # for (def in defs) {
  #   if (!is.null(def$width)) {
  #     if (grepl("^[0-9]+(\\.[0-9]+)?%$", def$width)) {
  #       fraction <- as.numeric(sub("%", "", def$width)) / 100
  #       user_width <- round(content_width * fraction, 2)
  #     } else {
  #       user_width <- def$width
  #     }
  #     
  #     if (def$var_c %in% names(dat) & !is.null(user_width) && user_width > 0) {
  #       if (user_width >= mwidths[[def$var_c]])
  #         ret[[def$var_c]] <- user_width
  #       else 
  #         ret[[def$var_c]] <- mwidths[[def$var_c]] + char_width
  #       
  #       defnms[length(defnms) + 1] <- def$var_c
  #     }
  #   }
  # }
  
  # Deal with stub
  # if (!is.null(ts$stub)) {
  # 
  #   # Add stub width if exists   
  #   if (!is.null(ts$stub$width)) {
  #     ret[["stub"]] <- ts$stub$width
  #     defnms[length(defnms) + 1] <- "stub"
  #   }
  #   
  #   # print(ret[["stub"]])
  #   # print(ts$stub$width)
  # }
  
  # Turn into vector if needed
  ret <- unlist(ret)
  
  # Remove control columns
  ret <- ret[!sapply(names(ret), is.control)]
  
  # Deal with table width
  if (!is.null(ts$width)) {
     # print(paste("Table width:", ts$width))
    
     # Adjusted to keep 
     #blnkw <- (length(ret) - 0) * char_width
     # print(paste("Blank width:", blnkw))
     # print(paste("Before:", sum(ret)))
     # print(ret)
     
    if (sum(ret) < ts$width) {
      
      # Get columns not defined by user
      variable <- ret[!names(ret) %in% defnms]
      fixed <- ret[names(ret) %in% defnms]
      
      available <- ts$width - sum(fixed)
      
      variable <- variable * available / sum(variable)
      
      for (nm in names(variable))
        ret[[nm]] <- variable[[nm]]
      
      # print("diff")
      # Add one character to first column so it fills the entire page
      ret[[1]] <- ret[[1]] + char_width

    } else {
      # If table width is less than the sum of the columns.
      # This means the table should wrap to the next page, 
      # but each page should be set to the table width.
      # Having a hard time figuring out how to do this.
      # Will come back to it later.
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



#' Get the column widths
#' @import stringi
#' @noRd
get_col_widths_variable <- function(dat, ts, labels, font, 
                               font_size, uom, gutter_width,
                               merge_label_row = TRUE,
                               allow_rtf_code = FALSE,
                               allow_html_code = FALSE,
                               content_width = NULL,
                               styles = NULL) {
  
  dat_orig <- dat
  defs <- ts$col_defs
  if (uom == "cm") {
    #12.7
    max_col_width = 12.7  
    min_col_width = .254
  } else if (uom == "inches") {
    max_col_width = 5  
    min_col_width = .1
  } else if (uom == "char") {
    max_col_width = 60
    min_col_width = 2
  }
  
  nms <- names(labels)
  #print(nms)
  dwidths <- c()
  mwidths <- c()
  
  bold_multiplier <- 1.015
  
  # Set default widths based on length of data
  for (nm in nms) {
    

    if (is.control(nm) | all(is.na(dat[[nm]]) == TRUE))
      w <- 0
    else {
      
      # Clear out label rows, as these can mess up column width calculations.
      # Label row widths are dealt with later.
      blank_idx <- rep(F, dim(dat)[1])
      if ("..blank" %in% names(dat) & merge_label_row) {
        dat[[nm]] <- ifelse(dat[["..blank"]] %in% c("L", "B", "A"), " ", dat[[nm]])
        blank_idx <- dat[["..blank"]] %in% c("L", "B", "A")
      }
      
      # Remove all RTF code for accurate widths
      if (allow_rtf_code) {
        # Replace "\\line" with "\n" to make sure width calculation is correct
        line_regex <- "\\\\line"
        dat[[nm]] <- gsub(line_regex, "\n", dat[[nm]])
        
        # includes RTF code containing numbers, letters, and the following space
        rtf_control_regex <- "\\\\[a-z]+-?[0-9]* ?"
        # includes RTF special control like \\*
        rtf_symbol_regex  <- "\\\\[^a-z]"
        # includes [] and {}
        rtf_brace_regex   <- "[{}]"
        
        dat[[nm]] <- gsub(rtf_control_regex, "", dat[[nm]])
        dat[[nm]] <- gsub(rtf_symbol_regex, "", dat[[nm]])
        dat[[nm]] <- gsub(rtf_brace_regex, "", dat[[nm]])
      }
      
      # Remove all html code for accurate widths
      if (allow_html_code) {
        # Replace "<br>" with "\n" to make sure width calculation is correct
        line_regex <- "<br>"
        dat[[nm]] <- gsub(line_regex, "\n", dat[[nm]])
        
        # includes html code
        html_control_regex <- "(?i)<\\/?([a-z]+)[^>]*>|&[a-z0-9#]+;"
        
        dat[[nm]] <- gsub(html_control_regex, "", dat[[nm]])
      }
      
      # --------------------------------- #
      #   Detect Bold Position
      # --------------------------------- #
      bold_idx <- rep(F, length(dat[[nm]]))
      
      if (!is.null(styles[[nm]])) {
        
        stl_lst <- styles[[nm]]
        for (k in seq_len(length(stl_lst))) {
          if (is.null(stl_lst[[k]]$bold)) {
            stl_lst[[k]]$bold <- FALSE 
          }
          if (is.null(stl_lst[[k]]$indicator)) {
            stl_lst[[k]]$indicator <- NA 
          }
          stl_lst[[k]] <- stl_lst[[k]][c("bold", "indicator")]
        }
        
        stl <- do.call(rbind, lapply(stl_lst, as.data.frame))
        
        stl$seq2 <- seq_len(nrow(stl))
        stl$seq1 <- ifelse(is.na(stl$indicator), 5,
                           ifelse(stl$indicator == "blankrow", 4,
                                  ifelse(stl$indicator == "labelrow", 3,
                                         ifelse(stl$indicator == "datarow", 2, 1))))
        stl <- stl[order(stl$seq1, stl$seq2),]
        
        apply_style <- rep(NA, length(dat[[nm]]))
        style_order <- rep(NA, length(dat[[nm]]))
        for (i in seq_len(nrow(stl))) {
          ind <- stl$indicator[i]
          
          override_idx <- is.na(apply_style) | apply_style == FALSE | apply_style == ""
          if (ind %in% names(dat)) {
            apply_style[override_idx] <- dat[[ind]][override_idx]
            
          } else if (ind %in% "datarow") {
            
            if ("..blank" %in% names(dat)) {
              apply_style[override_idx & dat$..blank == ""] <- TRUE
            } else {
              apply_style[override_idx] <- TRUE
            }
            
          } else if (ind %in% "labelrow") {
            apply_style[override_idx & dat$..blank == "L"] <- TRUE
          } else if (is.na(ind)) {
            apply_style[override_idx] <- TRUE
          }
          
          style_order[is.na(style_order) & apply_style == TRUE] <- i
        }
        
        for (b in seq_len(nrow(stl))) {
          bold_flag <- stl$bold[b]
          bold_idx[!is.na(style_order) & style_order == b] <- bold_flag
        }
      }
      # if (!is.null(styles[[nm]])) {
      #   if (!is.null(styles[[nm]]$bold)){
      #     if (any(styles[[nm]]$bold) == TRUE) {
      #       
      #       # Locate the bolding position
      #       # if (is.null(styles[[nm]]$indicator)) {
      #       #   
      #       #   bold_idx <- rep(T, length(dat[[nm]]))
      #       #   
      #       # } else if (styles[[nm]]$indicator == "labelrow") {
      #       #   
      #       #   bold_idx[dat$..blank == "L"] <- TRUE
      #       #   
      #       # } else if (styles[[nm]]$indicator == "datarow") {
      #       #   
      #       #   if ("..blank" %in% names(dat)) {
      #       #     bold_idx[dat$..blank == ""] <- TRUE
      #       #   } else {
      #       #     bold_idx <- rep(T, length(dat[[nm]]))
      #       #   }
      #       #   
      #       # } else if (styles[[nm]]$indicator %in% names(dat)) {
      #       #   
      #       #   ind_var <- styles[[nm]]$indicator
      #       #   bold_idx[dat[[ind_var]] == TRUE & !is.na(dat[[ind_var]])] <- TRUE
      #       #   
      #       # }
      #       
      #       # Locate the bold position
      #       if (is.null(styles[[nm]]$indicator)) {
      #         
      #         bold_idx <- rep(T, length(dat[[nm]]))
      #         
      #       } else {
      #         apply_style <- rep(NA, length(dat[[nm]]))
      #         style_order <- rep(NA, length(dat[[nm]]))
      #         
      #         for (i in seq_len(length(styles[[nm]]$indicator))) {
      #           ind <- styles[[nm]]$indicator[i]
      # 
      #           if (ind %in% names(dat)) {
      #             apply_style[is.na(apply_style) | apply_style == FALSE] <- dat[[ind]][is.na(apply_style) | apply_style == FALSE]
      #             
      #           } else if (ind == "datarow") {
      #             
      #             if ("..blank" %in% names(dat)) {
      #               apply_style[is.na(apply_style) & dat$..blank == ""] <- TRUE
      #             } else {
      #               apply_style[is.na(apply_style)] <- TRUE
      #             }
      #             
      #           } else if (ind == "labelrow") {
      #             apply_style[is.na(apply_style) & dat$..blank == "L"] <- TRUE
      #           }
      #           
      #           style_order[is.na(style_order) & apply_style == TRUE] <- i
      #         }
      #         
      #         for (i in seq_len(length(styles[[nm]]$bold))) {
      #           bold_flag <- styles[[nm]]$bold[i]
      #           bold_idx[!is.na(style_order) & style_order == i] <- bold_flag
      #         }
      #         
      #       }
      #     }
      #   }
      # }
      
      # --------------------------------- #
      #   Maximum width of a column
      # --------------------------------- #
      w_each <- rep(NA, length(dat[[nm]]))
      w_each[!bold_idx] <-  get_text_width(dat[[nm]][!bold_idx], units=uom, font=font, font_size = font_size)
      w_each[bold_idx] <- get_text_width(dat[[nm]][bold_idx], units=uom, font=font, font_size = font_size, multiplier = bold_multiplier)

      # Add indention width
      if (!is.null(defs[[nm]]$indent)) {
        
        w_each <- w_each + defs[[nm]]$indent
        
      } else if (!is.null(ts$stub) & nm == "stub") {
        stub_var <- ts$stub$vars
        
        for (v in stub_var) {
          if (!is.null(defs[[v]]$indent)) {
            idx <- dat$..stub_var == v & !is.na(dat$stub) & blank_idx == F
            w_each[idx] <- w_each[idx] + defs[[v]]$indent
          }
        }
      }
      
      w <-  max(w_each)
      
      # --------------------------------- #
      #   Minimum width of a column
      # --------------------------------- #
      if (!is.null(ts$stub) & nm == "stub") {
        mwidths[[nm]] <- 0
        
        # For stub column, derive maximum word width by ..stub_var because each
        # stub var may have different indenting width
        stub_var <- ts$stub$vars
        for (v in stub_var) {
          idx <- dat$..stub_var == v & !is.na(dat$stub) & blank_idx == F
          
          idx_non_bold <- idx & !bold_idx
          idx_bold <- idx & bold_idx
          
          mwidth_non_bold <- 0
          if (any(idx_non_bold)) {
            sd_non_bold <- stri_split(as.character(dat[[nm]][idx_non_bold]), regex=" |\n|\r|\t", simplify = TRUE)
            mwidth_non_bold <- max(get_text_width(as.character(sd_non_bold), units=uom, font=font, font_size = font_size))
          }
          
          mwidth_bold <- 0
          if (any(idx_bold)) {
            sd_bold <- stri_split(as.character(dat[[nm]][idx_bold]), regex=" |\n|\r|\t", simplify = TRUE)
            mwidth_bold <- max(get_text_width(as.character(sd_bold), units=uom, font=font, font_size = font_size, multiplier = bold_multiplier))
          }
          
          mwidth <- max(mwidth_non_bold, mwidth_bold)
          
          if (!is.null(defs[[v]]$indent)) {
            mwidth <- mwidth + defs[[v]]$indent
          }
          
          mwidths[[nm]] <- max(mwidths[[nm]], mwidth)
        }
      } else {
        
        mwidth_non_bold <- 0
        if (any(!bold_idx)) {
          sd_non_bold <- stri_split(as.character(dat[[nm]][!bold_idx]), regex=" |\n|\r|\t", simplify = TRUE)
          mwidth_non_bold  <- max(get_text_width(as.character(sd_non_bold), units=uom,
                                                 font=font, font_size = font_size))
        }
        
        mwidth_bold <- 0
        if (any(bold_idx)) {
          sd_bold <- stri_split(as.character(dat[[nm]][bold_idx]), regex=" |\n|\r|\t", simplify = TRUE)
          mwidth_bold  <- max(get_text_width(as.character(sd_bold), units=uom,
                                             font=font, font_size = font_size, multiplier = bold_multiplier))
        }
        
        mwidths[[nm]] <- max(mwidth_non_bold, mwidth_bold)
        
        if (!is.null(defs[[nm]]$indent)) {
          mwidths[[nm]] <- mwidths[[nm]] + defs[[nm]]$indent
        }
      }
      
    }
    
    if (w > max_col_width)
      w <- max_col_width
    else if (w < min_col_width)
      w <- min_col_width
    else
      w <- (ceiling(w * 100)/100) 
    
    # print(paste("w:", w))
    # print(paste("Label:", labels[[nm]]))
    
    # Determine width of words in label for this column
    s <- stri_split(labels[[nm]], regex=" |\n|\r|\t", simplify = TRUE)
    l <- max(get_text_width(as.character(s), units=uom, 
                            font=font, font_size = font_size))
    
    # print(paste("s:", s))
    # print(paste("l:", l))
    
    # If the max word width is greater than the data width,
    # set column width to max label word width
    # so as not to break any words in the label
    if (max(l) > w)
      dwidths[[nm]] <- max(l) + gutter_width
    else
      dwidths[[nm]] <- w + gutter_width
  }
  
  # Set names for easy access
  #names(dwidths) <- names(dat)
  
  # Set default widths
  ret <-  dwidths
  #print("Default Widths")
  #print(ret)
  
  # Let widths on orig df override defaults
  orig <- widths(dat_orig)
  for (nm in names(orig)) {
    ret[[nm]] <- orig[[nm]]
  }
  
  defnms <- c()
  
  # Let user definitions override everything
  
  # Process the user width:
  # If all the widths are percentage, then...
  # -- If total percentage <= 100%, the last width should be min(remaining width, total width*percentage)
  # -- to make sure total length won't exceed total width
  # -- If total percentage > 100%, let it just overrides. It would go to page wrap next in next step
  # If some widths are percentage and others are absolute values, just override.
  
  width_vars <- c()
  width_lst <- c()
  width_per_lst <- c()
  
  # Stub width
  if (!is.null(ts$stub)) {
    if (!is.null(ts$stub$width)) {
      width_vars <- c(width_vars, "stub")
      
      if (grepl("^[0-9]+(\\.[0-9]+)?%$", ts$stub$width)) {
        width_per_lst <- c(width_per_lst, as.numeric(sub("%", "", ts$stub$width)) / 100)
        fraction <- as.numeric(sub("%", "", ts$stub$width)) / 100
        width_lst <- c(width_lst, round(content_width * fraction, 2))
      } else {
        width_lst <- c(width_lst, ts$stub$width)
      }
    }
  }
  
  # Column width
  for (def in defs) {
    if (!is.null(def$width)) {
      width_vars <- c(width_vars, def$var_c)
      
      if (grepl("^[0-9]+(\\.[0-9]+)?%$", def$width)) {
        width_per_lst <- c(width_per_lst, as.numeric(sub("%", "", def$width)) / 100)
        fraction <- as.numeric(sub("%", "", def$width)) / 100
        width_lst <- c(width_lst, round(content_width * fraction, 2))
      } else {
        width_lst <- c(width_lst, def$width)
      }
    }
  }
  
  # If all the widths are percentage, then...
  # -- If total percentage <= 100%, the last width should be min(remaining width, total width*percentage)
  # -- to make sure total length won't exceed total width
  if (length(width_per_lst) > 1 & length(width_per_lst) == length(width_lst)) {
    if (sum(width_per_lst) <= 1) {
      remain_width <- content_width - sum(width_lst[1:length(width_lst)-1])
      last_width <- min(remain_width, width_lst[length(width_lst)])
      width_lst[length(width_lst)] <- last_width
    }
  }
  
  if (length(width_vars) > 0) {
    for (i in seq_len(length(width_vars))) {
      width_var <- width_vars[i]
      user_width <- width_lst[i]
      
      if (width_var %in% names(dat) & !is.null(user_width) && user_width > 0) {
        if (user_width >= mwidths[[width_var]]) {
          ret[[width_var]] <- user_width
        }
        else {
          ret[[width_var]] <- mwidths[[width_var]] + gutter_width
        }

        defnms[length(defnms) + 1] <- width_var
      }
    }
  }
  
  # for (def in defs) {
  #   
  #   # Process the percentage width
  #   if (!is.null(def$width)) {
  #     if (grepl("^[0-9]+(\\.[0-9]+)?%$", def$width)) {
  #       fraction <- as.numeric(sub("%", "", def$width)) / 100
  #       user_width <- round(content_width * fraction, 2)
  #     } else {
  #       user_width <- def$width
  #     }
  #     
  #     if (def$var_c %in% names(dat) & !is.null(user_width) && user_width > 0) {
  #       if (user_width >= mwidths[[def$var_c]]) {
  #         ret[[def$var_c]] <- user_width
  #       }
  #       else {
  #         ret[[def$var_c]] <- mwidths[[def$var_c]] + gutter_width
  #       }
  #       
  #       defnms[length(defnms) + 1] <- def$var_c
  #     }
  #   }
  # }
  
  # Deal with stub
  # if (!is.null(ts$stub)) {
  #   
  #   # Add stub width if exists   
  #   if (!is.null(ts$stub$width)) {
  #     if (ts$stub$width >= mwidths[["stub"]]) {
  #       ret[["stub"]] <- ts$stub$width
  #     } else {
  #       ret[["stub"]] <- mwidths[["stub"]] + gutter_width
  #     }
  #     defnms[length(defnms) + 1] <- "stub"
  #   }
  #   
  #   # print(ret[["stub"]])
  #   # print(ts$stub$width)
  # }
  
  # Turn into vector if needed
  ret <- unlist(ret)
  
  # Remove control columns
  ret <- ret[!sapply(names(ret), is.control)]
  
  # Deal with table width
  if (!is.null(ts$width)) {
    # print(paste("Table width:", ts$width))
    
    # Adjusted to keep 
    #blnkw <- (length(ret) - 0) * char_width
    # print(paste("Blank width:", blnkw))
    # print(paste("Before:", sum(ret)))
    # print(ret)
    
    if (sum(ret) < ts$width) {
      
      # Get columns not defined by user
      variable <- ret[!names(ret) %in% defnms]
      fixed <- ret[names(ret) %in% defnms]
      
      available <- ts$width - sum(fixed)
      
      variable <- variable * available / sum(variable)
      
      for (nm in names(variable))
        ret[[nm]] <- variable[[nm]]
      
      # print("diff")
      # Add one character to first column so it fills the entire page
      #ret[[1]] <- ret[[1]] + gutter_width
      
    } else {
      # If table width is less than the sum of the columns.
      # This means the table should wrap to the next page, 
      # but each page should be set to the table width.
      # Having a hard time figuring out how to do this.
      # Will come back to it later.
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
  has_cell_border <- FALSE
  for (d in defs) {
    if (!is.null(d$label_align) & d$var_c %in% names(aligns))
      ret[[d$var_c]] <-  d$label_align
    
    if (!is.null(d[["group_border"]])) {
      if (d[["group_border"]] == TRUE) {
        has_cell_border <- TRUE
      }
    }
  }
  
  # Set a value so that it won't be NA in pi
  if (has_cell_border) {
    ret[["..group_border"]] <- "left"
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
  has_cell_border <- FALSE
  for (d in defs) {
    if (!is.null(d$align) & d$var_c %in% nms)
      ret[d$var_c] <- d$align
    
    if (!is.null(d[["group_border"]])) {
      if (d[["group_border"]] == TRUE) {
        has_cell_border <- TRUE
      }
    }
  }
  
  # Assign cell border to let it not be NA in pi
  if (has_cell_border) {
    ret[["..group_border"]] <- "left"
  }
  
  # Deal with stub
  if (!is.null(ts$stub)) {
    
    # Remove variables associated with stub
    ret <- ret[-which(names(ret) %in% ts$stub$vars)]  
    
    # Assign alignment from stub definition
    ret[["stub"]] <- ts$stub$align 
    
    # Assign stub indicator to let it not be NA
    ret[["..stub_var"]] <- ts$stub$align
    
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
  has_cell_border <- FALSE
  for (def in defs) {

    if (!is.null(def[["label"]]))
      ls[[def$var]] <- def[["label"]]
    
    if (!is.null(def$n) ) {
      ls[[def$var]] <- paste0(ls[[def$var]],  nfmt(def$n))
    }
    
    if (!is.null(def[["group_border"]])) {
      if (def[["group_border"]] == TRUE) {
        has_cell_border <- TRUE
      }
    }
  }
  
  if (has_cell_border) {
    ls <- c(ls, ..group_border = "..group_border")
  }
  
  # Deal with stub
  if (!is.null(ts$stub)) {
    
    # Remove variables associated with stub
    ls <- ls[-which(names(ls) %in% ts$stub$vars)]  
    
    # Assign label from stub definition
    ls <- c(stub = ts$stub$label, ls)
    
    # Assign label for stub indicator
    ls <- c(ls, ..stub_var = "..stub_var")
    
  }
  
  
  return(ls)
}


#' @title Get the columns for the table
#'
#' @description Get the columns for the table
#'
#' @param x The Table spec object
#' @noRd
get_table_cols <- function(x, control_cols) {
  
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
  else {
    
    diff <- setdiff(x$show_cols, names(dat))
    if (length(diff) > 0)
      stop("Columns not found in data: ", paste(diff, collapse = ","))
    
  }
  
  # Deal with visible options
  if (!is.null(x$col_defs)) {
    for (def in x$col_defs) {
      
      if (show_all == FALSE & def$visible)
        ret[length(ret) + 1] <- def$var_c
      #else if (show_all == TRUE & def$visible == FALSE)
      else if (def$visible == FALSE) {
        if (def$var_c %in% ret) {
          ret <- ret[!ret %in% def$var_c]
        }
      }
    }
    
    ret <- unique(ret)
    
    diff <- setdiff(ret, names(dat))
    if (length(diff) > 0)
      stop("Columns not found in data: ", paste(diff, collapse = ","))
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
                            content_offsets, ts, count_row_var = FALSE) {
  
  defs <- ts$col_defs
  

  # Calculate where page breaks should occur
  # Based on available height, content size, and offsets
  # Function adds a ..page variable with page indicator
  if (any(grepl("..group_cohesion", names(x)))) {
    group_cohesion <- names(x)[grepl("..group_cohesion", names(x))]
    group_cohesion <- group_cohesion[order(group_cohesion)]
  } else {
    group_cohesion <- NULL
  }
  if (any(grepl("..min_page_prop",names(x)))) {
    min_page_prop_col <- names(x)[grepl("..min_page_prop", names(x))]
    min_page_prop <- c()
    for (col in min_page_prop_col) {
      min_page_prop <- c(min_page_prop, unique(x[[col]][!is.na(x[[col]])]))
    }
    
  } else {
    min_page_prop <- NULL
  }
  
  if (is.null(ts$auto_page)){
    ts$auto_page <- TRUE
  }
  
  pgs <- get_page_breaks(x, page_size, lpg_rows, content_offsets, count_row_var,
                         group_cohesion = group_cohesion, 
                         min_page_prop = min_page_prop,
                         userPage = !ts$auto_page)
  
  # Eliminate pages that have only blank lines
  sb <- subset(pgs, trimws(pgs$..blank) == "")
  non_blank_pages <- unique(sb$..page)
  sbst <- subset(pgs, pgs$..page %in% non_blank_pages)
  
  # If the first blank row is from blank after, then remove it
  first_page_pos <- get_first_pos(sbst$..page)
  rm_blank <- first_page_pos & sbst$..blank == "B"
  sbst <- sbst[!rm_blank,]
  
  # Add break labels if needed
  if (any(grepl("..break_label", names(sbst)))) {
    sbst$..temp_seq <- 1:nrow(sbst)
    break_pos <- which(!is.na(sbst$..break_occur))
    
    for (p in break_pos) {
      rep_col <- as.numeric(strsplit(sbst$..break_occur[p], ",")[[1]])
      rev_order <- rep_col[order(rep_col, decreasing = T)]
      for (r in rep_col) {
        temp_df <- sbst[p,]
        
        # If first row is blank before, add after it
        if (temp_df$..blank[1] == "A") {
          temp_df$..temp_seq[1] <- temp_df$..temp_seq[1] + 1
        }
        
        # Revise to label of row
        temp_df$..blank[1] <-  "L"
        
        # Create label of row
        temp_df[1, !grepl("^\\.\\.", names(temp_df))] <- ""
        temp_df[1,1] <- temp_df[[paste0("..break_label", r)]]
        temp_df$..temp_seq[1] <- temp_df$..temp_seq[1] - rev_order[r]*0.1
        
        # Put the right lines count to ..row
        temp_df$..row[1] <- temp_df[[paste0("..break_label_lines", r)]]
        
        # Set ..break_occur for current column for later use
        temp_df$..break_occur <- r
        
        # Reset the row name for PDF process
        row.names(temp_df) <- paste0("break_label", 1:nrow(temp_df))
        
        # Stack to original data, no need to repeat if blank
        if (temp_df[1,1] != "") {
          sbst <- rbind(sbst, temp_df)
        }
      }
    }
    # re-order to put repeated label in right position
    sbst <- sbst[order(sbst$..temp_seq),]
    
    # drop temporary variable
    sbst <- sbst[, setdiff(names(sbst), c("..temp_seq"))]
  }
  
  # Split the data frame at the page indicators
  ret <- split(sbst, sbst$..page)
  
  # Perform column deduping
  ret <- dedupe_pages(ret, defs)
  
  # Dedupe stub if child column dedupe was requested.
  # Somewhat unsure of this.
  if (stub_dedupe(ts$stub, defs)) {
    sdef <- define_c("stub", dedupe = TRUE)
    ret <- dedupe_pages(ret, list("stub" = sdef))
  }

  
  return(ret)
  
}


#' Function to calculate page breaks
#' @param x Data frame to page.
#' @param page_size Available data height in number of rows.
#' @param lpg_rows Last page rows.  Subtracted from available height.
#' @param content_offsets Blank rows requested above or below content
#' @param count_row_var Whether to use ..row to count lines
#' @param group_cohesion A vector of variables for group cohesion
#' @param min_page_prop A vector of minimum page proportions for each cohesion
#' variable.
#' @param userPage Whether to follow user's page and turn off auto paging
#' @return Data frame with ..page column populated with page numbers.
#' @noRd
get_page_breaks <- function(x, page_size, lpg_rows, content_offsets, 
                            count_row_var = FALSE,
                            group_cohesion = NULL,
                            min_page_prop = NULL,
                            userPage = FALSE){
  
  pg <- 1
  counter <- 0
  offset <- lpg_rows + content_offsets[["blank_upper"]]
  
  # If the upper line has name, it means using page_by
  using_pgby_up <- F
  using_pgby_low <- F
  if (is.null(names(content_offsets[["upper"]]))) {
    ttfl <- content_offsets["upper"] + content_offsets["lower"]
  } else {
    using_pgby_up <- T
    
    if (!is.null(names(content_offsets[["lower"]]))) {
      using_pgby_low <- T
    }
  }
  
  
  # User Paging variable
  currentPage <- NA
  lastPage <- NA
  userForce <- FALSE
  
  # print(paste("Content Upper:", content_offsets["upper"]))
  # print(paste("Content Lower:", content_offsets["lower"]))
  # print(paste("Page size:", page_size))
  # print(paste("Last Page Rows:", lpg_rows))
  # print(paste("Content offset:", offset))
  
  if (!is.null(group_cohesion)) {
    # Set min page percent as 0.8 if not assigned
    if (is.null(min_page_prop)) {
      min_page_prop <- rep(0.8, length(group_cohesion))
    }

    group_change_df <- as.data.frame(matrix(NA, dim(x)[1], length(group_cohesion)))
    group_counts_df <- as.data.frame(matrix(NA, dim(x)[1], length(group_cohesion)))
    
    for (i in 1:length(group_cohesion)) {
      col <- group_cohesion[i]
      
      if (count_row_var) {
        group_counts <- get_group_count(x[[col]], x$..row)
      } else {
        group_counts <- get_group_count(x[[col]])
      }
      group_counts_df[[i]] <- group_counts
      
      group_change <- as.logical(c(TRUE, mapply(function(x, y) !identical(x, y), 
                                                x[[col]][-1], 
                                                x[[col]][-length(x[[col]])])))
      
      group_change_df[[i]] <- group_change
    }
  }
  
  # Transfer break labels into sequence to detect the change
  break_label_df <- NULL
  if (any(grepl("..break_label", names(x)))) {
    break_label_df <- x[,names(x)[grepl("..break_label", names(x))]]
    for (nm in names(break_label_df)) {
      if (grepl("..break_label\\d+", nm)){
        break_label_df[[nm]] <- get_group_seq(break_label_df[[nm]])
        break_label_num <- sub(".*?([0-9]+).*", "\\1", nm)
        break_label_df[[paste0("..break_group_first", break_label_num)]] <- get_first_pos(break_label_df[[nm]])
      }
    }
    break_label_col <- names(break_label_df)[grepl("..break_label\\d+", names(break_label_df))]
    break_label_col <- break_label_col[order(break_label_col)]
    break_label_lines_col <- names(break_label_df)[grepl("..break_label_lines\\d+", names(break_label_df))]
    break_label_lines_col <- break_label_lines_col[order(break_label_lines_col)]
    
    break_label_retain <- break_label_df[1, break_label_col]
    
    pre_pg <- pg
  }
  
  for (i in seq_len(nrow(x))){
    
    if (using_pgby_up & !using_pgby_low) {
      pgby_up_idx <- names(content_offsets[["upper"]]) %in% x$..page_by[i]
      ttfl <- content_offsets[["lower"]] + as.numeric(content_offsets[["upper"]][pgby_up_idx])
    } else if (using_pgby_up & using_pgby_low) {
      pgby_up_idx <- names(content_offsets[["upper"]]) %in% x$..page_by[i]
      pgby_low_idx <- names(content_offsets[["lower"]]) %in% x$..page_by[i]
      ttfl <- as.numeric(content_offsets[["lower"]][pgby_low_idx]) + as.numeric(content_offsets[["upper"]][pgby_up_idx])
    }
    
    if (count_row_var) {
      if (is.na(x$..row[i])) {
        counter <- counter + 1
        cur_count <- 1
      } else {
        counter <- counter + x$..row[i]
        cur_count <- x$..row[i]
      }
    } else {
      counter <- counter + 1
      cur_count <- 1
    }
    
    if (i == nrow(x)) {
      # Exception where last line is equal to number of available lines
      # Don't put the blank row in this case.
      if (counter < (page_size - ttfl)) {  
        offset <- offset + content_offsets[["blank_lower"]]
        #print(paste("Lower Blank:", offset))
      }
    }
    # print(paste("Counter:", counter))
    # print(paste("Condition:", (page_size - offset - ttfl)))
    

    # Get current page value
    currentPage <- x$..page[i]
    
    # If last page is not equal to current page, and neither is NA,
    # force a page break
    # if ( !is.na(lastPage) & !is.na(currentPage) &
    #     trimws(lastPage) != "NA" & trimws(currentPage) != "NA" &
    #     trimws(lastPage) != "" & trimws(currentPage) != "" &
    #     currentPage != lastPage) {
    #   userForce <- TRUE
    # 
    # } else
    #   userForce <- FALSE
    
    # Need to check the inserted blank lines, multiple tables
    
    # For the very first row, no need to check user's page
    if (i > 1) {
      page_equal <- (is.na(lastPage) == is.na(currentPage)) && 
        (is.nan(lastPage) == is.nan(currentPage)) && 
        (is.na(lastPage) || (lastPage == currentPage))
    } else {
      page_equal <- TRUE
    }
    
    userForce <- !page_equal
    
    # After comparison, set last page value
    lastPage <- currentPage
    
    # Force a break if the next row has wraps that exceed the page size
    # if (count_row_var & i < nrow(x)) {
    #   nextRowCount <- x$..row[i + 1]
    #   if (!is.na(nextRowCount)) {
    #     if (nextRowCount > 1) {
    #       if (page_size  - offset - ttfl - nextRowCount - counter < -1) {
    #         userForce <- TRUE
    #         # print(pg)
    #         # print(page_size  - offset - ttfl - nextRowCount - counter)
    #       }
    #     }
    #   }
    # }
    
    cohesionBreak <- FALSE
    if (!is.null(group_cohesion)) {
      j <- 1
      while (j <= length(group_cohesion) & userForce == FALSE) {
        group_change <- group_change_df[[j]]
        group_counts <- group_counts_df[[j]]
        min_page_prop_j <- min_page_prop[j]
        
        # Rules of moving a group into next page
        # - Remaining space is not enough for a whole group
        # - After moving, the existed lines meet minimum requirement
        # - A whole group is not bigger than the capacity for a page
        if (group_change[i]) {
          pre_counter <- counter - cur_count
          if ((group_counts[i] > (page_size  - offset - ttfl - pre_counter)) &
              (pre_counter >= round((page_size  - offset - ttfl) * min_page_prop_j)) &
              (group_counts[i] <= (page_size - offset - ttfl))) {
            cohesionBreak <- TRUE
          }
        }
        
        j <- j + 1
      }
    }

    # If userPage is used, then follow user's pagination, or detect if line 
    # count is greater than page size to start a new page
    newPage <- FALSE
    if (userPage) {
      newPage <- userForce
    } else if ((counter > (page_size  - offset - ttfl)) | cohesionBreak | userForce) {
      newPage <- TRUE
    }
    
    # print("--------------------------------------")
    # print(paste0("newPage is ", newPage))
    # print(paste0("row: ", i))
    # print(paste0("counter: ", counter))
    # print(paste0("ttfl: ", ttfl))
    # print(paste0("page_size: ", page_size))
    # print(paste0("Capacity: ", page_size  - offset - ttfl))
    # print(paste0("Page before: ", pg))
    
    if (newPage) {
    # If line count is greater than page size, start a new page
    # if ((counter > (page_size  - offset - ttfl)) | userForce) {
      
      # Don't understand why this adjustment is needed.  But it is.
      # Have to figure it out, because it seems wrong.
      # if (pg == 1)
      #   page_size <- page_size - 1
      
      # Increment page count
      pg <- pg + 1
      
      # Reset line counter
      if (count_row_var) {
        if (is.na(x$..row[i])) {
          counter <- 1
        } else {
          counter <- x$..row[i]
        }
      } else {
        counter <- 1
      }
      
      # If last record of last page is blank before, move it to new page
      if ("..blank" %in% names(x) & i > 1) {
        if (x$..blank[i-1] == "A") {
          x$..page[i-1] <- pg
          counter <- counter + 1
        }
      }
      
      # Break label process
      if (!is.null(break_label_df) & i > 1) {
        break_label_cur <- break_label_df[i, break_label_col]
        break_label_pre <- break_label_df[i-1, break_label_col]

        # In case the break label are all NA, need to detect first
        if (any(!is.na(break_label_pre == break_label_cur))) {
          # Check if same break label is spanned across pages
          if (any(break_label_pre == break_label_cur)){
            # If the break label is blank value, no need to repeat it
            break_idx <- (break_label_pre == break_label_cur) & break_label_cur != ""
            
            if ("..blank" %in% names(x)) {
              if (x$..blank[i-1] == "A") {
                break_group_first_col <- names(break_label_df)[grepl("..break_group_first\\d+", names(break_label_df))]
                break_label_first <- break_label_df[i - 1, break_group_first_col]
                break_idx <- (break_label_pre == break_label_cur) & (!break_label_first)
              }
            }
            
            if (any(break_idx)) {
              counter <- counter + sum(x[i, break_label_lines_col[break_idx]])
              x$..break_occur[i] <- paste0(which(break_idx), collapse = ", ")
            }
          }
        }
      }
      
      # If the first row is blank line from blank_after, set as 0 because it
      # will be removed later
      if ("..blank" %in% names(x)){
        if (x$..blank[i] == "B") {
          counter <- 0
        }
      }

      # Set offset to zero on first page
      offset <- 0

    }

    # print(paste0("Page after: ", pg))
    # print("--------------------------------------")
  
    x$..page[i] <- pg
    # print(pg)
  }

  
  # Convert ..page back to number if necessary
  if (all(class(x$..page) != "numeric"))
    x$..page <- as.numeric(x$..page)
  
  #print(paste("Page count:", counter))
  
  return(x)
}



stub_dedupe <- function(stb, defs) {
  
  ret <- FALSE
  
  for (df in defs) {
    
    if (df$var_c %in% stb$vars) {
      if (df$dedupe == TRUE & df$label_row == FALSE) {
        ret <- TRUE
      }
    }
  }
  
  return(ret)
}


#' @import stringi
#' @noRd
get_pgby_cnt <- function(pgby) {
  
  ret <- 0
  
  if (!is.null(pgby)) {
    
    upgby <- unique(pgby)
  
    for (pgb in upgby) {
    
      lns <- unlist(stri_split_fixed(pgb, "\n"))
      
      if (length(lns) > ret)
        ret <- length(lns)
    
    }
  
  }
  
  return(ret)
  
}

#' @noRd
get_pgby_value <- function(value, pgby_cnt) {
  
  ret <- ""
  if (is.null(value)) {
    
    if (!is.null(pgby_cnt)) {
      
      if (pgby_cnt > 0) {
        ret <- rep("x", pgby_cnt)
        ret <- paste0(ret, collapse = "\n")
      } 
    }

  }
  
  return(ret)
}

get_nchar <- function(s, chinese_weight = 1.88) {
  s <- as.character(s)
  
  chinese_count <- stri_count_regex(s, "[\\p{Han}\\u3000-\\u303F\\uFF00-\uFFEF]")
  non_chinese_count <- nchar(s) - chinese_count
  
  total_count <- round(chinese_count*chinese_weight + non_chinese_count)
  
  return(total_count)
}
