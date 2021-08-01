
# Special Functions for PDF and RTF ---------------------------------------


#' @title
#' Create a text report, but not on the file system
#'
#' @description
#' This function creates a report, using the
#' parameters provided in the report_spec object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
create_report_text <- function(rs) {
  
  # Calculate available space for content 
  rs <- page_setup(rs)
  
  # Get page template for easy access
  pt <- rs$page_template
  
  # Put content in new variable for easy access
  ls <- rs$content
  
  # Break content into pages
  ret <- paginate_content(rs, ls)
  
  # Assign table column widths back to report spec for reference
  rs$column_widths <- ret[["widths"]]
  
  # Assign graphics flag back to report spec
  rs$has_graphics <- ret[["has_graphics"]]
  
  # Assign pages to ls and continue processing
  ls <- ret[["pages"]]
  
  # Deal with preview
  if (!is.null(rs$preview)) {
    if (rs$preview < length(ls[[1]]$pages))
      ls[[1]]$pages <- ls[[1]]$pages[seq(1, rs$preview)]
  }
  
  # Combine all content pages 
  res <- create_content(rs, ls, pt)
  rs <- res$rs
  ls <- res$ls
  
  # After report is created, fix the page numbers.
  # Reason is we don't really know how many pages there are 
  # until the report is created.
  upt <- update_page_numbers(rs, ls)
  
  res <- list(rs = rs, ls = upt)
  
  return(res)
}


#' @title Create content for PDF and RTF output type.
#' @description This loop prepares pages created in paginate_content
#' @noRd
create_content <- function(rs, ls, pt) {
  
  ret <- list()

  counter <- 0
  page <- 0
  last_object <- FALSE
  last_page <- FALSE
  page_open <- FALSE
  
  blank_margin_top <- rep("", rs$blank_margin_top)
  blank_margin_left <- paste0(rep(" ", rs$blank_margin_left), collapse = "")
  
  
  for (cont in ls) {
    
    
    # Increment counter
    counter <- counter + 1
    page <- 0
    
    
    # Set last_object flag
    if (counter == length(ls))
      last_object <- TRUE
    else 
      last_object <- FALSE
    
    
    for (pg in cont$pages) {
    
      page <- page + 1
      
      # Vector to hold lines on a page
      if (page_open == FALSE)
        lns <- c()
      
      if (page == length(cont$pages))
        last_page <- TRUE
      else
        last_page <- FALSE
      
      
      
      if (rs$blank_margins)
        lns <- append(lns, blank_margin_top)
      
      
      #print(page_open)
      if (page_open == FALSE) {
        if (!is.null(pt$page_header))
          lns <- append(lns, paste0(blank_margin_left, pt$page_header))
        
        if (!is.null(pt$title_hdr))
          lns <- append(lns, paste0(blank_margin_left, 
                                     trimws(pt$title_hdr, which = "right")))
        
        if (!is.null(pt$titles))
          lns <- append(lns, paste0(blank_margin_left, 
                                     trimws(pt$titles, which = "right")))
        
      }
      
      if (!is.null(pg)) {
        tmp <- pad_any(pg, rs$line_size, get_justify(cont$align))
        lns <- append(lns, paste0(blank_margin_left, tmp))
        
      }
      
      # Set page_open flag based on status of page_break and current objects
      if (last_object == FALSE & last_page == TRUE & cont$page_break == FALSE)
        page_open <- TRUE
      else 
        page_open <- FALSE
      
      if (page_open == FALSE) {
        
        if (!is.null(pt$footnotes))
          lns <- append(lns, paste0(blank_margin_left,
                                     trimws(pt$footnotes, which = "right")))
        
        if (!is.null(pt$page_footer))
          lns <- append(lns, paste0(blank_margin_left, pt$page_footer))
        
        # Increment total page count
        if (is.null(rs$pages))
          rs$pages <- 1
        else 
          rs$pages <- rs$pages + 1 
        
        # If page not open, add to list
        ret[[length(ret) + 1]] <- lns 
        
        
      }
  
      
    }
    
  }
  
  res <- list(rs = rs, ls = ret)
  
  return(res)
  
}

#' @description Update page numbers in text file
#' @details Logic is to read in each line of the file, loop through and replace
#' tokens as needed.  Pages numbers are incremented every time a form feed
#' is encountered.  Total pages is retrieved from the report pages property.
#' Total pages is calculated when a page break occurs.
#' @noRd
update_page_numbers <- function(rs, lns) {
  
  
  # Set up page variables
  tpg <- rs$pages

  
  # Define vectorized function to replace tokens
  replace_tokens <- Vectorize(function(x, srch, just) {
    
    ret <- x
    
    # Replace tokens, but keep overall width the same
    if (grepl(srch, x, fixed = TRUE)) {
      #print("found it")
      
      tmp <- sub("[tpg]", tpg, srch, fixed = TRUE)
      tmp <- sub("[pg]", pg, tmp, fixed = TRUE)
      tmp <- pad_any(tmp, nchar(srch), just)
      ret <- sub(srch, tmp, x, fixed = TRUE)
    }
    
    return(ret)
  }, USE.NAMES = FALSE)
  
  
  for (pg in seq_along(lns)) {
  
    # Call vectorized function on entire page header/footer segment
    
    if (token_check(rs$page_header_left)) {
      for (i in seq_len(length(rs$page_header_left))) {
        if (token_check(rs$page_header_left[i])) {
  
          lns[[pg]] <- replace_tokens(lns[[pg]], rs$page_header_left[i], "left")
        }
      }
    }
    
    if (token_check(rs$page_header_right)) {
      for (i in seq_len(length(rs$page_header_right))) {
        if (token_check(rs$page_header_right[i])) {
  
          lns[[pg]] <- replace_tokens(lns[[pg]], rs$page_header_right[i], "right")
        }
      }
    }
    
    if (token_check(rs$page_footer_left)) {
      for (i in seq_len(length(rs$page_footer_left))) {
        if (token_check(rs$page_footer_left[i])) {
          lns[[pg]] <- replace_tokens(lns[[pg]], rs$page_footer_left[i], "left")
        }
      }
    }
    if (token_check(rs$page_footer_center)) {
      for (i in seq_len(length(rs$page_footer_center))) {
        if (token_check(rs$page_footer_center[i])) {
          lns[[pg]] <- replace_tokens(lns[[pg]], rs$page_footer_center[i], "centre")
        }
      }
    }
    if (token_check(rs$page_footer_right)) {
      
      for (i in seq_len(length(rs$page_footer_right))) {
        if (token_check( rs$page_footer_right[i])) {
          lns[[pg]] <- replace_tokens(lns[[pg]], rs$page_footer_right[i], "right")
        }
      }
    }
    if (!is.null(rs$title_hdr)) {
      if (token_check(rs$title_hdr$right)) {
        for (i in seq_len(length(rs$title_hdr$right))) {
          if (token_check(rs$title_hdr$right[i])) {
            lns[[pg]] <- replace_tokens(lns[[pg]], rs$title_hdr$right[i], "right")
          }
        }
      }
    }
    
    for (cntnt in rs$content) {
      if (!is.null(cntnt$object$title_hdr)) { 
        if (token_check(cntnt$object$title_hdr$right)) {
          for (i in seq_len(length(cntnt$object$title_hdr$right))) {
            if (token_check(cntnt$object$title_hdr$right[i])) {
              lns[[pg]] <- replace_tokens(lns[[pg]], cntnt$object$title_hdr$right[i], "right")
            }
          }
        }
        
      }
    }
    
  }
  
  
  return(lns)
}



# Write Text Driver Function ----------------------------------------------



#' @title
#' Write a text report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_text <- function(rs) {

  # Calculate available space for content 
  rs <- page_setup(rs)
  
  # Get page template for easy access
  pt <- rs$page_template

  # Put content in new variable for easy access
  ls <- rs$content

  # Break content into pages
  ret <- paginate_content(rs, ls)
  
  # Assign table column widths back to report spec for reference
  rs$column_widths <- ret[["widths"]]
 
  # Assign graphics flag back to report spec
  rs$has_graphics <- ret[["has_graphics"]]
  
  # Assign pages to ls and continue processing
  ls <- ret[["pages"]]

  # Deal with preview
  if (!is.null(rs$preview)) {
    if (rs$preview < length(ls[[1]]$pages))
      ls[[1]]$pages <- ls[[1]]$pages[seq(1, rs$preview)]
  }
  
  # Write pages to file
  rs <- write_content(rs, ls, pt)

  # After report is written, reopen and fix the page numbers.
  # Reason is we don't really know how many pages there are 
  # until the report is written.
  rs <- write_page_numbers(rs)

  invisible(rs)
}


#' @title Paginate content
#' @description This loop breaks long content into separate pages.
#' @details 
#' Tricky part is taking account of a partially filled page with no page break.
#' Basically we need to track how many lines are on the last page,
#' and send to next pagination call to use for an offset.
#' @noRd
paginate_content <- function(rs, ls) {
  
  last_page_lines <- 0 
  last_object <- FALSE
  table_widths <- list()
  tmp_dir <- tempdir()
  has_graphics <- FALSE
  
  for(i in seq_along(ls)){
    
    if (i == length(ls))
      last_object <- TRUE
    else 
      last_object <- FALSE
    
    # Break content into multiple pages if needed.
    # If there are multiple pages, each function is responsible for 
    # filling out the page body entirely.  Only the last page will
    # remain open, in case there is more content and no page break between.
    # Blanks on last page are handled below.
    # If a last page is left unfinished, and there is no page break, 
    # the last_page_lines variable is populated with the number of lines
    # on the last page. That is passed to the next function, so it can
    # subtract those lines from the first page of the next piece of content.
    if (class(ls[[i]]$object)[1] == "table_spec"){
      
      ret <- create_table_pages_text(rs, ls[[i]], last_page_lines)
      
      table_widths[[length(table_widths) + 1]] <- ret[["widths"]] 
      pgs <- ret[["page_list"]]
      
    } else if (class(ls[[i]]$object)[1] == "text_spec") {
      
      pgs <- create_text_pages_text(rs, ls[[i]], last_page_lines)
      
    } else if (class(ls[[i]]$object)[1] == "plot_spec") {
      
      pgs <- create_plot_pages_text(rs, ls[[i]], last_page_lines, tmp_dir)
      has_graphics <- TRUE
    }
    
    # Append pages to content page list
    # These will be written out in following step
    ls[[i]]$pages <- pgs
    
    last_page <- pgs[[length(pgs)]]
    last_page_lines <- length(last_page) + last_page_lines
    
    # If there is more than one page returned, then a page break occurred
    # and we can reset the last page lines.
    if (length(pgs) > 1)
      last_page_lines <- length(last_page)
    
    #print(paste("Last page lines:", last_page_lines))
    
    # If there is a requested page break, or it is the last object/last page,
    # then fill up the remaining page with blanks.
    if (ls[[i]]$page_break | last_object) {
      # Fill blanks on last page 
      blnks <- c()
      bl <- rs$body_line_count - last_page_lines 
      if (bl > 0)
        blnks <- rep("", bl)
      
      last_page <- append(last_page, blnks)
      last_page_lines <- 0  # Needed for requested page breaks
      #print(paste("Last Page Line Count:", length(last_page)))
    } 

    # Replace last page with any modifications
    ls[[i]]$pages[[length(pgs)]] <- last_page

  }
  
  ret <- list(widths = table_widths, pages = ls, has_graphics = has_graphics)
  
  return(ret)
}



#' @title Write out content
#' @description This loop writes out pages created in paginate_content
#' @noRd
write_content <- function(rs, ls, pt) {
  
  # Kill existing file
  if (file.exists(rs$modified_path))
    file.remove(rs$modified_path)
  
  counter <- 0
  page <- 0
  last_object <- FALSE
  last_page <- FALSE
  page_open <- FALSE
  
  blank_margin_top <- rep("", rs$blank_margin_top)
  blank_margin_left <- paste0(rep(" ", rs$blank_margin_left), collapse = "")
  

  for (cont in ls) {
    
    
    # Increment counter
    counter <- counter + 1
    page <- 0
    
    # Set last_object flag
    if (counter == length(ls))
      last_object <- TRUE
    else 
      last_object <- FALSE
    
    
    for (pg in cont$pages) {
      
      page <- page + 1
      
      if (page == length(cont$pages))
        last_page <- TRUE
      else
        last_page <- FALSE
      
      
      f <- file(rs$modified_path, open="a", encoding = "native.enc")
      
      if (rs$blank_margins)
        writeLines(enc2utf8(blank_margin_top), con = f, useBytes = TRUE)
      
      
      #print(page_open)
      if (page_open == FALSE) {
        if (!is.null(pt$page_header))
          writeLines(enc2utf8(paste0(blank_margin_left, pt$page_header)), 
                     con = f, useBytes = TRUE)
        
        if (!is.null(pt$title_hdr))
          writeLines(enc2utf8(paste0(blank_margin_left, 
                                     trimws(pt$title_hdr, which = "right"))), 
                     con = f, useBytes = TRUE)
        
        if (!is.null(pt$titles))
           writeLines(enc2utf8(paste0(blank_margin_left, 
                             trimws(pt$titles, which = "right"))), 
                      con = f, useBytes = TRUE)
        
      }
      
      if (!is.null(pg)) {
        tmp <- trimws(pad_any(pg, rs$line_size, get_justify(cont$align)),
                      which = "right")
        writeLines(enc2utf8(paste0(blank_margin_left, tmp)), 
                   con = f, useBytes = TRUE)
        
      }
      
      # Set page_open flag based on status of page_break and current objects
      if (last_object == FALSE & last_page == TRUE & cont$page_break == FALSE)
        page_open <- TRUE
      else 
        page_open <- FALSE
      
      if (page_open == FALSE) {
        
        if (!is.null(pt$footnotes))
          writeLines(enc2utf8(paste0(blank_margin_left,
                            trimws(pt$footnotes, which = "right"))), 
                     con = f, useBytes = TRUE)
        
        if (!is.null(pt$page_footer))
          writeLines(enc2utf8(paste0(blank_margin_left, pt$page_footer)), 
                     con = f, useBytes = TRUE)
        
        # Add form feed character for text page break
        if (last_object == FALSE | last_page == FALSE) {
          
          if (is.null(rs$pages))
            rs$pages <- 1
          else 
            rs$pages <- rs$pages + 1 
          
          writeLines(enc2utf8(""), con = f, sep = "\f", useBytes = TRUE) 
        
        }
      }
      
      close(f)
      
    }
    
  }
  
  return(rs)
  
}

#' @description Setup page for content
#' @details  Calculates available space for content and prepares text lines
#' for page template.
#' @noRd
page_setup <- function(rs) {
  
  debug <- FALSE
  
  if (debug) {
    print(paste("Line height:", rs$line_height))
    print(paste("Character width:", rs$char_width))
  }
  
  # Content size is the page size minus margins, in units of measure
  rs$content_size <- get_content_size(rs)
  if (debug)
    print(paste0("Content Size: ", rs$content_size))
  
  # Line size is the number of characters that will fit in the content size width
  if (is.null(rs$user_line_size)) {
    if (rs$output_type == "RTF") {
      # 1 char adjustment to avoid occasional wrapping 
      rs$line_size <- floor(rs$content_size[["width"]] / rs$char_width) - 1 
    } else {
      rs$line_size <- floor(rs$content_size[["width"]] / rs$char_width) 
    }
  } else 
    rs$line_size <- rs$user_line_size
  if (debug)
    print(paste0("Line Size: ", rs$line_size))
  
  # Line count is the number of lines that will fit in the content size height
  if (is.null(rs$user_line_count))
    rs$line_count <- floor(rs$content_size[["height"]] / rs$line_height)
  else
    rs$line_count <- rs$user_line_count
  if (debug)
    print(paste0("Line Count: ", rs$line_count))
  
  # Get page template
  # Page template is the headers, footers, titles, and footnotes attached to report
  # Requires the line_size
  # Returns a page_template_text object
  pt <- page_template_text(rs)
  rs$page_template <- pt
  #print(pt)
  
  # Get the page template row count
  # Include all the rows associated with the page template
  rs$page_template_header_count <- length(pt$page_header) + length(pt$titles) + 
    length(pt$title_hdr) + length(pt$page_by)
  if (debug)
    print(paste("Page Template Header Count:", rs$page_template_header_count))

  rs$page_template_footer_count <- length(pt$footnotes) + length(pt$page_footer)
  if (debug)
    print(paste("Page Template Footer Count:", rs$page_template_footer_count))

  rs$page_template_row_count <- rs$page_template_header_count + 
                                rs$page_template_footer_count
  if (debug)
    print(paste("Page Template Row Count:", rs$page_template_row_count))
  
  # Body line count is the number of rows available for content on each page
  rs$body_line_count <- rs$line_count - rs$page_template_row_count
  if (debug)
    print(paste0("Body Line Count: ", rs$body_line_count))
  
  
  # Calculate blank margins, if specified
  if (rs$blank_margins) {
    rs$blank_margin_top <- floor((rs$margin_top - rs$min_margin) / rs$line_height)
  } else 
    rs$blank_margin_top <- 0
  if (debug)
    print(paste("Blank Margin Top:", rs$blank_margin_top))
  
  if (rs$blank_margins)
    rs$blank_margin_left <- ceiling((rs$margin_left - rs$min_margin) / rs$char_width)
  else 
    rs$blank_margin_left <- 0
  if (debug)
    print(paste("Blank Margin Left:", rs$blank_margin_left))
  

  
  return(rs)
}

#' @description Update page numbers in text file
#' @details Logic is to read in each line of the file, loop through and replace
#' tokens as needed.  Pages numbers are incremented every time a form feed
#' is encountered.  Total pages is retrieved from the report pages property.
#' Total pages is calculated when a page break occurs.
#' @noRd
write_page_numbers <- function(rs) {
 
  # Read file into vector
  lns <- readLines(rs$modified_path, encoding = "UTF-8")
  

  # Adjust page count
  rs$pages <- rs$pages + 1
  
  # Set up page variables
  tpg <- rs$pages 
  pg <- 1
  
  # Define vectorized function to replace tokens
  replace_tokens <- Vectorize(function(x, srch, just) {
    
    ret <- x
  
    # Update page number if hit a page break
    if (grepl("\f", x, fixed = TRUE)) {
      #print("Updated page number")
      # This double arrow <<- is updating the pg variable in the containing
      # function write_page_numbers().  It is not updating the global environment.
      pg <<- pg + 1 
    }
    
    # Replace tokens, but keep overall width the same
    if (grepl(srch, x, fixed = TRUE)) {
      #print("found it")
      
      tmp <- sub("[tpg]", tpg, srch, fixed = TRUE)
      tmp <- sub("[pg]", pg, tmp, fixed = TRUE)
      tmp <- pad_any(tmp, nchar(srch), just)
      ret <- sub(srch, tmp, x, fixed = TRUE)
    }
    
    return(ret)
  })

  # Call vectorized function on entire page header/footer segment

  if (token_check(rs$page_header_left)) {
    for (i in seq_len(length(rs$page_header_left))) {
      if (token_check(rs$page_header_left[i])) {
        pg <- 1
        lns <- replace_tokens(lns, rs$page_header_left[i], "left")
      }
    }
  }

  if (token_check(rs$page_header_right)) {
    for (i in seq_len(length(rs$page_header_right))) {
      if (token_check(rs$page_header_right[i])) {
        pg <- 1
        lns <- replace_tokens(lns, rs$page_header_right[i], "right")
      }
    }
  }
  
  if (token_check(rs$page_footer_left)) {
    for (i in seq_len(length(rs$page_footer_left))) {
      if (token_check(rs$page_footer_left[i])) {
        pg <- 1
        lns <- replace_tokens(lns, rs$page_footer_left[i], "left")
      }
    }
  }
  if (token_check(rs$page_footer_center)) {
    for (i in seq_len(length(rs$page_footer_center))) {
      if (token_check(rs$page_footer_center[i])) {
        pg <- 1
        lns <- replace_tokens(lns, rs$page_footer_center[i], "centre")
      }
    }
  }
  if (token_check(rs$page_footer_right)) {

    for (i in seq_len(length(rs$page_footer_right))) {
      if (token_check( rs$page_footer_right[i])) {
        pg <- 1
        lns <- replace_tokens(lns, rs$page_footer_right[i], "right")
      }
    }
  }
  if (!is.null(rs$title_hdr)) {
    for (th in rs$title_hdr) {
      if (token_check(th$right)) {
        for (i in seq_len(length(th$right))) {
          if (token_check(th$right[i])) {
            pg <- 1
            lns <- replace_tokens(lns, th$right[i], "right")
          }
        }
      }
    }
  }
  
  for (cntnt in rs$content) {
   if (!is.null(cntnt$object$title_hdr)) { 
     for (th in cntnt$object$title_hdr) {
       if (token_check(th$right)) {
         for (i in seq_len(length(th$right))) {
           if (token_check(th$right[i])) {
            pg <- 1
            lns <- replace_tokens(lns, th$right[i], "right")
           }
         }
       }
     }
   }
  }
  
  # Replace file with updated lines
  f <- file(rs$modified_path, open="w+", encoding = "native.enc")
  

  writeLines(enc2utf8(lns), con = f, useBytes = TRUE)
  close(f)
  
  return(rs)
}

#' @noRd
token_check <- function(x) {
  
  ret <- FALSE
  
  if (!is.null(x))
    if (any(nchar(x) > 0))
      if (any(grepl("[pg]", x, fixed = TRUE)) |
          any(grepl("[tpg]", x, fixed = TRUE)))
            ret <- TRUE
      
  return(ret)
}

