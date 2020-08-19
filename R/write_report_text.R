

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
  
  ret <- ""

  # Kill existing file
  if (file.exists(rs$file_path))
    file.remove(rs$file_path)
  
  # Calculate available space
  rs$content_size <- get_content_size(rs)
  rs$line_size <- floor(rs$content_size[["width"]] / rs$char_width)
  rs$body_size <- get_body_size(rs)
  #print(rs$body_size)
  rs$body_line_count <- floor(rs$body_size[["height"]] / rs$line_height)
  
  # Get page template
  pt <- page_template_text(rs)

  ls <- rs$content
  last_page_lines <- 0 
  
  # Write out content
  for(i in seq_along(ls)){

    # Break content into multiple pages if needed
    if (class(ls[[i]]$object)[1] == "table_spec"){

      pgs <- create_table_pages_text(rs, ls[[i]]$object, last_page_lines)
      
    } else if (class(ls[[i]]$object)[1] == "text_spec") {
      
      pgs <- create_text_pages_text(rs, ls[[i]]$object, last_page_lines)
      
    }
    
    ls[[i]]$pages <- pgs
    
    last_page <- pgs[[length(pgs)]]
    last_page_lines <- length(last_page)
    print(last_page_lines)
    
    if (ls[[i]]$page_break) {
      # Fill blanks on last page 
      blnks <- c()
      bl <- rs$body_line_count - last_page_lines
      if (bl > 0)
        blnks <- rep("", bl)

      last_page <- append(last_page, blnks)
      last_page_lines <- 0
    } 
    
    ls[[i]]$pages[[length(pgs)]] <- last_page
  }
  
  counter <- 0
  page <- 0
  last_object <- FALSE
  last_page <- FALSE
  
  # Write out content
  for (cont in ls) {
    
    # print("Page length")
    # print(length(cont$pages))
    # print(cont$pages)
    
    # Increment counter
    counter <- counter + 1
    
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
      
      #print(length(pg))

      f <- file(rs$file_path, open="a")
      
      if (!is.null(pt$page_header))
       writeLines(pt$page_header, con = f)
      
      if (!is.null(pt$titles))
       writeLines(pt$titles, con = f)
      
      if (!is.null(pg)) {
        tmp <- format(pg, width = rs$line_size,
               justify = get_justify(cont$align))
        writeLines(tmp, con = f)
        
      }
      
      if (!is.null(pt$footnotes))
       writeLines(pt$footnotes, con = f)
      
      if (!is.null(pt$page_footer))
       writeLines(pt$page_footer, con = f)
      
      # Do something with page_break property
      if (last_object == FALSE | last_page == FALSE) {
        
        if (is.null(rs$pages))
          rs$pages <- 1
        else 
          rs$pages <- rs$pages + 1 
        
        writeLines("", con = f, sep = "\f") 
        
        
      }
        #write_page_break(rs)
      
      close(f)
      
    }
    
  }

  # After report is written, reopen and fix the page numbers.
  # Reason is we don't really know how many pages there are 
  # until the report is written.
  rs <- write_page_numbers(rs)
  
  invisible(rs)
}

#' @noRd
write_report_text2 <- function(rs) {
  
  ret <- ""
  
  # Kill existing file
  if (file.exists(rs$file_path))
    file.remove(rs$file_path)
  
  # Calculate available space
  rs$content_size <- get_content_size(rs)
  rs$line_size <- floor(rs$content_size[["width"]] / rs$char_width)
  rs$body_size <- get_body_size(rs)
  #print(rs$body_size)
  rs$body_line_count <- floor(rs$body_size[["height"]] / rs$line_height)
  
  # Get page template
  pt <- page_template_text(rs)
  
  ls <- rs$content
  
  counter <- 0
  last_object <- FALSE
  
  # Write out content
  for(cont in ls){
    
    # Increment counter
    counter <- counter + 1
    
    # Set last_object flag
    if (counter == length(ls))
      last_object <- TRUE
    else 
      last_object <- FALSE
    
    
    if (class(cont$object)[1] == "table_spec"){
      
      # Break table into multiple pages if needed
      # This function returns a list of pages
      ttx <- create_table_pages_text(rs, cont$object)
      
      # Write pages
      # This function takes the page template (headers, titles, etc.)
      # and combines with the table pages and writes to file.
      rs <- write_tables_text(rs, ttx, pt, last_object)
      
    } else if (class(cont$object)[1] == "text_spec") {
      
      txt <- create_text_pages_text(rs, cont$object)
      rs <- write_tables_text(rs, txt, pt, last_object)
      
    } else if (all(class(cont$object)[1] == "character") & 
               cont$object == "page_break"){  
      
      # ** Currently Not Used **
      
      # Add page break except for the last page
      # so there is no empty page at the end
      if (!last_object) {
        rs <- write_page_break(rs)
      }
      
    } 
    
    
  }
  
  # After report is written, reopen and fix the page numbers.
  # Reason is we don't really know how many pages there are 
  # until the report is written.
  rs <- write_page_numbers(rs)
  
  invisible(rs)
}


#' Write a list of tables to the report file
#' @param rs The Report Spec.
#' @param ttx A list of tables to write.
#' @param pt A page template object.
#' @param last_object Whether the table is the last object to be written.
#' @return The report spec
#' @noRd
write_tables_text <- function(rs, ttx, pt, last_object) {
  
  
  for (i in seq_along(ttx)) {
    
    rs <- write_table_text(rs, ttx[[i]], pt)
    
    if (i < length(ttx))
      rs <- write_page_break(rs)
    else if (last_object == FALSE)
       rs <- write_page_break(rs)
    
  }
  
  
  return(rs)
}

#' Write a single page to a file
#' @param path The path to the report file 
#' @param ttx A list of tables to write
#' @param pt A page template object
#' @return The report spec, unmodified
#' @noRd
write_page <- function(path, ttx, pt) {
  
  
  
  f <- file(path, open="a")
  
  if (!is.null(pt$page_header))
    writeLines(pt$page_header, con = f)
  
  if (!is.null(pt$titles))
    writeLines(pt$titles, con = f)
  
  if (!is.null(ttx))
    writeLines(ttx, con = f)
  
  if (!is.null(pt$footnotes))
    writeLines(pt$footnotes, con = f)
  
  if (!is.null(pt$page_footer))
    writeLines(pt$page_footer, con = f)
  
  close(f)
  
  return(rs)
}


#' Write a single table to a file
#' @param rs The Report Spec
#' @param ttx A list of tables to write
#' @param pt A page template object
#' @return The report spec, unmodified
#' @noRd
write_table_text <- function(rs, ttx, pt) {
  

  
  f <- file(rs$file_path, open="a")
  
  if (!is.null(pt$page_header))
    writeLines(pt$page_header, con = f)
  
  if (!is.null(pt$titles))
    writeLines(pt$titles, con = f)
  
  if (!is.null(ttx))
    writeLines(ttx, con = f)
  
  if (!is.null(pt$footnotes))
    writeLines(pt$footnotes, con = f)
  
  if (!is.null(pt$page_footer))
    writeLines(pt$page_footer, con = f)
  
  close(f)
  
  return(rs)
}
#' @description Write out a page break to the report file
#' @details For text, the page break is a form feed character.  This function
#' also calculates the total page count and records in the report pages
#' property.
#' @param rs The report spec
#' @return The report spec, unmodified
#' @noRd
write_page_break <- function(rs) {
  
  f <- file(rs$file_path, open="a")
  
  if (is.null(rs$pages))
    rs$pages <- 1
  else 
    rs$pages <- rs$pages + 1 
  
  writeLines("", con = f, sep = "\f")
  
  close(f)
  
  return(rs)

}

#' @description Update page numbers in text file
#' @details Logic is to read in each line of the file, loop through and replace
#' tokens as needed.  Pages numbers are incremented every time a form feed
#' is encountered.  Total pages is retrieved from the report pages property.
#' Total pages is calculated in the write_page_break function.
#' @noRd
write_page_numbers <- function(rs) {
 
  # Read file into vector
  lns <- readLines(rs$file_path)
  
  # Set up page variables
  tpg <- rs$pages
  pg <- 1
  
  # Define vectorized function to replace tokens
  replace_tokens <- Vectorize(function(x, srch, just) {
    
    ret <- x
  
    # Update page number if hit a page break
    if (grepl("\f", x, fixed = TRUE)) {
      #print("Updated page number")
      pg <<- pg + 1 
    }
    
    # Replace tokens, but keep overall width the same
    if (grepl(srch, x, fixed = TRUE)) {
      #print("found it")
      
      tmp <- sub("[tpg]", tpg, srch, fixed = TRUE)
      tmp <- sub("[pg]", pg, tmp, fixed = TRUE)
      tmp <- format(tmp, width = nchar(srch), justify = just)
      ret <- sub(srch, tmp, x, fixed = TRUE)
    }
    
    return(ret)
  })
  
  # Call vectorized function on entire page header/footer segment
  if (token_check(rs$page_header_left)) {
    pg <- 1
    lns <- replace_tokens(lns, rs$page_header_left, "left")
  }
  if (token_check(rs$page_header_right)) {
    pg <- 1
    lns <- replace_tokens(lns, rs$page_header_right, "right")
  }
  if (token_check(rs$page_footer_left)) {
    pg <- 1
    lns <- replace_tokens(lns, rs$page_footer_left, "left")
  }
  if (token_check(rs$page_footer_center)) {
    pg <- 1
    lns <- replace_tokens(lns, rs$page_footer_center, "centre")
  }
  if (token_check(rs$page_footer_right)) {
    pg <- 1
    lns <- replace_tokens(lns, rs$page_footer_right, "right")
  }
  
  # Replace file with updated lines
  f <- file(rs$file_path, open="w")
  writeLines(lns, con = f)
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

