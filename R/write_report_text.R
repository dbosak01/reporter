

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

  counter <- 1

  # Write out content
  for(o in ls){
    if (class(o)[1] == "table_spec"){

      ttx <- create_tables_text(rs, o)

      rs <- write_tables_text(rs, ttx, pt)
    } else if (class(o)[1] == "character" & o == "page_break"){
      
      if (counter < length(ls))
        rs <- write_page_break(rs)
    } else if (class(o)[1] == "character") {
      
      txt <- create_text(rs, o)
      rs <- write_tables_text(rs, txt, pt)
    }
    
    counter <- counter + 1
  }

  
  invisible(rs)
}

#' Write a list of tables to the report file
#' @param rs The Report Spec
#' @param ttx A list of tables to write
#' @param pt A page template object
#' @return The report spec
#' @noRd
write_tables_text <- function(rs, ttx, pt) {
  
  
  for (i in seq_along(ttx)) {
    
    rs <- write_table_text(rs, ttx[[i]], pt)
    
    if (i < length(ttx))
      rs <- write_page_break(rs)
    
  }
  
  
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
#' Write out a page break to the report file
#' For text, the page break is a form feed character 
#' @param rs The report spec
#' @return The report spec, unmodified
#' @noRd
write_page_break <- function(rs) {
  
  f <- file(rs$file_path, open="a")
  
  writeLines("", con = f, sep = "\f")
  
  close(f)
  
  return(rs)

}


