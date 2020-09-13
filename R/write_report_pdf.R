

#' @title
#' Write a PDF report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_pdf <- function(rs) {
  
  debug <- FALSE
  
  orig_path <- rs$file_path
  
  # Create temp path for text output
  if (debug) {
    tmp_path <- file.path(base_path, "output/tmp.txt")
    rmd_path <- file.path(base_path, "output/tmp.Rmd")
  } else {
    tmp_path <- tempfile(fileext = ".txt")
    rmd_path <- tempfile(fileext = ".Rmd")
    #rmd_path <- sub(".pdf", ".Rmd", orig_path)
  }


  
  if (file.exists(orig_path))
    file.remove(orig_path)
  
  # Replace original path
  rs$file_path <- tmp_path
  
  # Create text output normally to temp location
  ret <- write_report_text(rs)
  
  # Read lines from text output
  ls <- readLines(tmp_path)

  # Revise text and write to pdf
  write_pdf_output(rs, ls, rmd_path, orig_path)

  # Restore original path
  rs$file_path <- orig_path
  
  # Clean up
  if (!debug) {
    file.remove(tmp_path)
    file.remove(rmd_path)
  }
  
  return(ret)
}

#' @import rmarkdown
#' @noRd
write_pdf_output <- function(rs, ls, rmd_path, pdf_path) {
  
  # Set up vectors
  hdr <- c() 
  body <- c() 

  # Prepare header
  hdr[length(hdr) + 1] <- "---"
  hdr[length(hdr) + 1] <- "output: pdf_document"
  if (rs$orientation == "landscape") {
    hdr[length(hdr) + 1] <- "classoption: landscape"
  } else {
    hdr[length(hdr) + 1] <- "classoption: portrait"
  }
  
  if (rs$font_size == 10)
    hdr[length(hdr) + 1] <- "fontsize: 10pt"
  else if (rs$font_size == 12)
    hdr[length(hdr) + 1] <- "fontsize: 12pt"
  
  hdr[length(hdr) + 1] <- "geometry: margin=0cm"
  # Figure out paper size options
  if (rs$paper_size == "letter")
    hdr[length(hdr) + 1] <- "papersize: letter"
  else if (rs$paper_size == "A4")
    hdr[length(hdr) + 1] <- "papersize: a4"
  hdr[length(hdr) + 1] <- "header-includes:"
  hdr[length(hdr) + 1] <- "  - \\renewcommand{\\familydefault}{\\ttdefault}"
  hdr[length(hdr) + 1] <- "---"
  
  
  # Make body replacements
  if (rs$font_size == 10)
    body <- gsub(" ", "\\hspace{5pt}", ls, fixed = TRUE)
  else if (rs$font_size == 12)
    body <- gsub(" ", "\\hspace{6pt}", ls, fixed = TRUE)
  
  body <- gsub("\f", "\\newpage\n&nbsp;", body, fixed = TRUE)
  body <- gsub("-", "--", body, fixed = TRUE)
  
  
  # Write to file  
  f <- file(rmd_path, open="a")

  writeLines(hdr, con = f)
  writeLines(paste0("&nbsp;", body, "\\"), con = f)
  
  close(f)
  
  # Write PDF
  render(rmd_path, pdf_document(), pdf_path)
}


