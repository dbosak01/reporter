

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
    
    b_path <- file.path(getwd(), "tests/testthat")
    
    tmp_path <- file.path(b_path, "output/tmp.txt")
    rmd_path <- file.path(b_path, "output/tmp.Rmd")
    if (file.exists(tmp_path))
      file.remove(tmp_path)
    if (file.exists(rmd_path))
      file.remove(rmd_path)
    if (file.exists(orig_path))
      file.remove(orig_path)
  } else {
    tmp_dir <- tempdir()
    if (!file.exists(tmp_dir))
      dir.create(tmp_dir)
    tmp_path <- tempfile(fileext = ".txt", tmpdir = tmp_dir)
    rmd_path <- tempfile(fileext = ".Rmd", tmpdir = tmp_dir)
    #rmd_path <- sub(".pdf", ".Rmd", orig_path)
  }
  # print(tmp_path)
  # print(rmd_path)
  # print(getwd())
  
  if (file.exists(orig_path))
    file.remove(orig_path)
  
  # Replace original path
  rs$file_path <- tmp_path
  
  # Create text output normally to temp location
  rs <- write_report_text(rs)
  
  # Read lines from text output
  ls <- readLines(tmp_path, encoding = "UTF-8")

  # Revise text and write to pdf
  write_pdf_output(rs, ls, rmd_path, orig_path, tmp_dir)

  # Restore original path
  rs$file_path <- orig_path
  
  # Clean up
  if (!debug) {
    file.remove(tmp_path)
    file.remove(rmd_path)
    nms <- list.files(tmp_dir, pattern = "(.*)\\.png", full.names = TRUE)
    file.remove(nms)
    #unlink(tmp_dir, recursive = TRUE)
  }
  
  return(rs)
}

#' @noRd
write_pdf_output <- function(rs, ls, rmd_path, pdf_path, tmp_dir) {
  
  # Set up vectors
  hdr <- c() 
  body <- c() 

  # Prepare header
  hdr[length(hdr) + 1] <- "---"
  hdr[length(hdr) + 1] <- "output: pdf_document"
  if (rs$orientation == "landscape") {
    hdr[length(hdr) + 1] <- "classoption: landscape"
  } else {
   # hdr[length(hdr) + 1] <- "classoption: portrait"
  }
  
  if (rs$font_size == 10)
    hdr[length(hdr) + 1] <- "fontsize: 10pt"
  else if (rs$font_size == 12)
    hdr[length(hdr) + 1] <- "fontsize: 12pt"
  
  # "left=3cm,right=3cm,top=2cm,bottom=2cm"
  if (rs$units == "inches") {
    geom <- paste0(" \"left=", sprintf("%.3f", rs$margin_left * .80), 
                   "in, right=", sprintf("%.3f", rs$margin_right * .65), 
                   "in, top=", sprintf( "%.3f", rs$margin_top * .85), 
                   "in, bottom=", sprintf("%.3f", rs$margin_bottom/2), "in\"")
  } else {
    geom <- paste0("\"left=", rs$margin_left, "cm, right=", 
                   rs$margin_right/2, "cm, top=", rs$margin_top, 
                   "cm, bottom=", rs$margin_bottom/2, "cm\"")
  }
  hdr[length(hdr) + 1] <- paste("geometry:", geom)
  # Figure out paper size options
  if (rs$paper_size == "letter")
    hdr[length(hdr) + 1] <- "papersize: letter"
  else if (rs$paper_size == "A4")
    hdr[length(hdr) + 1] <- "papersize: a4"
  hdr[length(hdr) + 1] <- "header-includes:"
  hdr[length(hdr) + 1] <- "  - \\renewcommand{\\familydefault}{\\ttdefault}"
  hdr[length(hdr) + 1] <- "  - \\thispagestyle{empty}"
  hdr[length(hdr) + 1] <- "---"
  hdr[length(hdr) + 1] <- "\\pagenumbering{gobble}"

  # Start with all lines
  body <- ls
  
  if (rs$has_graphics) {
    # Remove fill lines
    fill_tags <- grep("```fill```", body, fixed = TRUE)
    body <- body[-fill_tags]
    
    # Replace any plot tags with latex codes
    plt_tags <- grep("```([^}]*)```", body)
    
    for (i in plt_tags) {
      
      # Remove braces
      rw <- trimws(gsub("`", "", body[i], fixed = TRUE))  
      
      # Split on pipe
      spec <- strsplit(rw, "|", fixed = TRUE)[[1]]
      
      pth <- gsub("\\", "/", spec[[1]], fixed = TRUE)
  
      # 1 = path
      # 2 = height
      # 3 = width
      # 4 = align
      
      # Create latex codes
      if (spec[[4]] == "left") {
        ltx <- paste0("\\begin{figure}[h!]\n",
                      "\\begin{flushleft}\n", 
                      "\\includegraphics{", pth, "}\n",
                      "\\end{flushleft}\n",
                      "\\end{figure}\n"  )
        
      } else if (spec[[4]] == "right") {
        ltx <- paste0("\\begin{figure}[h!]\n",
                       "\\begin{flushright}\n", 
                       "\\includegraphics{", pth, "}\n",
                       "\\end{flushright}\n",
                       "\\end{figure}\n"  )
      } else  {
      ltx <- paste0("\\begin{figure}[h!]\n",
                    "\\centering\n", 
                    "\\includegraphics{", pth, "}\n",
                    "\\end{figure}"  )
      }
      
      # Replace original line with latex codes
      body[[i]] <- ltx
    }
  }
  
  # Make body replacements
  if (rs$font_size == 10)
    body <- gsub(" ", "\\hspace{5pt}", body, fixed = TRUE)
  else if (rs$font_size == 12)
    body <- gsub(" ", "\\hspace{6pt}", body, fixed = TRUE)
  
  breaks <- grep("\f", body, value = FALSE, fixed = TRUE)

  
  body <- gsub("\f", "", body, fixed = TRUE)
  body <- gsub("-", "--", body, fixed = TRUE)
  
  
  ## Break up doc into separate pdfs to prevent render function 
  ## from running out of memory.  Then stitch them back together 
  ## with qpdf
  
  t1 <- tempfile(tmpdir = tmp_dir, fileext = ".pdf")
  t2 <- tempfile(tmpdir = tmp_dir, fileext = ".pdf")
  t3 <- tempfile(tmpdir = tmp_dir, fileext = ".pdf")
  
  # Add break at end to handle last page
  breaks[length(breaks) + 1] <- length(body) + 1
  
  endpos <- 0
  
  for (i in seq_along(breaks)) {
    
    startpos <- endpos + 1
    endpos <- breaks[i] - 1
    
    if (file.exists(rmd_path))
      file.remove(rmd_path)
    
    # Write to file  
    f <- file(rmd_path, open="w+", encoding = "native.enc")
  

    writeLines(enc2utf8(hdr), con = f, useBytes = TRUE)
  
    writeLines(enc2utf8(paste0("&nbsp;", body[startpos:endpos], "\\")), 
               con = f, useBytes = TRUE)
    
    close(f)
    
    # Write PDF to tmp directory and then copy to desired folder to avoid errors
    

    
    rmarkdown::render(rmd_path, rmarkdown::pdf_document(), t2, quiet = TRUE)
    
    if (startpos == 1)
      file.copy(t2, t1)
    else {
      
      # t1 <- "C:\\packages\\rptr\\tests\\testthat\\pdf\\test6.pdf"
      # t2 <- "C:\\packages\\rptr\\tests\\testthat\\pdf\\test7.pdf"
      # t3 <- "C:\\packages\\rptr\\tests\\testthat\\pdf\\test99.pdf"
      cmd <- paste0("qpdf --empty --pages \"", t1, "\" \"", 
                    t2, "\" -- \"", t3, "\"") 
      res <- system(cmd) 
      
      if (res == 0) {
        file.copy(t3, t1, overwrite = TRUE)
        file.remove(t2)
        file.remove(t3)
      } else 
        stop("PDF concatenation failed.")
    }
  
  }
  file.copy(t1, pdf_path)
  
  if (file.exists(t1))
    file.remove(t1)
  if (file.exists(t2))
    file.remove(t2)
  if (file.exists(t3))
    file.remove(t3)

}


