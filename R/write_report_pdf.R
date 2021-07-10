

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
  
  # if (rmarkdown::pandoc_available("1.12.3") == FALSE) {
  #  stop("pandoc version >= 1.12.3 is required. Install to continue.") 
  # }
  
  debug <- FALSE
  
  orig_path <- rs$modified_path
  
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
  rs$modified_path <- tmp_path
  
  # Create text output normally to temp location
  rs <- write_report_text(rs)
  
  # Read lines from text output
  ls <- readLines(tmp_path, encoding = "UTF-8")

  # Revise text and write to pdf
  fls <- write_pdf_output(rs, ls, orig_path)

  # Restore original path
  rs$modified_path <- orig_path
  
  #print(tmp_path)
  
  # Clean up
  if (!debug) {
    file.remove(tmp_path)
  }
  
  return(rs)
}

#' @noRd
write_pdf_output <- function(rs, ls, pdf_path) {
  
  pgs <- list()
  pg <- c()
  
  for (ln in ls) {
    
    res <- grep("\f", ln, fixed = TRUE, value = FALSE) 
    if (length(res) > 0) {
      
      pgs[[length(pgs) + 1]] <- pg
      pg <- c(gsub("\f", "", ln, fixed = TRUE))
    
    } else {
      
      pg[length(pg) + 1] <- ln   
      
    }
  }
  
  if (length(pg) > 0)
    pgs[[length(pgs) + 1]] <- pg
  
  if (file.exists(pdf_path))
    file.remove(pdf_path)
  
  
  write_pdf(pdf_path, pgs, 
            margin_top = rs$margin_top,
            margin_left = rs$margin_left,
            fontsize = rs$font_size,
            page_height = rs$page_size[1],
            page_width = rs$page_size[2],
            orientation = rs$orientation,
            info = TRUE)
  
  return(pdf_path)
  
}

#' @noRd
write_pdf_output2 <- function(rs, ls, rmd_path, pdf_path, tmp_dir) {
  
  # Set up vectors
  hdr <- c() 
  body <- c() 
  ret <- c()
  
  # Prepare header
  hdr[length(hdr) + 1] <- "---"
  hdr[length(hdr) + 1] <- "output:"
  hdr[length(hdr) + 1] <- "  pdf_document:"
  hdr[length(hdr) + 1] <- "    latex_engine: xelatex"
  if (rs$orientation == "landscape") {
    hdr[length(hdr) + 1] <- "classoption: landscape"
  } # default if portrait
  
  if (rs$font_size == 10)
    hdr[length(hdr) + 1] <- "fontsize: 10pt"
  else if (rs$font_size == 12)
    hdr[length(hdr) + 1] <- "fontsize: 12pt"
  # if (rs$font_size == 8)
  #   hdr[length(hdr) + 1] <- "classoption: 8pt"


  
  # "left=3cm,right=3cm,top=2cm,bottom=2cm"
  if (rs$units == "inches") {
    geom <- paste0(" \"left=", sprintf("%.3f", rs$margin_left * .80), 
                   "in, right=", sprintf("%.3f", rs$margin_right * .65), 
                   "in, top=", sprintf( "%.3f", rs$margin_top * .85), 
                   "in, bottom=", sprintf("%.3f", rs$margin_bottom/2), "in\"")
  } else {
    geom <- paste0("\"left=", rs$margin_left * .80, "cm, right=", 
                   rs$margin_right  * .65, "cm, top=", rs$margin_top * .85, 
                   "cm, bottom=", rs$margin_bottom/2, "cm\"")
  }
  hdr[length(hdr) + 1] <- paste("geometry:", geom)
  # Figure out paper size options
  if (rs$paper_size == "letter")
    hdr[length(hdr) + 1] <- "papersize: letter"
  else if (rs$paper_size == "A4")
    hdr[length(hdr) + 1] <- "papersize: a4"
  hdr[length(hdr) + 1] <- "header-includes:"
  if (rs$font_size == 8)
    hdr[length(hdr) + 1] <- "  - \\usepackage[fontsize=8pt]{scrextend}"
  hdr[length(hdr) + 1] <- "  - \\renewcommand{\\familydefault}{\\ttdefault}"
  hdr[length(hdr) + 1] <- "  - \\thispagestyle{empty}"
  hdr[length(hdr) + 1] <- "---"

  hdr[length(hdr) + 1] <- "\\pagenumbering{gobble}"
  hdr[length(hdr) + 1] <- "\\begin{verbatim}"

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
      ret[length(ret) + 1] <- pth
  
      # 1 = path
      # 2 = height
      # 3 = width
      # 4 = align
      
      # Create latex codes
      if (spec[[4]] == "left") {
        ltx <- paste0("\\end{verbatim}\n",
                      "\\begin{figure}[h!]\n",
                      "\\begin{flushleft}\n", 
                      "\\includegraphics{", pth, "}\n",
                      "\\end{flushleft}\n",
                      "\\end{figure}\n",
                      "\\begin{verbatim}")
        
      } else if (spec[[4]] == "right") {
        ltx <- paste0("\\end{verbatim}\n",
                      "\\begin{figure}[h!]\n",
                       "\\begin{flushright}\n", 
                       "\\includegraphics{", pth, "}\n",
                       "\\end{flushright}\n",
                       "\\end{figure}\n",
                       "\\begin{verbatim}")
      } else  {
      ltx <- paste0("\\end{verbatim}\n",
                    "\\begin{figure}[h!]\n",
                    "\\centering\n", 
                    "\\includegraphics{", pth, "}\n",
                    "\\end{figure}\n",
                    "\\begin{verbatim}")
      }
      
      # Replace original line with latex codes
      body[[i]] <- ltx
    }
  }

  
  body <- gsub("\f", "\\end{verbatim}\n\\newpage\n\\begin{verbatim}", body, fixed = TRUE)
  body[length(body) + 1] <- "\\end{verbatim}"
  
  t1 <- tempfile(tmpdir = tmp_dir, fileext = ".pdf")
  
  if (file.exists(rmd_path))
    file.remove(rmd_path)

  # Write to file
  f <- file(rmd_path, open="w+", encoding = "native.enc")


  writeLines(enc2utf8(hdr), con = f, useBytes = TRUE)

  writeLines(enc2utf8(body), con = f, useBytes = TRUE)

  close(f)
  
  # There is a known warning on rendering 8pt font.
  # Can't figure out how to get rid of it.
  # So going to suppress it for now.
  # Other fonts sizes shouldn't have any warnings.
  # if (rs$font_size == 8) {
  #   suppressWarnings(rmarkdown::render(rmd_path, 
  #                                      rmarkdown::pdf_document(), t1, 
  #                                      quiet = TRUE))
  # } else {
  #   rmarkdown::render(rmd_path, 
  #                     rmarkdown::pdf_document(), t1, 
  #                     quiet = TRUE)
  # }
  
  file.copy(t1, pdf_path)

  if (file.exists(t1))
    file.remove(t1)
  
  return(ret)
  
}


