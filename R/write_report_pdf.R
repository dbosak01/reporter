

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
  
  
  if (file.exists(rs$modified_path))
    file.remove(rs$modified_path)
  
  
  # Create text output 
  res <- create_report_text(rs)
  
  rs <- res[["rs"]]
  ls <- res[["ls"]]

  # Revise text and write to pdf
  fls <- write_pdf_output(rs, ls, rs$modified_path)
  
  return(rs)
}

#' @noRd
write_pdf_output <- function(rs, ls, pdf_path) {
  
  pgs <- list()
  pg <- list(text = c(), images = c())

  # Loop through text pages
  for (i in seq_along(ls)) {
  
    
    # Replace fill lines with blanks
    if (rs$has_graphics) {
      
      ls[[i]] <- gsub("```fill```", "", ls[[i]], fixed = TRUE)
      
      # Need to split up text and images
      for (ln in ls[[i]]) {
        
        res <- grep("```([^}]*)```", ln)
        if (length(res) > 0) {
          pg$text <- append(pg$text, "")
          pi <- parse_image(ln)
          pi$line_start <- length(pg$text)
          pg$images[[length(pg$images) + 1]] <- pi
        } else {
          pg$text <- append(pg$text, ln)   
        }
        
      }
      
    } else 
      pg$text <- ls[[i]]
    

    
    pgs[[length(pgs) + 1]] <- pg
    pg <- list(text = c(), images = c())
  }
  
  
  if (file.exists(pdf_path))
    file.remove(pdf_path)
  
  r <- create_pdf(filename = pdf_path,
                  margin_top = rs$margin_top,
                  margin_left = rs$margin_left,
                  fontsize = rs$font_size,
                  page_height = rs$page_size[2],
                  page_width = rs$page_size[1],
                  orientation = rs$orientation,
                  units = rs$units,
                  info = TRUE)
  
  for (pg in pgs) {
    
    if (length(pg$images) > 0) {
      
      imgs <- list()
      for (img in pg$images) {

        imgs[[length(imgs) + 1]] <- page_image(img$filename,
                                             height = img$height,
                                             width = img$width,
                                             align = img$align,
                                             line_start = img$line_start)
                  
      }
     
      r <- add_page(r, page_text(pg$text), imgs)
                    
    } else {
      r <- add_page(r, page_text(pg$text)) 
    }
  }
  
  
  write_pdf(r)
  
  
  return(pdf_path)
  
}

parse_image <- function(image_string) {
 
  ret <- list()
  
  # Remove braces
  rw <- trimws(gsub("`", "", image_string, fixed = TRUE))  
  
  # Split on pipe
  spec <- strsplit(rw, "|", fixed = TRUE)[[1]]
  
  pth <- gsub("\\", "/", spec[[1]], fixed = TRUE)
  
  # 1 = path
  # 2 = height
  # 3 = width
  # 4 = align 
  
  ret$filename <- pth
  ret$height <- as.numeric(spec[[2]])
  ret$width <- as.numeric(spec[[3]])
  ret$align <- spec[[4]]
  
  return(ret)
  
}

#' Old rmarkdown version
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


