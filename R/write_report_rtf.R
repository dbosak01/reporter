

#' @title
#' Write a RTF report to the file system
#'
#' @description
#' This function writes a report_spec object to the file system, using the
#' parameters provided in the object.
#'
#' @param x The report_spec object to write.
#' @return The report spec.
#' @noRd
write_report_rtf <- function(rs) {
  
  debug <- FALSE
  
  orig_path <- rs$file_path
  
  # Create temp path for text output
  if (debug) {
    
    b_path <- file.path(getwd(), "tests/testthat")
    
    tmp_path <- file.path(b_path, "output/tmp.txt")
    rtf_path <- file.path(b_path, "output/tmp.rtf")
    if (file.exists(tmp_path))
      file.remove(tmp_path)
    if (file.exists(rtf_path))
      file.remove(rtf_path)
    if (file.exists(orig_path))
      file.remove(orig_path)
  } else {
    tmp_dir <- tempdir()
    if (!file.exists(tmp_dir))
      dir.create(tmp_dir)
    tmp_path <- tempfile(fileext = ".txt", tmpdir = tmp_dir)
    rtf_path <- tempfile(fileext = ".rtf", tmpdir = tmp_dir)
    #rtf <- sub(".pdf", ".Rmd", orig_path)
  }
  # print(tmp_path)
  # print(rtf_path)
  # print(getwd())
  
  if (file.exists(orig_path))
    file.remove(orig_path)
  
  # Replace original path
  rs$file_path <- tmp_path
  
  # Create text output normally to temp location
  rs <- write_report_text(rs)
  
  # Read lines from text output
  ls <- readLines(tmp_path)
  
  # Revise text and write to pdf
  write_rtf_output(rs, ls, rtf_path, orig_path, tmp_dir)
  
  # Restore original path
  rs$file_path <- orig_path
  
  # Clean up
  if (!debug) {
    file.remove(tmp_path)
    #file.remove(rtf_path)
    nms <- list.files(tmp_dir, pattern = "(.*)\\.png", full.names = TRUE)
    file.remove(nms)
    #unlink(tmp_dir, recursive = TRUE)
  }
  
  return(rs)
}


# May need some adjustments/sophistication/options to this function
#' @noRd
write_rtf_output <- function(rs, ls, rtf_path, orig_path, tmp_dir) {
  
  # Set up vectors
  hdr <- c() 
  body <- c() 

  
  # Get conversion factor to twips
  if (rs$units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
  
  # Prepare header
  hdr[length(hdr) + 1] <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Courier;}}"
  if (rs$orientation == "landscape") {
    hdr[length(hdr) + 1] <- "\\landscape\\horzdoc"
    hdr[length(hdr) + 1] <- paste0("\\paperw", round(rs$page_size[2] * conv),
                                   "\\paperh", round(rs$page_size[1] * conv))
  } else {
    hdr[length(hdr) + 1] <- "\\vertdoc"
    hdr[length(hdr) + 1] <- paste0("\\paperw", round(rs$page_size[1] * conv),
                                   "\\paperh", round(rs$page_size[2] * conv))
  }
  
  hdr[length(hdr) + 1] <- paste0("\\margl", round(rs$margin_left * conv),
                                "\\margr", round(rs$margin_right * .9 * conv),
                                "\\margt", round(rs$margin_top * conv),
                                "\\margb", round(rs$margin_bottom/2 * conv))

  if (rs$font_size == 10)
    hdr[length(hdr) + 1] <- "\\fs20"
  else if (rs$font_size == 12)
    hdr[length(hdr) + 1] <- "\\fs24"
  
  # Start with all lines
  body <- encodeRTF(ls)
  
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

      # 1 = path
      # 2 = height
      # 3 = width
      # 4 = align
      
      img <- get_image_rtf(spec[[1]], as.numeric(spec[[3]]), as.numeric(spec[[2]]), rs$units)
      
      # Create latex codes
      if (spec[[4]] == "left") {
        ltx <- paste0("\\par\\ql\n"  )

      } else if (spec[[4]] == "right") {
        ltx <- paste0("\\par\\qr\n"  )
      } else  {
        ltx <- paste0("\\par\\qc\n"  )
      }

      # Replace original line with RTF codes
      body[[i]] <- paste0(ltx, img)
    }
  }
  
  body <- gsub("\f", "\\page ", body, fixed = TRUE)
  
  # Write to file  
  f <- file(orig_path, open="a")
  
  writeLines(hdr, con = f)
  
  writeLines(paste0(body, "\\line"), con = f)
  
  writeLines("}", con = f)
  
  close(f)
  
#   file.copy(rtf_path, orig_path)
#   file.remove(rtf_path)

}

#' @noRd
encodeRTF <- Vectorize(function(x) {
  
  
  ints <- utf8ToInt(x)
  res <- paste0(encodeRTF_internal(ints), collapse = "")
  
  return(res)
})

#' @noRd
encodeRTF_internal <- Vectorize(function(x) {
  
  ret <- intToUtf8(x, multiple = TRUE)
  if (ret == "}")
    ret <- "\\'7d"
  else if (ret == "{")
    ret <- "\\'7b"
  else if (ret == "\\")
    ret <- "\\'5c"
  else if (x == 175)
    ret <- "\\u175\\u175"
  else if (x > 127)
    ret <- paste0("\\u", x, "  ")
  
  return(ret)
  
})

