

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
  
  orig_path <- rs$modified_path
  
  # Create temp path for text output
  if (debug) {
# 
#     b_path <- file.path(getwd(), "tests/testthat")
# 
#     tmp_path <- file.path(b_path, "output/tmp.txt")
#     rtf_path <- file.path(b_path, "output/tmp.rtf")
#     if (file.exists(tmp_path))
#       file.remove(tmp_path)
#     if (file.exists(rtf_path))
#       file.remove(rtf_path)
#     if (file.exists(orig_path))
#       file.remove(orig_path)
  } else {
    tmp_dir <- tempdir()
    if (!file.exists(tmp_dir))
      dir.create(tmp_dir)
    tmp_path <- tempfile(fileext = ".txt", tmpdir = tmp_dir)
    rtf_path <- tempfile(fileext = ".rtf", tmpdir = tmp_dir)

  }
  # print(tmp_path)
  # print(rtf_path)
  # print(getwd())

  if (file.exists(orig_path))
    file.remove(orig_path)
  
  # Replace original path
  rs$modified_path <- tmp_path
  
  # Create text output normally to temp location
  rs <- write_report_text(rs)

  # Read lines from text output
  ls <- readLines(tmp_path)

  # Revise text and write to rtf
  fls <- write_rtf_output(rs, ls, rtf_path, orig_path, tmp_dir)
  
  # Restore original path
  rs$modified_path <- orig_path

  # Clean up
  if (!debug) {
    file.remove(tmp_path)
    #file.remove(rtf_path)
    #nms <- list.files(tmp_dir, pattern = "(.*)\\.png", full.names = TRUE)
    if (length(fls) > 0)
      file.remove(fls)
    #unlink(tmp_dir, recursive = TRUE)
  }

  return(rs)
}


# May need some adjustments/sophistication/options to this function
#' @return Vector of graphic file paths
#' @noRd
write_rtf_output <- function(rs, ls, rtf_path, orig_path, tmp_dir) {
  
  # Set up vectors
  hdr <- c() 
  body <- c() 
  
  ret <- c()

  
  # Get conversion factor to twips
  if (rs$units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
  
  # Prepare header
  hdr[length(hdr) + 1] <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Courier;}}"
  if (rs$orientation == "landscape") {
    #hdr[length(hdr) + 1] <- "\\landscape\\horzdoc"
    hdr[length(hdr) + 1] <- "\\landscape"
    hdr[length(hdr) + 1] <- paste0("\\paperw", round(rs$page_size[2] * conv),
                                   "\\paperh", round(rs$page_size[1] * conv))
  } else {
    #hdr[length(hdr) + 1] <- "\\vertdoc"
    hdr[length(hdr) + 1] <- paste0("\\paperw", round(rs$page_size[1] * conv),
                                   "\\paperh", round(rs$page_size[2] * conv))
  }
  
  hdr[length(hdr) + 1] <- paste0("\\margl", round(rs$margin_left * conv),
                                "\\margr", round(rs$margin_right * conv),
                                "\\margt", round(rs$margin_top * conv),
                                "\\margb", round(rs$margin_bottom  * conv))

  # Line spacing values determined be trial and error.
  # Needed for LibreOffice.  Appear to be ignored in Word.
  if (rs$font_size == 10)
    hdr[length(hdr) + 1] <- "\\sl-225\\slmult0\\fs20"
  else if (rs$font_size == 12)
    hdr[length(hdr) + 1] <- "\\sl-275\\slmult0\\fs24"
  else if (rs$font_size == 8)
    hdr[length(hdr) + 1] <- "\\sl-180\\slmult0\\fs16"
  else if (rs$font_size == 11)
    hdr[length(hdr) + 1] <- "\\sl-250\\slmult0\\fs22"
  else if (rs$font_size == 9)
    hdr[length(hdr) + 1] <- "\\sl-200\\slmult0\\fs18"
  
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
      
      img <- get_image_rtf(spec[[1]], as.numeric(spec[[3]]), 
                           as.numeric(spec[[2]]), rs$units)
      ret[length(ret) + 1] <- spec[[1]]
      
      # Create rtf codes
      if (spec[[4]] == "left") {
        hd <- paste0("\\par\\sl0\\ql\n"  )

      } else if (spec[[4]] == "right") {
        hd <- paste0("\\par\\sl0\\qr\n"  )
      } else  {
        hd <- paste0("\\par\\sl0\\qc\n"  )
      }
      
      # Restore sizing and alignment
      if (rs$font_size == 8) {
        ft <- "\\par\\sl-180\\ql"
      } else if (rs$font_size ==  12) {
        ft <- "\\par\\sl-275\\ql"
      } else if (rs$font_size ==  11) {
        ft <- "\\par\\sl-250\\ql"
      } else if (rs$font_size ==  9) {
        ft <- "\\par\\sl-200\\ql"
      } else {
        ft <- "\\par\\sl-225\\ql"
      }

      # Replace original line with RTF codes
      body[[i]] <- paste0(hd, img, ft)
    }
  }
  
  # Get page breaks
  pgs <- grepl("\f", body, fixed = TRUE)
  
  # Adjust by one to get previous line
  pgs <- pgs[2:length(pgs)]
  
  # Add one for end of document
  pgs[length(pgs) + 1] <- TRUE 
  # print(length(body))
  # print(length(pgs))
  # print(pgs)
  
  body <- gsub("\f", "\\page ", body, fixed = TRUE)
  body <- paste0(body, ifelse(pgs, "", "\\line"))
  
  # Write to file  
  f <- file(orig_path, open="a")
  
  writeLines(hdr, con = f)
  
  writeLines(body, con = f)
  
  writeLines("}", con = f)
  
  close(f)
  
#   file.copy(rtf_path, orig_path)
#   file.remove(rtf_path)
  
  return(ret)

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

