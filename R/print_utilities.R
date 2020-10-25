
#' @import crayon
#' @noRd
print_title_header <- function(t_hdr) {
  
  # Print title header
  if (!is.null(t_hdr)) {
    
    mx <- max(length(t_hdr$titles), length(t_hdr$right))
    
    ttlcnt <- 1
    for (i in seq(1, mx)) {
      
      cat("- title header " %+% as.character(ttlcnt) %+% ": ")
      if (!is.na(t_hdr$titles[i]))
        cat("'" %+% t_hdr$titles[[i]] %+% "' ")
      if (!is.na(t_hdr$right[i]))
        cat("right='" %+% t_hdr$right[i] %+% "'")
      
      cat("\n")
      
      ttlcnt <- ttlcnt + 1
    }
  } 
  
  
}


#' @import crayon
#' @noRd
print_titles <- function(ttls) {
  
  # Print titles
  if (!is.null(ttls)) {
    
    ttlcnt <- 1
    for (i in seq_along(ttls)) {
      
      for (j in seq_along(ttls[[i]]$titles)) {
        cat("- title " %+% as.character(ttlcnt) %+% ": '" 
            %+% substring(ttls[[i]]$titles[[j]], 1) %+% "'\n")
        ttlcnt <- ttlcnt + 1
      }
      
    }
  } 
  
  
}


#' @import crayon
#' @noRd
print_footnotes <- function(ftnts) {
  
  # Print footnotes
  if (!is.null(ftnts)) {
    
    ftncnt <- 1
    # There can be more than 1 footnote block
    for (i in seq_along(ftnts)) {
      
      for (j in seq_along(ftnts[[i]]$footnotes)) {
        cat("- footnote " %+% as.character(ftncnt) %+% ": '" 
            %+% substring(ftnts[[i]]$footnotes[[j]], 1) %+% "'\n")
        ftncnt <- ftncnt + 1
      }
      
    }
  } 
}
