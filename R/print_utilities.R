
#' @import crayon
#' @noRd
print_title_header <- function(t_hdr) {
  
  # Print title header
  if (!is.null(t_hdr)) {
    
    ttlcnt <- 1
    
    for (i in seq_along(t_hdr)) {
      
      mx <- max(length(t_hdr[[i]]$titles), length(t_hdr[[i]]$right))
      
      for (j in seq(1, mx)) {
        
        cat(paste0("- title header ", as.character(ttlcnt), ": "))
        if (!is.na(t_hdr[[i]]$titles[j]))
          cat(paste0("'", t_hdr[[i]]$titles[j], "' "))
        
        if (!is.na(t_hdr[[i]]$right[j]))
          cat(paste0("right='", t_hdr[[i]]$right[j], "'"))
        
        cat("\n")
        
      }
      
      ttlcnt <- ttlcnt + 1
    }
  } 
  
  
}

tslst <- c("one")
is.na(tslst[2])


#' @import crayon
#' @noRd
print_titles <- function(ttls) {
  
  # Print titles
  if (!is.null(ttls)) {
    
    ttlcnt <- 1
    for (i in seq_along(ttls)) {
      
      for (j in seq_along(ttls[[i]]$titles)) {
        cat(paste0("- title ", as.character(ttlcnt), ": '" 
            , substring(ttls[[i]]$titles[[j]], 1), "'\n"))
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
        cat(paste0("- footnote ", as.character(ftncnt), ": '" 
            , substring(ftnts[[i]]$footnotes[[j]], 1), "'\n"))
        ftncnt <- ftncnt + 1
      }
      
    }
  } 
}
