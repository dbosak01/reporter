

# Decodes
arm_decode <- c(A = "Placebo", B = "Treatment 1")
sex_decode <- c("M" = "Male",
                "F" = "Female")

race_decode <- c("WHITE" = "White", 
                 "BLACK OR AFRICAN AMERICAN" = "Black or African American", 
                 "ASIAN" = "Asian", 
                 "NATIVE AMERICAN" = "Native American",
                 "UNKNOWN" = "Unknown")


# Range formatting 
range_fmt <- function(x) {
  
  if (length(x) != 2) {
    stop("Input vector length must be equal to 2.")  
  }
  
  ret <- paste(x[1], "-", x[2])
  
  return(ret)
}

n_fmt <- function(x) {
 
  ret <- as.character(sum(!is.na(x)))
  
  return(ret)
}

# Range formatting 
quantile_range <- function(x, y) {
  
  
  ret <- paste(sprintf("%.1f", x), "-", sprintf("%.1f", y))
  
  return(ret)
}

median_fmt <- function(x) {
 
  
   ret <- sprintf("%.1f", x)
   
   return(ret)
  
}


# Age categorization
age_cat <- Vectorize(function(x) {  
  
  ret <- NA
  
  if (!is.na(x)) {
    
    if (x < 18) {
      ret <- "< 18"
    } else if (x >= 18 & x < 24) {
      ret <- "18 to 23"
    } else if (x >= 24 & x < 45) {
      ret <- "24 to 44"
    } else if (x >= 45 & x < 60) {
      ret <- "45 to 59"
    } else if (x >= 60) {
      ret <- ">= 60"
    } 
  } 
  
  return(ret)  
})



## Generate unique subject id ##
usubjid_fmt <- function(study, site, subjid) {
  
  ret <- paste0(toupper(study), "-", toupper(site), "-", subjid)
  
  return(ret)
}


# Count and percent function
cnt_pct <- function(cnt, denom) {
  
  # Calculate Percent
  pcts <- cnt/denom * 100
  
  # Deal with values between 0 and 1
  pctf <- ifelse(pcts > 0 & pcts < 1 , "< 1.0", sprintf("%5.1f", pcts))
  
  # Format result
  ret <- sprintf("%d (%s%%)", cnt, pctf)
  
  return(ret)
}


mean_sd <- function(x, y) {
  
  # Format result
  ret <- sprintf("%.1f (%.1f)", x, y)
  
  return(ret)
}

sex_fmt <- Vectorize(function(x) {
  
  ret <- NA
  
  if (!is.na(x)) {
   if (x == "M") {
     ret <- "Male"
   } else if (x == "F") {
     ret <- "Female"
   } else if (x == "") {
     ret <- ""
   } else {
     ret <- "Unknown" 
   }
  }
  
  return(ret)
})

race_fmt <- Vectorize(function(x) {
  
  ret <- NA
  
  if (!is.na(x)) {
    if (x == "ASIAN") {
      ret <- "Asian"
    } else if (x == "BLACK OR AFRICAN AMERICAN") {
      ret <- "Black or African American"
    } else if (x == "WHITE") {
      ret <- "White"
    } else {
      ret <- "Unknown" 
    }
  }
  
  return(ret)
})
