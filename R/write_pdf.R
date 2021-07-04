
# Write PDF ---------------------------------------------------------------

#' A function to write out a PDF file
#' @noRd
write_pdf <- function(contents, 
                      page_size = NULL, 
                      fontname = "Courier", 
                      fontsize = "10",
                      margins = NULL) {
  
  
  
  
  
  
  
}


# Render Functions ---------------------------------------------------------


#' g <- function(x) {
#'   
#'  UseMethod("g") 
#' }
#' 
#' #' @exportS3Method g integer
#' g.integer <- function(x) {
#'  
#'   
#'   return(x + 2) 
#' }
#' 
#' #' @exportS3Method g character
#' g.character <- function(x) {
#'   
#'  return(paste(x, "two")) 
#' }


#' Generic render function
#' @noRd
render <- function(x) {
  
  UseMethod("render", x)
  
}

#' @exportS3Method render default
render.default <- function(x) {
  
  
  return(as.character(x))
  
}

#' @exportS3Method render pdf_contents
render.pdf_contents <- function(x) {
  
  
  ret <- "contents"
  
  return(ret)
  
}



#' @exportS3Method render pdf_array
render.pdf_array <- function(x) {
  
  
  ret <- paste0("[", paste(x, collapse = " "), "]") 
  
  
  return(ret)
  
}


#' @exportS3Method render pdf_object
render.pdf_object <- function(x) {
  
  ret <- paste0(x$id, " ", x$version, " obj")
  ret <- paste0(ret, render(x$parameters), "\n")
  
  if (!is.null(x$contents)) {
    
    ret <- paste0(ret, render(x$contents)) 
  }
  
  ret <- paste0(ret, "endobj\n")
  
  return(ret)
  
}

#' @exportS3Method render pdf_dictionary
render.pdf_dictionary <- function(x) {
  
  cnts <- c()
  
  for (itm in names(x)) {

    cnts[length(cnts) + 1] <- paste0("/", itm, " ", render(x[[itm]]))
  }
  
  cnts <- paste(cnts, collapse = " ")
  
  ret <- paste0("<<", cnts , ">>")
  
  return(ret)
  
}

#' @exportS3Method render pdf_stream
render.pdf_stream <- function(x) {
  

  cnts <- render(x$contents)
  
  strm <-   paste0("stream\n", cnts, "\nendstream\n")
  
  obj <- pdf_object(x$id, pdf_dictionary(Length = nchar(cnts)), strm)
                                         
  
  ret <- render.pdf_object(obj)
  
  return(ret)
  
  
}

#' @exportS3Method render pdf_document
render.pdf_document <- function(x) {
  
  
  cnts <- c("%PDF-1.7\n", "%âãÏÓ\n")
  xrefs <- c()
  
  for (itm in x) {
    tmp <-  render(itm) 
    cnts[length(cnts) + 1] <- tmp
    
    
    # Sum up the number of bytes for all previous objects
    # Add 4 bytes for the binary designator
    xrefs[length(xrefs) + 1] <- sum(nchar(cnts, type = "bytes")) + 4
    
  }
  
  
  cnt <- paste0(cnts, collapse = "")

  ret <- paste0(cnt, render.xref(xrefs, x[[1]]$id, 
                                  nchar(cnt)), 
                                  "%%EOF",
                                  collapse = "")
  
  
  return(ret)
  
}


#' @exportS3Method render pdf_document
render.xref <- function(xrefs, rootID, startpos) {
  
  ret <- paste0("xref\n", "0 ", length(xrefs) + 1, "\n")
  
  ret <- paste0(ret, "0000000000 65535 f\n")
  
  
  refs <- paste0(sprintf("%010d", xrefs), " 00000 n\n", collapse = "")
    
  ret <- paste0(ret, refs)
  
  ret <- paste0(ret, "trailer", 
                render(pdf_dictionary(Size = length(xrefs) + 1, 
                                      Root = paste(rootID, "0 R"))),
                "\n", 
                "startxref\n",
                startpos, "\n")
  
  return(ret)
  
}


# Class Definitions -------------------------------------------------------

#' Class definition for the Document 
#' Accepts a set of objects
#' @noRd
pdf_document <- function(...) {
  
  lst <- list(...)
  

  doc <- structure(list(), class = c("pdf_document", "list"))
  
  
  for (itm in lst) {
    
    if (!"pdf_object"  %in% class(itm))
      stop("Document item must be of class pdf_object.")
    else 
      doc[[itm$id]] <- itm
  }
  
  
  return(doc)
  
}


#' Define a class to contain a PDF array
#' @noRd
pdf_array <- function(...) {
  

  arr <- structure(list(...), class = c("pdf_array", "list"))
  
  
  return(arr)
  
}

#' @noRd
pdf_object <- function(id, params, contents = NULL) {
  
  
  if (!class(id) %in% c("integer", "numeric"))
    stop("Class of id must be integer or numeric.")
  
  if (!"pdf_dictionary" %in% class(params))
    stop("Class of params must be pdf_dictionary.")
  
  obj <- structure(list(), class = c("pdf_object", "list"))
  
  obj$id <- id
  obj$version <- 0
  obj$parameters <- params
  obj$contents <- contents
  
  return(obj)
}


#' @noRd
pdf_dictionary <- function(...) {
  
  
  d <- list(...)
  
  if (length(names(d)) == 0)
    stop("Dictionary entries must have names.")
  

  dict <- structure(d, class = c("pdf_dictionary", "list"))
  
  
  return(dict)
  
}

pdf_stream <- function(id, contents = NULL) {

  
  strm <- structure(list(), class = c("pdf_stream", "pdf_object", "list"))

  strm$id <- id
  strm$contents <- contents
  
  return(strm)
}

