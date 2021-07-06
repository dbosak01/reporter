
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
  

  cnts <- paste0(x$contents, collapse = "\n")
  cnts <- paste0(cnts, "\n")
  
  strm <-   paste0("stream\n", cnts, "endstream\n")
  
  obj <- pdf_object(x$id, pdf_dictionary(Length = chars(cnts)), strm)
                                         
  
  ret <- render.pdf_object(obj)
  
  return(ret)
  
}

#' @exportS3Method render pdf_document
render.pdf_document <- function(x) {
  
  
  cnts <- c("%PDF-1.7\n", "%âãÏÓ\n")
  xrefs <- c()
  
  for (itm in x) {
    
    # Sum up the number of bytes for all previous objects
    # Plus 1 to offset to first character of block
    xrefs[length(xrefs) + 1] <- chars(cnts) + 1 
    
    #print(itm$id)
    tmp <-  render(itm) 
    #print(tmp)
    cnts[length(cnts) + 1] <- tmp
    
  }
  
  cnt <- paste0(cnts, collapse = "")
  
  sm <- sum(chars(cnt), 1)

  ret <- paste0(cnt, render.xref(xrefs, x[[1]]$id, 
                                  sm), 
                                  "%%EOF",
                                  collapse = "")
  
  
  return(ret)
  
}


render.xref <- function(xrefs, rootID, startpos) {
  
  ret <- paste0("xref\n", "0 ", length(xrefs) + 1, "\n")
  
  ret <- paste0(ret, "0000000000 65535 f\n")
  
  
  refs <- paste0(sprintf("%010d", xrefs), " 00000 n\n", collapse = "")
    
  ret <- paste0(ret, refs)
  
  ret <- paste0(ret, "trailer ", 
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
    
    if ("pdf_object" %in% class(itm)) {
      doc[[itm$id]] <- itm
    } else if (all("list" %in% class(itm))){
      
      for (sub in itm) {
        if (!"pdf_object"  %in% class(sub))
          stop("Document subitem must be of class pdf_object.")
        else 
          doc[[sub$id]] <- sub
      }
        
    } else 
      stop("Document item must be of class pdf_object.")
      
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



pdf_header <- function() {
  
  lst <- list()
  
  lst[[1]] <- pdf_object(1, pdf_dictionary(Type = "/Catalog",
                                           Pages = "2 0 R"))
  
  lst[[2]] <- pdf_object(2, pdf_dictionary(Type = "/Pages",
                                           Kids = pdf_array("3 0 R"),
                                           Count = 1))
  
  lst[[3]] <- pdf_object(3, pdf_dictionary(Type = "/Page",
                                           Parent = "2 0 R",
                                           Resources = "4 0 R",
                                           MediaBox = pdf_array(0, 0, 600, 700),
                                           Contents = "6 0 R"))
  lst[[4]] <- pdf_object(4, pdf_dictionary(Font = pdf_dictionary(F1 = "5 0 R")))
  
  lst[[5]] <- pdf_object(5, pdf_dictionary(Type = "/Font", 
                                           Subtype = "/Type1", 
                                           BaseFont = "/Courier"))
  
  
  
  return(lst)
  
}


# Utilities ---------------------------------------------------------------


#' Takes a vector of lines and returns the number of bytes.
#' Extra bytes are added for the end characters depending on the OS.
chars <- function(lines) {
  
  
  ret <- sum(nchar(enc2utf8(lines), type = "bytes"))
  
  
  if (Sys.info()["sysname"] == "Windows") {
    ret <- ret + sum(stringi::stri_count(lines, fixed = "\n")) - 2
  }
  
  return(ret)
}
