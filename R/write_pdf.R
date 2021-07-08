pointsize <- 1/72
inchsize <- 72

# Write PDF ---------------------------------------------------------------

#' A function to write out a PDF file
#' @noRd
write_pdf <- function(filename, contents, 
                      page_height = 8.5,
                      page_width = 11,
                      fontname = "Courier", 
                      fontsize = 10,
                      margin_top = 1,
                      margin_left = 1, 
                      info = FALSE,
                      author = "",
                      title = "",
                      subject = "",
                      keywords = "") {
  
  # Check font size is valid
  if (!fontsize %in% c(8, 10, 12))
    stop(paste0("Fontsize ", fontsize, " not valid."))
  else 
    fontsize <- fontsize - 1
  
  # Remove existing file if needed
  if (file.exists(filename))
    file.remove(filename)
  
  bp <- dirname(base_path)
  
  # Check that base path exists
  if (!file.exists(bp))
    stop(paste0("Base path '", bp, "' does not exist."))
  
  # Get x starting position in points
  stx <- margin_left * inchsize
  
  # Get y starting position in points
  sty <- (page_height * inchsize) -  (margin_top * inchsize)
  
  # Calculate reasonable line height
  lh <- fontsize  + round(fontsize * .3) 
                
  # Get header objects
  hd <- pdf_header(fontname = fontname,
                   pageheight = (page_height * inchsize),
                   pagewidth = (page_width * inchsize),
                   length(contents))

  # Get stream objects
  strmlst <- list()
  for (pg in contents) {
    
    idno <- length(hd) + length(strmlst) + 1
    
    strmlst[[length(strmlst) + 1]] <- pdf_stream(idno, 
                                                 get_stream(pg,
                                                 stx,
                                                 sty,
                                                 lh,
                                                 fontsize))
  }
  

  # Add info if desired
  if (info) {
    
    i <- Sys.info()
    
    idno <- length(hd) + length(strmlst) + 1
    
    inf <- pdf_info(idno, author = author,
                    title = title, 
                    subject = subject,
                    keywords = keywords)
    
    doc <- pdf_document(hd, strmlst, inf)
    
  } else {

    doc <- pdf_document(hd, strmlst)
  }
  
  # Render document
  rdoc <- render.pdf_document(doc)
  
  
  # Write document to file system
  # Encoding is important.  Has to be UTF-8.
  # This is the only way to do it on Windows.
  f <- file(filename, open="w+", encoding = "native.enc")
  
  
  writeLines(enc2utf8(rdoc), con = f, useBytes = TRUE)
  
  
  close(f)
  
}

#' @noRd
get_stream <- function(contents, startx, starty, lineheight, fontsize) {
 

  ypos <- seq(from = starty, length.out = length(contents), by = -lineheight)
  
  ret <- paste0("BT /F1 ", fontsize, 
                " Tf ", startx, " ", ypos, " Td (", 
                contents, ")Tj ET")
  
  
  return(ret)
  
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

#' @exportS3Method render pdf_info
render.pdf_info <- function(x) {
  
  
  # Get current time
  tm <- Sys.time()
  cdt <- paste0(as.character(tm, "D:%Y%m%d%H%M%S"), 
                stri_sub(as.character(tm, "%z"), 1, 3), "'",
                stri_sub(as.character(tm, "%z"), 4, 5), "'")
  
  
  dict <- pdf_dictionary(Producer = paste0("(", x$producer, ")"),
                         Author = paste0("(", x$author, ")"),
                         Title = paste0("(", x$title, ")"),
                         Subject = paste0("(", x$subject, ")"),
                         Creator = paste0("(", x$creator, ")"),
                         Keywords = paste0("(", paste(x$keywords, collapse = " ")
                                           , ")\n"),
                         CreationDate = paste0("(", cdt, ")"),
                         ModDate = paste0("(", cdt, ")"))
  
  obj <- pdf_object(x$id, dict)
  
  ret <- render.pdf_object(obj)
  
  
  return(ret)
}

#' @exportS3Method render pdf_document
render.pdf_document <- function(x) {
  
  
  cnts <- c("%PDF-1.7\n", "%âãÏÓ\n")
  xrefs <- c()
  infoid <- NULL
  
  for (itm in x) {
    
    # Sum up the number of bytes for all previous objects
    # Plus 1 to offset to first character of block
    xrefs[length(xrefs) + 1] <- chars(cnts) + 1 
    
    #print(itm$id)
    tmp <-  render(itm) 
    #print(tmp)
    cnts[length(cnts) + 1] <- tmp
    
    if ("pdf_info" %in% class(itm))
      infoid <- itm$id
    
  }
  
  cnt <- paste0(cnts, collapse = "")
  
  sm <- sum(chars(cnt), 1)


  ret <- paste0(cnt, render.xref(xrefs, x[[1]]$id, infoid,
                                  sm), 
                                  "%%EOF",
                                  collapse = "")
  
  
  return(ret)
  
}

#' Create cross-reference table
#' @noRd
render.xref <- function(xrefs, rootID, infoID, startpos) {
  
  
  # Create first entry of cross reference table
  ret <- paste0("xref\n", "0 ", length(xrefs) + 1, "\n")
  
  
  # Dynamically create subsequent entries
  # Windows has two character end character, 
  # and therefore doesn't need an extra space.
  # Total width has to be 20 characters exactly.
  if (Sys.info()[["sysname"]] == "Windows") {
    ret <- paste0(ret, "0000000000 65535 f\n")
    refs <- paste0(sprintf("%010d", xrefs), " 00000 n\n", collapse = "")
  } else {
    ret <- paste0(ret, "0000000000 65535 f \n")
    refs <- paste0(sprintf("%010d", xrefs), " 00000 n \n", collapse = "")
  }
    
  # Combine first and subsequent entries
  ret <- paste0(ret, refs)
  
  # Create dictionary for trailer
  dict <- pdf_dictionary(Size = length(xrefs) + 1, 
                         Root = paste(rootID, "0 R"))
  
  if (!is.null(infoID)) {

    dict$Info <- paste(infoID, "0 R")    
  } 
  
  # Create trailer
  ret <- paste0(ret, "trailer ", 
                render(dict),
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



pdf_header <- function(pagecount = 1, 
                       fontname = "Courier", 
                       pageheight = 612, 
                       pagewidth = 792) {
  
  lst <- list()
  
  lst[[1]] <- pdf_object(1, pdf_dictionary(Type = "/Catalog",
                                           Pages = "4 0 R"))
  
  lst[[2]] <- pdf_object(2, pdf_dictionary(Font = pdf_dictionary(F1 = "3 0 R")))
  
  lst[[3]] <- pdf_object(3, pdf_dictionary(Type = "/Font", 
                                           Subtype = "/Type1", 
                                           BaseFont = paste0("/", fontname)))
  
  pgseq <- seq(5, 4 + pagecount)

  kds <- paste(pgseq, "0 R", collapse = " ")
  
  lst[[4]] <- pdf_object(4, pdf_dictionary(Type = "/Pages",
                                           Kids = pdf_array(kds),
                                           Count = pagecount))
  
  strmseq <- seq(max(pgseq) + 1, max(pgseq) + pagecount)
  
  for ( i in seq_along(pgseq)) {
    lst[[pgseq[i]]] <- pdf_object(pgseq[i], 
                                  pdf_dictionary(Type = "/Page",
                                           Parent = "4 0 R",
                                           Resources = "2 0 R",
                                           MediaBox = pdf_array(0, 0, 
                                                                pagewidth, 
                                                                pageheight),
                                           Contents = paste(strmseq[i], "0 R")))

  }
  

  
  
  return(lst)
  
}

pdf_info <- function(id, 
                     author = NULL, title = NULL,
                     subject = NULL, creator = NULL,
                     keywords = NULL) {
  
  
  info <- structure(list(), class = c("pdf_info", "pdf_object", "list"))
  
  info$id <- id
  info$producer <- paste0("reporter v", getNamespaceVersion("reporter"))
  info$author <- author
  info$title <- title
  info$subject <- subject
  info$creator <- R.Version()["version.string"]
  info$keywords <- keywords
  info$create_date <- Sys.time()
  info$mod_date <- Sys.time()
  
  return(info)
}


# Utilities ---------------------------------------------------------------


#' Takes a vector of lines and returns the number of bytes.
#' Extra bytes are added for the end characters depending on the OS.
#' @noRd
chars <- function(lines) {
  
  
  ret <- sum(nchar(enc2utf8(lines), type = "bytes"))
  
  
  if (Sys.info()["sysname"] == "Windows") {
    ret <- ret + sum(stringi::stri_count(lines, fixed = "\n")) - 2
  }
  
  return(ret)
}
