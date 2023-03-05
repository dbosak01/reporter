
# Globals -----------------------------------------------------------------

pointsize <- 1/72
inchsize <- 72
in2cm <- 2.54
binchars <- charToRaw("\xe2\xe3\xcf\xd3")



# Create PDF --------------------------------------------------------------

#' @noRd
create_pdf <- function(filename = NULL, 
                       page_height = 11,
                       page_width = 8.5,
                       fontname = "Courier", 
                       fontsize = 10,
                       margin_top = 1,
                       margin_left = 1, 
                       orientation = "landscape",
                       units = "inches",
                       conversion = 72,
                       info = TRUE) {
  
  # Check font size is valid
  if (!fontsize %in% c(8, 9, 10, 11, 12))
    stop(paste0("Fontsize ", fontsize, " not valid."))
  
  
  rpt <- structure(list(), class = c("pdf_report", "list"))
  
  rpt$filename <- filename
  rpt$page_height <- page_height
  rpt$page_width <- page_width
  rpt$fontname <- fontname
  rpt$fontsize <- fontsize
  rpt$margin_top <- margin_top
  rpt$margin_left <- margin_left
  rpt$info <- info
  rpt$author <- Sys.info()[["user"]]
  rpt$title <- ""
  rpt$subject <- ""
  rpt$keywords <- ""
  rpt$orientation <- orientation
  rpt$units <- units
  rpt$conversion <- conversion
  rpt$pages <- list()
    
  
  return(rpt)
}


#' @param x The pdf_report object to add a page to.
#' @param ... The page_text or page_image content to add.
#' @noRd
add_page <- function(x, ...) {
  
  
  pg <- structure(list(), class = c("pdf_page", "list"))
  
  inc <- list(...)
  
  for (itm in inc) {
    
    if (all(class(itm) == "list")) {
      
      for (sub in itm) {
        pg[[length(pg) + 1]] <- sub 
        
      }
      
      
    } else 
      pg[[length(pg) + 1]] <- itm
  }

  
  
  x$pages[[length(x$pages) + 1]] <- pg
  
  return(x)
  
}


add_info <- function(x,
                     author = "",
                     title = "",
                     subject = "",
                     keywords = "") {
  
  x$info <- TRUE
  x$author <- author
  x$title <- title
  x$subject <- subject
  x$keywords <- keywords
  
  return(x)
  
}


#' @noRd
page_text <- function(text, font_size = NULL, 
                      xpos = NULL, ypos = NULL, bold = FALSE,
                      align = NULL, alignx = NULL, has_page_numbers = NULL,
                      italics = FALSE) {
  
  txt <- structure(list(), class = c("page_text", "page_content", "list"))
  
  txt$text <- text
  txt$font_size <- font_size
  txt$xpos <- xpos
  txt$ypos <- ypos
  txt$bold <- bold
  txt$italics <- italics
  txt$align <- align
  txt$alignx <- alignx  # In units of measure
  
  res1 <- grepl("[pg]", text, fixed = TRUE)
  res2 <- grepl("[tpg]", text, fixed = TRUE)
  
  txt$has_page_numbers <- FALSE
  if (any(res1 == TRUE) | any(res2 == TRUE))
    txt$has_page_numbers <- TRUE
  
  return(txt)
  
}




#' Either pass align parameter and line_start, or pass specific xpos and ypos
#' All measurements in units specified.  Processing function will convert
#' appropriately.
#' @noRd
page_image <- function(filename, height, width,  
                       align = "center", line_start = NULL,
                       xpos = NULL, ypos = NULL, units = "inches",
                       dpi = 300) {
  
  img <- structure(list(), class = c("page_image", "page_content", "list"))
  
  if (!is.null(xpos) & !is.null(ypos)) {
    
    img$xpos <- xpos
    img$ypos <- ypos

  } else if (!is.null(align) * !is.null(line_start)) {
    
    img$align <- align 
    img$line_start <- line_start
    
  } else {
    
    stop("Must set either align parameter or xpos.") 
  }
  
  img$filename <- filename
  img$height <- height
  img$width <- width
  img$units <- units
  img$dpi <- dpi

  
  return(img)
  
}

page_line <- function(startx, starty, endx, endy) {
  
  ln <- structure(list(), class = c("page_line", "page_content", "list"))
  
  
  ln$startx <- startx
  ln$starty <- starty
  ln$endx <- endx
  ln$endy <- endy
  
  return(ln)
}

page_hline <- function(startx, starty, pwidth) {
  
  ln <- structure(list(), class = c("page_line", "page_content", "list"))
  
  
  ln$startx <- startx
  ln$starty <- starty
  ln$endx <- startx + pwidth
  ln$endy <- starty
  
  return(ln)
}

page_vline <- function(startx, starty, pheight) {
  
  ln <- structure(list(), class = c("page_line", "page_content", "list"))
  
  
  ln$startx <- startx
  ln$starty <- starty
  ln$endx <- startx
  ln$endy <- starty + pheight
  
  return(ln)
}


page_box <- function(startx, starty, pheight, pwidth, except = "") {
  
  
  bx <- structure(list(), class = c("page_box", "page_content", "list"))
  
  bx$startx <- startx
  bx$starty <- starty
  bx$pheight <- pheight
  bx$pwidth <- pwidth
  bx$except <- except
  
  return(bx)
  
}

page_grid <- function(startx, starty, rows, cols, pheights, pwidths) {
  
  
  grd <- structure(list(), class = c("page_grid", "page_content", "list"))
  
  grd$startx <- startx
  grd$starty <- starty
  grd$nrow <- nrow
  grd$rows <- rows
  grd$cols <- cols
  grd$pheights <- pheights
  grd$pwidths <- pwidths
  
  return(grd)
  
}

# Write PDF ---------------------------------------------------------------

#' A function to write out a PDF file
#' @param rpt The pdf_report object to write.
#' @param filename An optional file name.  If no filename is supplied,
#' it will use the filename from the report object. 
#' @noRd
write_pdf <- function(rpt, filename = NULL) {
  

  if (is.null(filename)) {
    if (is.null(rpt$filename))
      stop("Filename cannot be NULL.")
    
    filename <- rpt$filename
    
  }
  
  if (rpt$orientation == "landscape") {
    tmp <- rpt$page_height
    page_height <- rpt$page_width
    page_width <- tmp
    
  } else {
    
    page_height <- rpt$page_height
    page_width <- rpt$page_width
  }
  
  if (rpt$units == "cm") {
    
   page_height <- round(page_height / in2cm, 2)
   page_width <- round(page_width / in2cm, 2)
   margin_top <- round(rpt$margin_top / in2cm, 2)
   margin_left <- round(rpt$margin_left / in2cm, 2)
  } else {
    
   margin_top <- rpt$margin_top
   margin_left <- rpt$margin_left
  }
    
  
  # Remove existing file if needed
  if (file.exists(filename))
    file.remove(filename)
  
  bp <- dirname(filename)
  
  # Check that base path exists
  if (!file.exists(bp))
    stop(paste0("Base path '", bp, "' does not exist."))
  

  bdy <- get_pages(rpt$pages, margin_left, margin_top, 
                   page_height, page_width, rpt$fontsize, units = rpt$units,
                   fontname = rpt$fontname, conversion = rpt$conversion)
  
  rpt$pages <- length(rpt$pages)
                
  kids <- bdy$page_ids
  pgs <- bdy$objects


  # Get header objects
  hd <- get_header(font_name = rpt$fontname,
                   page_height = (page_height * inchsize),
                   page_width = (page_width * inchsize),
                   page_count = length(kids), 
                   page_ids = kids)
  

  # Add info if desired
  if (rpt$info) {
    
    idno <- length(hd) + length(pgs) + 1
    
    inf <- pdf_info(idno, author = rpt$author,
                    title = rpt$title, 
                    subject = rpt$subject,
                    keywords = rpt$keywords)
    
    doc <- pdf_document(hd, pgs, inf)
    
  } else {

    doc <- pdf_document(hd, pgs)
  }
  
  # Render document
  rdoc <- render.pdf_document(doc)
  
  # Write document to file system
  # Encoding is important.  Has to be UTF-8.
  # This is the only way to do it on Windows.
  f <- file(filename, open="wb", encoding = "native.enc")
  
  
  writeBin(rdoc, con = f, useBytes = TRUE)
  
  
  close(f)
  
  
  return(rpt)
}



#' A function to create a list of objects for the PDF header.
#' This includes the catalog, font, and pages.
#' @noRd
get_header <- function(page_count = 1, 
                       font_name = "Courier", 
                       page_height = 612, 
                       page_width = 792, 
                       page_ids = c()) {
  
  lst <- list()
  
  lst[[1]] <- pdf_object(1, pdf_dictionary(Type = "/Catalog",
                                           Pages = ref(5)))

  # 
  fn <- "Courier"
  fb <- "Courier-Bold"
  fi <- "Courier-Oblique"
  if (tolower(font_name) == "times") {
    fn <- "Times-Roman"
    fb <- "Times-Bold"
    fi <- "Times-Italic"
  } else if (tolower(font_name) == "arial") {
    fn <- "Helvetica"
    fb <- "Helvetica-Bold"
    fi <- "Helvetica-Oblique"
  }

  
  lst[[2]] <- pdf_object(2, pdf_dictionary(Type = "/Font", 
                                           Subtype = "/Type1", 
                                           BaseFont = paste0("/", fn),
                                           Encoding = "/WinAnsiEncoding"))
  
  lst[[3]] <- pdf_object(3, pdf_dictionary(Type = "/Font", 
                                           Subtype = "/Type1", 
                                           BaseFont = paste0("/", fb),
                                           Encoding = "/WinAnsiEncoding"))
  
  lst[[4]] <- pdf_object(4, pdf_dictionary(Type = "/Font", 
                                           Subtype = "/Type1", 
                                           BaseFont = paste0("/", fi),
                                           Encoding = "/WinAnsiEncoding"))

  
  if (page_count > 10)
    kds <- paste(page_ids, "0 R\n", collapse = " ")
  else 
    kds <- paste(page_ids, "0 R", collapse = " ")
  
  lst[[5]] <- pdf_object(5, pdf_dictionary(Type = "/Pages",
                                           Kids = pdf_array(kds),
                                           Count = page_count,                                                 
                                           MediaBox = pdf_array(0, 0, 
                                                        page_width, 
                                                        page_height)))
  
  
  return(lst)
  
}


#' Purpose of this function is to create the appropriate pdf objects
#' based on the pages added to the report.  Each page can have 1 or more 
#' pieces of content.  For instance, a page can have text and 2 images.
#' Object to pages is not 1 to 1.  There are at least two objects for a single
#' page: one for the page, and one for the page content.  If there is an image
#' on the page, there is another object to hold the image stream.  Note
#' that the image has to be referenced in both the page object and the 
#' the content object.  That is why the function creates the page and image
#' objects last: you can't really complete these objects until you examine
#' all the page contents.
#' @return A two part list, with a list of objects and a vector of ids for the 
#' pages.
#' @noRd
get_pages <- function(pages, margin_left, margin_top, page_height, page_width,
                      fontsize, units = "inches", fontname = "Courier", 
                      conversion = 72) {
  
  # Vector for object IDs of pages only
  kids <- c()
  
  # List for all objects
  ret <- list()
  
  # Determined by trial and error
  if (tolower(fontname) == "fixed")
    fontscale <- 87  # 87
  else 
    fontscale <- 100
  
  # Hate this.  Don't know why it is needed.  Width estimates should be the same.
  pg_adj <- 0
  if (tolower(fontname) == "arial" | tolower(fontname) == "times") {
    pg_adj <- -1
  } else if (tolower(fontname) == "courier" & units == "inches") {
    pg_adj <- 1 
  }
  
  # Get x starting position in points
  stx <- (margin_left * inchsize) - 5  # This was for fixed width
  
  # Get y starting position in points
  sty <- ((page_height * inchsize) -  (margin_top * inchsize)) - 5
  
  # Calculate reasonable line height
  # Also trial and error
  lh <- fontsize  + round(fontsize * .19, 2) 
  
  # Starting ID is 6 because of standard header objects.
  # This id variable will be incremented along the way 
  # as needed to get unique ids for the objects. 
  id <- 6
  
  pgnum <- 0
  tpg <- length(pages)
  
  # Loop through added pages
  for (pg in pages) {
    
    pgnum <- pgnum + 1
    
    # Set current page id
    page_id <- id
    
    # Add this page to the kids list
    kids <- append(kids, page_id)
    
    # There will always be one content object per page
    content_id <- id + 1
    
    # Increment id in preparation for next object that needs an id
    id <- content_id + 1
    
    # May or may not be image ids
    img_ids <- c()
    
    # Create content object
    # Content will be appended as we go along
    cnto <- pdf_text_stream(content_id, "")
    
    # Create a list of image streams for this page
    imgs <- list()
    
    for (cnt in pg) {
    
      if ("page_text" %in% class(cnt)) {
        
        # If fixed width PDF, will not have x/y positions
        if (is.null(cnt$xpos) | is.null(cnt$ypos)) {

          tmp <- get_byte_stream(cnt$text,
                                 stx, sty, lh, fontsize, fontscale)
        } else {
          
          # For PDF2 with page number flag set
          if (cnt$has_page_numbers & !is.null(cnt$align)) {
            
            # Need to adjust x position if there are page numbers,
            # because when the page number tokens are replaced, 
            # the string width has changed and can throw off the alignment.
            # Not working perfectly, but better.
            txt <- get_page_numbers_pdf(cnt$text, pgnum, tpg)
            w <- get_text_width(txt, fontname, 
                                ifelse(is.null(cnt$font_size), 
                                                      fontsize, cnt$font_size), 
                                units,
                                multiplier = 1.03) # 1.03 to match split_strings
            if (cnt$align == "left")
              nx <- cnt$alignx * conversion
            else if (cnt$align == "right")
              nx <- ((cnt$alignx - w) * conversion) + pg_adj  # No idea why pg_adj is needed
            else
              nx <- (cnt$alignx - (w / 2)) * conversion + pg_adj
            
            # For PDF2
            tmp <- get_byte_stream(txt, 
                                   stx + nx, sty - cnt$ypos, 
                                   lh, ifelse(is.null(cnt$font_size), 
                                              fontsize, cnt$font_size),
                                   fontscale, cnt$bold, cnt$italics)
            
          } else {
            
            # For other PDF2
            tmp <- get_byte_stream(cnt$text, 
                                   stx + cnt$xpos, sty - cnt$ypos, 
                                   lh, ifelse(is.null(cnt$font_size), 
                                              fontsize, cnt$font_size),
                                   fontscale, cnt$bold, cnt$italics)
            
          }
          

        }
    
      
      } else if ("page_image" %in% class(cnt)) {
        
        # Don't know how many there will be
        img_ids <- append(img_ids, id)
        
        # Convert measurements to points
        d <- calc_points(cnt, margin_left, margin_top, page_height, 
                             page_width, cnt$units, lh)
        
        # print(paste("wth:", d$wth))
        # print(paste("hgt:", d$hgt))
        # print(paste("xpos:", d$xpos))
        # print(paste("ypos:", d$ypos))


        # Every image needs a "Do" command on the content page
        tmp <- get_image_text(img_ref = id,
                              width = d$wth,
                              height = d$hgt,
                              xpos = d$xpos,
                              ypos = d$ypos)
        
        # Add stream to the list
        imgs[[length(imgs) + 1]] <- pdf_image_stream(id, 
                                                     height = d$phgt,
                                                     width = d$pwth,
                                       get_image_stream(cnt$filename))
          
        # Increment id in preparation for next object
        id <- id + 1
        
      } else if ("page_line" %in% class(cnt)) {
        
        tmp <- get_line_segment(startx = stx + cnt$startx,
                                starty = sty - cnt$starty,
                                endx = stx + cnt$endx,
                                endy = sty - cnt$endy)
        
      } else if ("page_box" %in% class(cnt)) {
        
        tmp <- get_box(startx = stx + cnt$startx,
                       starty = sty - cnt$starty, 
                       pheight = cnt$pheight,
                       pwidth = cnt$pwidth, 
                       except = cnt$except)
        
      } else if ("page_grid" %in% class(cnt)) {
        
        tmp <- get_grid(startx = stx + cnt$startx,
                        starty = sty - cnt$starty, 
                        rows = cnt$rows,
                        cols = cnt$cols,
                        pheights = cnt$pheight,
                        pwidths = cnt$pwidth)
        
      }
      
      # Append or replace content as appropriate
      if (all(cnto$contents == ""))
        cnto$contents <- tmp
      else 
        cnto$contents <- append(cnto$contents, tmp)
    
    
    }
    
    # Now can finally create all objects
    ret[[length(ret) + 1]] <- pdf_page(page_id, content_id, img_ids)
    ret[[length(ret) + 1]] <- cnto
    if (length(imgs) > 0)
      ret <- append(ret, imgs)
    
  }
  
  
  res <- list()
  res[["page_ids"]] <- kids
  res[["objects"]] <- ret
  
  return(res)
  
}



get_pages_back <- function(pages, margin_left, margin_top, page_height, page_width,
                      fontsize, units = "inches") {
  
  # Vector for object IDs of pages only
  kids <- c()
  
  # List for all objects
  ret <- list()
  
  # Determined by trial and error
  fontscale <- 87
  
  # Get x starting position in points
  stx <- (margin_left * inchsize) - 5
  
  # Get y starting position in points
  sty <- ((page_height * inchsize) -  (margin_top * inchsize)) - 5
  
  # Calculate reasonable line height
  # Also trial and error
  lh <- fontsize  + round(fontsize * .19, 2) 
  
  # Starting ID is 5 because of standard header objects.
  # This id variable will be incremented along the way 
  # as needed to get unique ids for the objects. 
  id <- 4
  
  # Loop through added pages
  for (pg in pages) {
    
    # Set current page id
    page_id <- id
    
    # Add this page to the kids list
    kids <- append(kids, page_id)
    
    # There will always be one content object per page
    content_id <- id + 1
    
    # Increment id in preparation for next object that needs an id
    id <- content_id + 1
    
    # May or may not be image ids
    img_ids <- c()
    
    # Create content object
    # Content will be appended as we go along
    cnto <- pdf_text_stream(content_id, "")
    
    # Create a list of image streams for this page
    imgs <- list()
    
    for (cnt in pg) {
      
      if ("page_text" %in% class(cnt)) {
        
        # Under current logic, there should only be one of these
        # In the future, will need to account for multiple pieces
        # of text, and may define their own positions and font size.
        # Right now everything is Courier from top left.
        tmp <- get_byte_stream(cnt$text,
                               stx, sty, lh, fontsize, fontscale)
        
        
      } else if ("page_image" %in% class(cnt)) {
        
        # Don't know how many there will be
        img_ids <- append(img_ids, id)
        
        # Convert measurements to points
        d <- calc_points(cnt, margin_left, margin_top, page_height, 
                         page_width, cnt$units, lh)
        
        # print(paste("wth:", d$wth))
        # print(paste("hgt:", d$hgt))
        # print(paste("xpos:", d$xpos))
        # print(paste("ypos:", d$ypos))
        
        
        # Every image needs a "Do" command on the content page
        tmp <- get_image_text(img_ref = id,
                              width = d$wth,
                              height = d$hgt,
                              xpos = d$xpos,
                              ypos = d$ypos)
        
        # Add stream to the list
        imgs[[length(imgs) + 1]] <- pdf_image_stream(id, 
                                                     height = d$phgt,
                                                     width = d$pwth,
                                                     get_image_stream(cnt$filename))
        
        # Increment id in preparation for next object
        id <- id + 1
        
      }
      
      # Append or replace content as appropriate
      if (all(cnto$contents == ""))
        cnto$contents <- tmp
      else 
        cnto$contents <- append(cnto$contents, tmp)
      
      
    }
    
    # Now can finally create all objects
    ret[[length(ret) + 1]] <- pdf_page(page_id, content_id, img_ids)
    ret[[length(ret) + 1]] <- cnto
    if (length(imgs) > 0)
      ret <- append(ret, imgs)
    
  }
  
  
  res <- list()
  res[["page_ids"]] <- kids
  res[["objects"]] <- ret
  
  return(res)
  
}

#' Convert all measurement to points, so they can be sent to pdf.
#' @import jpeg
#' @noRd
calc_points <- function(cnt, margin_left, margin_top, 
                        page_height, page_width, 
                        units, row_height) {
  
  pw <- page_width
  ph <- page_height
  ml <- margin_left
  mt <- margin_top
  
  if (units == "inches") {
    cw <- cnt$width
    ch <- cnt$height
    xp <- cnt$xpos 
    yp <- cnt$ypos

  } else if (units == "cm") {
    
    cw <- cnt$width / in2cm
    ch <- cnt$height / in2cm
    xp <- round(cnt$xpos / in2cm, 2)
    yp <- round(cnt$ypos/ in2cm, 2)
    
  } else 
    stop("Specified units not supported.")
  
  ret <- list()
  
  
  img <- readJPEG(cnt$filename)
  d <- dim(img)
  
  ret$pwth <- d[2]
  ret$phgt <- d[1]
  ret$wth <- round(cw  * inchsize, 2)
  ret$hgt <- round(ch * inchsize, 2)
  
  if (!is.null(cnt$align)) {
    if (cnt$align == "left")
      ret$xpos <- ml * inchsize
    else if (cnt$align == "right")
      ret$xpos <- (pw * inchsize) - ret$wth - (ml * inchsize)
    else 
      ret$xpos <- ((pw * inchsize)/2) - (ret$wth / 2)
    
    ret$ypos <- (ph * inchsize) - ret$hgt - 
      (row_height * (cnt$line_start - 1)) - (mt * inchsize)
    
  } else {
    ret$xpos <- xp * inchsize
    ret$ypos <- (ph * inchsize) - ret$hgt - (yp * inchsize) 
  }
  
  return(ret)
  
}

# Render Functions ---------------------------------------------------------



#' Generic render function
#' @noRd
render <- function(x) {
  
  UseMethod("render", x)
  
}

#' @exportS3Method render default
render.default <- function(x) {
  
  
  return(as.character(x))
  
}


#' @exportS3Method render pdf_array
render.pdf_array <- function(x) {
  
  
  ret <- paste0("[", paste(x, collapse = " "), "]") 
  
  
  return(ret)
  
}


#' @exportS3Method render pdf_object
render.pdf_object <- function(x) {
  
  ret <- paste0(x$id, " ", x$version, " obj")
  
  if (!is.null(x$parameters)) {
    ret <- paste0(ret, render(x$parameters), "\n")
  }
  
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

#' @exportS3Method render pdf_text_stream
render.pdf_text_stream <- function(x) {
  

  if (length(x$contents) > 1) {
    cnts <- paste0(x$contents, collapse = "\n")
    cnts <- paste0(cnts, "\n")
  } else {
    
    cnts <- paste0(x$contents, "\n") 
  }
  
  strm <-   paste0("stream\n", cnts, "endstream\n")
  
  obj <- pdf_object(x$id, pdf_dictionary(Length = chars(cnts)), strm) 

                                         
  ret <- render.pdf_object(obj)
  
  return(ret)
  
}


# This was hard to make work. Took a lot of research and trial
# and error.  PDF has a lot of different way to render an image, and most
# are very complicated.  This is the simplest one. Requires a JPEG.  
#' @exportS3Method render pdf_image_stream
render.pdf_image_stream <- function(x, view = FALSE) {
  
  
  if (length(x$contents) > 1) {
    cnts <- unlist(x$contents)
  } 
  
  
  if (rawToChar(cnts[[length(cnts)]]) != "\n")
    cnts[[length(cnts) + 1]] <- charToRaw("\n")
    
  strm <- list()
  strm[[1]] <- paste0(x$id, " ", x$version, " obj\n")
  strm[[2]] <- paste0(render(pdf_dictionary(Type = "/XObject",
                                     Subtype = "/Image",
                                     Width = x$width, 
                                     Height = x$height,
                                     BitsPerComponent = 8,
                                     ColorSpace = "/DeviceRGB",
                                     Filter = "/DCTDecode",
                                     # BitsPerComponent = 8,
                                     # ColorSpace = pdf_array("/Indexed", 
                                     #                        "/DeviceRGB",
                                     #                        111,
                                     #                        ref(10)),
                                     Length = chars(cnts))), "\n")
  strm[[3]] <- "stream\n"
  strm[[4]] <- cnts
  strm[[5]] <- "endstream\n"
  strm[[6]] <- "endobj\n"
  

  if (view) 
    ret <- strm
  else 
    ret <- unlist(vraw(strm))
  
  
  return(ret)
  
}


#' @exportS3Method render pdf_info
render.pdf_info <- function(x) {
  
  
  # Get current time
  tm <- Sys.time()
  cdt <- paste0(format(tm, format = "D:%Y%m%d%H%M%S"), 
                stri_sub(format(tm, format = "%z"), 1, 3), "'",
                stri_sub(format(tm, format = "%z"), 4, 5), "'")
  
  
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

# Returns a raw byte array that can be written
# directly to disk with writeBin
#' @encoding UTF-8
#' @exportS3Method render pdf_document
render.pdf_document <- function(x) {
  
  
  #cnts <- c("%PDF-1.7\n", "%âãÏÓ\n")
  
  cnts <- list()
  # First line identifies this a a PDF file of a particular version.
  cnts[[1]] <- "%PDF-1.7\n"
  
  # Second line is comment of binary characters to identify the content as binary,
  # so editors or operating systems won't mess with it.
  cnts[[2]] <- paste0("%", rawToChar(binchars), "\n")
  xrefs <- c()
  infoid <- NULL
  
  for (itm in x) {
    
    if (!is.null(itm)) {
      
      # Sum up the number of bytes for all previous objects
      # Plus 1 to offset to first character of block
      xrefs[length(xrefs) + 1] <- chars(cnts) 
      
      #print(itm$id)
      tmp <-  render(itm) 
      #print(tmp)
      cnts[[length(cnts) + 1]] <- tmp
      
      if ("pdf_info" %in% class(itm))
        infoid <- itm$id
    
    }
    
  }


  cnts[[length(cnts) + 1]] <- render.xref(xrefs, x[[1]]$id, infoid, chars(cnts)) 
  cnts[[length(cnts) + 1]] <- "%%EOF"
  
  ret <- unlist(vraw(cnts))
  
  
  return(ret)
  
}

#' Create cross-reference table
#' @noRd
render.xref <- function(xrefs, rootID, infoID, startpos) {
  
  
  # Create first entry of cross reference table
  ret <- paste0("xref\n", "0 ", length(xrefs) + 1, "\n")
  
  
  # # Dynamically create subsequent entries
  # # Windows has two character end character, 
  # # and therefore doesn't need an extra space.
  # # Total width has to be 20 characters exactly.
  # if (Sys.info()[["sysname"]] == "Windows") {
  #   ret <- paste0(ret, "0000000000 65535 f\n")
  #   refs <- paste0(sprintf("%010d", xrefs), " 00000 n\n", collapse = "")
  # } else {
    ret <- paste0(ret, "0000000000 65535 f \n")
    refs <- paste0(sprintf("%010d", xrefs), " 00000 n \n", collapse = "")
  # }
    
  # Combine first and subsequent entries
  ret <- paste0(ret, refs)
  
  # Create dictionary for trailer
  dict <- pdf_dictionary(Size = length(xrefs) + 1, 
                         Root = ref(rootID))
  
  if (!is.null(infoID)) {

    dict$Info <- ref(infoID)    
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
    
    # Input can be a single object or a list of objects
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
#' This is a list of numbers, strings, or object references
#' @noRd
pdf_array <- function(...) {
  

  arr <- structure(list(...), class = c("pdf_array", "list"))
  
  
  return(arr)
  
}

#' Define a general object class
#' @param id The id number for the object
#' @param params A dictionary of parameters for this object
#' @param contents Contents for this object.  May be null.
#' @noRd
pdf_object <- function(id, params = NULL, contents = NULL) {
  
  
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


#' Define a dictionary class, which is a list of name/value pairs
#' @noRd
pdf_dictionary <- function(...) {
  
  
  d <- list(...)
  
  if (length(d) > 0 & length(names(d)) == 0)
    stop("Dictionary entries must have names.")
  

  dict <- structure(d, class = c("pdf_dictionary", "list"))
  
  
  return(dict)
  
}

#' @param id The id for this page object
#' @param content_id The id of the content object for this page
#' @param graphic_ids The ids for the graphics streams for this page
#' @noRd
pdf_page <- function(id, content_id, graphic_ids = NULL) {
  
  
  pg <- structure(list(), class = c("pdf_page", "pdf_object", "list"))
  
  # If there are graphics, expand the procedures
  if (length(graphic_ids) > 0) {
    
    procs <- pdf_array("/PDF", 
                      "/Text", 
                      "/ImageB", 
                      "/ImageC", 
                      "/ImageI")
    
    xobj <- pdf_dictionary()
    
    for(g in graphic_ids) {
      
      xobj[[paste0("X", g)]] <- ref(g) 
      
    }
    
    
    res <- pdf_dictionary(Font = pdf_dictionary(F1 = ref(2),
                                                F2 = ref(3),
                                                F3 = ref(4)), 
                          ProcSet = procs,
                          XObject = xobj)
    
    parms <-  pdf_dictionary(Type = "/Page",
                             Parent = ref(5),
                             Contents = ref(content_id),
                             Resources = res)
                             
  } else {
    
    procs <- pdf_array("/PDF", "/Text")
    
    res <- pdf_dictionary(Font = pdf_dictionary(F1 = ref(2),
                                                F2 = ref(3),
                                                F3 = ref(4)), 
                          ProcSet = procs)
    
    parms <-  pdf_dictionary(Type = "/Page",
                             Parent = ref(5),
                             Contents = ref(content_id),
                             Resources = res)
  }

  
  # Assign properties
  pg$id <- id
  pg$version <- 0
  pg$parameters <-parms
  pg$content_id <- content_id
  pg$graphic_ids <- graphic_ids
  
  
  return(pg)
  
}

#' Define a stream for text content
#' @noRd
pdf_text_stream <- function(id, contents = NULL) {

  
  strm <- structure(list(), class = c("pdf_text_stream", "pdf_object", "list"))

  strm$id <- id
  strm$contents <- contents
  
  return(strm)
}

#' Define a stream for image content
#' @noRd
pdf_image_stream <- function(id, height, width, contents = NULL) {
  
  
  strm <- structure(list(), class = c("pdf_image_stream", "pdf_object", "list"))
  
  strm$id <- id
  strm$contents <- contents
  strm$version <- 0
  strm$height <- height
  strm$width <- width
  
  return(strm)
}

#' A function to create an info object.  The info object contains the 
#' author, etc.  of the document.  The create date in this is desirable
#' for production files, but make development a pain because it guarantees
#' that the PDF file will change every time a test case is run, and therefore
#' it is hard to tell which files have actually changed.  The only way to 
#' tell is to open them all up and look at them.
#' @noRd
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



#' A function to cast lists of mixed content to lists of raw vectors
#' @noRd
vraw <- Vectorize(function(line) {
  
  
 if (any(class(line) == "raw")) {
   ret <- line
 } else {
   
  ret <- charToRaw(line)
 }
  
  return(ret)
  
})

#' Count actual bytes as will exist in the file
#' @noRd
chars <- function(lines) {
  
  ret <- length(unlist(vraw(lines)))
  
  return(ret)
}


#' Return a reference.  Version numbers are all zero.  Not dealing with
#' versions in these documents. 
#' @noRd
ref <- function(id) {
  
 ret <- paste(id, "0 R")
 
 return(ret)
  
}

#' Utility function to create content for a text stream.  This will 
#' take a vector of strings, and create pdf statements to display each one.
#' Multiple lines are separated by the distance specified in lineheight.
#' Would like to add compression to this function to reduce size of PDF file.
#' This function largely replaces get_text_stream() because it supports more special 
#' characters.
#' @noRd
get_byte_stream <- function(contents, startx, starty, 
                            lineheight, fontsize, fontscale, bold = FALSE, 
                            italics = FALSE) {
  
  # Calculate y positions
  ypos <- seq(from = starty, length.out = length(contents), by = -lineheight)
  
  # lns <- stri_encode(contents, from = stri_enc_detect2(contents),
  #                    to = "Adobe-Standard-Encoding")
  
  
  #print(stri_enc_detect(contents))
  
  cnts <- c()
  
  # Convert text characters to byte codes
  for (ln in contents) {
    cnts[length(cnts) + 1] <- paste0(charToRaw(viconv(ln)), collapse = "")
  
  }
  
  bld <- "/F1 "
  if (bold == TRUE)
    bld <- "/F2 "
  
  if (italics == TRUE)
    bld <- "/F3 "
  
  # Create report line
  ret <- paste0("BT ", bld , fontsize, 
                " Tf ", fontscale, " Tz ", startx, " ", ypos, " Td <", 
                cnts, ">Tj ET")
  
  return(ret)
}


#' Utility function to create content for a text stream.  This will 
#' take a vector of strings, and create pdf statements to display each one.
#' Multiple lines are separated by the distance specified in lineheight.
#' Would like to add compression to this function to reduce size of PDF file.
#' This function largely replace by get_byte_stream() to support more special 
#' characters.
#' @noRd
get_text_stream <- function(contents, startx, starty, 
                            lineheight, fontsize, fontscale) {
  
  
  ypos <- seq(from = starty, length.out = length(contents), by = -lineheight)
  
  ret <- paste0("BT /F1 ", fontsize, 
                " Tf ", fontscale, " Tz ", startx, " ", ypos, " Td (", 
                viconv(contents), ")Tj ET")
  
  # Not working
  #ret <- memCompress(ret, type = "gzip")
  
  #ret <- paste0(ret, collapse = "")
  
  return(ret)
  
}

#' Utility function to create content for an image stream.
#' Has to be a JPEG, as that is the only format that PDF supports natively.
#' @noRd
get_image_stream <- function(filename) {
  

  
  if (!file.exists(filename))
    stop(paste0("File does not exist: ", filename))
  
  # Get file size and add 10% for safety
  fz <- file.info(filename)[["size"]] * 1.1

  # Read in bytes
  f <- file(filename, open="rb", encoding = "native.enc")
  
  ret <- readBin(con = f, "raw", fz)
  
  close(f)
  
  return(ret)
  
}

#' Utility function to create text content for an image
#' @noRd
get_image_text <- function(img_ref, height, width, xpos, ypos) {
  
  ret <- c()
  
  ret[1] <- "q"
  ret[2] <- paste(width, 0, 0, height, (xpos - 5), ypos, "cm")
  ret[3] <- paste0("/X", img_ref, " Do")
  ret[4] <- "Q"
  
  
  return(ret)
  
}


#' Utility function to create pdf codes for a line segment.
#' @noRd
get_line_segment <- function(startx, starty, endx, endy) {
  
  
  ret <- paste(startx, starty, "m", endx, endy, "l S")
  
  
  return(ret)
  
}

#' Utility function to create pdf codes for a box.
#' @noRd
get_box <- function(startx, starty, pheight, pwidth, except = "") {
  
  ret <- c()
  
  if (except != "top")
    ret[length(ret) + 1] <- paste(startx, starty, "m", 
                                  startx + pwidth, starty, "l S")
  if (except != "left")
    ret[length(ret) + 1] <- paste(startx, starty, "m", startx, starty - pheight, "l S")
  if (except != "bottom")
    ret[length(ret) + 1] <- paste(startx, starty - pheight, "m", startx + pwidth, 
                  starty - pheight, "l S")
  if (except != "right")
    ret[length(ret) + 1] <- paste(startx + pwidth, starty, "m", startx + pwidth, 
                  starty - pheight, "l S")
  
  return(ret)
  
}

#' Utility function to create pdf codes for a grid.
#' @noRd
get_grid <- function(startx, starty, rows, cols, pheights, pwidths) {
  
  ret <- c()
  
  wdths <- rep(pwidths, cols/length(pwidths)) 
  hgths <- rep(pheights, rows/length(pheights))
  twidth <- sum(wdths)
  theight <- sum(hgths)

  # Start top and left lines  
  ret[1] <- paste(startx, starty, "m", startx + twidth, starty, "l S")
  ret[2] <- paste(startx, starty, "m", startx, starty - theight, "l S")

  
  # Create remaining rows
  yline <- starty
  for (rw in hgths) {
    yline <- yline - rw
    ret[length(ret) + 1] <- paste(startx, yline, "m", 
                                  startx + twidth, yline, "l S")
  
  }
  
  # Create remaining columns
  xcol <- startx
  for (cl in wdths) {
    
    xcol <- xcol + cl
    ret[length(ret) + 1] <- paste(xcol, starty, "m", 
                                  xcol, starty - theight, "l S")
    
  }
  
  return(ret)
  
}

#' @description This is a vectorized version of iconv(), which converts 
#' encodings on a string.  The CP1252 is a Windows superset of Latin1
#' which the PDF spec happens to support natively.  This is useful because
#' we get more characters out of this than Latin1.  It is basically ANSI
#' plus a few extra characters.  Also added logic to convert characters that
#' fall outside this range to a question mark, which is better than the default
#' empty box.
#' @noRd
viconv <- Vectorize(function(vstr) {
  
  
  #print(paste0(Encoding(vstr), ": ", vstr))
  
  if (Encoding(vstr) == "UTF-8") {
    ret <- iconv(vstr, from =Encoding(vstr), to = "CP1252", sub = "?")
    
    if (length(grep("???", ret, fixed = TRUE)) > 0) {
      ret <- gsub("???", "?", ret, fixed = TRUE)
      if (Encoding(ret) == "UTF-8")
        ret <- iconv(ret, from ="UTF-8", to = "CP1252", sub = "?")
    }
    
  } else {
    
    ret <- vstr
  }
  
  return(ret)
  
}, USE.NAMES = FALSE, SIMPLIFY = TRUE)
