# setMethodS3("addPlot", "RTF", function(this,plot.fun=plot.fun,width=3.0,height=0.3,res=300, ...) {
#   if(!is.null(this$.font.size)) {
#     font.size = this$.font.size  # default
#   }
# 
#   tmp.file<-tempfile("temp_rtf_plot")
#   this$.rtf <- paste(this$.rtf,.start.paragraph(indent=this$.indent,font.size=font.size),sep="")
#   this$.rtf <- paste(this$.rtf,.rtf.plot(plot.fun=plot.fun,tmp.file=tmp.file,width=width,height=height,res=res, ...),sep="")
#   this$.rtf <- paste(this$.rtf,.end.paragraph(),sep="")
# 
#   if(file.exists(tmp.file) ) {
#     unlink(tmp.file)
#   }
# })
# 
# 
# setMethodS3("addPng", "RTF", function(this,file,width=3.0,height=0.3, ...) {
#   if(!is.null(this$.font.size)) {
#     font.size = this$.font.size  # default
#   }
# 
#   this$.rtf <- paste(this$.rtf,.start.paragraph(indent=this$.indent,font.size=font.size),sep="")
#   this$.rtf <- paste(this$.rtf,.add.png(file,width=width,height=height,verbose=FALSE),sep="")
#   this$.rtf <- paste(this$.rtf,.end.paragraph(),sep="")
# })
# 
# 
# # width and height are in inches
# add_png<-function(file,width=3,height=3,verbose=FALSE) {
#   # return a hexadecimal version of a file
#   max.bytes<-50000000  # maximum file size in bytes (~50MB)
#   dat<-readBin(file, what="raw", size=1, signed=TRUE, endian="little",n=max.bytes);
#   if(verbose) {
#     cat(paste(length(dat),"bytes read\n"))
#   }
#   paste("{\\rtf1\\ansi\\deff0{\\pict\\pngblip\\picwgoal",round(width*1440),"\\pichgoal",round(height*1440)," ",paste(dat,collapse=""),"}}",sep="")
#   # paste("{\\rtf1\\ansi\\deff0{\\pict\\pngblip\\picwgoal",round(width*1440),"\\pichgoal",round(height*1440)," \n",.chunk.vector(dat),"}}",sep="")
# }
# 
# .rtf.plot<-function(plot.fun,tmp.file="temp.png",width=3.0,height=0.3,res=300, ...) {
#   width.px<-round(width*res)
#   height.px<-round(height*res)
#   #png(tmp.file,width=width.px,height=height.px,units="px",pointsize=8,bg = "white",res=res)
#   png(tmp.file,width=width.px,height=height.px,units="px",pointsize=8,bg = "transparent",res=res)
#   plot.fun(...)
#   dev.off()
#   .add.png(tmp.file,width=width,height=height)
# }

# Right now only png supported
#' @noRd
get_image_rtf <- function(file_path, width, height, units) {
  
  # Get conversion factor to twips
  if (units == "inches") {
    conv <- 1440
  } else {
    conv <- 566.9291
  }
  
  ret <- paste0("{\\pict\\pngblip\\picwgoal",round(width*conv),"\\pichgoal",round(height*conv)," \n")
  ret <- paste0(ret, get_image_bytes(file_path), "\n} \\par\\ql")
  
  return(ret)
}

#' @noRd
get_image_bytes <- function(file_path) {
  
  
  if (!file.exists(file_path))
    stop(paste("Image file not found:", file_path))

  max.bytes<-50000000

  # Read in bytes
  dat<-readBin(file_path, what="raw", size=1, 
               signed=TRUE, endian="little", n=max.bytes);

  # Split single vector into list of 80 character vectors
  sdat <- split(dat, ceiling(seq_along(dat)/40))

  # Collapse each 80 character vector into a string
  lns <- sapply(sdat, paste, collapse = "")

  # Append a line return to each line and collapse into single string
  ret <- paste0(lns, "\n", collapse = "")

  
  return(ret)

}

          
#          
# img <- "./man/images/cat.png"
# 
# dat <- get_image_rtf(img, 3, 3, "inches")
# writeLines(dat, "bytes.txt")          
          
          
