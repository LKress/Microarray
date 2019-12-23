# /**
#
# \name{plot.imageMatrix}
#
# \title{Visualisation of a matrix}
#
# \alias{plot.imageMatrix}
#
# \keyword{hplot}
#
# \description{Visualisation of a data matrix, e.g. a matrix representing
#  a two dimensional area, a matrix of distance or similarity scores,
#  or any numeric matrix. Coloured boxes represent values, labels will
#  be drawn inside the box.}
#
# @usage
#
# \arguments{
#  \item{x}{data matrix, class \code{matrix}; required; default missing}
#  \item{labelMatrix}{matrix of labels of class \code{array} of dim(\code{x});
#                     optional; default missing}
#  \item{zlim}{numeric vector; defines the maximal z-range of the plot;
#              default missing}
#  \item{zScale}{logical; adds a scale to the plot; required; default TRUE}
#  \item{separateZScale}{logical; extra plot of the scale; required; default FALSE}
#  \item{colourRamp}{vector of colours; optional; default missing}
#  \item{reverseYaxis}{logical; required; default \code{TRUE}}
#  \item{labels}{vector of names corresponding to  a quadratic \code{x};
#                optional; default missing}
#  \item{xLabels}{vector of names corresponding to the columns of
#                 \code{x} overrides \code{labels}; optional; default missing}
#  \item{yLabels}{vector of names corresponding to the rows of
#                 \code{x} overrides \code{labels}; optional; default missing}
#  \item{width}{graphics window width; required; default: 8}
#  \item{height}{graphics window height; required; default: 7}
#  \item{labelMatrixTextScaling}{numeric; required; default: 0.7}

#  \item{plotOutput}{character string specifying
#          either "standard", "screen", "twoScreens", "pdf" or "win.metafile";
#          required; default: "standard"}
#  \item{fileName}{character string specifying the file path and file name;
#                  optional; default missing}
#  \item{...}{arguments are passed to \code{image}(), for example
#             \code{xlab}, \code{ylab} (x- and y-axis label) and
#             \code{main} (plot title) and \code{asp=1} to set the
#             aspect ratio to 1}
# }
#
# \details{If no labels are supplied, the dimnames of \code{x} or if missing
#          a simple numbering is used instead.}
#
# \examples{
#
#
# plot.imageMatrix(x=matrix(c(3,4,4,3),nrow=2, ncol=2),labels=c("one","two"))
# ma <- matrix(c(0.3,0.01,0.7,0.1,0.5,0.3,1,0.5,01), nrow=3,ncol=3)
# class(ma) <- c("imageMatrix", "matrix")
# plot(ma, labelMatrix=ma,labels=c("one","two", "three"), zlim=c(0,1))
# ma <- matrix(c(0.3, 0.01, 0.7, 0.1, 0.5, 0.3, 1, 0.5, 1, 0, 1, 0), nrow = 4, ncol = 3, byrow=TRUE )
# class(ma) <- c("imageMatrix", "matrix")
# plot(ma, labelMatrix = ma, xLabels = c("one", "two", "three"), zlim=c(0,1))
# plot(ma, reverseYaxis=FALSE, labelMatrix = ma, xLabels = c("one", "two", "three"), zlim=c(0,1))
#
#\dontshow{
#
# plot.imageMatrix(main="Similarities",x=matrix(c(3,4,4,3),nrow=2, ncol=2),labels=c("one","two"))
# ma <- matrix(c(0.3,0.01,0.7,0.1,0.5,0.3,1,0.5,01), nrow=3,ncol=3)
# plot.imageMatrix(main="Similarities",x=ma, labelMatrix=ma,labels=c("one","two", "three"), zlim=c(0,1))
# ma <- matrix(c(0.3, 0.01, 0.7, 0.1, 0.5, 0.3, 1, 0.5, 1, 0, 1, 0), nrow = 4, ncol = 3, byrow=TRUE )
# plot.imageMatrix(main = "x", x = ma, labelMatrix = ma, xLabels = c("one", "two", "three"), zlim=c(0,1) )
# plot.imageMatrix(matrix(1:10,nrow=1))
#}
#
#
# }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
#
# */

plot.imageMatrix <- function(x, labelMatrix, zlim,
                        separateZScale=FALSE, zScale=TRUE,
                        colourRamp, reverseYaxis = TRUE,
                        labels, xLabels, yLabels,
                        width=8, height=7,
                        labelMatrixTextScaling=0.7,
                        plotOutput="standard",
                        fileName, ...){

  

  if( missing(fileName) ){
    fileName <- "plot.imageMatrix.output"
  }
  
  if( ! plotOutput %in% c("standard","twoScreens","screen","pdf","win.metafile") ){
    warning(" unknown plotOutput in plot.imageMatrix", call.=FALSE)
    return()
  }

  if( plotOutput == "win.metafile" ){
    if( .Platform$OS.type != "windows" ){
      stop( paste(plotOutput, " only valid if running under Windows in plot.imageMatrix "), call.=FALSE )
    }
  }

  if( ! missing(zlim) ){

    zlimRange <- range(as.numeric(zlim))
    minimum <- min(zlimRange, na.rm=TRUE)
    maximum <- max(zlimRange, na.rm=TRUE)

    if( ! (minimum < maximum) ){
      warning(" zlim: minimum is not less than maximum in plot.imageMatrix", call.=FALSE)
      return()
    }  
    if( is.infinite(minimum) ){
      warning(" zlim: minimum is infinite in plot.imageMatrix", call.=FALSE)
      return()
    }
    if( is.infinite(maximum) ){
      warning(" zlim: maximum is infinite in plot.imageMatrix", call.=FALSE)
      return()
    }
    
  }


  if( ! is.matrix(x) ){
    warning(" class matrix is required in plot.imageMatrix", call.=FALSE)
    return()
  }

  nrOfRows <- dim(x)[1]       ## relates to length of y-axis
  nrOfColumns <- dim(x)[2]    ## relates to length of x-axis

  if( ! all(is.numeric(x)) ){
    cat(" matrix x is automatically coerced to numeric in plot.imageMatrix \n")
    x <- as.numeric(x)
    dim(x) <- c(nrOfRows,nrOfColumns)
  }
  

  if( !missing(labels) ){
    if( nrOfRows == nrOfColumns ){
      if( missing( xLabels ) ){
        xLabels <- labels
      }
      if( missing( yLabels ) ){
        yLabels <- labels
      }
    }else if( nrOfRows != nrOfColumns){
      warning(" labels are only useful together with quadratic x in plot.imageMatrix ", call.=FALSE)
      return()
    }
  }

  if( !missing(xLabels) ){
    if( length(xLabels) != nrOfColumns ){
      warning(paste(" xLabels must be of length ", nrOfColumns," (number of columns) instead of ", length(xLabels)), call.=FALSE)
      return()
    }
  }

  if( !missing(yLabels) ){
    if( length(yLabels) != nrOfRows ){
      warning(paste(" yLabels must be of length ", nrOfRows," (number of rows) instead of ", length(yLabels)), call.=FALSE)
      return()
    }
  }
  
  
  if( missing(xLabels) || all(is.null(xLabels)) || all(is.na(xLabels)) ){
    dn <- dimnames(x)
    if( !is.null(dn[[2]]) ){
      xLabels <- dn[[2]]
    }else{
      xLabels <- as.character(1:nrOfColumns)
    }
  }
  if( missing(yLabels) || all(is.null(yLabels)) || all(is.na(yLabels)) ){
    dn <- dimnames(x)
    if( !is.null(dn[[1]]) ){
      yLabels <- dn[[1]]
    }else{
      yLabels <- as.character(1:nrOfRows)
    }
  }
    
    
  if( !missing(labelMatrix) ){
    if( ! is.array(labelMatrix) ){
      warning("  labelMatrix must be of type array in plot.imageMatrix", call.=FALSE)
       return()
    }
    if( any(dim(labelMatrix) != dim(x)) ){
      warning("  labelMatrix and x must have same dimension in plot.imageMatrix", call.=FALSE)
      return()
    }
  }

  ## "mirror" y-axis
  if( reverseYaxis ){

    reverse <- nrOfRows:1
    yLabels <- yLabels[reverse]
    x <- x[reverse,,drop=FALSE]
    if( ! missing(labelMatrix) ){
      labelMatrix <- labelMatrix[reverse,,drop=FALSE]
    }
  }
  
    
  if( missing(colourRamp) ){
    colourRamp <- rgb(seq(0,1,l=256),seq(0,1,l=256),seq(1,0,l=256))
  }
  if( !missing(colourRamp) ){
    if( any(is.na(colourRamp))   | any(is.null(colourRamp)) ){
      colourRamp <- rgb(seq(0,1,l=256),seq(0,1,l=256),seq(1,0,l=256))
    }
  }

  localMinimum <- min(x, na.rm=TRUE)
  localMaximum <- max(x, na.rm=TRUE)

  if( is.infinite(localMinimum) | is.infinite(localMaximum) ){
    warning("data minimum or maximum is infinite in plot.imageMatrix",
            call.=FALSE)
    return()
  }
  
  if( !missing(zlim) ){

    if( ! (localMinimum >= minimum && localMaximum <= maximum ) ){
      warning("data are not in the range of (minimum,maximum) in plot.imageMatrix", call.=FALSE)
      return()
    }
      
    # global scale
    collevs = seq( minimum, maximum, length=length(colourRamp))
    grade <- length(colourRamp)
    gradeUnit <- (maximum - minimum)/grade
    gradeMin <- (localMinimum - minimum) %/% gradeUnit
    gradeMax <- (localMaximum - minimum) %/% gradeUnit
    if( is.nan(gradeMin) | is.nan(gradeMax) ){
      warning(" is.nan gradeMin or gradeMax in plot.imageMatrix ", call.=FALSE)
      return()
    }
    if( gradeMin < 0 | gradeMax < 0 ){
      warning(" gradeMin or gradeMax < 0 in plot.imageMatrix ", call.=FALSE)
      return()
    }
    if( gradeMin > length(colourRamp) | gradeMax > length(colourRamp) ){
      warning(" gradeMin or gradeMax > length(colourRamp) in plot.imageMatrix ", call.=FALSE)
      return()
    }
    localColourRamp <-  colourRamp[gradeMin:gradeMax]

  }else{

    # local scale
    collevs = seq(localMinimum, localMaximum, length=length(colourRamp))
    localColourRamp <- colourRamp

  }

  if( plotOutput == "standard" ){

  }else if( plotOutput == "screen" || plotOutput == "twoScreens" ){
    if( interactive() ){
      x11(width=width,height=height)
    }
  }else if( plotOutput == "pdf" ){
    pdf(width=width,height=height,file=fileName)
  }else if( plotOutput == "win.metafile" ){
    win.metafile(width=width,height=height,file=fileName)
  }else{
    stop(" unexpected case; unknown plotOutput in plot.imageMatrix ")
  }
  
  ## just trying to avoid trouble with Windows Screens ...
  if(  plotOutput %in% c("standard","twoScreens","screen") ){
    if( .Platform$OS.type == "windows" ){
      par(mar=c(3, 2.5, 2.5, 2))
    }
  }
  
  
  if( zScale ){
    if( ! separateZScale ){
      ## aspect = length(yLabels)/length(xLabels)
      layout(matrix(data = c(1,2), nrow=1, ncol=2), widths=c(4, 1))
      ## layout(matrix(data = c(1,2), nrow=1, ncol=2), widths=c(4, 1), heights=c(1,1))
    }
  }

  image(x=1:length(xLabels),
        y=1:length(yLabels),
        z=t(x),
        col=localColourRamp,
        axes=FALSE, ...)
  axis(1, 1:length(xLabels), xLabels, las=2)
  axis(2, 1:length(yLabels), yLabels, las=2)

  if( zScale ){
    if( separateZScale ){
      if( plotOutput == "extraScreen" ){
        if( interactive() ){
          x11(width=width,height=height)
        }
      }
    }
    if( collevs[1] < collevs[length(collevs)] ){
      image(x=1,
            y=collevs,
            z=matrix(data=collevs, ncol=length(collevs), nrow=1),
            col=colourRamp, xlab="", ylab="", xaxt="n" )
    }else{
      plot(0,type="n",axes=FALSE,xlab="",ylab="")
      cat(paste(" no colour bar in plot.imageMatrix: the minimum is not less than the maximum ( minimum:", collevs[1], " and maximum:", collevs[length(collevs)], ")\n",sep=""))
    }
  }
    
  if( ! missing( labelMatrix ) ){

    par(mfg=c(1,1))
    par(new=TRUE)
    plot(1,1, xlim=c(1-0.5,length(xLabels)+0.5), ylim=c(1-0.5,length(yLabels)+0.5) , type="n", xlab="", ylab="", axes=FALSE)
    labelMatrix <- t(labelMatrix)
    xx <- rep(1:nrow(labelMatrix),ncol(labelMatrix))
    xx <- ((ncol(labelMatrix)+0.5)/ncol(labelMatrix) )*xx - 0.375
    yy <- as.numeric(as.vector(gl(ncol(labelMatrix),nrow(labelMatrix))))
    yy <- ((nrow(labelMatrix)+0.5)/nrow(labelMatrix) )*yy - 0.375
    zz <- as.vector(labelMatrix)
    stopifnot( length(xx) == length(yy) & length(xx) == length(zz) )
    text(xx,yy,zz,cex=labelMatrixTextScaling, adj=c(0,0))

  }

  # reset layout
  if( zScale ){
    if( ! separateZScale ){
      layout(1)
    }
  }
  
  if( plotOutput == "pdf" || plotOutput == "win.metafile" ){
    dev.off()
  }
  
}# end of the function definition plot.imageMatrix

