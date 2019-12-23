## Idea (possible extension):
## use image and density instead of boxes
## to visualise the density inside the box

#
# /**
#
# \name{plotDistributions}
#
# \title{Visualisation of distributions}
#
# \alias{plotDistributions}
#
# @usage
#
# \keyword{hplot}
#
# \description{Boxplot like visualisation of distributions, only the boxes,
#   i.e. the median and the second and third quartile are plotted
#   (cf. \code{boxplot.stats} for the details on the calculation).
#   The plots may help to identify shortcomings of the raw data
#   or normalised data. The argument \code{quantiles} can be used
#   to visualize two or three arbitrary quantiles with boxes.}
#
# \value{The function is called for its side effect.}
#
#
# \arguments{
#  \item{dataMatrix}{numeric data matrix,
#                    where columns represent distributions, e.g.
#                    raw array data or normalised data;
#                    required; default missing.} 
#  \item{transFunc}{unary function; optional; default missing.
#                   Data transformation function, e.g. log}
#  \item{quantiles}{missing by default; a vector of two or
#          three increasing quantiles used to determine the boxes to be drawn}
#  \item{main}{plot title; type character string; optional; default missing}
#  \item{labels}{vector of names, may substitute column names
#                of \code{dataMatrix}; optional; default missing}
#  \item{xlab}{label for x axis; type character string; optional; default missing}
#  \item{ylab}{label for y axis; type character string; optional; default missing}
#  \item{colourVector}{vector of colours; optional; default missing}
#  \item{width}{graphics window width; required; default: 8}
#  \item{height}{graphics window height; required; default: 6}
#  \item{fileName}{optional; default: "plotDistributionsOutput"}
#  \item{savePath}{optional; default: missing}
#  \item{plotOutput}{character string specifying
#        either "standard", "screen", "pdf", "win.metafile";
#        default: "standard"}
#  \item{automaticSubtitle}{logical; required; default: \code{TRUE};
#                           The quantiles values are added
#                           as subtitle to the plot.}
#
# }
# 
# \details{Default of \code{transFunc} is no transformation, i.e. identity.
#          If \code{labels} are supplied at first the column names or
#          secondly a numbering  are used instead.
#          By default the \code{colourVector} is defined as alternating
#          "darkred" and "darkgreen".
#          A data matrix which only consists of \code{NA, NaN, Inf, -Inf}
#          will cause a cryptical error.}
#
# \seealso{\code{boxplot.stats}}
#
# \examples{
#      plotDistributions(cbind(rnorm(100),rnorm(100)),
#             main="Random Gaussians", labels = c("N1","N2"), ylab="scale")
#      plotDistributions(as.matrix(1:100), quantiles=c(0.25,0.85),
#             main = "Random Gaussians", labels = c("N1"), ylab = "scale")
#      plotDistributions(as.matrix(1:100), transFunc=log,
#             quantiles=c(0.1,0.95), main = "Random Gaussians",
#             labels = c("N1"), ylab = "scale")
#           }
# \author{Andreas Buness <a.buness@dkfz.de>}
# */
plotDistributions <- function(dataMatrix, transFunc, quantiles, main, labels, xlab, ylab, colourVector, width=8, height=6, fileName="plotDistributionsOutput", savePath, plotOutput="standard", automaticSubtitle=TRUE){

  if( ! is.logical(automaticSubtitle) ){
    cat(" automaticSubtitle must be of type logical in plotDistributions\n")
    return()
  }
  
  if( ! plotOutput %in% c("standard", "screen", "pdf", "win.metafile") ){
    cat(" unknown plotOutput in plotDistributions\n")
    return()
  }
  
  if( plotOutput == "win.metafile" ){
    if( .Platform$OS.type != "windows" ){
      stop( paste(plotOutput, " only valid if running under Windows in plotDistributions "), call.=FALSE )
    }
  }
  
  if( length(dim(dataMatrix)) != 2 ){
    cat(" dataMatrix must be two dimensional in plotDistributions \n")
    return()
  }

  if( !missing(savePath) ){
    fileName <- file.path(savePath, fileName)
  }
  
  #nrOfItems <- dim(dataMatrix)[1]
  nrOfDist <- dim(dataMatrix)[2]

  if( missing(main) ){
    main <- ""
  }
  
  if( missing(xlab) ){
    xlab <- ""
  }
  if( missing(ylab) ){
    ylab <- ""
  }
  
  if( missing(transFunc) ){
    transFunc <- function(x) x
  }
  
  if( missing(labels) ){
    dn <- dimnames(dataMatrix)
    if( !is.null(dn[[2]]) ){
      labels <- dn[[2]]
    }else{
      labels <- as.character(1:nrOfDist)
    }
  }

  if( missing(colourVector) ){
    colourVector <- rep(c("darkgreen","darkred"),(nrOfDist/2)+1)[1:nrOfDist]
  }

  if( !missing(quantiles) ){
    if( !is.vector(quantiles) ){
      cat("quantiles have to be of type vector in plotDistributions\n")
      return()
    }
    if( length(quantiles) > 3 | length(quantiles) < 2 ){
      cat("quantiles have to be of length two or three in plotDistributions\n")
      return()
    }
    if(! all( sort(quantiles) == quantiles) ){
      cat("quantiles must be increasing in plotDistributions\n")
      return()
    }
    if( min(quantiles) < 0 | max(quantiles) > 1 ){
      cat("quantiles must be in [0,1] in plotDistributions\n")
      return()
    }
  }
  
  BOXCONST <- 0.4 # i.e. 80% of the width is used for the box
  YMARGIN <- 0.1 # i.e. 20% are used for the margin

  if( plotOutput == "standard" ){
    
  }else if( plotOutput == "screen" ){
    if( interactive() ){
      x11(width=width,height=height)
    }
  }else if( plotOutput == "pdf" ){
    pdf(width=width,height=height,file=fileName)
  }else if( plotOutput == "win.metafile" ){
    win.metafile(width=width,height=height,file=fileName)
  }else{
    stop(" unexpected case; unknown plotOutput in plotDistributions ")
  }
  
  if( missing(quantiles) ){
    boxplotOut <- boxplot(transFunc(dataMatrix) ~ col(dataMatrix), axes=FALSE, plot=FALSE);
    boxStatistics <- boxplotOut$stats
    ## roughly first quartile, median and third quartile
    ## cf. help(boxplot.stats); possibly use instead directly fivenum and bxp
    quantileMatrix <- boxStatistics[c(2,3,4),,drop=FALSE]
    dimnames(quantileMatrix) <- list(c("lower","middle","upper"),NULL)
    quantiles <- c(0.25,0.5,0.75)
    names(quantiles) <- c("lower","middle","upper")
  }else{
    quantileMatrix <- as.matrix(apply(transFunc(dataMatrix),2,function(x) quantile(x, probs=quantiles)))
    if( length(quantiles) == 2 ){
      dimnames(quantileMatrix) <- list(c("lower","upper"),NULL)
      names(quantiles) <- c("lower","upper")
    }else if( length(quantiles) == 3 ){
      dimnames(quantileMatrix) <- list(c("lower","middle","upper"),NULL)
      names(quantiles) <- c("lower","middle","upper")
    }else{
      stop("unexpected case in plotDistributions")
    }
  }

  tmpRange <- range(quantileMatrix, na.rm=TRUE, finite=TRUE)
  minY <- tmpRange[1]
  maxY <- tmpRange[2]
  yRange <- maxY-minY
  minY <- minY - (YMARGIN*abs(yRange))
  maxY <- maxY + (YMARGIN*abs(yRange))
  plot(x=c(0.5,dim(quantileMatrix)[2]+0.5), y=c(minY,maxY),type="n", axes=FALSE, xlab="",ylab="")
  for(i in 1:dim(quantileMatrix)[2]){
    rect(i-BOXCONST, quantileMatrix["lower",i],
         i+BOXCONST,quantileMatrix["upper",i],
         col=colourVector[i])
    if( dim(quantileMatrix)[1] == 3 ){
      lines(x=c(i-BOXCONST, i+BOXCONST),
            y=c(quantileMatrix["middle",i],
                quantileMatrix["middle",i]),
            lwd=3)
    }
  }
  axis(2)
  title(main=main , sub=NULL, xlab=xlab, ylab=ylab)
  axis(1,at=1:ncol(dataMatrix),labels=labels)
  if( automaticSubtitle ){
    mtext( paste("quantiles: ", paste(paste(names(quantiles),quantiles, sep=":"),collapse="; "))) 
  }
  if( plotOutput == "pdf" || plotOutput == "win.metafile" ){
    dev.off()
  }
  
}# end of function definition
