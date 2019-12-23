# /**
#
# \name{visualiseQualityParameters}
#
# \title{Graphical representation of quality parameters}
#
# \alias{visualiseQualityParameters}
#
# \description{Graphical representation of quality parameters}
#
# \value{A graphical representation of quality parameters.
#        \code{\link{qualityDiagnostics}} does call
#        \code{visualiseQualityParameters} }
#
# @usage
#
# \arguments{
#  \item{qualityParameters}{\code{data.frame}; required; 
#                           contained in the return value of
#                           \code{\link{qualityParameters}} }
#  \item{fileName}{character string; required;
#                  default: "visualiseQualityParametersOutput.pdf" }
#  \item{savePath}{character string; optional; default: missing}
#  \item{width}{graphics window width}
#  \item{height}{graphics window height}
#  \item{plotOutput}{character string; required; either "screen", "pdf"
#                    or "win.metafile"; default: "pdf"}
# }
# 
# \details{For details on the specific parameters
#          see \code{\link{qualityParameters}}.
#          Lines are only used for easier
#          detection of outliers. The ordering
#          of the hybridisation is somehow arbitrary,
#          and only reflects the (initial) ordering. 
#          The "correlation" line for example
#          graphically represents the correlation
#          coefficients calculated between the
#          green and red channel for each hybridisation.
#          The horizontal blue line is drawn
#          at the height of one. The function
#          \code{\link{qualityDiagnostics}} calls
#          \code{visualiseQualityParameters}.
#         }
#
# \seealso{\code{\link{qualityParameters}};
#          \code{\link{qualityDiagnostics}} }
#
# \examples{
#	}
#
# \keyword{hplot} ## required
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

visualiseQualityParameters <- function(qualityParameters,
                                       savePath,
                                       fileName="visualiseQualityParametersOutput.pdf",
                                       width=25,
                                       height=25,
                                       plotOutput="pdf"){

  if( ! plotOutput %in% c("pdf", "screen", "win.metafile") ){
    cat(" unknown plotOutput in visualiseQualityParameters\n")
    return()
  }

  if( plotOutput == "win.metafile" ){
    if( .Platform$OS.type != "windows" ){
      stop( paste(plotOutput, " only valid if running under Windows in qualityDiagnostics "), call.=FALSE )
    }
  }

  
  if( ! missing(savePath) ){
    fileName<-file.path(savePath, fileName)
  }else{
    fileName<-file.path( fileName)
  }
    
  changeScale <- function(x,minimum,maximum){
    if( missing(minimum) ){
      minimum <- min(x)
    }
    if( missing(maximum) ){
      maximum <- max(x)
    }
    return((x-minimum) / (maximum-minimum))
  }

  if( plotOutput == "screen" ){
    if( interactive() ){
      x11(width=width,height=height)
    }
    if( .Platform$OS.type == "windows" ){
      par(mar=c(3, 2.5, 2.5, 2))
    }
  }else if( plotOutput == "pdf" ){
    pdf(width=width,height=height,file=fileName)
  }else if( plotOutput == "win.metafile" ){
    if( .Platform$OS.type == "windows" ){
      win.metafile(width=width,height=height,file=fileName)
    }else{
      stop( paste(plotOutput, " only valid if running under Windows in visualiseQualityParameters "), call.=FALSE )
    }
  }else{
    stop(" unexpected case; unknown plotOutput argument in visualiseQualityParameters ", call.=FALSE)
  }


  layout(matrix(data=c(1,1,2,3,4,5,6),nrow=7,ncol=1))
  hLabel <- qualityParameters$hybridisation
  hL <- length(hLabel)  
  charSymbols <- rep(c("1","2","3","4","5","6","7","8","9","0"), (hL/10)+1)[1:hL]
  
  MEANSIGNAL <- FALSE
  if( MEANSIGNAL ){
    i <- qualityParameters$meanSignal
    iGreen<- qualityParameters$meanSignalGreen
    iRed<- qualityParameters$meanSignalRed
    iGreen[ is.infinite(iGreen) ] <- NA
    iRed[ is.infinite(iRed) ] <- NA
    if( ! (all(is.na(iGreen)) | all(is.na(iRed)))  ){
      mi <- min(c(i,iGreen,iRed), na.rm=TRUE)
      ma <- max(c(i,iGreen,iRed), na.rm=TRUE)
      plot(1:hL, main="mean signal intensity",ylim=c(0,1),changeScale(i,mi,ma), axes=FALSE, pch=charSymbols, col="black", xlab="", ylab="bad - good") 
      lines(1:hL,changeScale(iGreen,mi,ma), lwd=2, col="darkgreen", pch=charSymbols)
      lines(1:hL,changeScale(iRed,mi,ma), lwd=2, col="darkred", pch=charSymbols)
      axis(1)
    }else{
      plot(0, main=title, type="n", axes=FALSE, ann=FALSE)
    }
  }

  ## 1,1
  title <- "signal: dynamic range"
  rangeGreen <-  qualityParameters[["signalRange.Green"]]
  rangeRed <-  qualityParameters[["signalRange.Red"]]
  rangeGreen[ is.infinite(rangeGreen) ] <- NA
  rangeRed[ is.infinite(rangeRed) ] <- NA
  if( ! (all(is.na(rangeGreen)) | all(is.na(rangeRed)))  ){
    mi <- min(c(rangeGreen, rangeRed), na.rm=TRUE)
    ma <- max(c(rangeGreen, rangeRed), na.rm=TRUE)
    plot(1:hL, main=title,ylim=c(0,1),changeScale(rangeGreen,mi,ma), axes=FALSE, type="b", lwd=2, col="darkgreen", xlab="", ylab="bad - good", pch=charSymbols) 
    lines(1:hL,changeScale(rangeRed,mi,ma), type="b", lwd=2, col="darkred", pch=charSymbols)
    axis(1)
  }else{
    plot(0, main=title, type="n", axes=FALSE, ann=FALSE)
  }
    
  
  ## 2
  title <- "background: dynamic range"
  rangeBackGreen <-  qualityParameters[["backgroundRange.Green"]]
  rangeBackRed <-  qualityParameters[["backgroundRange.Red"]]
  rangeBackGreen[ is.infinite(rangeBackGreen) ] <- NA
  rangeBackRed[ is.infinite(rangeBackRed) ] <- NA
  if( ! (all(is.na(rangeBackGreen)) | all(is.na(rangeBackRed)))  ){
    mi <- min(c(rangeBackGreen, rangeBackRed), na.rm=TRUE)
    ma <- max(c(rangeBackGreen, rangeBackRed), na.rm=TRUE)
    plot(1:hL, main=title,ylim=c(0,1),changeScale(rangeBackGreen,mi,ma), axes=FALSE, type="b", lwd=2, col="darkgreen", xlab="", ylab="good - bad", pch=charSymbols) 
    lines(1:hL,changeScale(rangeBackRed,mi,ma), type="b", lwd=2, col="darkred", pch=charSymbols)
  }else{
    plot(0, main=title, type="n", axes=FALSE, ann=FALSE)
  }

  ## 3
  title <- "signal to background ratio"
  sngreen <- qualityParameters[["signalToBackground.Green"]]
  snred <- qualityParameters[["signalToBackground.Red"]]
  sngreen[ is.infinite(sngreen) ] <- NA
  snred[ is.infinite(snred) ] <- NA
  if( ! (all(is.na(sngreen)) | all(is.na(snred)))  ){
    mi <- min(c(sngreen, snred), na.rm=TRUE)
    ma <- max(c(sngreen, snred), na.rm=TRUE)
    plot(1:hL, main=title,ylim=c(0,1),changeScale(sngreen,mi,ma), axes=FALSE, type="b", lwd=2, col="darkgreen", xlab="", ylab="bad - good", pch=charSymbols) 
    lines(1:hL,changeScale(snred,mi,ma), type="b", lwd=2, col="darkred", pch=charSymbols)
  }else{
    plot(0,main=title, type="n", axes=FALSE, ann=FALSE)
  }
    
  ## 4
  w <- qualityParameters$width
  plot(1:hL, type="b", main="width",ylim=c(0,1),changeScale(w), axes=FALSE, col="black", xlab="", ylab="good - bad", pch=charSymbols) 

  ## 5
  d <- qualityParameters$medianDistance
  plot(1:hL, type="b", main="medianDistance",ylim=c(0,1),changeScale(d), axes=FALSE, col="black", xlab="", ylab="good - bad", pch=charSymbols) 

  ## 6
  c <- qualityParameters$correlation
  plot(1:hL, type="b", main="correlation",ylim=c(0,1),c, axes=FALSE, col="black", xlab="", ylab="bad - good", pch=charSymbols) 
  abline(1,0,col="blue")
  tickPoints <- axTicks(side=1)
  ## or alternatively
  ## tickPoints <- seq(par("xaxp")[1], par("xaxp")[2], length=(par("xaxp")[3]+1))
  if( min(tickPoints) == 0 ){
    tickPoints <- tickPoints[-1]
  }

  if(  min(tickPoints)    >= 1  &&
       max(tickPoints)    <= hL &&
       length(tickPoints) <= hL &&
       all(is.integer(tickPoints))  ){
    axis(side=1, at=tickPoints, labels=hLabel[tickPoints])
  }else{
    axis(side=1)
  }

  layout(1)  
  if( plotOutput == "pdf" || plotOutput == "win.metafile" ){
    dev.off()
  }

  
}
