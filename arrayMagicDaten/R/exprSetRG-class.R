## Recall: possibly cf. AffyBatch as an example for an extension
##         of the class exprSet  
##

## Note: set the description after a new("exprSetRG", ...) call via
##       description(newObject) <- description(oldObject)
##       since this is not supported by the initialize method 


## Note: the se.exprs() assignment function can be used to
##       set matrixes of wrong dimensions or to spoil the
##       implicit red/green ordering ...

## se.exprs: for variance/standard deviation pooling: cf. the t-test

##Recall: do not put any comments in the Rd-section below
##not allowed, since not part of the class:
##\alias{cbind} ## cf. help for cbind.exprSetRG
##\alias{cbind,exprSetRG,exprSetRG-method} ## cf. help for cbind.exprSetRG
##\alias{as.RGList} already exists in arrayData


# /**
#
#
#\name{exprSetRG-class}
#\docType{class}
#
#\alias{exprSetRG-class}
#\alias{exprSetRG}
#\alias{as.exprSet}
#\alias{getExprSetGreen}
#\alias{getExprSetGreenMinusRed}
#\alias{getExprSetRedMinusGreen}
#\alias{getExprSetRed}
#\alias{getExprSetLogRatio}
#\alias{getIndGreen}
#\alias{getIndRed}
#\alias{pDataGreen}
#\alias{pDataRed}
#\alias{pDataSlide}
#\alias{phenoDataGreen}
#\alias{phenoDataRed}
#\alias{phenoDataSlide}
#\alias{slideSubset}
#
#\alias{[,exprSetRG-method}
#\alias{as.exprSet,exprSetRG-method}
#\alias{as.RGList,exprSetRG-method}
#\alias{getExprSetGreen,exprSetRG-method}
#\alias{getExprSetGreenMinusRed,exprSetRG-method}
#\alias{getExprSetRedMinusGreen,exprSetRG-method}
#\alias{getExprSetRed,exprSetRG-method}
#\alias{getExprSetLogRatio,exprSetRG-method}
#\alias{getIndGreen,exprSetRG-method}
#\alias{getIndRed,exprSetRG-method}
#\alias{initialize,exprSetRG-method}
#\alias{pDataGreen,exprSetRG-method}
#\alias{pDataRed,exprSetRG-method}
#\alias{pDataSlide,exprSetRG-method}
#\alias{phenoDataGreen,exprSetRG-method}
#\alias{phenoDataRed,exprSetRG-method}
#\alias{phenoDataSlide,exprSetRG-method}
#\alias{show,exprSetRG-method}
#\alias{slideSubset,exprSetRG-method}
#
#
#
# \title{Class exprSetRG for two colour DNA microarray data
#        (extension of exprSet)} 
#
# \description{ This is a class representation for two colour
#               DNA microarray data. The class is based on the
#               class \code{exprSet} of the \code{Biobase} package.
#               The red and green channels  are stored in a
#               single \code{exprSet} object. The information on the
#               corresponding red-green pairs is stored separately.
#               Several class methods offer a convienent way
#               to access and set data.
#               }
#
#
#\section{Creating Objects}{
#\code{  new('exprSetRG',}\cr
#\code{    channels = ..., # object of class matrix with columns "green" and "red" }\cr
#\code{    exprs    = ..., # object of class matrix}\cr
#\code{    se.exprs = ..., # object of class matrix}\cr
#\code{    phenoData= ..., # object of class phenoData}\cr
#\code{    notes    = ..., # object of class character}\cr
#\code{    annotation    = ..., # object of class character}\cr
#\code{  )}}
#
#\section{Slots}{
#  \describe{
#    \item{\code{indGreen}:}{Object of class \code{vector}; indexes of the
#      green channel}
#    \item{\code{indRed}:}{Object of class \code{vector}; indexes of the
#      red channel}
#    \item{\code{exprs}:}{Object of class \code{matrix}; the observed
#      expression levels. This is a matrix with columns representing
#      the red and green channels and rows representing genes. Each row
#      in \code{channels} lists the indexes of the corresponding
#      pair of red and green channels representing a single
#      microarray slide.}
#    \item{\code{se.exprs}:}{Object of class \code{matrix}; this is
#      a matrix of the same dimensions as \code{exprs}, e.g. useful to
#      represent the standard error estimates for the corresponding
#      expression levels. }
#    \item{\code{phenoData}:}{Object of class \code{phenoData}, i.e.
#      an instance of class \code{phenoData} 
#      containing annotation information on the individual channels.
#      The columns of the pData slot of this entity represent
#      variables and the rows represent channels. }
#    \item{\code{notes}:}{Object of class \code{character} containing
#      explanatory text; default: ""}
#    \item{\code{annotation}:}{Object of class \code{character};
#      default: ""}
#  }
#}
#
#\section{Extends}{
#Class \code{exprSet}, directly.
#}
#
#
# \section{Methods}{
#  \describe{
#
#    \item{show}{(exprSetRG):
#      renders information about the exprSetRG
#      in a concise way on stdout, cf. class exprSet.}
#    
#    \item{getExprSetLogRatio}{(exprSetRG, seExprsHandling):
#      Returns an \code{exprSet}
#      object of the difference of the expression levels, i.e. the green
#      channel minus the red channel. 
#      The \code{phenoData} slot it the result
#      of calling the class method \code{phenoDataSlide}
#      on the object \code{exprSetRG}.
#      The \code{se.exprs} slot contains
#      the root-mean-square or the mean of the se.exprs of both channels
#      depending on the argument \code{seExprsHandling}.
#      The root-mean-square might be useful if the two
#      se.exprs values are estimated standard deviations
#      based on the same number observations and identical
#      distribution. \code{seExprsHandling} must be a character string;
#      possible values are "rootMeanSquare" or "mean";
#      the default value is "rootMeanSquare". }
#
#    \item{getExprSetGreenMinusRed}{ same as method
#      \code{getExprSetLogRatio} }
#
#    \item{getExprSetRedMinusGreen}{ similar to method
#      \code{getExprSetLogRatio}; the "negative" difference
#      of the expression levels is returned, i.e. the red
#      channel minus the green channel.
#      }
#
#    \item{getExprSetGreen}{(exprSetRG): Returns an \code{exprSet}
#      object which contains the expression levels of the green channel
#      and the corresponding annotation.}
#    
#    \item{getExprSetRed}{(exprSetRG): Returns an \code{exprSet}
#      object which contains the expression levels of the red channel
#      and the corresponding annotation.}
#
#    \item{phenoDataSlide}{(exprSetRG): Returns an \code{phenoData}
#      object characterizing all microarray slides.
#      Those annotation information, which is the same for both channels
#      (and not \code{NA}) is taken directly, e.g. the
#      the slide number. All other annotation variables are added
#      specifically for each channel, i.e. prefixed with
#      "greenSpecific\_" and "redSpecific\_".
#      Do not use varLabels-names for subsetting.}
#      
#    \item{pDataSlide}{(exprSetRG): Returns an \code{pData}
#      object, i.e. \code{pData(phenoDataSlide(exprSetRG))};
#      cf. \code{phenoDataSlide}.
#      Do not use varLabels-names for subsetting.}
#
#    \item{phenoDataGreen}{(exprSetRG): Returns an \code{phenoData}
#      object of the annotation information given for the green channel.
#      Do not use varLabels-names for subsetting.}
#      
#    \item{pDataGreen}{(exprSetRG): Returns the \code{pData}
#      object taken out of the result of calling \code{phenoDataGreen}.
#      Do not use varLabels-names for subsetting.}
#
#    \item{phenoDataRed}{(exprSetRG): Returns an \code{phenoData}
#      object of the annotation information given for the red channel.
#      Do not use varLabels-names for subsetting.}
#      
#    \item{pDataRed}{(exprSetRG): Returns the \code{pData}
#      object taken out of the result of calling \code{phenoDataRed}.
#      Do not use varLabels-names for subsetting.}
#
#    \item{slideSubset}{(exprSetRG,i,j): Subsetting operation; where
#      i corresponds to the rows  and j corresponds to the
#      microarray slides.
#      j is given by indexes or logicals related to the order
#      of the channel pairs in \code{channels};
#      cf. the constructor slot \code{channels}.}
#
#    \item{getIndGreen}{(exprSetRG): An accessor function for slot
#      \code{indGreen}. The corresponding elements of indGreen and indRed
#      define microarray slides.}
#
#    \item{getIndRed}{(exprSetRG): An accessor function for slot
#      \code{indRed}. The corresponding elements of indGreen and indRed
#      define microarray slides.}
#
#    \item{[}{(exprSetRG,i,j,type):
#      A subset operator.
#      Ensures that both the data and the annotation
#      information (\code{phenoData}) are subseted properly.
#      This may only mix up the pairing of the channels,
#      i.e. the validity of an exprSetRG,
#      if you use argument option type == "invalidExprSetRG".
#      Default: "validExprSetRG". See also: \code{slideSubset}.}
#
#    \item{cbind}{(...): Concatenates \code{exprSetRG} objects.
#                        Genes (rows) are assumed to match;
#                        cf. \code{\link{cbind.exprSetRG}} }
#
#    \item{as.exprSet}{(exprSetRG): Class cast, returns an object of
#      \code{exprSet}, the information on the red and green channels
#      is discarded.}
#
#    \item{as.RGList}{(exprSetRG, func): Returns an object of
#      \code{RGList} which in general refers to raw data.
#      Hence, func(expression values) is returned, i.e.
#      \code{exp(x)} by default.
#      All background values in the RGList object are
#      set to zero.}
#
#  }
#}
# 
# \keyword{classes}
# \keyword{methods}
#
# \examples{
#  indGreen=1:3
#  indRed=4:6
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  eSA <- new("exprSetRG", exprs=matrix(1:60, ncol=6, nrow=10), phenoData=
#          new("phenoData", pData=data.frame(matrix(0,nrow=6,ncol=1)),
#              varLabels=list(rep("varLabel1",1))), channels=channels)
#  stopifnot( all(pDataSlide(eSA) ==pData(eSA)[1:3,,drop=FALSE]) )
#  eSAGreen <- getExprSetGreen(eSA)
#  eSARed <- getExprSetRed(eSA)
#  eSALogRatio <- getExprSetLogRatio(eSA)
#  eSALogRatio2 <- getExprSetGreenMinusRed(eSA)
#  eSALogRatio3 <- getExprSetRedMinusGreen(eSA)
#  stopifnot( identical( eSALogRatio, eSALogRatio2 ) )
#  stopifnot( identical( exprs(eSALogRatio), exprs(eSAGreen)-exprs(eSARed)) )
#  stopifnot( all.equal.numeric( as.vector(exprs(eSALogRatio3)), as.vector(exprs(eSARed)-exprs(eSAGreen)) ))
#  eSAPart <- eSA[,c(1,3,1,4,6,4)]
#  eSAInvalid <- eSA[,c(1,3,1,5,6,4),type="invalidExprSetRG"]
#  eSAPart2 <- slideSubset(eSA,j=c(1,3,1))
#
#  eSAeSA <- cbind(eSA, eSA)
#  eSAeSAPart2 <- cbind(eSA, eSAPart2)
#  stopifnot( class(as.exprSet(eSA)) == "exprSet" )
#
#  \dontshow{
#  rg <- as.RGList(eSA)
#  stopifnot( identical( exprs(eSAPart), exprs(eSAPart2) ) )
#  ind <- c(1,1,3,2,3,3)
#  stopifnot( identical( exprs(eSALogRatio[,ind]), exprs(eSAGreen[,ind])-exprs(eSARed[,ind])) )
#  stopifnot( dim(exprs(eSALogRatio[,ind]))[2] == length(ind) )
#  eSAPart <- eSA[,1:3, type="invalidExprSetRG"] # not recommended
#  stopifnot(identical(eSAGreen, getExprSetGreen(eSAPart)))
#  #stopifnot(identical(eSAGreen[,c(1,1,1)], getExprSetGreen(eSAPart[,c(1,1,1),type="invalidExprSetRG"])))
#  stopifnot(all.equal.numeric(as.vector(exprs(eSAGreen[,c(1,1,1)])), as.vector(exprs(getExprSetGreen(eSAPart[,c(1,1,1),type="invalidExprSetRG"])))))
#  #stopifnot(identical(eSAGreen[,c(TRUE,TRUE,TRUE)], getExprSetGreen(eSAPart[,c(TRUE,TRUE,TRUE),type="invalidExprSetRG"])))
#  stopifnot(all.equal.numeric(exprs(eSAGreen[,c(TRUE,TRUE,TRUE)]), exprs(getExprSetGreen(eSAPart[,c(TRUE,TRUE,TRUE),type="invalidExprSetRG"]))))
#  #stopifnot(identical(eSAGreen[,c(FALSE,FALSE,FALSE)], getExprSetGreen(eSAPart[,c(FALSE,FALSE,FALSE), type="invalidExprSetRG"])))
#  stopifnot(all.equal.numeric(exprs(eSAGreen[,c(FALSE,FALSE,FALSE)]), exprs(getExprSetGreen(eSAPart[,c(FALSE,FALSE,FALSE), type="invalidExprSetRG"]))))
#  #stopifnot(identical(eSAGreen[,c(FALSE,TRUE,FALSE)], getExprSetGreen(eSAPart[,c(FALSE,TRUE,FALSE), type="invalidExprSetRG"])))
#  stopifnot(all.equal.numeric(exprs(eSAGreen[,c(FALSE,TRUE,FALSE)]), exprs(getExprSetGreen(eSAPart[,c(FALSE,TRUE,FALSE), type="invalidExprSetRG"]))))
#  print(eSA)
#  print(phenoDataSlide(eSA))
#  print(pDataSlide(eSA))
#  print(phenoDataGreen(eSA))
#  print(pDataGreen(eSA))
#  print(phenoDataRed(eSA))
#  print(pDataRed(eSA))
#
#  indGreen <- c(2,4); indRed <- c(1,3)
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  myPData <- data.frame(commonOne=c(1,1,2,2), first=c(1,2,3,4))
#  myPDataTwo <- data.frame(commonOne=c(1,1,2,2), second=c("a","b","a","b"))
#  myPDataThree <- data.frame(commonOne=c(1,1,2,2))
#  myPhenoData <- new("phenoData", pData=myPData, varLabels=as.list(colnames(myPData)))
#  myPhenoDataTwo <- new("phenoData", pData=myPDataTwo, varLabels=as.list(colnames(myPDataTwo)))
#  myPhenoDataThree <- new("phenoData", pData=myPDataThree, varLabels=as.list(colnames(myPDataThree)))
#  myMatrix <- cbind(c(1,1,1,1),c(2,2,2,2),c(3,3,3,3),c(4,4,4,4))
#  myExprSetRG <- new("exprSetRG", exprs=myMatrix, se.exprs=myMatrix, phenoData=myPhenoData, channels=channels)
#  myExprSetRGTwo <- new("exprSetRG", exprs=myMatrix, se.exprs=myMatrix, phenoData=myPhenoDataTwo, channels=channels)
#  myExprSetRGThree <- new("exprSetRG", exprs=myMatrix, se.exprs=myMatrix, phenoData=myPhenoDataThree, channels=channels)
#  stopifnot( all.equal.numeric(as.vector(se.exprs(getExprSetGreen(myExprSetRG))), as.vector(cbind(c(2,2,2,2),c(4,4,4,4)))))
#  stopifnot( all.equal.numeric(as.vector(se.exprs(getExprSetRed(myExprSetRG))),as.vector( cbind(c(1,1,1,1),c(3,3,3,3)))))
#  stopifnot( all.equal.numeric(as.vector(se.exprs(getExprSetLogRatio(myExprSetRG, seExprsHandling="mean"))), as.vector(0.5*cbind(c(3,3,3,3),c(7,7,7,7)))))
#  stopifnot( all.equal.numeric(as.vector(se.exprs(myExprSetRG[2,])) , c(1,2,3,4)) )
#  stopifnot( all.equal.numeric(as.vector(se.exprs(myExprSetRG[2,3:4])) , c(3,4)) )
#  stopifnot( all.equal.numeric( as.vector(se.exprs(cbind(myExprSetRG,myExprSetRG))), as.vector(cbind(se.exprs(myExprSetRG),se.exprs(myExprSetRG))) ))
#  stopifnot( all.equal.numeric(as.vector(exprs(getExprSetLogRatio(myExprSetRG))), as.vector(matrix(1,ncol=2,nrow=4))))
#  cTwo <- cbind(myExprSetRG, myExprSetRGTwo)
#  cThree <- cbind(myExprSetRG, myExprSetRGTwo, myExprSetRGThree)
#  cTwob <- cbind(myExprSetRG, myExprSetRGThree)
#  # match corresponds to \%in\% , i.e. %in%
#  stopifnot( all( match(varLabels(cTwo),  c("commonOne", "first", "second")) ) ) 
#  stopifnot( all( match(varLabels(cTwob),c("commonOne", "first")) ) )
#  stopifnot( all( match(varLabels(cThree), c("commonOne", "first", "second")) ) )
#  stopifnot( all( pData(cTwo)[,"commonOne"] ==  pData(cTwob)[,"commonOne"] ) )
#
#  indGreen <- c(2,4,6); indRed <- c(1,3,5)
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  myPData <- data.frame(commonOne=c(1,1,2,2,3,3), commonTwo=c(1,1,1,1,2,2), redOne=c(1,NA,2,NA,3,NA), redTwo=c(1,-1,2,-1,3,-1), greenOne=c(NA,10,NA,20,NA,30), greenTwo=c(-10,10,-10,20,-10,30))
#  myPhenoData <- new("phenoData", pData=myPData, varLabels=as.list(colnames(myPData)))
#  myExprSetRG <- new("exprSetRG", exprs=matrix(1,nrow=10,ncol=6), phenoData=myPhenoData, channels=channels)
#  stopifnot( all( myPData[indGreen,] == pDataGreen(myExprSetRG) ,na.rm=TRUE) )
#  stopifnot( all( myPData[indGreen,] == pDataGreen(myExprSetRG) ,na.rm=TRUE) )
#  stopifnot( identical(varLabels(myPhenoData[indRed, ]), varLabels(phenoDataRed(myExprSetRG))) )
#  stopifnot( identical( varLabels(myPhenoData[indGreen,]), varLabels(phenoDataGreen(myExprSetRG))) )
# stopifnot(all(pData(myPhenoData[indRed,]) == pData(phenoDataRed(myExprSetRG)), na.rm=TRUE))
# stopifnot(all(pData(myPhenoData[indGreen,]) == pData(phenoDataGreen(myExprSetRG)), na.rm=TRUE))
#  stopifnot( all( pDataSlide(myExprSetRG)[,c("commonOne","commonTwo")] == pDataRed(myExprSetRG)[,c("commonOne","commonTwo")] ) )
#  stopifnot( all( pDataSlide(myExprSetRG)[,c("commonOne","commonTwo")] == pDataGreen(myExprSetRG)[,c("commonOne","commonTwo")] ) )
#  stopifnot( all( pDataSlide(myExprSetRG)[,c("greenSpecific_greenOne","greenSpecific_greenTwo")] == pDataGreen(myExprSetRG)[,c("greenOne","greenTwo")] ) )
#  stopifnot( all( pDataSlide(myExprSetRG)[,c("redSpecific_redOne","redSpecific_redTwo")] == pDataRed(myExprSetRG)[,c("redOne","redTwo")] ) )
#  stopifnot( all( pDataSlide(myExprSetRG)[,c("greenSpecific_redTwo")] == pDataGreen(myExprSetRG)[,c("redTwo")] ) )
#  stopifnot( all( pDataSlide(myExprSetRG)[,c("redSpecific_greenTwo")] == pDataRed(myExprSetRG)[,c("greenTwo")] ) )
#  }
# }
#
#  \seealso{ \code{exprSet-class},
#            \code{\link{arrayData-class}}
#          }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

## Note:
## no link found for exprSet-class

setClass(Class="exprSetRG", representation(
           indGreen="vector", indRed="vector"
           ),
         prototype=list( indGreen=numeric(), indRed=numeric()), 
         contains="exprSet")

## the method inititialize seem to have to set the slots ... 
setMethod("initialize", signature=signature(.Object="exprSetRG"), definition=
function(.Object,
         channels=array(0, dim=c(0,2), dimnames=list(NULL,c("green","red"))),
         exprs=matrix(nr=0,nc=0), phenoData=new("phenoData"),
         se.exprs = matrix(nr=0,nc=0), notes="", annotation="") {
  ## channels
  stopifnot( ! any(is.null(channels)) )
  stopifnot( ! any(is.na(channels)) )
  stopifnot( ! any(is.nan(channels)) )
  stopifnot( length(dim(channels)) == 2 ) ## two dimensions
  stopifnot( "green" %in% colnames(channels) )
  stopifnot( "red" %in% colnames(channels) )
  indGreen <- channels[,"green"]
  indRed <- channels[,"red"]
  
  ## setting of slots
  .Object@exprs <- exprs
  .Object@se.exprs <- se.exprs
  .Object@notes <- notes
  .Object@phenoData <- phenoData
  .Object@indGreen <- indGreen
  .Object@indRed <- indRed
  .Object@annotation <- annotation

  ## do not allow NA or NULL for annotation
  if( is.null(annotation) ){
    stop(" NULL is not valid value for annotation in exprSetRG-class ", call.=FALSE)
  }
  if( is.na(annotation) ){
    stop(" NA is not valid value for annotation in exprSetRG-class ", call.=FALSE)
  }
  
  ## do not allow NA
  stopifnot( ! any(is.na(indGreen)) )
  stopifnot( ! any(is.na(indRed)) )
  
  ## actually NULL is impossible since is.vector(NULL) is FALSE, cf.
  ## the representation definition which is checked at initialization
  if(! is.null(getIndGreen(.Object)) & ! is.null(getIndRed(.Object)) &
     ! is.null(exprs(.Object)) ){
    stopifnot(length(getIndGreen(.Object)) == length(getIndRed(.Object)))
    stopifnot(length(getIndGreen(.Object)) + length(getIndRed(.Object)) ==
              ncol(exprs(.Object)) )
    if( !(length(getIndGreen(.Object)) == 0 & length(getIndRed(.Object)) == 0 )){
      stopifnot( any(!duplicated(getIndGreen(.Object))) )
      stopifnot( any(!duplicated(getIndRed(.Object))) )
        
      stopifnot( setequal((1:ncol(exprs(.Object))),union(getIndGreen(.Object),
                                                         getIndRed(.Object))))
    }
    if(! is.null(exprs(.Object)) &
       ! is.null(pData(.Object))
       ){
      stopifnot( ncol(exprs(.Object)) ==  nrow(pData(.Object)) )
    }
  }
  return(.Object)
})


## other possible approach via combine as in eset.R
## did not suceed to define cbind as S4 method ...
cbind.exprSetRG <- function(...) {

  dataList <- list(...)
  objectOne <- dataList[[1]]
  annotationInfo <- annotation(objectOne)
  descriptionInfo <- description(objectOne)
  
  for(objectTwo in dataList[2:length(dataList)]){

    if( ! identical(descriptionInfo, description(objectTwo)) ){
      stop(" description(exprSetRG) information differs in cbind.exprSetRG ", call.=FALSE)
    }
    
    if( annotationInfo != annotation(objectTwo) ){
      stop(" annotation(exprSetRG) information differs in cbind.exprSetRG ", call.=FALSE)
    }
    
#     if( length(is.na(objectOne)) > 0 ){
#       if( all(is.na(objectOne)) ){
#         stop(" objectOne  is NA in cbind.exprSetRG ", call.=FALSE)
#       }
#     }
    
#     if( length(is.na(objectTwo)) > 0 ){
#       if( all(is.na(objectTwo)) ){
#         stop(" objectTwo  is NA in cbind.exprSetRG ", call.=FALSE)
#       }
#     }

    if( is.null(objectOne) | is.null(objectTwo) ){
      stop(" objectOne or objectTwo is NULL in cbind.exprSetRG ", call.=FALSE)
    }
  
    vLOne <- varLabels(phenoData(objectOne))
    vLTwo <- varLabels(phenoData(objectTwo))
    
    if( is.null(vLOne) | is.null(vLTwo) ){
      stop(" varLabels is NULL in cbind.exprSetRG ", call.=FALSE)
    }
    if( length(is.na(vLOne)) > 0 ){
      if( all(is.na(vLOne)) ){
        stop(" varLabels vLOne is NA in cbind.exprSetRG ", call.=FALSE)
      }
  }
    if( length(is.na(vLTwo)) > 0 ){
      if( all(is.na(vLTwo)) ){
        stop(" varLabels vLTwo is NA in cbind.exprSetRG ", call.=FALSE)
    }
  }
    
    pDOne <- pData(phenoData(objectOne))
    pDTwo <- pData(phenoData(objectTwo))

    if( any(dim(pDOne) == 0) | any(dim(pDTwo) == 0 ) ){
      stop(" any(pData(phenoData(object)) == 0) in cbind.exprSetRG ", call.=FALSE)
    }
    
    if( is.null(pDOne) | is.null(pDTwo) ){
    stop(" pData is NULL in cbind.exprSetRG ", call.=FALSE)
  }
    if( length(is.na(pDOne)) > 0 ){
      if( all(is.na(pDOne)) ){
        stop(" pData pDOne is NA in cbind.exprSetRG ", call.=FALSE)
      }
    }
    if( length(is.na(pDTwo)) > 0 ){
      if( all(is.na(pDTwo)) ){
      stop(" pData pDTwo is NA in cbind.exprSetRG ", call.=FALSE)
    }
    }

    commonVarLabels <- intersect(unlist(vLOne), unlist(vLTwo))
    varIndsOne <- vLOne %in% commonVarLabels
    varIndsTwo <- vLTwo %in% commonVarLabels

    varLabelsList <- as.list(commonVarLabels)
    pDataAllTmp <- rbind(pDOne[, varIndsOne, drop=FALSE],
                         pDTwo[, varIndsTwo, drop=FALSE])

    onlyOneVarLabels <- setdiff(unlist(vLOne), unlist(vLTwo))
    if( length(onlyOneVarLabels) > 0 ){
      varIndsOnlyOne <- vLOne %in% onlyOneVarLabels
      tmp <- pDOne[, varIndsOnlyOne, drop=FALSE]
      tmpMatrix <- matrix(NA, ncol=sum(varIndsOnlyOne), nrow=dim(pDTwo)[1])
      colnames(tmpMatrix) <- colnames(tmp)
      tmpPData <- rbind(tmp, tmpMatrix)
      varLabelsList <- c(varLabelsList, as.list(onlyOneVarLabels))
      pDataAllTmp <- cbind(pDataAllTmp,tmpPData)
    }
    onlyTwoVarLabels <- setdiff(unlist(vLTwo), unlist(vLOne))
    if( length(onlyTwoVarLabels) > 0 ){
      varIndsOnlyTwo <- vLTwo %in% onlyTwoVarLabels
      tmp <- pDTwo[, varIndsOnlyTwo, drop=FALSE]
      tmpMatrix <- matrix(NA, ncol=sum(varIndsOnlyTwo), nrow=dim(pDOne)[1])
      colnames(tmpMatrix) <- colnames(tmp)
      tmpPData <- rbind(tmpMatrix, tmp)
      varLabelsList <- c(varLabelsList, as.list(onlyTwoVarLabels))
      pDataAllTmp <- cbind(pDataAllTmp,tmpPData)
    }

    if( ! is.data.frame(pDataAllTmp) ){
      pDataDataFrame <- as.data.frame(pDataAllTmp)
    }else{
      pDataDataFrame <- pDataAllTmp
    }
    dimnames(pDataDataFrame) <- list( dimnames(pDataDataFrame)[[1]], unlist(varLabelsList) )
    phenoDataObject <- new("phenoData", pData=pDataDataFrame, varLabels=varLabelsList)
    
    ##
    
    if( dim(exprs(objectOne))[1] != dim(exprs(objectTwo))[1] ){
      stop(" different number of exprs rows in cbind.exprSetRG ", call.=FALSE)
    }
    
    gNOne <- geneNames(objectOne)
    gNTwo <- geneNames(objectTwo)
    if( ! is.null(gNOne) && ! is.null(gNTwo) ){
      if( ! identical( gNOne, gNTwo ) ){
        stop(" geneNames differ in cbind.exprSetRG ", call.=FALSE)
      }
    }
  
    exprsMatrix <- cbind(exprs(objectOne),exprs(objectTwo))
    
    if( dim(se.exprs(objectOne))[1] != dim(se.exprs(objectTwo))[1] ){
      stop(" different number of se.exprs rows in cbind.exprSetRG ", call.=FALSE)
    }
    
    se.exprsMatrix <- cbind(se.exprs(objectOne),se.exprs(objectTwo))
    
    if( length(getIndGreen(objectOne)) != length(getIndRed(objectOne)) ){
      stop(" object one invalid exprSetRG in cbind.exprSetRG ", call.=FALSE)
    }
    if( length(getIndGreen(objectTwo)) != length(getIndRed(objectTwo)) ){
      stop(" object two invalid exprSetRG in cbind.exprSetRG ", call.=FALSE)
  }
    
    offset <- length(getIndGreen(objectOne)) + length(getIndRed(objectOne))
    channels<-as.matrix(cbind(c(getIndGreen(objectOne),
                                (getIndGreen(objectTwo)+offset)),
                              c(getIndRed(objectOne),
                                (getIndRed(objectTwo)+offset)) ))
    colnames(channels) <- c("green","red")
    
    notes <- paste(notes(objectOne)," cbind(ed) with ", notes(objectTwo))

    ## duplicated rownames are not allowed, but after subsetting this should not occur
    if( is.null(colnames(exprsMatrix)) & !is.null(rownames(pData(phenoDataObject)))){
      colnames(exprsMatrix) <- rownames(pData(phenoDataObject))
    }else{
      tmpLogical <- as.logical(all.equal(colnames(exprsMatrix),rownames(pData(phenoDataObject))))
      if( all(is.na(tmpLogical)) | ! all(tmpLogical) ){
        if( any(duplicated(colnames(exprsMatrix) ) ) ){
          colnames(exprsMatrix) <- make.names(colnames(exprsMatrix), unique=TRUE)
        }
        rownames(pData(phenoDataObject)) <- colnames(exprsMatrix)
      }
    }
    

    if( ! (is.null( se.exprsMatrix) | length(se.exprsMatrix) == 0 )  ){
      colnames(se.exprsMatrix) <- colnames(exprsMatrix)
    }

    concatenatedObject <- new("exprSetRG", exprs=exprsMatrix,
                        se.exprs=se.exprsMatrix, phenoData = phenoDataObject,
                        channels=channels, notes=notes,
                        annotation=annotationInfo
                              )
    description(concatenatedObject) <- descriptionInfo

    objectOne <- concatenatedObject
    
  }
    
  return(objectOne)

}


  

  ###
  
    setMethod("show", signature=signature(object="exprSetRG"), definition=function(object) {
    dm <-dim(exprs(object))
    ngenes <- dm[1]
    nsamples <- dm[2]
    nslides <- length(getIndGreen(object))
    stopifnot(length(getIndGreen(object)) == length(getIndRed(object)))
    cat(paste("exprSetRG with \n\t", ngenes, " genes\n\t", sep=""))
    cat(paste(nslides, " slides, i.e. ", nsamples," channels \n",sep=""))
    cat("phenoDataSlide:\n")
    show(phenoDataSlide(object))
    cat("phenoDataGreen:\n")
    show(phenoDataGreen(object))
    cat("phenoDataRed:\n")
    show(phenoDataRed(object))
  })

  ###

  if( !isGeneric("as.exprSet") ){
    setGeneric("as.exprSet", function(object) standardGeneric("as.exprSet"))
  }
   setMethod("as.exprSet",signature=signature(object="exprSetRG"), definition=function(object){

     return(new("exprSet", exprs=exprs(object), se.exprs=se.exprs(object),
                notes=notes(object), phenoData=phenoData(object),
                annotation=annotation(object))
            )

   })
  

  ###

  if( !isGeneric("as.RGList") ){
    setGeneric("as.RGList", function(object, ...) standardGeneric("as.RGList"))
  }
  setMethod("as.RGList",signature=signature(object="exprSetRG"), definition=function(object, func=exp){

     RG <- list(R=matrix(), G=matrix(), Rb=matrix(), Gb=matrix())
     RG$R <- func(exprs(getExprSetRed(object)))
     RG$Rb <- matrix(data=0, nrow=nrow(RG$R), ncol=ncol(RG$R))
     RG$G <- func(exprs(getExprSetGreen(object)))
     RG$Gb <- matrix(data=0, nrow=nrow(RG$G), ncol=ncol(RG$G))
     return(new("RGList", RG))

   })
  

  ###


  
  if( !isGeneric("getIndGreen") ){
    setGeneric("getIndGreen", function(object) standardGeneric("getIndGreen"))
  }
   setMethod("getIndGreen",signature=signature(object="exprSetRG"), definition=function(object){
     return(object@indGreen)
   })


  if( !isGeneric("getExprSetGreen") ){
    setGeneric("getExprSetGreen", function(object) standardGeneric("getExprSetGreen"))
  }
  setMethod("getExprSetGreen",signature=signature(object="exprSetRG"), definition=function(object){
     exprsGreen <- exprs(object)[, getIndGreen(object), drop=FALSE]
     phenoDataGreen <- phenoData(object)[getIndGreen(object), ,drop=FALSE]
     if( is.null(dim(se.exprs(object))) | length(se.exprs(object)) == 0 ){
       se.exprsGreen <- se.exprs(object)
     }else{
       se.exprsGreen <- se.exprs(object)[, getIndGreen(object), drop=FALSE]
     }
     notes <- paste( notes(object), " result of getExprSetGreen", sep=";")

     
     ## duplicated rownames are not allowed, but after subsetting this should not occur
     if( is.null(colnames(exprsGreen)) & !is.null(rownames(pData(phenoDataGreen)))){
       colnames(exprsGreen) <- rownames(pData(phenoDataGreen))
     }else{
       tmpLogical <- as.logical(all.equal(colnames(exprsGreen),rownames(pData(phenoDataGreen))))
       if( all(is.na(tmpLogical)) | ! all(tmpLogical) ){
         if( any(duplicated(colnames(exprsGreen) ) ) ){
           colnames(exprsGreen) <- make.names(colnames(exprsGreen), unique=TRUE)
         }
         rownames(pData(phenoDataGreen)) <- colnames(exprsGreen)
       }
     }

     if( ! (is.null(dim(se.exprsGreen)) | length(se.exprsGreen) == 0 ) ){
       colnames(se.exprsGreen) <- colnames(exprsGreen)
     }
     
     return(new("exprSet", exprs=exprsGreen, phenoData=phenoDataGreen, se.exprs=se.exprsGreen, notes=notes, annotation=annotation(object)))
   })
  
  ###
  
  if( !isGeneric("getIndRed") ){
    setGeneric("getIndRed", function(object) standardGeneric("getIndRed") )
  }
   setMethod("getIndRed",signature=signature(object="exprSetRG"), definition=function(object){
     return(object@indRed)
   })
  if( !isGeneric("getExprSetRed") ){
    setGeneric("getExprSetRed", function(object) standardGeneric("getExprSetRed"))
  }
  setMethod("getExprSetRed",signature=signature(object="exprSetRG"), definition=function(object){
     exprsRed <- exprs(object)[, getIndRed(object), drop=FALSE]
     phenoDataRed <- phenoData(object)[getIndRed(object), , drop=FALSE]
     if( is.null(dim(se.exprs(object))) | length(se.exprs(object)) == 0 ){
       se.exprsRed <- se.exprs(object)
     }else{
       se.exprsRed <- se.exprs(object)[, getIndRed(object), drop=FALSE]
     }
     notes <- paste( notes(object), " result of getExprSetRed ", sep=";")

     ## duplicated rownames are not allowed, but after subsetting this should not occur
     if( is.null(colnames(exprsRed)) & !is.null(rownames(pData(phenoDataRed)))){
       colnames(exprsRed) <- rownames(pData(phenoDataRed))
     }else{
       tmpLogical <- as.logical(all.equal(colnames(exprsRed),rownames(pData(phenoDataRed))))
       if( all(is.na(tmpLogical)) | ! all(tmpLogical) ){
         if( any(duplicated(colnames(exprsRed) ) ) ){
           colnames(exprsRed) <- make.names(colnames(exprsRed), unique=TRUE)
         }
         rownames(pData(phenoDataRed)) <- colnames(exprsRed)
       }
     }

     if( ! (is.null(dim(se.exprsRed)) | length(se.exprsRed) == 0 ) ){
       colnames(se.exprsRed) <- colnames(exprsRed)
     }
     
     return(new("exprSet", exprs=exprsRed, phenoData=phenoDataRed, se.exprs=se.exprsRed, notes=notes, annotation=annotation(object)))
   })

  ###
    

  if( !isGeneric("getExprSetLogRatio") ){
    setGeneric("getExprSetLogRatio", function(object, ...) standardGeneric("getExprSetLogRatio"))
  }
  setMethod("getExprSetLogRatio",signature=signature(object="exprSetRG"), definition=function(object, seExprsHandling="rootMeanSquare"){

    exprsGreen <- exprs(object)[, getIndGreen(object), drop=FALSE]
    exprsRed <- exprs(object)[, getIndRed(object), drop=FALSE]
    
    if( is.null(dim(se.exprs(object))) | length(se.exprs(object)) == 0 ){
      se.exprsLogRatio <- se.exprs(object)
    }else{
      se.exprsGreen <- se.exprs(object)[, getIndGreen(object), drop=FALSE]
      se.exprsRed <- se.exprs(object)[, getIndRed(object), drop=FALSE]

      if( ! seExprsHandling %in% c("rootMeanSquare", "mean")  ){
        stop(paste(" unknown option :", seExprsHandling, ": for argument seExprsHandling in getExprSetLogRatio \n", sep=""), call.=FALSE)
      }
      if( seExprsHandling == "mean" ){
        seExprsHandlingFunction <- mean
      }else if( seExprsHandling == "rootMeanSquare" ){
        sqr <- function(x) x*x
        rootMeanSquare <- function(x){ return( sqrt(mean(sqr(x))) ) }
        seExprsHandlingFunction <- rootMeanSquare
      }else{
        stop(" unexpected case for argument seExprsHandling in getExprSetLogRatio ", call.=FALSE)
      }

      se.exprsBoth <- array( NA, dim=c(dim(se.exprsGreen),2))
      se.exprsBoth[,,1] <- se.exprsGreen
      se.exprsBoth[,,2] <- se.exprsRed

      se.exprsLogRatio <- controlledApply(arrayObject=se.exprsBoth,
                                          dimensions=c(1,2),
                                          func=seExprsHandlingFunction,
                                          funcResultDimensionality=1)

      se.exprsLogRatio <- controlledSubsetting(se.exprsLogRatio,
                                               ranges=list(NA,NA,1),
                                               drop=3)
      
      stopifnot( ! is.null(dim(se.exprsLogRatio)) )
      stopifnot( all( dim(se.exprsLogRatio) == dim( se.exprsGreen) ) )
    }
    
    exprsLogRatio <- exprsGreen - exprsRed
    
    phenoDataLogRatio <- phenoDataSlide(object)
    notes <- paste( notes(object), " result of getExprSetLogRatio (getExprSetGreenMinusRed)", sep=";")
    
    if( ! (is.null(dim(se.exprsLogRatio)) | length(se.exprsLogRatio) == 0 )){
      colnames(se.exprsLogRatio) <- colnames(exprsLogRatio)
    }

    ## duplicated rownames are not allowed, but after subsetting this should not occur
    if( is.null(colnames(exprsLogRatio)) & !is.null(rownames(pData(phenoDataLogRatio)))){
      colnames(exprsLogRatio) <- rownames(pData(phenoDataLogRatio))
    }else{
       tmpLogical <- as.logical(all.equal(colnames(exprsLogRatio),rownames(pData(phenoDataLogRatio))))
       if( all(is.na(tmpLogical)) | ! all(tmpLogical) ){
         if( any(duplicated(colnames(exprsLogRatio) ) ) ){
           colnames(exprsLogRatio) <- make.names(colnames(exprsLogRatio), unique=TRUE)
         }
         rownames(pData(phenoDataLogRatio)) <- colnames(exprsLogRatio)
       }
     }
    if( ! (is.null( se.exprsLogRatio) | length(se.exprsLogRatio) == 0 )  ){
      colnames(se.exprsLogRatio) <- colnames(exprsLogRatio)
    }
    
    return(new("exprSet", exprs=exprsLogRatio, se.exprs=se.exprsLogRatio,
               phenoData=phenoDataLogRatio, notes=notes,
               annotation=annotation(object)))
  })


  if( !isGeneric("getExprSetGreenMinusRed") ){
    setGeneric("getExprSetGreenMinusRed", function(object) standardGeneric("getExprSetGreenMinusRed"))
  }
  
  setMethod("getExprSetGreenMinusRed",signature=signature(object="exprSetRG"), definition=function(object){
    
    return(getExprSetLogRatio(object))
    
  })



  if( !isGeneric("getExprSetRedMinusGreen") ){
    setGeneric("getExprSetRedMinusGreen", function(object) standardGeneric("getExprSetRedMinusGreen"))
  }
  
  setMethod("getExprSetRedMinusGreen",signature=signature(object="exprSetRG"), definition=function(object){

    result <- getExprSetLogRatio(object)
    exprs(result) <- (-1) * exprs(result)
    return(result)
    
  })
  


    
  if( !isGeneric("phenoDataGreen") ){
    setGeneric("phenoDataGreen", function(object) standardGeneric("phenoDataGreen"))
  }
  setMethod("phenoDataGreen",signature=signature(object="exprSetRG"), definition=function(object){
    
    phenoDataGreen <- phenoData(object)[getIndGreen(object),,drop=FALSE]
    pDataGreen <- pData(phenoDataGreen)
    varLabelsGreen <- varLabels(phenoDataGreen)
    dimnames(pDataGreen) <- list( dimnames(pDataGreen)[[1]], unlist(varLabelsGreen) )
    
    return(new("phenoData",pData=pDataGreen, varLabels=varLabelsGreen))
    
  })

  if( !isGeneric("pDataGreen") ){
    setGeneric("pDataGreen", function(object) standardGeneric("pDataGreen"))
  }
  setMethod("pDataGreen",signature=signature(object="exprSetRG"), definition=function(object){
    
    return(pData(phenoDataGreen(object)))
    
  })
  

    
  if( !isGeneric("phenoDataRed") ){
    setGeneric("phenoDataRed", function(object) standardGeneric("phenoDataRed"))
  }
  setMethod("phenoDataRed",signature=signature(object="exprSetRG"), definition=function(object){
    
    phenoDataRed <- phenoData(object)[getIndRed(object),,drop=FALSE]
    pDataRed <- pData(phenoDataRed)
    varLabelsRed <- varLabels(phenoDataRed)
    dimnames(pDataRed) <- list( dimnames(pDataRed)[[1]], unlist(varLabelsRed) )
    
    return(new("phenoData",pData=pDataRed, varLabels=varLabelsRed))
    
  })

  if( !isGeneric("pDataRed") ){
    setGeneric("pDataRed", function(object) standardGeneric("pDataRed"))
  }
  setMethod("pDataRed",signature=signature(object="exprSetRG"), definition=function(object){
    
    return(pData(phenoDataRed(object)))
    
  })
  
  
  
  if( !isGeneric("phenoDataSlide") ){
    setGeneric("phenoDataSlide", function(object) standardGeneric("phenoDataSlide"))
  }
  setMethod("phenoDataSlide",signature=signature(object="exprSetRG"), definition=function(object){

    indGreen <-getIndGreen(object)
    indRed <- getIndRed(object)
    stopifnot( length(indGreen) == length(indRed) )
    if( length(indGreen) == 0 ){
      return( new("phenoData") )
    }
    channels <- matrix(c(indGreen,indRed),byrow=FALSE,
                       nrow=length(indGreen), ncol=2)
    fullPhenoData <- phenoData(object)
    fullPData <- pData(object)
    if( length(fullPData) == 0 ){
      return( new("phenoData")  )
    }

    ## dim(comp) nrOfslides x nrOfcovariates
    comp <- simpleApply(channels,
                        dimensions=1,
                        func=function(x){
               tmp <- vector(mode="logical", length=dim(fullPData)[2])
               for( i in 1:length(tmp) ){
                 tmp[i] <-  identical(fullPData[x[1],i], fullPData[x[2],i])
               }
               return(tmp)},
                        funcResultDimensionality=dim(fullPData)[2]
                        )

    stopifnot( dim(comp)[1] == length(indGreen)) 
    stopifnot( dim(comp)[2] == dim(fullPData)[2] )
    colBoolean <- apply(comp,2, function(x) return(all(x)) )
    stopifnot( length(colBoolean) == dim(fullPData)[2] )
        
    ## implicit drop=FALSE in [] of phenoData
    commonPhenoData <- fullPhenoData[indGreen,colBoolean] 
    if( all(colBoolean) || length(colBoolean) == 0 ){
      resultPhenoData <- commonPhenoData
    }else{

      greenSpecific <- "greenSpecific_"
      redSpecific <- "redSpecific_"
      commonPData <- pData(commonPhenoData)
      greenUniquePData <- pData(fullPhenoData[indGreen, !colBoolean])
      if( ! all(is.null(colnames(greenUniquePData))) ){
        colnames(greenUniquePData) <- paste(greenSpecific,colnames(greenUniquePData),sep="")
      }
      redUniquePData <- pData(fullPhenoData[indRed, !colBoolean])
      if( ! all(is.null(colnames(redUniquePData))) ){
        colnames(redUniquePData) <- paste(redSpecific,colnames(redUniquePData),sep="")
      }

      allPData <- cbind(commonPData, greenUniquePData, redUniquePData)
      if( ! (identical(rownames(commonPData), rownames(greenUniquePData)) &&
             identical(rownames(commonPData), rownames(redUniquePData))       )){
      ##dimnames(allPData) <- list(paste("green",rownames(greenUniquePData),
      ##                                 "red",rownames(redUniquePData),sep=""),
      ##                                 dimnames(allPData)[[2]])
        
      }
      varLabelsCommon <- varLabels(commonPhenoData)
      varLabelsRest <- varLabels(fullPhenoData[indGreen, !colBoolean])
      varLabelsAll <- c(varLabelsCommon,
                        paste(greenSpecific,varLabelsRest, sep=""),
                        paste(redSpecific,varLabelsRest, sep="")
                        )
      dimnames(allPData) <- list( dimnames(allPData)[[1]], unlist(varLabelsAll) )
      
      resultPhenoData <- new("phenoData", pData=allPData, varLabels=varLabelsAll)
      
    }
    
    return(resultPhenoData) 
    
  })

  if( !isGeneric("pDataSlide") ){
    setGeneric("pDataSlide", function(object) standardGeneric("pDataSlide"))
  }
  setMethod("pDataSlide",signature=signature(object="exprSetRG"), definition=function(object){
    pDataTmp <- pData(phenoDataSlide(object))
    varLabelsTmp <- varLabels(phenoDataSlide(object))
    dimnames(pDataTmp) <- list( dimnames(pDataTmp)[[1]], unlist(varLabelsTmp) )

    return(pDataTmp)
  })
  
  
  if( !isGeneric("slideSubset") ){
    setGeneric("slideSubset", function(object,i,j) standardGeneric("slideSubset"))
  }
  setMethod("slideSubset", signature=signature(object="exprSetRG"), definition=function(object,i,j){
    
    if( missing(j) ){
      j3 <-getIndGreen(object)
      j5 <-getIndRed(object)
    }else{
      if( any(is.character(j)) ){
        stop(" in slideSubset j is invalid, i.e. of type character ", call.=FALSE)
      }
       if( any(is.na(j)) ){
        stop(" in slideSubset j is invalid, i.e. contains NA ", call.=FALSE)
      }
      if( ! (all(is.logical(j)) || all(is.numeric(j))) ){
        stop(" in slideSubset j is invalid, i.e. must be of type logical or integer/numeric ", call.=FALSE)
      }
      if( is.logical(j) ){
        stopifnot( length(j) == length(getIndGreen(object)) )
        stopifnot( length(j) == length(getIndRed(object)) )
      }else{
        if( min(j) < 0 ){
          stop(" in slideSubset min(j) < 0, i.e. out of range ", call.=FALSE)
        }
        stopifnot( length(getIndGreen(object)) == length(getIndRed(object)) )
        if( max(j) > length(getIndGreen(object)) ){
          stop(" in slideSubset max(j) > number of slides, i.e. out of range ", call.=FALSE)
        }
      }
      j3 <- getIndGreen(object)[j]
      j5 <- getIndRed(object)[j]
      if( any(is.na(j3)) | any(is.na(j5)) ){
        stop(" in slideSubset j is invalid, e.g. out of range ", call.=FALSE)
      }
    }
    channelJ <- c(j3,j5)
    if( missing(i) ){
      result <- object[,channelJ,drop=FALSE]
    }else{
      if( any(is.character(i)) ){
        stop(" slideSubset i is invalid, i.e. of type character ", call.=FALSE)
      }
      result <- object[i,channelJ,drop=FALSE]
    }
    return(result)
  })
  
  ## [ taken from exprSet.R
  ## take care of indGreen and indRed which relates to j
  ## but the operation may not preserve the slide matching in general,
  ## hence parameter type is introduced to check consistency
  setMethod("[", "exprSetRG", function(x, i, j, type="validExprSetRG", ..., drop=FALSE) {

    if( !any(type %in% c( "validExprSetRG", "invalidExprSetRG" )) ){
      stop("invalid type in subsetting exprSetRG", call.=FALSE)
    }
    
    ## new line - before manipulation
    nrOfColumnsInExprsXOld <- dim(exprs(x))[2]
    
    if( missing(j) )
      pdata <- phenoData(x)
    else
      pdata <- phenoData(x)[j,, ..., drop=FALSE]
    haveSES <- nrow(se.exprs(x)) > 0
    if(missing(j) ) {
      if( missing(i) ) {
        nexprs <- exprs(x)
        if( haveSES )
          nses <- se.exprs(x)
      }
      else {
        nexprs <- exprs(x)[i, ,drop=FALSE]
        if( haveSES )
          nses <- se.exprs(x)[i, ,drop=FALSE]
      }
    }
    else {
      if( missing(i) ) {
        nexprs <- exprs(x)[,j, drop=FALSE]
        if( haveSES )
          nses <- se.exprs(x)[,j, drop=FALSE]
      }
      else {
        nexprs <- exprs(x)[i, j, drop=FALSE]
        if( haveSES )
          nses <- se.exprs(x)[i, j, drop=FALSE]
      }
    }
    exprs(x) <- nexprs
    if( haveSES )
      se.exprs(x) <- nses

    ##if( type != "validExprSetRG" ){
      ## just for consistency issues bewtween pdata and nexprs
      ##if( ! is.null(colnames(nexprs)) ){
      ##  rownames(pdata) <- colnames(nexprs)
      ##}
    ##}
    phenoData(x) <- pdata
    
    ## new part
    if( !missing(j) ){
      if( length(j) > 0 ){
        newIndexes <- 1:(length(j))
        if( is.logical(j) ){
          if( length(j) != nrOfColumnsInExprsXOld ){
            stop("logical j must match dimensions of the columns of exprs(object)", call.=FALSE)
          }
          j <- which( j == TRUE )
          newIndexes <- 1:length(j)
        }
        green <- newIndexes[j %in% getIndGreen(x)]
        red <- newIndexes[j %in% getIndRed(x)]

        
        ## check validity/consistency of exprSetRG
        if( type == "validExprSetRG" ){
          indexesInGreen <-sapply( j[j %in% getIndGreen(x)] ,
                                function(y) which( y==getIndGreen(x) ))
          indexesInRed <-sapply( j[j %in% getIndRed(x)] ,
                                function(y) which( y==getIndRed(x) ))
          if( length(indexesInGreen) != length(indexesInRed) ||
             ## case: one is empty
              !all(indexesInGreen == indexesInRed) ){
            stop("invalid subset operation, i.e. does not ensure validity of exprSetRG - possibly use type==\"invalidExprSetRG\" to proceed", call.=FALSE)
          }
        }
        
        ## set new channel indexes
        x@indGreen <- green
        x@indRed <- red

      }
    }
    return(x)
  })

