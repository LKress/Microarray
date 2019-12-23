## introduce NumericOrNullClass, CharacterOrNullClass as suggested in
## the S4 class introduction  

# /**
#
# \name{arrayData-class}
# \docType{class}
#
# \alias{arrayData-class}
# \alias{arrayData}
#
# \alias{intensities<-}
# \alias{weights<-}
# \alias{spotAttr<-}
# \alias{hybAttrList<-}
# \alias{getIntensities}
# \alias{getWeights}
# \alias{getSpotAttr}
# \alias{getHybAttr}
# \alias{getHybAttrGreen}
# \alias{getHybAttrRed}
# \alias{getHybAttrList}
# \alias{as.RGList}
#
# \alias{intensities<-,arrayData-method}
# \alias{weights<-,arrayData-method}
# \alias{spotAttr<-,arrayData-method}
# \alias{hybAttrList<-,arrayData-method}
# \alias{getIntensities,arrayData-method}
# \alias{getWeights,arrayData-method}
# \alias{getSpotAttr,arrayData-method}
# \alias{getHybAttr,arrayData-method}
# \alias{getHybAttrGreen,arrayData-method}
# \alias{getHybAttrRed,arrayData-method}
# \alias{getHybAttrList,arrayData-method}
# \alias{as.RGList,arrayData-method}
# \alias{initialize,arrayData-method}
# \alias{[,arrayData-method}
# \alias{show,arrayData-method}
#
#
#
# \title{Class arrayData, a simple container for
#        raw data and related information }
#
#
# \description{ A simple class to store
#        raw data, annotation information for spots and
#        hybridisations, as well as weights.
#        }
#
#\section{Creating Objects}{
# \code{  new('arrayData',}\cr
# \code{    intensities = ..., # optional; object of class array}\cr
# \code{    weights     = ..., # optional; object of class matrix}\cr
# \code{    spotAttr    = ..., # optional; object of class data.frame}\cr
# \code{    hybAttrList = ..., # optional; list of two objects of class data.frame}\cr
# \code{  )}
#}
#
#\section{Slots}{
#  \describe{
#    \item{\code{intensities}:}{Object of class \code{array};
#                               three-dimensional;
#        dim = nrOfSpots x nrOfChannels x nrOfHybridisations;
#        second dimension must contain "green"  and "red" and possibly
#        "greenBackground" and "redBackground"; default: \code{NULL}}
#    \item{\code{weights}:}{Object of class \code{matrix};
#                           dim = nrOfSpots x nrOfHybridisations;
#                           range = [0,1]; default: \code{NULL}.
#         }
#    \item{\code{spotAttr}:}{Object of class \code{data.frame};
#                            dim = nrOfSpots x nrOfSpotCharacteristics;
#        default: \code{NULL}}
#    \item{\code{hybAttrList}:}{list of two objects named "green" and "red"
#        of class \code{data.frame}; the dimension of
#        each \code{data.frame} is given by
#        nrOfHybridisations x nrOfHybridisationCharacteristics ;
#        default: \code{NULL}}
#  }
#}
#
# \section{Methods}{
#  \describe{
#
#    \item{intensities<-}{(arrayData): Set the \code{intensities} of
#                           \code{arrayData}. }
#    \item{getIntensities}{(arrayData): Returns the \code{intensities}
#                           \code{array}.}
#    \item{weights<-}{(arrayData): Set the \code{weights} of
#                           \code{arrayData}. }
#    \item{getWeights}{(arrayData): Returns the \code{weights} object. }
#    \item{spotAttr<-}{(arrayData): Set the \code{spotAttr} of
#                           \code{arrayData}. }
#    \item{getSpotAttr}{(arrayData): Returns the \code{spotAttr} \code{data.frame}.}
#    \item{getHybAttr}{(arrayData): Returns the "intersection" of the "red"
#                      and "green" \code{data.frame} of \code{hybAttrList}
#                      or if one is \code{NULL} the other one. The "intersection"
#                      are those columns which match in column name and its content
#                      otherwise \code{NULL} is returned.}
#    \item{getHybAttrGreen}{(arrayData): Returns the "green" list element 
#                         of \code{hybAttrList}, i.e. a \code{data.frame}. }
#    \item{getHybAttrRed}{(arrayData): Returns the "red" list element
#                         of \code{hybAttrList}, i.e. a \code{data.frame}. }
#    \item{hybAttrList<-}{(arrayData): Set the \code{hybAttrList} of
#                           \code{arrayData}. }
#    \item{getHybAttrList}{(arrayData): Returns the \code{hybAttrList} object. }
#    \item{as.RGList}{(arrayData): Returns an object of \code{RGList}.
#      Foreground and background intensity values of \code{intensities}
#      are returned in form of an \code{RGList} object. If no background
#      is given, all background values in the RGList object are
#      set to zero.}
#

#    \item{show}{(arrayData): Renders information about the \code{arrayData} object
#                             on standard out.}
#    \item{[}{(arrayData,i,j):
#             A subset operator, where i corresponds to the spots and
#             j to the hybridisations.}
#
#    \item{cbind}{(...): Concatenates \code{arrayData} objects.
#                        Spots/rows are assumed to match;
#                        possibly you have to subset and reorder
#                        the objects beforehand
#                        cf. function \code{\link{cbind.arrayData}}.
#                }
#
#     
#  }
#}
#
# \seealso{
#           \code{\link{exprSetRG-class}}
#         }
#
# \keyword{methods}
# \keyword{classes}
#
# \examples{
#
#intensities <- array(data=runif(120),dim=c(20,2,3))
#dimnames(intensities) <- list(NULL, c("green","red"), NULL)
#spotAttr <- data.frame(Name=I(rep(c("x","y","k","l","z"),4)),
#                       Index=c(1:20))
#arrayDataObject <- new("arrayData", intensities=intensities, weights=intensities[,1,],
#                       spotAttr=spotAttr, hybAttrList=NULL)
#print(arrayDataObject)
#hybs <- c(1,3)
#spots <- c(1:10, 14)
#aD <- arrayDataObject[spots,hybs]
#print(aD)
#stopifnot( all( getIntensities(arrayDataObject)[spots, , hybs] == getIntensities(aD) ) )
#stopifnot( all( getWeights(arrayDataObject)[spots, hybs] == getWeights(aD) ) )
#stopifnot( all( getSpotAttr(arrayDataObject)[spots, ] == getSpotAttr(aD) ) )
#
#hybAttr <- data.frame(Name=I(c("hx","hy","hz")),
#                      Index=c(1:3))
#arrayDataObject <- new("arrayData", intensities=intensities, weights=intensities[,1,],
#                       spotAttr=spotAttr, hybAttrList=list(green=hybAttr,red=hybAttr))
#hybAttrGreen <- data.frame(Name=I(c("hx","hy","hz")),
#                      Index=c(4:6))
#aDObject <- new("arrayData", intensities=intensities, weights=intensities[,1,],
#                       spotAttr=spotAttr, hybAttrList=list(green=hybAttrGreen,red=hybAttr))
#print(arrayDataObject)
#cbind(aDObject,aDObject)
#hybs <- c(1)
#spots <- c(1)
#aD <- arrayDataObject[spots,hybs]
#print(aD)
#stopifnot( all( getIntensities(arrayDataObject)[spots, , hybs] == getIntensities(aD) ) )
#stopifnot( all( getWeights(arrayDataObject)[spots, hybs] == getWeights(aD) ) )
#stopifnot( all( getSpotAttr(arrayDataObject)[spots, ] == getSpotAttr(aD) ) )
#stopifnot( all( getHybAttr(arrayDataObject)[spots, ] == getHybAttr(aD) ) )
#stopifnot( all( getHybAttrRed(arrayDataObject)[spots, ] == getHybAttrRed(aD) ) )
#stopifnot( all( getHybAttrGreen(arrayDataObject)[spots, ] == getHybAttrGreen(aD) ) )
#
#stopifnot( all( getHybAttrRed(aDObject) == hybAttr) )
#stopifnot( all( getHybAttrGreen(aDObject) == hybAttrGreen) )
#stopifnot( all( getHybAttr(aDObject) == data.frame(Name=I(c("hx","hy","hz"))) ) )
#weights(aDObject) <- intensities[,2,]
#intensities(aDObject) <- intensities
#spotAttr(aDObject) <- spotAttr
#hybAttrList(aDObject) <- list(green=hybAttr,red=hybAttrGreen)
#aD <- new("arrayData")
#stopifnot( class(aD) == "arrayData" )
# \dontshow{
# print(aD)
# xAD<-cbind(aD,aD)
# weights(aD) <- matrix(1,nrow=2,ncol=3)
# xAD<-cbind(aD,aD)
# spotAttr(aD) <- data.frame(hallo=c("a","b"))
# xAD<-cbind(aD,aD)
# hybAttrRed <- data.frame(colour=1:3)
# hybAttrGreen <- data.frame(colour=10:12)
# hybAttrList <- list(red=hybAttrRed,green=hybAttrGreen)
# hybAttrList(aD) <- hybAttrList
# xAD<-cbind(aD,aD)
# intensities <- array(data=c(1:12),dim=c(2,2,3))
# dimnames(intensities) <- list(NULL, c("green","red"), NULL)
# intensities(aD) <- intensities
# xAD<-cbind(aD,aD)
# rg <- as.RGList(aD)
# i <- intensities * 0
# aD2<- aD
# intensities(aD2) <- i
# weights(aD2) <- getWeights(aD2) * 0.5
# xAD2 <- cbind(aD, aD2)
# stopifnot( all( getIntensities(xAD2[,1:3])  == getIntensities(aD) ) )
# stopifnot( all( getIntensities(xAD2[,4:6])  == getIntensities(aD2) ) )
# stopifnot( all( getWeights(xAD2[,1:3])  == getWeights(aD) ) )
# stopifnot( all( getWeights(xAD2[,4:6])  == getWeights(aD2) ) )
# aDThreeA <- cbind(aD,aD,aD)
# aDThreeB <- cbind( aD, cbind(aD,aD) )
# aDThreeC <- cbind( cbind(aD,aD), aD)
# stopifnot( identical( aDThreeA, aDThreeC ) )
# stopifnot( all( getSpotAttr(aDThreeA)[,1,drop=FALSE] == getSpotAttr(aD) ) )
# stopifnot( identical(getSpotAttr(aDThreeA), getSpotAttr(aDThreeB) ) )
# stopifnot( identical(getWeights(aDThreeA), getWeights(aDThreeB) ) )
# stopifnot( identical(getIntensities(aDThreeA), getIntensities(aDThreeB)))
# stopifnot( all(getHybAttrGreen(aDThreeA) == getHybAttrGreen(aDThreeB)))
# stopifnot( all(getHybAttrGreen(aDThreeA) == getHybAttrGreen(aDThreeB)))
# }
#}
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

setClass(Class="arrayData", representation =
         representation(## checks via validity function
                        ## "array" does not allow NULL
                        intensities="ANY",
                        ## "matrix" does not allow NULL
                        weights="ANY",
                        ## "data.frame" does not allow NULL
                        spotAttr="ANY",
                        ## "list" does not allow NULL
                        hybAttrList="ANY"),

         prototype = prototype(intensities=NULL, weights=NULL,
                               spotAttr=NULL, hybAttrList=NULL)
         
         ,
         
         validity = function(object){

    ## attention: code duplication
    if( ! is.null(object@weights) ){
      stopifnot( is.matrix(object@weights) )
      stopifnot( !is.null(dim(object@weights)) )
      stopifnot( max(object@weights) <= 1 )
      stopifnot( min(object@weights) >= 0 )
      stopifnot( length(dim(object@weights)) == 2 )
    }
    ##
    if( ! is.null(object@spotAttr) ){
      stopifnot( is.data.frame(object@spotAttr) )
      stopifnot( length(dim(object@spotAttr)) == 2 |
                length(dim(object@spotAttr)) == 1 )
    }
    ##
    if( ! is.null(object@hybAttrList) ){
      stopifnot( is.list(object@hybAttrList) )
      stopifnot( length(object@hybAttrList) == 2 )
      stopifnot( "green" %in% names(object@hybAttrList) )
      stopifnot( "red"   %in% names(object@hybAttrList) )
      if( ! is.null(object@hybAttrList[["red"]]) ){
        stopifnot( length(dim(object@hybAttrList[["red"]])) == 2 )
      }
      if( ! is.null(object@hybAttrList[["green"]]) ){
        stopifnot( length(dim(object@hybAttrList[["green"]])) == 2 )
      }
    }
    ##
    if( ! is.null(object@intensities) ){
      stopifnot( is.array(object@intensities) )
      ## at least two channels
      stopifnot( !is.null(dim(object@intensities)) )
      stopifnot( length(dim(object@intensities)) == 3 )
      stopifnot(dim(object@intensities)[2] >= 2)
      ## specific channel names are required
      stopifnot( all(c("green","red") %in% dimnames(object@intensities)[[2]]) ) 
    }
    ##
    if( ! is.null(object@intensities) & ! is.null(object@weights) ){
      stopifnot( dim(object@intensities)[1] == dim(object@weights)[1] )
      stopifnot( dim(object@intensities)[3] == dim(object@weights)[2] )
    }
    if( ! is.null(object@intensities) & ! is.null(object@spotAttr) ){
      stopifnot( dim(object@intensities)[1] == dim(object@spotAttr)[1] )
    }
    if( ! is.null(object@intensities) & ! is.null(object@hybAttrList) ){
      if( ! is.null(object@hybAttrList[["red"]]) ){
        stopifnot( dim(object@intensities)[3] == dim(object@hybAttrList[["red"]])[1] )
      }
      if( ! is.null(object@hybAttrList[["green"]]) ){
        stopifnot( dim(object@intensities)[3] == dim(object@hybAttrList[["green"]])[1] )
      }
    }
    ##
    if( ! is.null(object@weights) & ! is.null(object@spotAttr) ){
      stopifnot( dim(object@intensities)[1] == dim(object@spotAttr)[1] )
    }
    if( ! is.null(object@weights) & ! is.null(object@hybAttrList) ){
      if( ! is.null(object@hybAttrList[["red"]]) ){
        stopifnot( dim(object@weights)[2] == dim(object@hybAttrList[["red"]])[1] )
      }
      if( ! is.null(object@hybAttrList[["green"]]) ){
        stopifnot( dim(object@weights)[2] == dim(object@hybAttrList[["green"]])[1] )
      }
    }
    ## no consistency check required for 
    ## if( ! is.null(object@hybAttrList) & ! is.null(object@spotAttr) ){

    return(TRUE)

    }
)


setMethod("initialize", "arrayData",
  function(.Object, intensities=NULL, weights=NULL, spotAttr=NULL, hybAttrList=NULL){

    if( ! is.null(intensities) ){
      .Object@intensities <- intensities
    }
    if( ! is.null(weights) ){
      .Object@weights <- weights
    }
    if( ! is.null(spotAttr) ){
      .Object@spotAttr <- spotAttr
    }
    if( ! is.null(hybAttrList) ){
      .Object@hybAttrList <- hybAttrList      
    }

    ## the following does not work ?!
    ##
    ## validObject(object=.Object) 
    ##
    ## hence copy & paste of the valid function

    object <- .Object

    ## attention: code duplication
    if( ! is.null(object@weights) ){
      stopifnot( is.matrix(object@weights) )
      stopifnot( !is.null(dim(object@weights)) )
      stopifnot( max(object@weights) <= 1 )
      stopifnot( min(object@weights) >= 0 )
      stopifnot( length(dim(object@weights)) == 2 )
    }
    ##
    if( ! is.null(object@spotAttr) ){
      stopifnot( is.data.frame(object@spotAttr) )
      stopifnot( length(dim(object@spotAttr)) == 2 |
                length(dim(object@spotAttr)) == 1 )
    }
    ##
    if( ! is.null(object@hybAttrList) ){
      stopifnot( is.list(object@hybAttrList) )
      stopifnot( length(object@hybAttrList) == 2 )
      stopifnot( "green" %in% names(object@hybAttrList) )
      stopifnot( "red"   %in% names(object@hybAttrList) )
      if( ! is.null(object@hybAttrList[["red"]]) ){
        stopifnot( length(dim(object@hybAttrList[["red"]])) == 2 )
      }
      if( ! is.null(object@hybAttrList[["green"]]) ){
        stopifnot( length(dim(object@hybAttrList[["green"]])) == 2 )
      }
    }
    ##
    if( ! is.null(object@intensities) ){
      stopifnot( is.array(object@intensities) )
      ## at least two channels
      stopifnot( !is.null(dim(object@intensities)) )
      stopifnot( length(dim(object@intensities)) == 3 )
      stopifnot(dim(object@intensities)[2] >= 2)
      ## specific channel names are required
      stopifnot( all(c("green","red") %in% dimnames(object@intensities)[[2]]) ) 
    }
    ##
    if( ! is.null(object@intensities) & ! is.null(object@weights) ){
      stopifnot( dim(object@intensities)[1] == dim(object@weights)[1] )
      stopifnot( dim(object@intensities)[3] == dim(object@weights)[2] )
    }
    if( ! is.null(object@intensities) & ! is.null(object@spotAttr) ){
      stopifnot( dim(object@intensities)[1] == dim(object@spotAttr)[1] )
    }
    if( ! is.null(object@intensities) & ! is.null(object@hybAttrList) ){
      if( ! is.null(object@hybAttrList[["red"]]) ){
        stopifnot( dim(object@intensities)[3] == dim(object@hybAttrList[["red"]])[1] )
      }
      if( ! is.null(object@hybAttrList[["green"]]) ){
        stopifnot( dim(object@intensities)[3] == dim(object@hybAttrList[["green"]])[1] )
      }
    }
    ##
    if( ! is.null(object@weights) & ! is.null(object@spotAttr) ){
      stopifnot( dim(object@intensities)[1] == dim(object@spotAttr)[1] )
    }
    if( ! is.null(object@weights) & ! is.null(object@hybAttrList) ){
      if( ! is.null(object@hybAttrList[["red"]]) ){
        stopifnot( dim(object@weights)[2] == dim(object@hybAttrList[["red"]])[1] )
      }
      if( ! is.null(object@hybAttrList[["green"]]) ){
        stopifnot( dim(object@weights)[2] == dim(object@hybAttrList[["green"]])[1] )
      }
    }
    ## no consistency check required for 
    ## if( ! is.null(object@hybAttrList) & ! is.null(object@spotAttr) ){



    
    return(.Object)
})



if( !isGeneric("weights<-") )
    setGeneric("weights<-", function(object, value)
               standardGeneric("weights<-"))

setReplaceMethod("weights", "arrayData",
                 function(object, value) {
                   object@weights <- value
                   validObject(object=object)
                   return(object)
                 })


if( !isGeneric("intensities<-") )
    setGeneric("intensities<-", function(object, value)
               standardGeneric("intensities<-"))

setReplaceMethod("intensities", "arrayData",
                 function(object, value) {
                   object@intensities <- value
                   validObject(object=object)
                   return(object)
                 })


if( !isGeneric("spotAttr<-") )
    setGeneric("spotAttr<-", function(object, value)
               standardGeneric("spotAttr<-"))

setReplaceMethod("spotAttr", "arrayData",
                 function(object, value) {
                   object@spotAttr <- value
                   validObject(object=object)
                   return(object)
                 })


if( !isGeneric("hybAttrList<-") )
    setGeneric("hybAttrList<-", function(object, value)
               standardGeneric("hybAttrList<-"))

setReplaceMethod("hybAttrList", "arrayData",
                 function(object, value) {
                   object@hybAttrList <- value
                   validObject(object=object)
                   return(object)
                 })


if( !isGeneric("getIntensities") ){
  setGeneric("getIntensities", function(object) standardGeneric("getIntensities"))
}
setMethod("getIntensities", "arrayData", function(object){return(object@intensities)})

if( !isGeneric("getWeights") ){
  setGeneric("getWeights", function(object) standardGeneric("getWeights"))
}
setMethod("getWeights", "arrayData", function(object){return(object@weights)})

if( !isGeneric("getSpotAttr") ){
  setGeneric("getSpotAttr", function(object) standardGeneric("getSpotAttr"))
}
setMethod("getSpotAttr", "arrayData", function(object){return(object@spotAttr)})

if( !isGeneric("getHybAttrList") ){
  setGeneric("getHybAttrList", function(object) standardGeneric("getHybAttrList"))
}
setMethod("getHybAttrList", "arrayData", function(object){ return(object@hybAttrList)})


if( !isGeneric("getHybAttr") ){
  setGeneric("getHybAttr", function(object) standardGeneric("getHybAttr"))
}

## method is NOT comparable with pDataSlide/phenoDataSlide in exprSetRG
setMethod("getHybAttr", "arrayData", function(object){

  redDF <- getHybAttrRed(object)
  greenDF <- getHybAttrGreen(object)
  if( is.null(redDF) ){
    return(greenDF)
  }
  if( is.null(greenDF) ){
    return(redDF)
  }
  
  commonColumnNames <- colnames(redDF)[ colnames(redDF) %in% colnames(greenDF) ]
  if( length( commonColumnNames ) == 0 ){
    return(NULL)
  }
  commonColumns <- sapply(commonColumnNames, function(x){
    return(identical(redDF[,x], greenDF[,x]))
  })
  if( length( commonColumns  ) == 0 ){
    return(NULL)
  }  
  result <- redDF[,commonColumns,drop=FALSE]
  return(result)
})

if( !isGeneric("getHybAttrRed") ){
  setGeneric("getHybAttrRed", function(object) standardGeneric("getHybAttrRed"))
}
setMethod("getHybAttrRed", "arrayData", function(object) object@hybAttrList[["red"]])

if( !isGeneric("getHybAttrGreen") ){
setGeneric("getHybAttrGreen", function(object) standardGeneric("getHybAttrGreen"))
}
setMethod("getHybAttrGreen", "arrayData", function(object) object@hybAttrList[["green"]])

## [ taken from exprSetRG-class
setMethod("[", "arrayData", function(x, i, j, ..., drop=FALSE) {

  intensities <- getIntensities(x)
  weights <- getWeights(x)
  spotAttr <- getSpotAttr(x)
  hybAttrRed <- getHybAttrRed(x)
  hybAttrGreen <- getHybAttrGreen(x)
  
  if( missing(i) & missing(j) ){
    return(x)
  } else if( missing(i) ){
    if( ! is.null(intensities) ){
      intensities <- intensities[,,j,drop=FALSE]
    }
    if( ! is.null(weights) ){
      weights <- weights[,j,drop=FALSE]
    }
    if( ! is.null(spotAttr) ){
      spotAttr <- spotAttr[,,drop=FALSE]
    }
    if( ! is.null(hybAttrGreen) ){
      hybAttrGreen <- hybAttrGreen[j,,drop=FALSE]
    }
    if( ! is.null(hybAttrRed) ){
      hybAttrRed <- hybAttrRed[j,,drop=FALSE]
    }
   
    
  }else if( missing(j) ){
    
    if( ! is.null(intensities) ){
      intensities <- intensities[i,,,drop=FALSE]
    }
    if( ! is.null(weights) ){
      weights <- weights[i,,drop=FALSE]
    }
    if( ! is.null(spotAttr) ){
      spotAttr <- spotAttr[i,,drop=FALSE]
    }
    if( ! is.null(hybAttrGreen) ){
      hybAttrGreen <- hybAttrGreen[,,drop=FALSE]
    }
    if( ! is.null(hybAttrRed) ){
      hybAttrRed <- hybAttrRed[,,drop=FALSE]
    }

    
  }else if( !missing(i) & !missing(j) ){
    
    if( ! is.null(intensities) ){
      intensities <- intensities[i,,j,drop=FALSE]
    }
    if( ! is.null(weights) ){
      weights <- weights[i,j,drop=FALSE]
    }
    if( ! is.null(spotAttr) ){
      spotAttr <- spotAttr[i,,drop=FALSE]
    }
    if( ! is.null(hybAttrGreen) ){
      hybAttrGreen <- hybAttrGreen[j,,drop=FALSE]
    }
    if( ! is.null(hybAttrRed) ){
      hybAttrRed <- hybAttrRed[j,,drop=FALSE]
    }
    
  }else{
    stop(" unexpected case in [ ")
  }
  
  aD <- new("arrayData", intensities=intensities, weights=weights, spotAttr=spotAttr, hybAttrList=list(green=hybAttrGreen, red=hybAttrRed) )
  
  return(aD)
  
})



  if( !isGeneric("as.RGList") ){
    setGeneric("as.RGList", function(object, ...) standardGeneric("as.RGList"))
  }
  setMethod("as.RGList",signature=signature(object="arrayData"), definition=function(object){

     RG <- list(R=matrix(), G=matrix(), Rb=matrix(), Gb=matrix())
     intens <- getIntensities(object)
     if( is.null(intens) ){
       return(NULL)
     }       
     channels <- dimnames(intens)[[2]]
     RG$R <- controlledSubsetting(a=intens,
                                  ranges=list(NA, which(channels=="red"),NA),
                                  drop=2)
     indRed <- which(channels=="redBackground")
     if( length(indRed) == 0 ){
       RG$Rb <- matrix(data=0, nrow=nrow(RG$R), ncol=ncol(RG$R))
     }else{
       RG$Rb <- controlledSubsetting(a=intens,
                                     ranges=list(NA, indRed,NA), drop=2)
     }
     RG$G <- controlledSubsetting(a=intens,
                                  ranges=list(NA, which(channels=="green"),NA),
                                  drop=2)
     indGreen <- which(channels=="greenBackground")
     if( length(indGreen) == 0 ){
       RG$Gb <- matrix(data=0, nrow=nrow(RG$G), ncol=ncol(RG$G))
     }else{
       RG$Gb <- controlledSubsetting(a=intens,
                                     ranges=list(NA, indGreen,NA),
                                     drop=2)
     }
     return(new("RGList", RG))
     
   })
  




setMethod("show", signature=signature(object="arrayData"), definition=function(object) {

  intensities <- getIntensities(object)
  weights <- getWeights(object)
  spotAttr <- getSpotAttr(object)
  hybAttr <- getHybAttr(object)
  hybAttrGreen <- getHybAttrGreen(object)
  hybAttrRed <- getHybAttrRed(object)

  if( ! is.null(intensities) ){
    d <- dim(intensities)
    stopifnot( length(d) == 3 )
    cat(" intensities: nrOfSpots:",d[1]," nrOfChannels:",d[2]," nrOfHybridisations:",d[3],"\n")
    cat("              channelNames:",paste(dimnames(intensities)[[2]],collapse=", "),"\n")
  }else{
    cat(" intensities: NULL \n")
  }

  if( ! is.null(weights) ){
    d <- dim(weights)
    stopifnot( length(d) == 2 )
    cat(" weights: nrOfSpots:",d[1]," nrOfHybridisations:",d[2],"\n")
  }else{
    cat(" weights: NULL \n")
  }
  
  if( ! is.null(spotAttr) ){
    d <- dim(spotAttr)
    stopifnot( length(d) == 2 )
    cat(" spotAttr: nrOfSpots:",d[1]," nrOfSpotCharacteristics:",d[2],"\n")
    cat("           spotCharacteristics:",paste(dimnames(spotAttr)[[2]],collapse=", "),"\n")
  }else{
    cat(" spotAttr: NULL \n")    
  }
  
  if( ! is.null(hybAttr) ){
    d <- dim(hybAttr)
    stopifnot( length(d) == 2 )
    cat(" hybAttr: nrOfSlides:",d[1]," nrOfHybridisationCharacteristics:",d[2],"\n")
    cat("          hybridisationCharacteristics:",paste(dimnames(hybAttr)[[2]],collapse=", "),"\n")
  }else{
    cat(" hybAttr: NULL \n")
  }
  
  if( ! is.null(hybAttrGreen) ){
    d <- dim(hybAttrGreen)
    stopifnot( length(d) == 2 )
    cat(" hybAttrGreen: nrOfChannels:",d[1],
        " nrOfHybridisationCharacteristics:",d[2],"\n")
    cat("               hybridisationCharacteristics:",
        paste(dimnames(hybAttrGreen)[[2]],collapse=", "),"\n")
  }else{
    cat(" hybAttrGreen: NULL \n")
  }
  
 if( ! is.null(hybAttrRed) ){
    d <- dim(hybAttrRed)
    stopifnot( length(d) == 2 )
    cat(" hybAttrRed: nrOfChannels:",d[1],
        "nrOfHybridisationCharacteristics:",d[2],"\n")
    cat("               hybridisationCharacteristics:",
        paste(dimnames(hybAttrRed)[[2]],collapse=", "),"\n")
  }else{
    cat(" hybAttrRed: NULL \n")
  }
  

  
})
 


cbind.arrayData <- function(...) {

  dataList <- list(...)
  objectOne <- dataList[[1]]
                               
  for(objectTwo in dataList[2:length(dataList)]){

    spotAttrOne <- getSpotAttr(objectOne)
    spotAttrTwo <- getSpotAttr(objectTwo)
    if( ! identical(dim(spotAttrOne)[1],dim(spotAttrTwo)[1]) ){
      stop(" dim(spotAttrOne)[1] and dim(spotAttrTwo)[1] differ in cbind.arrayData ")
    }
    if(! (all( dimnames(spotAttrOne)[[1]] %in% dimnames(spotAttrTwo)[[1]] ) &&
          all( dimnames(spotAttrTwo)[[1]] %in% dimnames(spotAttrOne)[[1]] )   )
       ){
      cat(" Attention: dimnames(spotAttr)[[1]] differ in cbind.arrayData\n")
    }
    spotAttr <- cbind(spotAttrOne,spotAttrTwo)

    weightsOne <- getWeights(objectOne)
    weightsTwo <- getWeights(objectTwo)
    if( ! identical(dim(weightsOne)[1], dim(weightsTwo)[1] ) ){
      stop(" dim(weightsOne)[1] and dim(weightsTwo)[1] differ in cbind.arrayData ")
    }
    if(! (all( dimnames(weightsOne)[[1]] %in% dimnames(weightsTwo)[[1]] ) &&
          all( dimnames(weightsTwo)[[1]] %in% dimnames(weightsOne)[[1]] )    )
       ){
      cat(" Attention: dimnames(weights)[[1]] differ in cbind.arrayData\n")
    }
    weights <- cbind(weightsOne,weightsTwo)

    intensitiesOne <- getIntensities(objectOne)
    intensitiesTwo <- getIntensities(objectTwo)
    if( ! identical(dim(intensitiesOne)[1], dim(intensitiesTwo)[1]) ){
      stop(" dim(intensitiesOne)[1] and dim(intensitiesTwo)[1] differ in cbind.arrayData")
    }
    if( ! identical(dim(intensitiesOne)[2], dim(intensitiesTwo)[2]) ){
      stop(" dim(intensitiesOne)[2] and dim(intensitiesTwo)[2] differ in cbind.arrayData")
    }
     if( ! (all( dimnames(intensitiesOne)[[1]] %in% dimnames(intensitiesTwo)[[1]] ) &&
            all( dimnames(intensitiesTwo)[[1]] %in% dimnames(intensitiesOne)[[1]] )   )
       ){
      cat(" Attention: dimnames(intensities)[[1]] differ in cbind.arrayData\n")
    }
    if( ! (all( dimnames(intensitiesOne)[[2]] %in% dimnames(intensitiesTwo)[[2]] ) &&
           all( dimnames(intensitiesTwo)[[2]] %in% dimnames(intensitiesOne)[[2]] )   )
       ){
      stop(" dimnames(intensities)[[2]] differ in cbind.arrayData ")
    }
    if( length(dim(intensitiesOne)[1]) > 0 ){
      intensities <- array(NA, dim = c(dim(intensitiesOne)[1],
                                 dim(intensitiesOne)[2],
                                 dim(intensitiesOne)[3] + dim(intensitiesTwo)[3] ) )
      dimnames(intensities) <- list( dimnames(intensitiesOne)[[1]],
                                    dimnames(intensitiesOne)[[2]],
                                    c(dimnames(intensitiesOne)[[3]],
                                      dimnames(intensitiesTwo)[[3]] ) )
                                  
      for( i in dimnames(intensitiesOne)[[2]] ){


        tmpIntensities <- cbind(
                                controlledSubsetting(intensitiesOne,
                                                     ranges=list(NA,i,NA),
                                                     drop=2),
                                controlledSubsetting(intensitiesTwo,
                                                     ranges=list(NA,i,NA),
                                                     drop=2)
                                )
        stopifnot( tmpIntensities[1,1] == intensitiesOne[1,i,1] )
        stopifnot( tmpIntensities[1,dim(intensitiesOne)[3]+1]
                  == intensitiesTwo[1,i,1] )
        
        intensities[,i,] <- tmpIntensities
        
      }
    }else{
      cat(" Attention: no intensity values found, thus intensities are set to NULL\n")
      intensities <- NULL
    }

    hybAttrRedOne <- getHybAttrRed(objectOne)
    hybAttrGreenOne <- getHybAttrGreen(objectOne)
    hybAttrRedTwo <- getHybAttrRed(objectTwo)
    hybAttrGreenTwo <- getHybAttrGreen(objectTwo)

    if( ! identical(dim(hybAttrRedOne)[2], dim(hybAttrRedTwo)[2]) ){
      stop(" dim(hybAttrRedOne)[2] and dim(hybAttrRedTwo)[2] differ in cbind.arrayData ")
    }
    if( ! identical(dim(hybAttrGreenOne)[2], dim(hybAttrGreenTwo)[2]) ){
      stop(" dim(hybAttrGreenOne)[2] and dim(hybAttrGreenTwo)[2] differ in cbind.arrayData ")
    }
    if(! (all( dimnames(hybAttrRedOne)[[2]] %in% dimnames(hybAttrRedTwo)[[2]] ) &&
          all( dimnames(hybAttrRedTwo)[[2]] %in% dimnames(hybAttrRedOne)[[2]] )   )
       ){
      stop(" dimnames(hybAttrRed)[[2]] differ in cbind.arrayData ")
    }
    if(! (all( dimnames(hybAttrGreenOne)[[2]] %in% dimnames(hybAttrGreenTwo)[[2]] ) &&
          all( dimnames(hybAttrGreenTwo)[[2]] %in% dimnames(hybAttrGreenOne)[[2]] )   )
       ){
      stop(" dimnames(hybAttrGreen)[[2]] differ in cbind.arrayData ")
    }
    
    hybAttrRed <- rbind( hybAttrRedOne, hybAttrRedTwo )
    hybAttrGreen <- rbind( hybAttrGreenOne, hybAttrGreenTwo )
    hybAttrList <- list(green=hybAttrGreen, red=hybAttrRed) 
    
    combinedObject <- new("arrayData",
                          spotAttr=spotAttr,
                          weights=weights,
                          intensities=intensities,
                          hybAttrList=hybAttrList)

    objectOne <- combinedObject

  }  
    
  return(objectOne)
  
}  
