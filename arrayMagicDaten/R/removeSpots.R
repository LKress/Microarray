# /**
#
# \name{removeSpots}
#
# \title{Remove specified spots from objects}
#
# \alias{removeSpots}
#
# @usage
#
# \description{All elements matching the strings in \code{spotsToBeRemoved}
#              are taken out of the object \code{arrayDataObject} and 
#              the corresponding ones out of the
#              object \code{exprSetRGObject} as well,
#              if supplied.
#              }
#
# \value{A named list containing
#        an object of type \code{\link{arrayData}} labelled "arrayDataObject",
#        an object of type \code{\link{exprSetRG}} labelled "exprSetRGObject"
#        and an integer specifying the number of removed items
#        labelled "nrOfRemovedItems".
#        }
#
#
# \arguments{
#
#  \item{arrayDataObject}{object of type \code{\link{arrayData}};
#                         required; default: missing}
#  \item{exprSetRGObject}{object of type \code{\link{exprSetRG}};
#                         optional; default: \code{NULL}}
#  \item{spotIdentifier}{character string, i.e. name of the
#                        column of \code{getSpotAttr(arrayDataObject)}
#                        used for matching;
#                        default "Name"}
#  \item{spotsToBeRemoved}{vector of character strings;
#                          default: \code{NULL}}
# }
# 
# \details{}
#
# \seealso{\code{\link{arrayData-class}},
#          \code{\link{exprSetRG-class}}
#         }
#
# \examples{
#
#  intensities <- array(data=runif(600),dim=c(100,2,3))
#  dimnames(intensities) <- list(NULL, c("green","red"), NULL)
#  arrayDataObject <- new("arrayData", intensities=intensities, spotAttr=data.frame(Name=I(rep(c("x","y","Blank","Blank","z"),20))), hybAttrList=NULL)
#  res <- removeSpots(arrayDataObject, spotsToBeRemoved=c("x","z"))
#  stopifnot( dim(getIntensities(res[["arrayDataObject"]]))[1] == 3*20 )
#
#   \dontshow{
#
#  indGreen=1:3
#  indRed=4:6
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  exprSetRGObject <- new("exprSetRG", exprs=matrix(data=runif(600),nrow=100,ncol=6), phenoData=new("phenoData", pData=data.frame(matrix(0,nrow=6,ncol=1)), varLabels=list(rep("varLabel1",1))), channels=channels)
#  res2 <- removeSpots(arrayDataObject=arrayDataObject,exprSetRGObject=exprSetRGObject, spotsToBeRemoved=c("x","z"))
#  stopifnot( dim(exprs(res2[["exprSetRGObject"]]))[1] == 3*20 )
#  res3 <- removeSpots(arrayDataObject=arrayDataObject,exprSetRGObject=exprSetRGObject, spotsToBeRemoved=c("ggrrrr"))
#  stopifnot( dim(exprs(res3[["exprSetRGObject"]]))[1] == 100 )
#  stopifnot( dim(getIntensities(res3[["arrayDataObject"]]))[1] == 100 )
#
#   }
#  }
#
# \keyword{utilities} ## required
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

removeSpots <- function(arrayDataObject,
                        exprSetRGObject=NULL,
                        spotsToBeRemoved=NULL,
                        spotIdentifier="Name"){

  ##
  
  if( missing(arrayDataObject) ){
     stop(" arrayDataObject is missing in removeSpots ")
   }
  if( ! class(arrayDataObject) == "arrayData" ){
    stop(" wrong class for object arrayDataObject in removeSpots ")
  }

  ##
  
  if( ! is.null(exprSetRGObject) ){
    if( ! class(exprSetRGObject) == "exprSetRG" ){
      stop(" wrong class for object exprSetRGObject  in removeSpots ")
    }
    if( dim(exprs(exprSetRGObject))[1] != dim(getIntensities(arrayDataObject))[1] ){
      stop(paste(" number of rows of exprSetRGObject:", (dim(exprs(exprSetRGObject))[1]), " and arrayDataObject:", (dim(getIntensities(arrayDataObject))[1])," differ in removeSpots"))
    }
    if(  dim(exprs(exprSetRGObject))[2]/2 != dim(getIntensities(arrayDataObject))[3] ){
      stop(paste(" number of hybridisations of exprSetRGObject:", (dim(exprs(exprSetRGObject))[2]/2)," and of arrayDataObject:",  (dim(getIntensities(arrayDataObject))[3])," differ in removeSpots"))
    }
  }

  ##
  
  spotAttr <- getSpotAttr(arrayDataObject)

  defaultReturnValue <- list(arrayDataObject=arrayDataObject, exprSetRGObject=exprSetRGObject, nrOfRemovedItems=0)

  ##
  
  if( is.null(spotAttr) ){
    cat(" getSpotAttr(arrayDataObject) is NULL in removeSpots, skipped operation ")
    return(defaultReturnValue)
  }
  if( length(spotAttr) < 1 ){
    cat(" getSpotAttr(arrayDataObject) has length < 1 in removeSpots, skipped operation ")
    return(defaultReturnValue)
  }

  ##
  
  if(is.null(spotIdentifier)){
    cat(" spot identifier is NULL in removeSpots, skipped operation ")
    return(defaultReturnValue)
  }
  if( all(is.na(spotIdentifier)) |
     length(spotIdentifier) != 1 ){
    cat(" no spot identifier given in removeSpots, skipped operation ")
    return(defaultReturnValue)
  }

  ##
  
  if( ! (spotIdentifier %in% colnames(spotAttr)) ){
    cat(" no appropriate column in getSpotAttr(arrayDataObject) in function removeSpots, skipped operation ")
    return(defaultReturnValue)
  }

  ##
  
  if(is.null(spotsToBeRemoved)){
    cat(" spotsToBeRemoved is NULL in removeSpots, skipped operation \n")
    return(defaultReturnValue)
  }
  if( any(is.na(spotsToBeRemoved)) ){
    cat(" spotsToBeRemoved contain NA in removeSpots, skipped operation \n")
    return(defaultReturnValue)
  }
  if( length(spotsToBeRemoved) < 1 ){
    cat(" no spotsToBeRemoved given in removeSpots, skipped operation \n")
    return(defaultReturnValue)
  }

  ##
  
  removeItems <- sapply(spotsToBeRemoved, function(x) return(which( spotAttr[,spotIdentifier] == x)), simplify=TRUE )
  removeItems <- na.exclude(as.numeric(unlist(removeItems)))

  nrOfRemovedItems <- length(removeItems)
  
  if( nrOfRemovedItems == 0 ){

    newAData <- arrayDataObject
    if( ! is.null(exprSetRGObject) ){
      newEData <- exprSetRGObject
    }else{
      newEData <- NULL
    }
    
  }else{

    newAData <- arrayDataObject[-removeItems,]

    if( ! is.null(exprSetRGObject) ){
      newEData <- exprSetRGObject[-removeItems,]
    }else{
      newEData <- NULL
    }
  }
  
  return(list(arrayDataObject=newAData,
              exprSetRGObject=newEData,
              nrOfRemovedItems=nrOfRemovedItems))
  
}
