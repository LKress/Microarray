# /**
#
# \name{processArrayDataObject}
#
#
# \title{Automated processing and normalisation of an arrayData-object
#        (part of function processArrayData)}
#
# \alias{processArrayDataObject}
#
# \keyword{utilities}
#
# \description{ Automated processing and normalisation of an
#               \code{\link{arrayData}}-object.
#               The function is called
#               by \code{\link{processArrayData}}.
#	}
#
# \value{
#        A list of objects, i.e.  an "exprSetRGObject" and 
#        "arrayDataObject"
#        with corresponding class types
#        \code{\link{exprSetRG-class}},
#        \code{\link{arrayData-class}}.
#       }
#
# @usage
#
# \arguments{
#
#  \item{arrayDataObject}{object of class \code{\link{arrayData-class}};
#                         required; default missing
#                        }
#
#
#
#  \item{spotIdentifier}{ character string; required; default "Name".
#                         \code{spotIdentifier} specifies the column
#                         in the image analysis result files which contain
#                         spot or gene identifiers.}
#  \item{verbose}{ logical; required; default: \code{TRUE} }
#
#
#
#  \item{normalisationMethod}{character string; required; default: "vsn";
#        cf. argument \code{method} of \code{\link{normalise}} }
#  \item{subtractBackground}{ logical; required; default: \code{FALSE}
#        cf. \code{\link{normalise}} }
#  \item{spotsRemovedBeforeNormalisation}{ vector of character strings;
#       required; default \code{NULL}.
#       All spots which match the string(s) will be excluded  
#       already before normalisation and will not be present
#       in the resulting objects at all.
#       }
#  \item{spotsRemovedAfterNormalisation}{ vector of character strings;
#       required; default: \code{NULL};
#       cf. argument \code{spotsRemovedBeforeNormalisation}
#       }
#  \item{subGroups}{character string or \code{NULL};
#                   required; cf. \code{\link{normalise}};
#                   default: \code{NULL} }
#  \item{channelsSeparately}{logical; required;
#                            cf. \code{\link{normalise}};
#                            default: \code{FALSE}}
#  \item{hybridisationGroups}{list of numeric vectors;
#                            cf. \code{\link{normalise}};
#                            required; default: \code{NULL}}
#
# }
#
# \examples{
#       LOADPATH <- file.path(.path.package("arrayMagic"), "extdata")
# 	SLIDEDESCRIPTIONFILE <- "slideDescription"
#
#       slideDescription <- readpDataSlides(
#                             loadPath=LOADPATH, 
#                             slideDescriptionFile=SLIDEDESCRIPTIONFILE
#                           )
# 	arrayDataObject <- readIntensities(
#                             loadPath=LOADPATH,
#                             slideDescription=slideDescription
#                          )
#
#       resultList <- processArrayDataObject( arrayDataObject=arrayDataObject )
#         
# }
# 
#
# \seealso{ \code{\link{processArrayData}},
#           \code{\link{normalise}},
#           \code{\link{exprSetRG-class}},
#           \code{\link{arrayData-class}}
#         }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
#
# */
processArrayDataObject <- function(
                                   arrayDataObject,
                                   ## options also used in
                                   ## processArrayData and ...
                                   spotIdentifier = "Name",
                                   verbose = TRUE,
                                   ## options also used in processArrayData
                                   normalisationMethod = "vsn",
                                   subtractBackground=FALSE,
                                   spotsRemovedBeforeNormalisation = NULL,
                                   spotsRemovedAfterNormalisation = NULL,
                                   subGroups = NULL,
                                   channelsSeparately = FALSE,
                                   hybridisationGroups = NULL
                           ){


  notes <- ""
  
  ## ################
  ## checks related to the normalisation
  
  
  if( ! normalisationMethod %in% c("loess", "loessScale", "loessQuantile", "quantile", "vsn", "none") ){
    stop(" invalid normalisationMethod ",normalisationMethod," in processArrayDataObject ")
  }

  ## ################
  ## general checks

  if( missing(arrayDataObject) ){
    stop(" arrayDataObject is missing in processArrayDataObject ", call.=FALSE)
  }
  if( ! class(arrayDataObject) == "arrayData" ){
    stop(" wrong class of object arrayDataObject in processArrayDataObject ", call.=FALSE)
  }

  ##
  
  spotAttr <- getSpotAttr(arrayDataObject)

  if( is.null(spotAttr) ){
    stop(" getSpotAttr(arrayDataObject) is NULL in processArrayDataObject ", call.=FALSE)
  }
  if( length(spotAttr) < 1 ){
    stop(" getSpotAttr(arrayDataObject) has length < 1 in processArrayDataObject ", call.=FALSE)
  }
  if(is.null(spotIdentifier)){
    stop(" spot identifier is NULL in processArrayDataObject ", call.=FALSE)
  }
  if( all(is.na(spotIdentifier)) |
     length(spotIdentifier) != 1 ){
    stop(" no spot identifier given in processArrayDataObject ", call.=FALSE)
  }
  if( ! spotIdentifier %in% colnames(spotAttr) ){
    stop(" spotIdentifer not found in colnames(getSpotAttr(arrayDataObject)) in processArrayDataObject ", call.=FALSE)
  }


  
  ###########
  
  ## spot removal before any analysis, even before normalisation
  removeSpotsResult <- removeSpots(arrayDataObject, spotsToBeRemoved=spotsRemovedBeforeNormalisation, spotIdentifier=spotIdentifier)
  arrayDataObject <- removeSpotsResult[["arrayDataObject"]]
  nrOfRemovedItems <- removeSpotsResult[["nrOfRemovedItems"]]

  extraNotes <- paste(" before normalisation ", nrOfRemovedItems, " spots are excluded from further analysis",sep="")
  notes <- paste( notes, extraNotes, sep=";")
  
  if(verbose){ cat("\n"); cat(extraNotes); cat("\n\n") }

  if( dim(getIntensities(arrayDataObject))[1] <= 1 ){
    stop(" stopped - only one or zero spots  are left after spot removal before normalisation", call.=FALSE)
  }

  ##
  
  exprSetRGObject <- normalise(
                               arrayDataObject=arrayDataObject,
                               subtractBackground=subtractBackground,
                               method=normalisationMethod,
                               subGroups=subGroups,
                               channelsSeparately=channelsSeparately,
                               hybridisationGroups=hybridisationGroups,
                               verbose=verbose,
                               spotIdentifier=spotIdentifier
                               )

  ##
  
  removeSpotsResultList <- removeSpots(arrayDataObject=arrayDataObject, exprSetRGObject=exprSetRGObject, spotsToBeRemoved=spotsRemovedAfterNormalisation, spotIdentifier=spotIdentifier)
  arrayDataObject <- removeSpotsResultList[["arrayDataObject"]]
  exprSetRGObject <- removeSpotsResultList[["exprSetRGObject"]]
  nrOfRemovedItems <- removeSpotsResultList[["nrOfRemovedItems"]]
  extraNotes <- paste(" after normalisation ", nrOfRemovedItems, " spots are excluded from further analysis",sep="")
  notes <- paste( notes, extraNotes, sep=";")
  
  if(verbose){ cat("\n"); cat(extraNotes); cat("\n\n") }

  if( dim(exprs(exprSetRGObject))[1] <= 1 ){
    stop(" stopped - only one or zero spots  are left after spot removal afer normalisation in processArrayDataObject", call.=FALSE)
  }

  ## possibly non unique identifiers are available now and not in normalise
  ## due to the second spot removal operation
  if( annotation(exprSetRGObject) == "" ){

    spotIds <- spotAttr[,spotIdentifier]
    if( ! any(duplicated(spotIds)) ){ 

      geneNames(exprSetRGObject) <- spotIds
      annotation <- paste("geneNames correspond to the column :",spotIdentifier,": of getSpotAttr(arrayDataObject) and have been set in function processArrayDataObject", sep="")

      annotation(exprSetRGObject) <- annotation


    }else{
      
      extraNotes <- paste(" non-unique identifiers found in",
                          " getSpotAttr(arrayDataObject)[,spotIdentifier],",
                          " no spot/gene names have been added to",
                          " the exprSetRG",
                          " at the end of processArrayDataObject",
                          sep="")
      notes <- paste(notes, extraNotes, sep=";")
      if(verbose){
        cat("\n");cat(extraNotes);cat("\n")
      }## if verbose

    }    
  }

  firstNotes <- notes(exprSetRGObject)
  notes(exprSetRGObject) <- paste(firstNotes, notes, sep=";")

  ## setting of sampleNames, i.e. slideNames
  slideNames <- dimnames(getIntensities(arrayDataObject))[[3]]
  if( ! is.null(slideNames) ){
    sampleNames <- c(slideNames,slideNames)[c(getIndRed(exprSetRGObject), getIndGreen(exprSetRGObject))]
    colnames(exprs(exprSetRGObject)) <- sampleNames
    ##duplicated rownames are not allowed ...
    ##rownames(pData(exprSetRGObject)) <- sampleNames
  }
    
  resultList <- list(exprSetRGObject=exprSetRGObject,
                     arrayDataObject=arrayDataObject
                     )

  return(resultList)
  
}
