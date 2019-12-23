# /**
#
# \name{writeToFile}
#
# \title{Writes a textual representation of an exprSetRG
#        and/or arrayData object}
#
# \alias{writeToFile}
#
# @usage
#
# \description{\code{writeToFile} generates a well-formated
#              tab-deliminated output file of data
#              stored in the \code{\link{exprSetRG}} and/or
#              \code{\link{arrayData}} objects.
#              Different "views" on either a corresponding pair of
#              an \code{\link{exprSetRG}}- and an
#              \code{\link{arrayData}}-object or of a
#              single object itself are possible.
#              The argument \code{channels} allows to specify the "views".
#              The microarray slide or channel annotation, as well as
#              the spot annotation is aligned to the data block
#              if given.
#              For further restriction and control of the output,
#              e.g. slide subsets or ordering of slides, you may use
#              the subsetting operations offered by the
#              \code{\link{exprSetRG-class}} and
#              \code{\link{arrayData-class}} beforehand
#              (e.g. slideSubset and [] ).
#             }
#
# \value{The function is called for its side effect, i.e. writing a file.}
#
#
# \arguments{
#  \item{arrayDataObject}{object of class \code{\link{arrayData}};
#                         optional/required,
#                         cf. \code{channels}; default: missing}
#  \item{exprSetRGObject}{object of class \code{\link{exprSetRG}};
#                         optional/required,
#                         cf. \code{channels}; default: missing}
#  \item{additionalDataMatrix}{class \code{matrix}; optional; default: missing.
#                              The rows must correspond to those of the
#                              data objects; \code{colnames} must be supplied.}
#  \item{rowSelection}{vector of indexes; optional; default: missing}
#  \item{slideNameColumn}{character string; optional; default: missing;
#     must refer to a valid column in
#     \code{\link{getHybAttr}}(\code{arrayDataObject})
#     or in \code{\link{pDataSlide}}(\code{exprSetRGobject}) }
#  \item{channels}{vector of character strings; default: \code{c("logRatio")};
#       valid character strings are:
#       "logRatio", "green", "red", "greenRelative", "redRelative",
#       "rawGreen", "rawRed", "rawGreenRelative", "rawRedRelative",
#       "se.exprsLogRatio", "se.exprsGreen", "se.exprsRed".
#       "Raw"-types must not mix with not "raw"-types and vice versa.
#       "logRatio"-types must not mix with "colour"-types.
#       The "raw" types require the argument \code{arrayDataObject},
#       all other types requires at least the argument \code{exprSetRGObject}.}
#  \item{fileName}{character string; required; default: "dumpedData.txt"}
#  \item{savePath}{character string; required; default: "."}
#  \item{fullOutput}{logical; adds \code{phenoData} information at the top
#                   of the table ; by default: \code{TRUE}}
#  \item{coding}{logical; adds integer-coded-\code{phenoData} information
#                at the top to the table only if \code{coding = TRUE} and
#                \code{fullOutput = TRUE}; default: \code{FALSE}}
# }
# 
# \details{}
#
# \seealso{
#           \code{\link{exprSetRG-class}},
#           \code{\link{arrayData-class}},
#           \code{\link{write.htmltable}}
#         }
#
# \examples{
#  \dontshow{
#   indGreen <- c(2,4,6); indRed <- c(1,3,5)
#   channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#   colnames(channels) <- c("green","red")
#   myPData <- data.frame(commonOne=c(1,1,2,2,3,3), commonTwo=c(1,1,1,1,2,2), redOne=c(1,NA,2,NA,3,NA), redTwo=c(1,-1,2,-1,3,-1), greenOne=c(NA,10,NA,20,NA,30), greenTwo=c(-10,10,-10,20,-10,30))
#   myPhenoData <- new("phenoData", pData=myPData, varLabels=as.list(colnames(myPData)))
#   e <- new("exprSetRG", exprs=matrix(1,nrow=10,ncol=6), se.exprs=matrix(0,nrow=10,ncol=6), phenoData=myPhenoData, channels=channels)
#   writeToFile(exprSetRGObject=e, savePath=tempdir())
#   writeToFile(exprSetRGObject=e, savePath=tempdir(), channels=c("red", "greenRelative"))
#   writeToFile(exprSetRGObject=e, channels=c("se.exprsLogRatio"), savePath=tempdir() )
#   writeToFile(exprSetRGObject=e, channels=c("logRatio","se.exprsLogRatio"), slideNameColumn="commonTwo", savePath=tempdir() )
#   writeToFile(exprSetRGObject=e, channels=c("logRatio","se.exprsLogRatio"), slideNameColumn="commonTwo", rowSelection=1:5, coding=TRUE, savePath=tempdir() )
#   spotAttr <- data.frame(spots=c("a","b","c","d","e","f","1","2","3","4"))
#   hL <- list(green=myPData[,indGreen],red=myPData[,indRed])
#   aD <- new("arrayData", spotAttr=spotAttr, hybAttrList=hL)
#   writeToFile(exprSetRGObject=e, arrayDataObject=aD, savePath=tempdir())
#   hL <- list(green=myPData[,indGreen],red=myPData[,indGreen]) ## Attention green twice
#   intensities <- array(data=runif(120),dim=c(10,2,6))
#   dimnames(intensities) <- list(NULL, c("green","red"), NULL)
#   aD <- new("arrayData", spotAttr=spotAttr, hybAttrList=hL, intensities=intensities)
#   writeToFile(arrayDataObject=aD, channels="rawRed", savePath=tempdir())
#   writeToFile(arrayDataObject=aD, channels=c("rawRed", "rawRedRelative"), savePath=tempdir())
#   writeToFile(arrayDataObject=aD, channels=c("rawGreen", "rawRedRelative") ,coding=TRUE, savePath=tempdir())
#   writeToFile(arrayDataObject=aD, channels=c("rawGreen", "rawRedRelative") , slideNameColumn="commonTwo", savePath=tempdir())
#  }
# }
#
# \keyword{IO}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */


writeToFile <- function(
                        arrayDataObject,
                        exprSetRGObject,
                        additionalDataMatrix,
                        rowSelection,
                        slideNameColumn,
                        channels=c("logRatio"),
                        fileName="dumpedData.txt",
                        savePath=".",
                        fullOutput=TRUE,
                        coding=FALSE
                        ){

  ##
  
  if( ! is.logical(fullOutput) ){
    stop(" fullOutput must be of type logical in writeToFile \n", call.=FALSE)
  }
  if( ! is.logical(coding) ){
    stop(" coding must be of type logical in writeToFile \n", call.=FALSE)
  }
  if( coding & !fullOutput ){
    stop(" coding only applies if fullOutput==TRUE in writeToFile \n", call.=FALSE)
  }

  ##
  
  fullFileName <- file.path(savePath,fileName)
  
  ####

  aChannels <- c( "rawGreen", "rawRed", "rawGreenRelative", "rawRedRelative" )
  eLogChannels <- c( "logRatio", "se.exprsLogRatio" )
  eColourChannels <- c( "green", "red", "greenRelative", "redRelative",
                       "se.exprsGreen", "se.exprsRed" )
  eChannels <- c( eLogChannels, eColourChannels )
  channelValues <- c(aChannels, eChannels)
  channelsAsText <- paste(channels,collapse=",")
  
  if( ! all(channels %in% channelValues) ){
    stop(paste(" invalid 'channels' selection:",channelsAsText,"\n"," all possible valid items are as follows: ", channelValues, sep=""), "\n end of invalid 'channels' selection in writeToFile \n", call.=FALSE)
  }

  ##
  
  if( any( channels %in% eChannels ) ){
    
    if( missing(exprSetRGObject) ){
      stop(paste(" exprSetRGObject is required for ", channelsAsText," but missing in writeToFile \n"), call.=FALSE)
    }
    if( class(exprSetRGObject) != "exprSetRG" ){
      stop(paste(" invalid class:", class(exprSetRGObject)," of exprSetRGObject in writeToFile \n",sep=""), call.=FALSE)
    }
  }

  ##
  
  if( any( channels %in% aChannels ) ){
    
    if( missing(arrayDataObject) ){
      stop(paste(" arrayDataObject is required for ", channelsAsText," but is missing in writeToFile \n"), call.=FALSE)
    }
    if( class(arrayDataObject) != "arrayData" ){
      stop(paste(" invalid class:", class(arrayDataObject)," of arrayDataObject in writeToFile \n",sep=""), call.=FALSE)
    }
    
  }
  
  ##
  
  if(any( channels %in% eChannels )  &&
     any( channels %in% aChannels )      ){
    stop(paste(" \"Raw\" type and other channels must no mix; but got ", channelsAsText," in writeToFile "), call.=FALSE)
  }

  if(any( channels %in% eLogChannels )  &&
     any( channels %in% eColourChannels )      ){
    stop(paste(" \"logRatio\" type and  \"colour\" type channels must no mix; but got ", channelsAsText," in writeToFile "), call.=FALSE)
  }
 
  ##### consistency check
  
  if( ! missing(exprSetRGObject) & ! missing(arrayDataObject) ){

    nrOfExprRows <- dim(exprs(exprSetRGObject))[1]
    spotAttr <- getSpotAttr(arrayDataObject) ## data.frame
    nrOfSpotRows <- dim(spotAttr)[1]
    
    if( nrOfSpotRows != nrOfExprRows ){
      stop(paste(" number of rows in arrayDataObject:",nrOfSpotRows,": and number of rows in exprSetRGObject:",nrOfExprRows,": do not match in writeToFile \n",sep=""), call.=FALSE)
    }
    
    nrOfExprSlides <- dim(exprs(getExprSetLogRatio(exprSetRGObject)))[2]
    if( ! is.null( getHybAttr(arrayDataObject) ) ){
      nrOfADSlides <- dim(getHybAttr(arrayDataObject))[1]      
      if( nrOfADSlides != nrOfExprSlides ){
        stop(paste(" number of slides in arrayDataObject:", nrOfADSlides,": and number of slides in exprSetRGObject:",nrOfExprSlides,": do not match in writeToFile \n",sep=""), call.=FALSE)
      }
    }
    
  }

  
  ##

  if( any( channels %in% eChannels ) ){
    
    nrOfRows <- dim(exprs(exprSetRGObject))[1]
    nrOfSlides <- dim(exprs(getExprSetLogRatio(exprSetRGObject)))[2]
    if( all( channels %in% eLogChannels ) ){
      nrOfVarLabels <-  length(unlist(varLabels(phenoDataSlide(exprSetRGObject))))
    }else if( all( channels %in% eColourChannels ) ){
      nrOfVarLabels <-  length(unlist(varLabels(phenoData(exprSetRGObject))))
    }else{
      stop(" unexpected case I in writeToFile", call.=FALSE)
    }
    
  }else if(any( channels %in% aChannels )  ){

    if( is.null( getIntensities(arrayDataObject) ) ){
      stop(" getIntensities(arrayDataObject) is required, but is NULL in writeToFile ", call.=FALSE)
    }
    nrOfRows <- dim(getIntensities(arrayDataObject))[1]
    nrOfSlides <- dim(getIntensities(arrayDataObject))[3]

    hybAttr <- getHybAttr(arrayDataObject)
    if( is.null(hybAttr) ){
      stop(" getHybAttr(arrayDataObject) is required, but is NULL in writeToFile ", call.=FALSE)
    }
    nrOfVarLabels <- dim(hybAttr)[2]
    
  }else{
    stop(" unexpected case II in writeToFile", call.=FALSE )
  }
  ####
  
  if( ! missing( arrayDataObject ) ){
    
    if( ! is.null(getSpotAttr(arrayDataObject)) ){
      
      spotAttr <- getSpotAttr(arrayDataObject) ## data.frame
      nrOfSpotColumns <- dim(spotAttr)[2]
      
    }else{

      spotAttr <- data.frame(I(as.character(1:nrOfRows)))
      colnames(spotAttr) <- "Index"
      nrOfSpotColumns <- 1
    }
    
  }else{
    
    if( ! is.null(geneNames(exprSetRGObject)) ){
      
      sdf <- data.frame(I(geneNames(exprSetRGObject)),check.names=FALSE)
      if( ! is.null(annotation(exprSetRGObject)) ){
        colName <- annotation(exprSetRGObject)
      }else{
        colName <- "geneNames"
      }
    }else{
      sdf <- data.frame(I(as.character(1:nrOfRows)))
      colName <- "Index"
    }
    
    spotAttr <- sdf
    colnames(spotAttr) <- colName
    nrOfSpotColumns <- 1
    
  }
    
  ####
  
  if( ! missing(additionalDataMatrix) ){
    if( class(additionalDataMatrix) != "matrix" ){
      stop(paste(" invalid class:", class(additionalDataMatrix)," of additionalDataMatrix in writeToFile \n",sep=""), call.=FALSE)

    }
    if( dim(additionalDataMatrix)[1] != nrOfRows ){
      stop(" number of rows in additionalDataMatrix and exprSetRGObject must match in writeToFile \n", call.=FALSE)

    }
    if( is.null(colnames(additionalDataMatrix) ) ){
      stop(" additionalDataMatrix must have colnames in writeToFile \n", call.=FALSE)

    }
  }
  
  ####

  if( ! missing( slideNameColumn ) ){
    
    if( ! missing(arrayDataObject) ){
      hybAttr <- getHybAttr(arrayDataObject)
    }else{
      hybAttr <- NULL
    }
    if( ! missing(exprSetRGObject) ){
      myPData <- pDataSlide(exprSetRGObject)
    }else{
      myPData <- NULL
    }
      
    if(! slideNameColumn %in% colnames(hybAttr) &&
       ! slideNameColumn %in% colnames(myPData)    ){
      
      stop(paste(" slideNameColumn :", slideNameColumn, ": neither found in colnames(getHybAttr(arrayDataObject)) nor colnames(pDataSlide(exprSetRGObject)) in writeToFile \n"), call.=FALSE)
      
    }else if(slideNameColumn %in% colnames(hybAttr) &&
             slideNameColumn %in% colnames(myPData)    ){
      
      slidesAD <- hybAttr[,slideNameColumn]
      slidesE <- myPData[,slideNameColumn]
      
      if( ! all.equal( slidesAD, slidesE ) ){
        stop(paste(" slideNameColumn :", slideNameColumn, ": differs in getHybAttr(arrayDataObject) and pDataSlide(exprSetRGObject) in writeToFile \n"), call.=FALSE)

      }else{
        slides <- slidesE
      }
      
    }else if( slideNameColumn %in% colnames(hybAttr) ){

      slides <- hybAttr[,slideNameColumn]
      
    }else if( slideNameColumn %in% colnames(myPData) ){

      slides <- myPData[,slideNameColumn]

    }else{
      stop(" unexpected case in writeToFile \n ", call.=FALSE)

    }
    
  }else{
    slides <- as.character(1:nrOfSlides)
  }
  
  #############

  allValueMatrixCols <- NULL
  allValueMatrix <- NULL
  emptyColumn <- matrix(data="",nrow=nrOfRows,ncol=1)
  emptySpotBlock <- matrix(data="",nrow=nrOfVarLabels,ncol=nrOfSpotColumns)
  fullOutputFrame <- emptySpotBlock
  fullCodingOutputFrame <- emptySpotBlock
  
  for( channel in channels){
    
    ## generation of a value matrix
    if( channel == "logRatio" ){
      tmpPhenoData <- phenoDataSlide(exprSetRGObject)
      valueMatrix <- exprs(getExprSetLogRatio(exprSetRGObject)) ## matrix
      valueMatrixCols <- paste("logRatioGreenRedOf",slides,sep="")

    }else if( channel == "green" || channel == "greenRelative" ){
      tmpPhenoData <- phenoDataGreen(exprSetRGObject)
      valueMatrix <- exprs(getExprSetGreen(exprSetRGObject)) ## matrix
      valueMatrixCols <- paste("GreenOf",slides,sep="")

    }else if( channel == "red" || channel == "redRelative" ){
      tmpPhenoData <- phenoDataRed(exprSetRGObject)
      valueMatrix <- exprs(getExprSetRed(exprSetRGObject)) ## matrix
      valueMatrixCols <- paste("RedOf",slides,sep="")

    }else if( channel == "rawGreen" || channel == "rawGreenRelative" ){
      tmpPData <- getHybAttrGreen(arrayDataObject)
      tmpPhenoData <- new("phenoData", pData=tmpPData,
                          varLabels=as.list(colnames(tmpPData)))
      valueMatrix <- getIntensities(arrayDataObject)
      tmpIndex <- which( dimnames(valueMatrix)[[2]] == "green" )
      valueMatrix <- controlledSubsetting(valueMatrix,
                                          ranges=list(NA,tmpIndex,NA),
                                          drop=2)
      valueMatrixCols <- paste("rawGreenOf",slides,sep="")
      
    }else if( channel == "rawRed" || channel == "rawRedRelative" ){
      tmpPData <- getHybAttrRed(arrayDataObject)
      tmpPhenoData <- new("phenoData", pData=tmpPData,
                          varLabels=as.list(colnames(tmpPData)))
      valueMatrix <- getIntensities(arrayDataObject)
      tmpIndex <- which( dimnames(valueMatrix)[[2]] == "red" )
      valueMatrix <- controlledSubsetting(valueMatrix,
                                          ranges=list(NA,tmpIndex,NA),
                                          drop=2)
      valueMatrixCols <- paste("rawRedOf",slides,sep="")

    }else if( channel == "se.exprsLogRatio" ){
      tmpPhenoData <- phenoDataSlide(exprSetRGObject)
      valueMatrix <- se.exprs(getExprSetLogRatio(exprSetRGObject))
      valueMatrixCols <-  paste("se.exprsLogRatioOf",slides,sep="")

    }else if( channel == "se.exprsGreen" ){
      tmpPhenoData <- phenoDataGreen(exprSetRGObject)
      valueMatrix <- se.exprs(getExprSetGreen(exprSetRGObject))
      valueMatrixCols <-  paste("se.exprsGreenOf",slides,sep="")

    }else if( channel == "se.exprsRed" ){
      tmpPhenoData <- phenoDataRed(exprSetRGObject)
      valueMatrix <- se.exprs(getExprSetRed(exprSetRGObject))
      valueMatrixCols <-  paste("se.exprsRedOf",slides,sep="")      

    }else{
      stop(" unknown channel :",channel,": in writeToFile\n", call.=FALSE)
    }

    ## relative
    if( length(grep( "elative", channel )) > 0 ){

      valueMatrix <- aperm(simpleApply(valueMatrix,dimension=2,func=rank,funcResultDimensionality=nrOfRows)/nrOfRows,c(2,1))
      valueMatrixCols <- paste("relative", valueMatrixCols, sep="")
      
    }

    if( sum(dim(valueMatrix)) == 0  ){
      stop(" no data found; probably se.exprs requested but not available in writeToFile ", call.=FALSE)
    }
    
    ## phenoData annotation
    if( is.null(unlist(varLabels(tmpPhenoData))) ){
      stop(" no slide/channel annotation column names found in writeToFile ", call.=FALSE)
    }
    varLabelsVec <- unlist(varLabels(tmpPhenoData))    
    dim(varLabelsVec) <- c(length(varLabelsVec),1)
    pDataObject <- pData(tmpPhenoData)  
    fullOutputFrame <- cbind(fullOutputFrame, varLabelsVec, t(pDataObject))

    ## integer coded phenoData annotation
    pDataCodes <- apply(pDataObject,2, function(x) as.integer(as.factor(x)))
    fullCodingOutputFrame <- cbind(fullCodingOutputFrame, varLabelsVec, t(pDataCodes))
    
    allValueMatrixCols <- c(allValueMatrixCols, "", valueMatrixCols)
    allValueMatrix <- cbind(allValueMatrix, emptyColumn, valueMatrix)
    
    
  }## end of for channel in channels loop


  
  if( ! missing(additionalDataMatrix) ){
    
    allValueMatrixCols <- c(allValueMatrixCols, "", colnames(additionalDataMatrix))
    allValueMatrix <- cbind(allValueMatrix, emptyColumn, additionalDataMatrix)
    
  }

  
  ## if fullOutput == TRUE : generation of a header
  ## which contains the phenoData information
  if( fullOutput ){
    APPEND <- TRUE

    write.table(fullOutputFrame, file=fullFileName, quote=FALSE, sep="\t",row.names=FALSE, col.names=FALSE)
    
     ## integer coded pData
    if( coding && fullOutput ){
      
      write.table(fullCodingOutputFrame, file=fullFileName, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE, append=TRUE)
      
    }
  }else{
    APPEND <- FALSE
  }

  ## defines the first line of the data block which
  ## contains the spotAttr followed by an empty columm
  ## followed by the value matrix

  outputColNames <- c(colnames(spotAttr),allValueMatrixCols)
  dim(outputColNames) <- c(1,length(outputColNames))

  if( missing(rowSelection) ){
    rowSelection <- 1:nrOfRows
  }

  outputFrame <-  cbind(spotAttr, allValueMatrix)[rowSelection,]
  
  write.table(outputColNames, file=fullFileName, quote=FALSE, sep="\t",row.names=FALSE, col.names=FALSE, append=APPEND)
  write.table(outputFrame, file=fullFileName, quote=FALSE, sep="\t",row.names=FALSE, col.names=FALSE, append=TRUE)

}


