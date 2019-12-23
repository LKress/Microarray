## possibly offer qualityParameters for a given arrayDataObject only, i.e.
## without an exprSetRGObject

# /**
#
# \name{qualityParameters}
#
# \title{Calculation of quality characteristics for
#        DNA microarray hybridisations}
#
# \alias{qualityParameters}
#
# \description{Several quality measures are calculated.
#              The return value, i.e. a list of quality scores,
#              should be used as input argument for the function
#              \code{\link{qualityDiagnostics}}.
#              For details on the quality measures
#              read the value section. } 
#
# @usage
#
# \value{returns a list of results, i.e. a
#   \code{data.frame} \code{qualityParameters} containing
#   several scores for each hybridisation, as well as
#   pairwise comparisons, i.e.
#   a matrix \code{slideDistance},
#   a matrix \code{slideDistanceLogRaw},
#   a matrix \code{slideDistanceGreen},
#   a matrix \code{slideDistanceGreenLogRaw},
#   a matrix \code{slideDistanceRed},
#   a matrix \code{slideDistanceRedLogRaw},
#   and an integer \code{replicateSpots},
#   i.e. the number of detected spot replicas.
# 
#   The matrix \code{slideDistanceLogRaw} contains a  calculated
#   distance (similarity) for each pair of slides$_{ij}$,
#   i.e. the median absolute deviation (mad)
#   taken over all spots of the log-ratio of the raw data;
#   alike the matrix \code{slideDistance}
#   the mad taken over all spots of the difference
#   of the log-ratios 
#   (here: the difference of the normalised and transformed
#          expression values of the two channels on the slide).
#   Similarly the matrices  \code{slideDistanceGreen},
#   \code{slideDistanceGreenLogRaw},
#   \code{slideDistanceRed},
#   and \code{slideDistanceRedLogRaw} contain calculated distances
#   for each pair of slides$_{ij}$ based on the mad of the difference
#   of the same channel (normalised or logged) taken over all spots.
#   
#
#   A brief summary of all parameters given in
#   the \code{data.frame} \code{qualityParameters}:
#
#   \code{width}
#   a robust estimate of the noise, i.e. the median absolute deviation
#   of the difference of the normalised channels taken over all spots, i.e.
#   the "width" of the scatterplot
#
#   \code{medianDistance}
#   a robust measure for the typical distance (similarity) of one slide
#   with all other slides, i.e. the median of the "distances"
#   between slides (c.f. \code{slideDistance}))
#
#   \code{correlation(LogRaw)}
#   of the expression values between the two normalised (log raw) channels
#   of the slide taken over all spots
#
#   \code{meanSignalGreen}
#   the mean taken over all spots of the green raw data channel 
#
#   \code{meanSignalRed}
#   the mean taken over all spots of the red raw data channel
#
#   \code{meanSignal}
#   mean taken over all spots of the raw data of both channels,
#
#   \code{signalRangeGreen}
#   the range between the 10th and 95th percentile
#   of the signal intensities given in the green raw data channel
#
#   \code{signalRangeRed}
#   the range between the 10th and 95th percentile
#   of the signal intensities given in the red raw data channel
#
#   \code{backgroundRangeGreen}
#   the range between the 10th and 95th percentile
#   of the background intensities given in the green raw data channel
#
#   \code{backgroundRangeRed}
#   the range between the 10th and 95th percentile
#   of the background intensities given in the red raw data channel
#
#   \code{signalToBackgroundGreen}
#   the ratio of the median signal intensity and the median background
#   intensity given in the green raw data channel
#
#   \code{signalToBackgroundRed}
#   the ratio of the median signal intensity and the median background
#   intensity given in the red raw data channel
#
#   \code{spotReplicatesConcordanceGreen(LogRaw)}
#    the median of the standard deviations of all spot replicas
#    for each unique identifier
#    of the normalised (log raw) green channel is calculated;
#    in case of duplicates, i.e.
#    \code{replicateSpots == 2},
#    the Pearson and Spearman correlation is calculated instead
#
#   \code{spotReplicatesConcordanceGreen(LogRaw)}
#    the median of the standard deviations of all spot replicas
#    for each unique identifier
#    of the normalised (log raw) green channel is calculated;
#    in case of duplicates, i.e.
#    \code{replicateSpots == 2},
#    the Pearson and Spearman correlation is calculated instead
#
#   \code{greenvsAllGreen} and \code{redvsAllRed}
#   the correlation between each channel is measured against the
#   averaged (median) channel over all hybridisations
#   (like a virtual reference) separately for each channel
# }
#
#
# \arguments{
#  \item{arrayDataObject}{object of type \code{\link{arrayData}};
#                         required; default: missing}
#  \item{exprSetRGObject}{object of type \code{\link{exprSetRG}};
#                         required; default: missing}
#  \item{spotIdentifier}{ character string; required; specifies a
#                         column of \code{getSpotAttr(arrayDataObject)};
#                         the column is used to determine spot replicas;
#                         default: "Name"}
#  \item{slideNameColumn}{ character string; required; specifies a
#                          column of \code{getHybAttr(arrayDataObject)};
#                          the column is used to extract the names
#                          of the hybridisations; if not found
#                          the hybridisations are consecutively numbered;
#                          default: "slideName"}
#  \item{identifiersToBeSkipped}{vector of character strings of
#                                spot identifiers to be excluded
#                                from calculations;
#                                required; default: \code{NA}}
#  \item{resultFileName}{character string; results are stored
#                        in a tab-deliminated file if supplied;
#                        default: missing}
#  \item{verbose}{logical; default \code{TRUE}}
# }
# 
# \details{For details on the quality measures read the value section.}
#
# \seealso{\code{\link{qualityDiagnostics}}}
#
#  \examples{
#      spotIdentifierVec <- c("A","A","Blank","B","B","Blank")
#      hybNames <- "H1"
#      R1 <- N1 <- c(1,1,9,2,2,10)
#      R2 <- N2 <- c(2,2,7,4,4,8)
#      rawDataIntensityValues <- array(0, dim=c(6,2,1))
#      rawDataIntensityValues[,1,] <- R1
#      rawDataIntensityValues[,2,] <- R2
#      dimnames(rawDataIntensityValues) <- list(NULL, c("green","red"), NULL)
#      spotAttr <- data.frame(Name=I(spotIdentifierVec))
#      hybAttr <- data.frame(slideName=I(hybNames))
#      arrayDataObject <- new("arrayData", intensities=rawDataIntensityValues, hybAttrList=list(red=hybAttr,green=hybAttr), spotAttr=spotAttr)
#      indGreen <- 1
#      indRed <- 2
#      channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#      colnames(channels) <- c("green","red")
#      exprSetRGObject <- new("exprSetRG", 	
#      exprs <- matrix(c(R1,R2), nrow=6, byrow=FALSE), phenoData=	
#          new("phenoData", pData=data.frame(matrix(0,nrow=2,ncol=1)),
#              varLabels=list(rep("varLabel1",1))), channels=channels)		
#      Re1 <- qualityParameters(arrayDataObject=arrayDataObject, exprSetRGObject=exprSetRGObject, identifiersToBeSkipped= "Blank")
#      stopifnot(all.equal.numeric(as.numeric(Re1$qualityParameters["H1",c("correlation")]),c(1)))
#      stopifnot(Re1$replicateSpots==2)
#
#   \dontshow{
#
#      hybNames <- c(hybNames,hybNames)
#      hybAttr=data.frame(slideName=I(hybNames))
#      rawDataIntensityValues <- array(0, dim=c(6,2,2))
#      dimnames(rawDataIntensityValues) <- list(NULL, c("green","red"), NULL)
#      rawDataIntensityValues[,1,] <- c(R1,R1)
#      rawDataIntensityValues[,2,] <- c(R2,R2)
#      arrayDataObject <- new("arrayData", intensities=rawDataIntensityValues, hybAttrList=list(red=hybAttr,green=hybAttr), spotAttr=spotAttr)
#      channels <- rbind(channels, channels+2)
#      exprSetRGObject <- new("exprSetRG", 	
#	exprs=matrix(c(R1,R1,R2,R2), nrow=6, byrow=FALSE), phenoData=	
#             new("phenoData", pData=data.frame(matrix(0,nrow=4,ncol=1)),
#              varLabels=list(rep("varLabel1",1))), channels=channels)
#      Re1 <- qualityParameters(arrayDataObject=arrayDataObject, exprSetRGObject=exprSetRGObject, identifiersToBeSkipped= "Blank")
#      stopifnot(Re1$replicateSpots==2)
#      stopifnot(all.equal.numeric(as.numeric(unlist(Re1$qualityParameters[, grep( "spotReplicatesConcordance", colnames(Re1$qualityParameters) )])), rep(1,(length(hybNames)*length(grep( "spotReplicatesConcordance", colnames(Re1$qualityParameters)) ))) ))
#
#      spotIdentifierVec <- c("Blank","A","A","A","B","B","B")
#      R1 <- N1 <- c(10,1,1,1,2,2,2)
#      R2 <- N2 <- c(11,2,2,2,4,4,4)
#      rawDataIntensityValues <- array(0, dim=c(7,2,1))
#      rawDataIntensityValues[,1,] <- R1
#      rawDataIntensityValues[,2,] <- R2
#      dimnames(rawDataIntensityValues) <- list(NULL, c("green","red"), NULL)
#      spotAttr=data.frame(Name=I(spotIdentifierVec))
#      hybNames <- "H1"
#      hybAttr=data.frame(slideName=I(hybNames))
#      arrayDataObject <- new("arrayData", intensities=rawDataIntensityValues, hybAttrList=list(red=hybAttr,green=hybAttr), spotAttr=spotAttr)
#      indGreen=1
#      indRed=2
#      channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#      colnames(channels) <- c("green","red")
#      exprSetRGObject <- new("exprSetRG", 	
#	exprs=matrix(c(R1,R2), nrow=7, byrow=FALSE), phenoData=	
#          new("phenoData", pData=data.frame(matrix(0,nrow=2,ncol=1)),
#              varLabels=list(rep("varLabel1",1))), channels=channels)		
#
#
#      Re2 <- qualityParameters(arrayDataObject=arrayDataObject, exprSetRGObject=exprSetRGObject, identifiersToBeSkipped= "Blank")
#
#      stopifnot(all.equal.numeric(as.numeric(Re2$qualityParameters["H1",c("correlation")]),c(1)))
#      stopifnot(Re2$replicateSpots==3)
#
#      rawDataIntensityValues[1,1,1] <- NA
#      intensities(arrayDataObject) <- rawDataIntensityValues
#      Re2NA <- qualityParameters(arrayDataObject=arrayDataObject, exprSetRGObject=exprSetRGObject, identifiersToBeSkipped= "Blank")
#
#      hybNames <- c("H1","H2")
#      Xr1 <- Xn1 <- cbind(R1,R2)
#      Xr2 <- Xn2 <- cbind(R2,R1)
#      rawDataIntensityValues <- array(0, dim=c(7,2,2))
#      rawDataIntensityValues[,1,1:2] <- Xr1
#      rawDataIntensityValues[,2,1:2] <- Xr2
#      dimnames(rawDataIntensityValues) <- list(NULL, c("green","red"), NULL)
#      spotAttr=data.frame(Name=I(spotIdentifierVec))
#      hybAttr=data.frame(slideName=I(hybNames))
#      arrayDataObject <- new("arrayData", intensities=rawDataIntensityValues, hybAttrList=list(red=hybAttr,green=hybAttr), spotAttr=spotAttr)
#      indGreen=1:2
#      indRed=3:4
#      channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#      colnames(channels) <- c("green","red")
#      exprSetRGObject <- new("exprSetRG", 	
#	exprs=as.matrix(cbind(Xr1,Xr2)), phenoData=	
#          new("phenoData", pData=data.frame(matrix(0,nrow=4,ncol=1)),
#              varLabels=list(rep("varLabel1",1))), channels=channels)	   
#
#      Re3 <- qualityParameters(arrayDataObject=arrayDataObject,
#                               exprSetRGObject=exprSetRGObject,
#                               identifiersToBeSkipped= "Blank",
#                               resultFileName=file.path(tempdir(),"qP.txt"))
#
#      Re3 <- qualityParameters(arrayDataObject=arrayDataObject,
#                               exprSetRGObject=exprSetRGObject,
#                               identifiersToBeSkipped= "Blank")
#
#      stopifnot(all.equal.numeric(as.numeric(Re3$qualityParameters["H2",c("correlation")]),c(1)))
#      stopifnot(Re3$replicateSpots==3)
#      exprs(exprSetRGObject)[2,c(2:4)] <- 0
#      Re3 <- qualityParameters(arrayDataObject=arrayDataObject,
#                               exprSetRGObject=exprSetRGObject,
#                               identifiersToBeSkipped= "Blank")
#      redHTwo <- median( c(sd(c(0,1,1)), sd(c(2,2,2) )))
#      redHOnegreenHTwo <- median( c(sd(c(0,2,2)), sd(c(4,4,4) )))
#      stopifnot( all.equal.numeric(Re3$qualityParameters$spotReplicatesConcordance.MedianSDGreen[2],redHOnegreenHTwo) )
#      stopifnot( all.equal.numeric(Re3$qualityParameters$spotReplicatesConcordance.MedianSDRed, c(redHOnegreenHTwo, redHTwo) ) )
#   }
#
#	}
#
# \keyword{utilities}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */


qualityParameters <- function(arrayDataObject,
                              exprSetRGObject,
                              spotIdentifier = "Name",
                              slideNameColumn = "slideName",
                              identifiersToBeSkipped=NA,
                              resultFileName,
                              verbose = TRUE
                              ){

  notes <- ""
  
  if( missing(exprSetRGObject) ){
    stop(" exprSetRGObject is missing in qualityParameters ", call.=FALSE)
  }
  
  if( missing(arrayDataObject) ){
    stop(" arrayDataObject is missing in qualityParameters ", call.=FALSE)
  }
  
  if( ! class(arrayDataObject) == "arrayData" ){
    stop(" arrayDataObject of wrong type in qualityParameters ", call.=FALSE)
  }
    
  if( ! class(exprSetRGObject) == "exprSetRG" ){
    stop(" exprSetRGObject of wrong type in qualityParameters ", call.=FALSE)
  }

  
  if( is.null(getIntensities(arrayDataObject)) ){
    stop(" getIntensities(arrayDataObject) is null in qualityParameters ", call.=FALSE)
  }
    
  if( ! all( c("green", "red") %in% dimnames(getIntensities(arrayDataObject))[[2]] ) ){
    stop(" getIntensities(arrayDataObject) is invalid in qualityParameters ", call.=FALSE)
  }

  
  nChannelGreen <- as.matrix(exprs(getExprSetGreen(exprSetRGObject)))
  nChannelRed   <- as.matrix(exprs(getExprSetRed(exprSetRGObject)))
  channelGreen  <- as.matrix(getIntensities(arrayDataObject)[,"green",])
  channelRed    <- as.matrix(getIntensities(arrayDataObject)[,"red",])

  if( "greenBackground" %in% dimnames(getIntensities(arrayDataObject))[[2]] ){
    channelGreenBack <-as.matrix(getIntensities(arrayDataObject)[,"greenBackground",])
  }else{
    channelGreenBack <- channelGreen * NA
  }
  if( "redBackground" %in% dimnames(getIntensities(arrayDataObject))[[2]] ){
    channelRedBack <- as.matrix(getIntensities(arrayDataObject)[,"redBackground",])
  }else{
    channelRedBack <- channelRed * NA
  }
  if( spotIdentifier %in% colnames(getSpotAttr(arrayDataObject)) ){
    spotIdentifierVec <- getSpotAttr(arrayDataObject)[,spotIdentifier]
  }else{
    stop(" spotIdentifier:",spotIdentifier," does not exist in colnames(getSpotAttr(arrayDataObject)) in qualityParameters", call.=FALSE)
  }
  if( slideNameColumn %in% colnames(getHybAttr(arrayDataObject)) ){
    hybNames <- getHybAttr(arrayDataObject)[, slideNameColumn]
  }else{
    extraNote <- paste(" slideNameColumn:",slideNameColumn," does not exist in colnames(getHybAttr(arrayDataObject))  in qualityParameters, \n", sep="")
    extraNote <- paste( extraNote,
                       "automatically generated indexes are used instead\n")
    notes <- paste( notes, extraNote, sep=";")
    cat(extraNote)
    hybNames <- as.character(1:dim(channelGreen)[2])
  }
  if( any(duplicated(hybNames)) ){
    extraNote <- paste(" Attention: slideNameColumn:",slideNameColumn,"\n contains non-unique entries in qualityParameters\n", sep="")
    extraNote <- paste(extraNote, "the entries are automatically converted to syntactically valid unique names\n")
    notes <- paste( notes, extraNote, sep=";")
    cat(extraNote)
    hybNames <- make.names(hybNames, unique=TRUE)
  }
  
  nrOfHybs <- length(hybNames)
  nrOfSpots <- length(spotIdentifierVec)

  dimVec <- c(nrOfSpots,nrOfHybs)

  if( any(dimVec != dim(nChannelGreen))    ||
      any(dimVec != dim(nChannelRed))      ||
      any(dimVec != dim(channelGreen))     ||
      any(dimVec != dim(channelRed))       ||
      any(dimVec != dim(channelGreenBack)) ||    
      any(dimVec != dim(channelRedBack))
     ){
    
    stop(" found channels of inconsistent dimensions in qualityParameters", call.=FALSE)
  }

  
  nonValidItems <- sapply(identifiersToBeSkipped, function(x) return(which( spotIdentifierVec == x)), simplify=TRUE )
  nonValidItems <- na.exclude(as.numeric(unlist(nonValidItems)))

  if( length(nonValidItems) > 0 ){

    extraNote <- paste(" the number of excluded spots in qualityParameters is ",length(nonValidItems), "\n",sep="")
    notes <- paste( notes, extraNote, sep=";")
    if(verbose){ cat(extraNote) }

    spotIdentifierVec <- spotIdentifierVec[-nonValidItems]
    nChannelGreen <- as.matrix(nChannelGreen[-nonValidItems,])
    nChannelRed   <- as.matrix(nChannelRed[-nonValidItems,])
    channelGreen  <- as.matrix(channelGreen[-nonValidItems,])
    channelRed    <- as.matrix(channelRed[-nonValidItems,])
    channelGreenBack <- as.matrix(channelGreenBack[-nonValidItems,])
    channelRedBack   <- as.matrix(channelRedBack[-nonValidItems,])
  }
  
  replicateSpots <- NA
  spotReplicatesConcordance <- matrix(NA, nrow=nrOfHybs, ncol=4)
  dimnames(spotReplicatesConcordance) <- list(hybNames,
                             c("Green", "Red", "GreenLogRaw", "RedLogRaw"))
  
  ## replicate spot correlation
  if( ! missing(spotIdentifierVec) ){

    if( ! any(duplicated(spotIdentifierVec)) ){
      ## single spots
      ## nothing to do

    }else if( max(table(spotIdentifierVec)) == 2 && min(table(spotIdentifierVec)) == 2 ){
      
      ## solely duplicate spots
      ## calculation of the correlation

      replicateSpots <- 2

      ##spotReplicas <- sapply(unique(spotIdentifierVec), function(x) return(which( spotIdentifierVec == x )) )
      spotReplicas <- sapply(split(1:length(spotIdentifierVec), spotIdentifierVec), function(x) return(x))
      
      spotReplicatesConcordance <- cbind(spotReplicatesConcordance,spotReplicatesConcordance)
      colnames(spotReplicatesConcordance) <- c("PearsonGreen",
                                               "PearsonRed",
                                               "PearsonGreenLogRaw",
                                               "PearsonRedLogRaw",
                                               "SpearmanGreen",
                                               "SpearmanRed",
                                               "SpearmanGreenLogRaw",
                                               "SpearmanRedLogRaw")

      for(i in 1:nrOfHybs){

        for( method in  c("pearson","spearman") ){

          if( method == "pearson" ){
            methodName <- "Pearson"
          }else if( method == "spearman" ){
            methodName <- "Spearman"
          }else{
            stop(" unexpected case in qualityParameters ")
          }
          
          spotReplicatesConcordance[i,paste(methodName,"Green",sep="")] <- cor(nChannelGreen[spotReplicas[1,],i], nChannelGreen[spotReplicas[2,],i], use="pairwise.complete.obs", method=method)
          spotReplicatesConcordance[i,paste(methodName,"Red",sep="")] <- cor(nChannelRed[spotReplicas[1,],i], nChannelRed[spotReplicas[2,],i], use="pairwise.complete.obs", method=method)
          
          spotReplicatesConcordance[i,paste(methodName,"GreenLogRaw",sep="")] <- cor(log(channelGreen[spotReplicas[1,],i]), log(channelGreen[spotReplicas[2,],i]), use="pairwise.complete.obs", method=method)
          spotReplicatesConcordance[i,paste(methodName,"RedLogRaw",sep="")] <- cor(log(channelRed[spotReplicas[1,],i]), log(channelRed[spotReplicas[2,],i]), use="pairwise.complete.obs", method=method)

        }## end of for method
      }## end of for nrOfHybs
      
    }else if( max(table(spotIdentifierVec)) == min(table(spotIdentifierVec))
                         && max(table(spotIdentifierVec)) > 2 ){  
      
      ## solely replicate spots of fixed number larger than two  
      ## calculation of the mad of the variance

      replicateSpots <- max(table(spotIdentifierVec))
      ##spotReplicas <- sapply(unique(spotIdentifierVec), function(x)
      ##                       return(which( spotIdentifierVec == x )) )
      spotReplicas <- sapply(split(1:length(spotIdentifierVec), spotIdentifierVec), function(x) return(x))
      
      colnames(spotReplicatesConcordance) <- c("MedianSDGreen",
                                               "MedianSDRed",
                                               "MedianSDGreenLogRaw",
                                               "MedianSDRedLogRaw")

      for(i in 1:nrOfHybs){
        
        spotReplicatesConcordance[i,"MedianSDGreen"] <-  median(apply(spotReplicas[,], 2, function(x) sd(nChannelGreen[x,i], na.rm=TRUE)), na.rm=TRUE)
        spotReplicatesConcordance[i,"MedianSDRed"] <-  median(apply(spotReplicas[,], 2, function(x) sd(nChannelRed[x,i], na.rm=TRUE)), na.rm=TRUE)

        spotReplicatesConcordance[i,"MedianSDGreenLogRaw"] <-  median(apply(spotReplicas[,], 2, function(x) sd(log(channelGreen[x,i]), na.rm=TRUE)), na.rm=TRUE)
        spotReplicatesConcordance[i,"MedianSDRedLogRaw"] <-  median(apply(spotReplicas[,], 2, function(x) sd(log(channelRed[x,i]), na.rm=TRUE)), na.rm=TRUE)

      }
      
    }else{

      ## any mixture of single, duplicate and multiple spots
      ## skipped so far

    }

  }else{## end of if not missing spotIdentifierVec
    extraNote <- " no spot identifiers available in qualityParameters \n"
    notes <- paste( notes, extraNote, sep=";")
    if( verbose ){ cat(extraNote) }
  }
    
  sigma <- matrix(NA, nrow=nrOfHybs, ncol=nrOfHybs)
  dimnames(sigma) <- list(hybNames,hybNames)
  sigmaLogRaw <- sigma
  sigmaGreenLogRaw <- sigma
  sigmaGreen <- sigma
  sigmaRedLogRaw <- sigma
  sigmaRed <- sigma
  
  medianSigma <- numeric(nrOfHybs) * NA
  names(medianSigma) <- hybNames
  sigmaVec <- numeric(nrOfHybs) * NA  ## width
  names(sigmaVec) <- hybNames
  corVec <- numeric(nrOfHybs) * NA
  names(corVec) <- hybNames
  corVecLogRaw <- numeric(nrOfHybs) * NA
  names(corVecLogRaw) <- hybNames
  
  meanIntVecGreen <- numeric(nrOfHybs) * NA
  names(meanIntVecGreen) <- hybNames
  meanIntVecRed <- numeric(nrOfHybs) * NA
  names(meanIntVecRed) <- hybNames

  signalRange <- matrix(NA, nrow=nrOfHybs, ncol=2)
  dimnames(signalRange) <- list( hybNames, c("Green","Red") )

  backgroundRange <- matrix(NA, nrow=nrOfHybs, ncol=2)
  dimnames(backgroundRange) <- list( hybNames, c("Green","Red") )

  signalToBackground <- matrix(NA, nrow=nrOfHybs, ncol=2)
  dimnames(signalToBackground) <- list( hybNames, c("Green","Red") )

  greenCorWithAvGreen <- numeric(nrOfHybs) * NA
  names(greenCorWithAvGreen) <- hybNames
  redCorWidthAvRed <-  numeric(nrOfHybs) * NA
  names(redCorWidthAvRed) <- hybNames
  
  ## cross comparison - pre-calculations
  if( nrOfHybs > 1 ){
    for(i in 1:(nrOfHybs-1)){

      vecI            <- nChannelGreen[,i]-nChannelRed[,i]
      vecIGreenLogRaw <- log(channelGreen[,i])
      vecIGreen       <- nChannelGreen[,i]
      vecIRedLogRaw   <- log(channelRed[,i])
      vecIRed         <- nChannelRed[,i]

      for(j in (i+1):nrOfHybs){

        vecJ            <- nChannelGreen[,j]-nChannelRed[,j]
        vecJGreenLogRaw <- log(channelGreen[,j])
        vecJGreen       <- nChannelGreen[,j]
        vecJRedLogRaw   <- log(channelRed[,j])
        vecJRed         <- nChannelRed[,j]

        sigma[i,j] <- sigma[j,i] <- mad(vecI - vecJ, na.rm=TRUE)
        sigmaLogRaw[i,j] <- sigmaLogRaw[j,i] <-
          mad( (vecIGreenLogRaw - vecIRedLogRaw) -
               (vecJGreenLogRaw - vecJRedLogRaw)   , na.rm=TRUE)
        sigmaGreen[i,j] <- sigmaGreen[j,i] <-
          mad(vecIGreen - vecJGreen, na.rm=TRUE)
        sigmaGreenLogRaw[i,j] <- sigmaGreenLogRaw[j,i] <-
          mad(vecIGreenLogRaw - vecJGreenLogRaw, na.rm=TRUE)
        sigmaRed[i,j] <- sigmaRed[j,i] <- mad(vecIRed - vecJRed, na.rm=TRUE)
        sigmaRedLogRaw[i,j] <- sigmaRedLogRaw[j,i] <-
          mad(vecIRedLogRaw - vecJRedLogRaw, na.rm=TRUE)
        
      }
    }
  }

  for(i in 1:nrOfHybs){

    corVec[i]       <- cor(nChannelGreen[,i],nChannelRed[,i],
                           use="pairwise.complete.obs")
    corVecLogRaw[i] <- cor(channelGreen[,i], channelRed[,i],
                           use="pairwise.complete.obs")
    sigmaVec[i]     <- mad(nChannelGreen[,i]-nChannelRed[,i], na.rm=TRUE)
    meanIntVecGreen[i] <- mean(channelGreen[,i], na.rm=TRUE)
    meanIntVecRed[i]   <- mean(channelRed[,i], na.rm=TRUE)
    signalToBackground[i, "Green"] <- median(channelGreen[,i], na.rm=TRUE) /
                                      median(channelGreenBack[,i], na.rm=TRUE)
    signalToBackground[i, "Red"] <- median(channelRed[,i], na.rm=TRUE)     /
                                    median(channelRedBack[,i], na.rm=TRUE)
    signalRange[i, "Green"] <- quantile(channelGreen[,i],
                                         probs=0.95, na.rm=TRUE) -
                               quantile(channelGreen[,i],
                                        probs=0.10, na.rm=TRUE)
    signalRange[i, "Red"] <- quantile(channelRed[,i],
                                      probs=0.95, na.rm=TRUE) -
                             quantile(channelRed[,i],
                                    probs=0.10, na.rm=TRUE)   
    backgroundRange[i, "Green"] <- quantile(channelGreenBack[,i],
                                            probs=0.95, na.rm=TRUE) -
                                   quantile(channelGreenBack[,i],
                                            probs=0.10, na.rm=TRUE)   
    backgroundRange[i, "Red"] <- quantile(channelRedBack[,i],
                                          probs=0.95, na.rm=TRUE) -
                                 quantile(channelRedBack[,i], 
                                        probs=0.10, na.rm=TRUE)   

    if( nrOfHybs > 1 ){
      medianSigma[i] <- median(sigma[i,-i], na.rm=TRUE)
    }
  }

  
  ## channel comparison with "averaged" channel
  averageFunction <- median
  averagedChannelGreen <- apply( nChannelGreen, 1, averageFunction)
  averagedChannelRed   <- apply( nChannelRed, 1, averageFunction)

  greenCorWithAvGreen <- apply( nChannelGreen, 2, function(x)
                               cor(x,averagedChannelGreen,
                                   use="pairwise.complete.obs") )
  redCorWithAvRed <- apply( nChannelRed, 2, function(x)
                           cor(x,averagedChannelRed,
                               use="pairwise.complete.obs") )



  resultDF <- data.frame(hybridisation = I(hybNames),
                         width         = sigmaVec,
                         medianDistance= medianSigma,
                         correlation      = corVec,
                         correlationLogRaw= corVecLogRaw,
                         meanSignalGreen= meanIntVecGreen,
                         meanSignalRed  = meanIntVecRed,
                         meanSignal     = (meanIntVecGreen + meanIntVecRed)/2,
                         signalRange       = signalRange,
                         backgroundRange   = backgroundRange,
                         signalToBackground= signalToBackground,
                         spotReplicatesConcordance=
                           spotReplicatesConcordance,
                         greenvsAllGreen= greenCorWithAvGreen,
                         redvsAllRed    = redCorWithAvRed)
    
  
  if( ! missing(resultFileName) ){
    ## write results to file
    qcTable <- cbind(getHybAttr(arrayDataObject), resultDF)
    write.table(qcTable, file=file.path(resultFileName), quote=FALSE, sep="\t",row.names=FALSE)
  }
  
  resultList <- list(qualityParameters=resultDF,
                     slideDistance = sigma,
                     slideDistanceLogRaw = sigmaLogRaw,
                     slideDistanceGreen = sigmaGreen,
                     slideDistanceGreenLogRaw = sigmaGreenLogRaw,
                     slideDistanceRed = sigmaRed,
                     slideDistanceRedLogRaw = sigmaRedLogRaw,
                     replicateSpots = replicateSpots,
                     notes = notes
                     )
  
  return(resultList)
  
}## end of function qualityParameters
