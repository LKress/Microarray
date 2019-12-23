
## possibly integrate arrayDataObject ?!
## anyway you may use an exprSetRGObject which simply contains the raw data
## another quick&dirty solution could offer:
## newTmpArrayDataObject <- new("arrayData", intensities=NULL,
##                              weights=NULL, hybAttrList=
##                              pDataSlide(exprSetRGSlideMerged),
##                              spotAttr=getSpotAttr(arrayDataObjectGiven))
## drawback here: you would have to check the consistency between
##                the arrayDataObject and exprSetRGObject; better
##                avoid this information duplication ...

## se.exprs: for variance/standard deviation pooling: cf. the t-test
## pooled unequal variance p of the t-test differs from rootMeanSquare
## when assumung equal number of observations N in each group k
## by the factor sqrt(K/N), i.e. p = sqrt(K/N) * rootMeanSquare


# /**
#
# \name{slideMerge}
#
# \title{Averaging of two colour microarray replicas}
#
# \alias{slideMerge}
#
# @usage
#
# \keyword{utilities}
#
# \description{ The mean of the expression values is calculated
#               separately for each channel.
#               If no \code{se.exprs} values are given
#               in \code{exprSetRGObject},
#               \code{se.exprs} is set to the standard deviation
#               of the expression values
#               (which is possibly \code{NA}).
#               If available it is set to the root-mean-square
#               or the mean of the given \code{se.exprs}
#               values depending on the
#               argument \code{seExprsHandling}.
#      The root-mean-square can be useful if the 
#      \code{se.exprs} values are estimated standard deviations
#      based on the same number of observations taken from identical
#      distributions.}
#
# \value{object of class \code{\link{exprSetRG-class}}, i.e.
#        the "merged" \code{exprSetRGObject}}
#
#
# \arguments{
#  \item{exprSetRGObject}{object of class \code{\link{exprSetRG}};
#                         required; default missing}
#  \item{slideMergeColumn}{character string specifying the variable
#            of the \code{phenoData} object of the \code{exprSetRGObject},
#            which is used to determine replicas; required; default missing}
#  \item{sampleAnnotationColumns}{ vector of character strings;
#      optional; default missing.
#      A vector which contains all \code{phenoData} variables
#      relevant for further analysis.
#      The \code{phenoData}-annotation should be consistent for slide replicas.
#      By default the argument \code{sampleAnnotationColumns} is missing
#      and all \code{phenoData} variables are used.}
#  \item{seExprsHandling}{ character string;
#          either "rootMeanSquare" or "mean";
#          required; default "rootMeanSquare" }
#  \item{verbose}{ logical; required; default: \code{TRUE} }
#
# }
# 
# \details{
#    The \code{phenoData}-annotation should be consistent for slide replicas,
#    The annotation within the group of replicas is checked for consistency
#    to assure a meaningful slide merge operation. Any ambiguities are
#    reported. This reporting shall help to avoid and discover slide
#    annotation errors. For example if samples are hybridized twice,
#    the "slideNumber" will not be consistent within the "sampleID"
#    which is used to determine replicas via the argument
#    \code{slideMergeColumn}.
# }
#
# \seealso{\code{\link{spotMerge}},
#          \code{\link{exprSetRG-class}} }
#
# \examples{
#
#  indGreen=1:2
#  indRed=3:4
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  exprsMatrix <- matrix(rep(1:10,4),nrow=10,ncol=4,byrow=FALSE)
#  phenoMatrix <- matrix(c(c(1,2),c(3,3),c(5,5)),nrow=2,ncol=3,byrow=FALSE)
#  colnames(phenoMatrix) <- c("one","two","usedForMerge")
#  phenoMatrix <- rbind(phenoMatrix,phenoMatrix)
#  eSA <- new("exprSetRG", exprs=exprsMatrix, phenoData=
#             new("phenoData", pData=data.frame(phenoMatrix, check.names=FALSE),
#                 varLabels=as.list(colnames(phenoMatrix))),
#             channels=channels)
#  eSM <- slideMerge(exprSetRGObject=eSA, slideMergeColumn="usedForMerge")
#  eSAOne <- slideSubset(eSA,j=c(1))
#  stopifnot( all(exprs(eSAOne) == exprs(eSM) ))
#  stopifnot( all( se.exprs(eSM) == 0 ) )
#
# \dontshow{
#
#
#  eSA2 <- new("exprSetRG", exprs=exprsMatrix, se.exprs=exprsMatrix,
#              phenoData= new("phenoData", pData=data.frame(phenoMatrix, check.names=FALSE),
#              varLabels=as.list(colnames(phenoMatrix))),
#              channels=channels)
#  eSM2 <- slideMerge(exprSetRGObject=eSA2, slideMergeColumn="usedForMerge")
#  eSA2One <- slideSubset(eSA2,j=c(1))
#  stopifnot( all(exprs(eSA2One) == exprs(eSM2) ) )
#  stopifnot( all(sqrt(0.5*((se.exprs(eSA2One)*se.exprs(eSA2One))+(se.exprs(eSA2One)*se.exprs(eSA2One)))) == se.exprs(eSM2)) ) 
#
#
#  indGreen=1:4
#  indRed=5:8
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  exprsMatrix <- matrix(c(rep(1:10,3),rep(rep(0,10),5)),
#                        nrow=10,ncol=8,byrow=FALSE)
#  phenoMatrix <- matrix(c(c(1:8),rep(1,8),c(1,2,1,3,1,2,1,3)),
#                        nrow=8,ncol=3,byrow=FALSE)
#  colnames(phenoMatrix) <- c("one","two","usedForMerge")
#  eSA3 <- new("exprSetRG", exprs=exprsMatrix, phenoData=
#             new("phenoData", pData=data.frame(phenoMatrix, check.names=FALSE),
#                 varLabels=as.list(colnames(phenoMatrix))),
#             channels=channels)
#  eSM3 <- slideMerge(exprSetRGObject=eSA3, slideMergeColumn="usedForMerge")
#
#  eSM3One <- slideSubset(eSM3,j=c(1))
#  eSM3Two <- slideSubset(eSM3,j=c(2))
#  eSM3Three <- slideSubset(eSM3,j=c(3))
#  stopifnot( all(exprs(eSM3Three) == 0) )
#  stopifnot( all( is.na(se.exprs(eSM3Three)) ) )
#
#  stopifnot( all( is.na(se.exprs(eSM3Two)) ) )
#  stopifnot( exprs(getExprSetGreen(eSM3Two)) == c(1:10) )
#  stopifnot( all(exprs(getExprSetRed(eSM3Two)) == 0) )
#  stopifnot( exprs(getExprSetGreen(eSM3One)) == (c(1:10)) )
#  stopifnot( all(exprs(getExprSetRed(eSM3One)) == 0) )
#  stopifnot( all(se.exprs(eSM3One) == 0) )
#
#  exprsMatrix <- matrix(c(rep(1:10,2),rep(rep(0,10),6)),
#                      nrow=10,ncol=8,byrow=FALSE)
#  eSA3 <- new("exprSetRG", exprs=exprsMatrix, phenoData=
#             new("phenoData", pData=data.frame(phenoMatrix, check.names=FALSE),
#                 varLabels=as.list(colnames(phenoMatrix))),
#             channels=channels)
#  eSM3 <- slideMerge(exprSetRGObject=eSA3, slideMergeColumn="usedForMerge")
#  eSM3One <- slideSubset(eSM3,j=c(1))
#  eSM3Two <- slideSubset(eSM3,j=c(2))
#  eSM3Three <- slideSubset(eSM3,j=c(3))
#  stopifnot( all(exprs(eSM3Three) == 0 ))
#  stopifnot( all( is.na(se.exprs(eSM3Three)) ) )
#
#  stopifnot( all( is.na(se.exprs(eSM3Two)) ) )
#  stopifnot( exprs(getExprSetGreen(eSM3Two)) == c(1:10) )
#  stopifnot( all(exprs(getExprSetRed(eSM3Two)) == 0) )
#  stopifnot( exprs(getExprSetGreen(eSM3One)) == 0.5*(c(1:10)) )
#  stopifnot( all(exprs(getExprSetRed(eSM3One)) == 0 ))
#  stopifnot( all(se.exprs(getExprSetRed(eSM3One)) == 0 ))
#  stopifnot( (se.exprs(getExprSetGreen(eSM3One)) == apply(cbind(rep(0,10),c(1:10)),1,sd) ))
#
#
#
#  indGreen=1:4
#  indRed=5:8
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  exprsMatrix <- matrix(c(rep(1:10,3),rep(rep(0,10),5)),
#                        nrow=10,ncol=8,byrow=FALSE)
#  se.exprsMatrix <- array(1, dim(exprsMatrix) )
#  phenoMatrix <- matrix(c(c(1:8),rep(1,8),c(1,2,1,3,1,2,1,3)),
#                        nrow=8,ncol=3,byrow=FALSE)
#  colnames(phenoMatrix) <- c("one","two","usedForMerge")
#  eSA4 <- new("exprSetRG", exprs=exprsMatrix, se.exprs=se.exprsMatrix,
#              phenoData= new("phenoData", pData=data.frame(phenoMatrix, check.names=FALSE),
#                 varLabels=as.list(colnames(phenoMatrix))),
#             channels=channels)
#  eSM4 <- slideMerge(exprSetRGObject=eSA4, slideMergeColumn="usedForMerge")
#
#  eSM4One <- slideSubset(eSM4,j=c(1))
#  eSM4Two <- slideSubset(eSM4,j=c(2))
#  eSM4Three <- slideSubset(eSM4,j=c(3))
#  stopifnot( all(exprs(eSM4Three) == 0) )
#  stopifnot( all(se.exprs(eSM4Three) == 1 ) ) 
#  stopifnot( all(se.exprs(eSM4Two) == 1 ) ) 
#  stopifnot( exprs(getExprSetGreen(eSM4Two)) == c(1:10) )
#  stopifnot( all(exprs(getExprSetRed(eSM4Two)) == 0) )
#  stopifnot( exprs(getExprSetGreen(eSM4One)) == (c(1:10)) )
#  stopifnot( all(exprs(getExprSetRed(eSM4One)) == 0) )
#  stopifnot( all(se.exprs(eSM4One) == 1) ) 
#
#
#  indGreen=1:4
#  indRed=5:8
#  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#  colnames(channels) <- c("green","red")
#  myMatrix <- matrix(c(1:8,rep(1:4,2),c(1,1,1,4,2,2,2,2),c(2,2,2,2,4,1,1,1) ,rep(c(1,1,2,2),2)),nrow=8,ncol=5,byrow=FALSE)
#  colnames(myMatrix) <- c("one","two","three","four","merge")
#  exprsMatrix <-  matrix(rep(1:10,4), ncol=8, nrow=5, byrow=FALSE) 
#  eSA <- new("exprSetRG", exprs=exprsMatrix, phenoData=
#                new("phenoData", pData=data.frame(myMatrix, check.names=FALSE),
#                varLabels=as.list(colnames(myMatrix))),
#              channels=channels)
#  eSM <- slideMerge(exprSetRGObject=eSA, slideMergeColumn="merge")
#  stopifnot( all(  dim(exprs(eSM)) == c(5,4) ) )
#  stopifnot( all(as.matrix(pData((phenoDataSlide(eSM)[,which(varLabels(phenoDataSlide(eSM)) == "slidesInGroup")]))) == matrix(c("1;2","3;4"),ncol=1,nrow=2)) )
#	  exprs(eSM)
#  stopifnot( all(exprs(getExprSetGreen(eSM)) == cbind(rowMeans(exprs(getExprSetGreen(eSA))[,1:2]), rowMeans(exprs(getExprSetGreen(eSA))[,3:4]))))
#  stopifnot( all(exprs(getExprSetRed(eSM)) == cbind(rowMeans(exprs(getExprSetRed(eSA))[,1:2]), rowMeans(exprs(getExprSetRed(eSA))[,3:4]))))
#  stopifnot(se.exprs(getExprSetGreen(eSM)) == cbind(as.matrix(apply((exprs(getExprSetGreen(eSA))[,1:2]),1,sd)), as.matrix(apply((exprs(getExprSetGreen(eSA))[,3:4]),1,sd))))
#  stopifnot(se.exprs(getExprSetRed(eSM)) == cbind(as.matrix(apply((exprs(getExprSetRed(eSA))[,1:2]),1,sd)), as.matrix(apply((exprs(getExprSetRed(eSA))[,3:4]),1,sd))))
#  stopifnot( all( match( unlist(varLabels(phenoDataSlide(eSM))), c("two", "greenSpecific_three", "greenSpecific_four", "redSpecific_three", "redSpecific_four", "merge", "slidesInGroup", "redSpecific_one", "greenSpecific_one")) ) )
#  }
#
#	}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

slideMerge <- function(exprSetRGObject,
                       slideMergeColumn,
                       sampleAnnotationColumns,
                       seExprsHandling="rootMeanSquare",
                       verbose=TRUE){

  
  if( missing(exprSetRGObject) ){
    cat(" argument exprSetRGObject is missing in slideMerge \n")
    return()
  }
  if( missing(slideMergeColumn) ){
    cat(" argument slideMergeColumn is missing in slideMerge \n")
    return()
  }
  if( length(slideMergeColumn) != 1 ){
    cat(" argument slideMergeColumn must be of length 1  in slideMerge \n")
    return()
  }
  if( ! slideMergeColumn %in% varLabels(phenoDataSlide(exprSetRGObject)) ){
    cat(" slideMergeColumn ", slideMergeColumn,
        " not given in varLabels(phenoDataSlide) in slideMerge \n")
    cat(" varLabels(phenoDataSlide):",
        paste(varLabels(phenoDataSlide(exprSetRGObject)),collapse=", "),
        " in slideMerge \n")
    return()
  }

  if( ! length(seExprsHandling) == 1 ){
    cat(" argument seExprsHandling must be of length 1 in slideMerge \n")
    return()
  }
  if( ! is.character(seExprsHandling) ){
    cat(" argument seExprsHandling must be of type character in slideMerge \n")
    return()
  }
  if( ! seExprsHandling %in% c("rootMeanSquare", "mean")  ){
    cat(paste(" unknown option :", seExprsHandling, ": for argument seExprsHandling in slideMerge \n", sep=""))
    return()
  }
  if( seExprsHandling == "mean" ){
    seExprsHandlingFunction <- mean
  }else if( seExprsHandling == "rootMeanSquare" ){
    sqr <- function(x) x*x
    rootMeanSquare <- function(x){ return( sqrt(mean(sqr(x))) ) }
    seExprsHandlingFunction <- rootMeanSquare
  }else{
    stop(" unexpected case for seExprsHandling in slideMerge ", call.=FALSE)
  }
  
  if(verbose){ cat("\n\n running merging of slides \n") }
  
  phenoSlide <-  phenoDataSlide(exprSetRGObject)
  if( any( duplicated(unlist(varLabels(phenoSlide))) ) ){
    if( verbose ){
      cat(" found duplicated varLabels(phenoDataSlide(exprSetRGObject)) in slideMerge \n")
    }
  }
  slideMergeColumnIndex <- which(varLabels(phenoSlide) == slideMergeColumn)
  if( length(slideMergeColumnIndex) > 1 ){
    cat(" more than one slideMergeColumn :", slideMergeColumn,
        ": found in in varLabels(phenoDataSlide(exprSetRGObject)) in slideMerge \n")
    cat(" varLabels(phenoDataSlide(exprSetRGObject)):",
        paste(varLabels(phenoSlide),collapse=", "),
        " in slideMerge \n")
    return()
  }
  slideGroups<-as.vector(as.matrix(pData(phenoSlide[, slideMergeColumnIndex])))
  

 
  phenoGreen <- phenoDataGreen(exprSetRGObject)
  phenoRed <- phenoDataRed(exprSetRGObject)
   
  if( ! missing(sampleAnnotationColumns) ){
    if( ! all(sampleAnnotationColumns %in% varLabels(phenoSlide)) ){
      cat(" not all sampleAnnotationColumns are  given in varLabels(phenoDataSlide(exprSetRGObject)) in slideMerge \n")
      cat(" varLabels(phenoDataSlide(exprSetRGObject)):", paste(varLabels(phenoSlide),collapse=", "), " in slideMerge \n")
      return()
    }

    ## implicit drop=FALSE
    phenoGreen <- phenoGreen[,which(varLabels(phenoGreen) %in% sampleAnnotationColumns)]
    phenoRed <- phenoRed[,which(varLabels(phenoRed) %in% sampleAnnotationColumns)]
    
  }

  ## implicit drop=FALSE
  phenoGreenMerged <- phenoGreen[ ! duplicated(slideGroups), ]
  phenoRedMerged <- phenoRed[ ! duplicated(slideGroups), ]

  
  if( verbose ){
    cat(" number of replicas for each group\n")
    tmpTable <- table(as.factor(slideGroups))
    cat(" group names:\n")
    cat(cat(names(tmpTable)));cat("\n")
    cat(" number of replicas:\n")
    cat(tmpTable);cat("\n")
  }
    
  uniqueGroups <- unique(slideGroups)

  exprsDataGreen <-  exprs(getExprSetGreen(exprSetRGObject))
  tmpExprsDataGreen<-array(0,dim=c(nrow(exprsDataGreen),length(uniqueGroups)))
  sdMergeGreen <- tmpExprsDataGreen * NA
  
  exprsDataRed <-  exprs(getExprSetRed(exprSetRGObject))
  tmpExprsDataRed <- array(0, dim=c(nrow(exprsDataRed), length(uniqueGroups)))
  sdMergeRed <- tmpExprsDataRed * NA

  geneNames <- geneNames(exprSetRGObject)
  
  se.exprsIndicator <- se.exprs(exprSetRGObject)
  if( length(se.exprsIndicator) < 1 ){
    se.exprsIndicator <- NULL                    ## force == NULL
  }else{
    if( ! all(dim(se.exprsIndicator) == dim(exprs(exprSetRGObject)) ) ){
      stop(" se.exprs dimensions must match dimensions of exprSetRGObject in slideMerge ")
    }
    se.exprsIndicator <- matrix(0,nrow=0,ncol=0) ## force != NULL
    se.exprsGreen <- se.exprs(getExprSetGreen(exprSetRGObject))
    se.exprsRed <- se.exprs(getExprSetRed(exprSetRGObject))
    if( ! all(dim(se.exprsGreen) == dim(exprsDataGreen)) ){
      stop(" se.exprs dimensions of the green channel are invalid in slideMerge ")
    }
    if( ! all(dim(se.exprsRed) == dim(exprsDataRed)) ){
      stop(" se.exprs dimensions of the red channel are invalid in slideMerge ")
    }
    se.exprsGreenMerge <- tmpExprsDataGreen * 0
    se.exprsRedMerge <- tmpExprsDataRed * 0
  }
  
  slidesInGroup <- vector(mode="character",length=length(uniqueGroups))

  for(i in 1:length(uniqueGroups)){
    
    groupSelection <- which(slideGroups==uniqueGroups[i])
    slidesInGroup[i] <- paste(groupSelection, collapse=";")

    tmpExprsDataGreen[,i] <- apply(exprsDataGreen[, groupSelection,drop=FALSE],1,mean)
    sdMergeGreen[,i] <- apply(exprsDataGreen[, groupSelection, drop=FALSE],1,sd)
    tmpExprsDataRed[,i] <- apply(exprsDataRed[, groupSelection, drop=FALSE],1,mean)
    sdMergeRed[,i] <- apply(exprsDataRed[, groupSelection ,drop=FALSE],1,sd)

    if( ! is.null( se.exprsIndicator ) ){

      se.exprsGreenMerge[,i] <- apply(se.exprsGreen[, groupSelection, drop=FALSE],1, seExprsHandlingFunction)
      se.exprsRedMerge[,i] <- apply(se.exprsRed[, groupSelection, drop=FALSE],1, seExprsHandlingFunction)

    }
    
  }


  consistencyCheck <- function(phenoDataObj, slideGroups, slideMergeColumn){
    
    inconsistentVariables <- vector(mode="character", length=0)
    varLabelsList <- varLabels(phenoDataObj)
    slideMergeIndex <- which( varLabels(phenoDataObj) == slideMergeColumn)
    slideMergeDataColumn <- as.vector(as.matrix(
                               pData(phenoDataObj[,slideMergeIndex])))
    
    for( i in varLabelsList ){
      iVarLabelIndex <- which( varLabels(phenoDataObj) == i )
      if( i != slideMergeColumn ){
        if( ! all(unlist(
          lapply(as.list(uniqueGroups), function(x) return(
                 length(unique(as.vector(as.matrix(pData(phenoDataObj[
                          slideMergeDataColumn == x, iVarLabelIndex]))))) == 1)
                 )))){
          
          if( verbose ){
            cat(" merging inconsistency for phenoData/annotation variable: ",i,"\n")
          }
          inconsistentVariables <- append(inconsistentVariables, i)

        }
      }
    }## end of for i

    return(inconsistentVariables)
    
  }## end of consistencyCheck

  generateInconsistentData <- function( myVector, slideGroups ){

    slideGroups <- as.character(slideGroups)
    stopifnot( length(myVector) == length(slideGroups) )
    uniqueGroups <- unique(slideGroups)
    newVector <- vector(mode="character", length=length(uniqueGroups))
    names(newVector) <- uniqueGroups
    
    for( group in uniqueGroups ){
      newVector[group] <- paste(myVector[which(slideGroups == group)], collapse="; ")
    }
    return( newVector )
    
  }## end of generateInconsistentData
  
  INCONSISTENCYTEXT <- "ambiguity while merging with respect to column"
  if( verbose ){
    cat("\n checking merging consistency of phenoDataGreen \n")
  }
  inconsistentInGreen <- consistencyCheck(phenoDataObj=phenoGreen, slideGroups=slideGroups, slideMergeColumn=slideMergeColumn)
  for(i in which( varLabels(phenoGreenMerged) %in% inconsistentInGreen)){
    inconsistentData <- generateInconsistentData(phenoGreen[[i]], slideGroups)
    phenoGreenMerged[[i]] <- paste( paste(INCONSISTENCYTEXT,slideMergeColumn, sep=" "), inconsistentData, sep=" : ")
  }

  if( verbose ){
    cat("\n checking merging consistency of phenoDataRed \n")
  }
  inconsistentInRed <- consistencyCheck(phenoDataObj=phenoRed, slideGroups=slideGroups, slideMergeColumn=slideMergeColumn)
  for(i in which( varLabels(phenoRedMerged) %in% inconsistentInRed)){
    inconsistentData <- generateInconsistentData(phenoRed[[i]], slideGroups)
    phenoRedMerged[[i]] <- paste( paste(INCONSISTENCYTEXT,slideMergeColumn, sep=" "), inconsistentData, sep=" : ")
  }
  
  exprsData <- as.matrix(cbind( tmpExprsDataGreen, tmpExprsDataRed ) )
  
  if( ! all(unlist(varLabels(phenoRedMerged)) == unlist(varLabels(phenoGreenMerged))) ){
    stop(" unexpected case in slideMerge ")
  }

  newColumnName <- "slidesInGroup"
  existingColumnNames <- varLabels(phenoGreenMerged)
  while( length( grep(pattern=newColumnName,
                      x=unlist(existingColumnNames)) ) > 0 ){
    newColumnName <- paste(newColumnName, "X", sep="")
  }
  varLabelsList <- append(existingColumnNames, newColumnName )

  ## does it preserve the data ?!
  pDataObj <- rbind(cbind(pData(phenoGreenMerged), I(slidesInGroup)),
                    cbind(pData(phenoRedMerged),   I(slidesInGroup)) )
  colnames(pDataObj) <- unlist(varLabelsList)
  phenoDataObj <- new("phenoData", pData=pDataObj, varLabels=varLabelsList)


  indGreen <- 1:ncol(tmpExprsDataGreen)
  indRed <- (ncol(tmpExprsDataGreen)+1):
            (ncol(tmpExprsDataGreen)+ncol(tmpExprsDataRed))

  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
  colnames(channels) <- c("green","red")

  if( is.null( se.exprsIndicator ) ){

    se.exprsData <- as.matrix(cbind(sdMergeGreen,sdMergeRed))
    notes <- paste(notes(exprSetRGObject)," and se.exprs is based on the standard deviation of the slide merge operation")

  }else{

    se.exprsData <- as.matrix(cbind(se.exprsGreenMerge,se.exprsRedMerge))
    notes <- paste(notes(exprSetRGObject)," and se.exprs is based on the ", seExprsHandling, " of the se.exprs values of the merged slides")
    
  }
  
  newObject <- new("exprSetRG",
                   exprs=exprsData,
                   se.exprs=se.exprsData,
                   notes=notes,
                   phenoData=phenoDataObj,
                   channels=channels,
                   annotation=annotation(exprSetRGObject))

  description(newObject) <- description(exprSetRGObject)
  
  geneNames(newObject) <- geneNames

  if( verbose ){ cat("\n\n slide merge operation finished \n") }
  
  return(newObject)

  
}
