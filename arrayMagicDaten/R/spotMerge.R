## not entirely clear how to deal with preexisting se.exprs; cf.
## slideMerge and getExprSetLogRatio
##
## allow different number of spot replicas to be merged as well
## (already implemented cf. Simone protein chips)
##
## implement more generic merging/summarizing of any kind of data
##
## identifiersToBeSkipped should not be required

# /**
#
# \name{spotMerge}
#
# \title{Averaging of spot replicas}
#
# \alias{spotMerge}
#
# @usage
#
# \keyword{utilities}
#
# \description{
# The mean of replicated spots with identical number of replicas is
# calculated by default for each channel separately
# (note: \code{spotMerge} does require an equal number of replicas).
# Other summarization functions can be applied if supplied as argument
# (cf. argument \code{mergeFunc})
# All other spots need to be eliminated with the help of the
# argument \code{identifiersToBeSkipped}.
# The mean (if not otherwise specified via argument \code{mergeFunc})
# is calculated for the expression levels
# \code{exprs} of the \code{exprSetRGObject} and for the
# \code{intensities} and \code{weights} of the \code{arrayDataObject}.
# Any existing \code{se.exprs}-\code{matrix}
# as part of \code{exprSetRGObject} is discarded.
# The standard deviation of the spot merge operation for the expression
# values is returned as \code{se.exprs}.
# The corresponding rows of \code{spotAttr} in \code{arrayDataObject}
# are concatenated and form a single row of respectively more columns.
#}
#
# \value{A list containing the "merged" \code{exprSetRGObject} and
#        \code{arrayDataObject}.}
#
#
# \arguments{
#  \item{arrayDataObject}{object of type \code{\link{arrayData}};
#                         required; default: missing}
#  \item{exprSetRGObject}{object of type \code{\link{exprSetRG}};
#                         required; default: missing}
#  \item{spotIdentifier}{ character string; required; default: "Name" }
#  \item{identifiersToBeSkipped}{ vector of character strings,
#       all spots which identifiers match are skipped; 
#       required; default: \code{NULL}}
#  \item{mergeFunc}{ function; required; default: \code{mean}}
# }
# 
# \details{}
#
# \seealso{  \code{\link{processArrayData}},
#            \code{\link{slideMerge}},
#            \code{\link{exprSetRG-class}},
#            \code{\link{arrayData-class}}
# }
#
# \examples{
#
#    intensities <- array(data=runif(600),dim=c(100,2,3))
#    dimnames(intensities) <- list(NULL, c("green","red"), NULL)
#    spotAttr <- data.frame(Name=I(c("y","x","k","l","z")[gl(5,20)]),
#                           Zahl=rep(c(1,2,3,4,5),20),
#                           Index=c(1:100))
#    arrayDataObject <- new("arrayData", intensities=intensities, weights=intensities[,1,],
#                           spotAttr=spotAttr, hybAttrList=NULL)
#    indGreen=1:3
#    indRed=4:6
#    channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#    colnames(channels) <- c("green","red")
#    exprMatrix <- matrix(data=1:600,nrow=100,ncol=6,byrow=FALSE)
#    pD <- data.frame(matrix(0,nrow=6,ncol=1))
#    exprSetRGObject <- new("exprSetRG", exprs=exprMatrix, se.expr=exprMatrix,
#                              phenoData=new("phenoData",
#                                pData= pD,varLabels=list(rep("varLabel1",1))),
#                              channels=channels)
#    resultList <- spotMerge(arrayDataObject=arrayDataObject,exprSetRGObject=exprSetRGObject, identifiersToBeSkipped=c("x","z"))
#    resultExprSetRG <- resultList[["exprSetRGObject"]]
#    resultY <- (0:5)*100 + sum(1:20)/20
#    stopifnot( all( all(exprs(resultExprSetRG)["y",] == resultY) ) )
#    resultK <- (0:5)*100 + sum(41:60)/20
#    stopifnot( all( all(exprs(resultExprSetRG)["k",] == resultK) ) )
#    resultL <- (0:5)*100 + sum(61:80)/20
#    stopifnot( all( all(exprs(resultExprSetRG)["l",] == resultL) ) )
#    stopifnot( dim(exprs(resultExprSetRG))[1] == 3 )
#    stopifnot( dim(exprs(resultExprSetRG))[2] == 6 )
#    resultArrayData <- resultList[["arrayDataObject"]]
#    stopifnot( all(getIntensities(resultArrayData)[,1,] == getWeights(resultArrayData)) )
#    nameColumns <- grep("Name", colnames(getSpotAttr(resultArrayData)))
#    zahlColumns <- grep("Zahl", colnames(getSpotAttr(resultArrayData)))
#    indexColumns <- grep("Index", colnames(getSpotAttr(resultArrayData)))
#    stopifnot( all(getSpotAttr(resultArrayData)["y",nameColumns] == rep ("y", 20 ) ))
#    stopifnot( all(getSpotAttr(resultArrayData)["y",indexColumns] == 1:20 ) )
#    stopifnot( all(getSpotAttr(resultArrayData)["y",zahlColumns] == rep (1:5, 4 ) ))
#
# \dontshow{
#    stopifnot( all(getSpotAttr(resultArrayData)["k",nameColumns] == rep ("k", 20 ) ))
#    stopifnot( all(getSpotAttr(resultArrayData)["k",zahlColumns] == rep (1:5, 4) ) )
#    stopifnot( all(getSpotAttr(resultArrayData)["l",nameColumns] == rep ("l", 20 ) ))
#
#    se.exprs(exprSetRGObject) <- matrix(nrow=0,ncol=0)
#    resultList <- spotMerge(arrayDataObject=arrayDataObject,exprSetRGObject=exprSetRGObject, identifiersToBeSkipped=c("x","z"))
#    stopifnot( all( all(exprs(resultExprSetRG)["y",] == resultY) ) )
#    stopifnot( all( all(exprs(resultExprSetRG)["k",] == resultK) ) )
#    stopifnot( all( all(exprs(resultExprSetRG)["l",] == resultL) ) )
#
#    intensities <- array(data=rep(2,600),dim=c(100,2,3))
#    dimnames(intensities) <- list(NULL, c("green","red"), NULL)
#    arrayDataObject <- new("arrayData", intensities=intensities, spotAttr=data.frame(Name=I(rep(c("x","y","k","l","z"),20))), hybAttrList=NULL)
#    indGreen=1:3
#    indRed=4:6
#    channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#    colnames(channels) <- c("green","red")
#    exprSetRGObject <- new("exprSetRG", exprs=matrix(data=rep(c(1,2,3,4,5),120),nrow=100,ncol=6,byrow=FALSE), phenoData=new("phenoData", pData=data.frame(matrix(0,nrow=6,ncol=1)), varLabels=list(rep("varLabel1",1))), channels=channels)
#    resultList2 <- spotMerge(arrayDataObject=arrayDataObject,exprSetRGObject=exprSetRGObject, identifiersToBeSkipped=c("x","z"))
#    resultES <- resultList2[["exprSetRGObject"]]
#    resultAD <- resultList2[["arrayDataObject"]]
#    stopifnot( dim(exprs(resultES))[1] == 3 )
#    stopifnot( dim(exprs(resultES))[2] == 6 )
#    stopifnot( all( exprs(resultES)["y",] == 2 ) )
#    stopifnot( all( exprs(resultES)["k",] == 3 ) )
#    stopifnot( all( exprs(resultES)["l",] == 4 ) )
#    stopifnot((all(se.exprs(resultES) == 0)))
#    stopifnot( all(getIntensities(resultAD)[,,] == 2) )
#
#    intensities <- array(data=rep(3,600),dim=c(100,2,3))
#    dimnames(intensities) <- list(NULL, c("green","red"), NULL)
#    arrayDataObject <- new("arrayData", intensities=intensities, spotAttr=data.frame(Name=I(rep(c("x","y","k","l","z"),20))), hybAttrList=NULL)
#    indGreen=1:3
#    indRed=4:6
#    channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
#    colnames(channels) <- c("green","red")
#    exprSetRGObject <- new("exprSetRG", exprs=matrix(data=rep(c(1,2,3,4,5,5,4,3,2,1),60),nrow=100,ncol=6,byrow=FALSE), phenoData=new("phenoData", pData=data.frame(matrix(0,nrow=6,ncol=1)), varLabels=list(rep("varLabel1",1))), channels=channels)
#    resultList2 <- spotMerge(arrayDataObject=arrayDataObject,exprSetRGObject=exprSetRGObject, identifiersToBeSkipped=c("x","z"))
#    resultES <- resultList2[["exprSetRGObject"]]
#    resultAD <- resultList2[["arrayDataObject"]]
#    stopifnot( dim(exprs(resultES))[1] == 3 )
#    stopifnot( dim(exprs(resultES))[2] == 6 )
#    stopifnot( all( exprs(resultES)["y",] == 3 ) )
#    stopifnot( all( exprs(resultES)["k",] == 3 ) )
#    stopifnot( all( exprs(resultES)["l",] == 3 ) )
#    stopifnot(all(exprs(resultES) == 3))
#    stopifnot( all(getIntensities(resultAD)[,,] == 3) )
#    stopifnot( all.equal.numeric( as.vector(se.exprs(resultES)["y",]), as.vector(rep(sd(rep(c(2,4),10)),6) )) )
#    stopifnot( all( se.exprs(resultES)["k",] ==  sd(rep(c(3,3),10)) ) )
#    stopifnot( all.equal.numeric( as.vector(se.exprs(resultES)["l",]), as.vector(rep(sd(rep(c(4,2),10)),6)) ) )
#
#   }
#    
#
#	}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */


spotMerge <- function(exprSetRGObject, arrayDataObject, spotIdentifier="Name", identifiersToBeSkipped=NULL, mergeFunc=mean){

  
  if( missing(exprSetRGObject) ){
    stop(" exprSetRGObject is missing in spotMerge ")
  }
  if( ! class(exprSetRGObject) == "exprSetRG" ){
    stop(" wrong class for object exprSetRGObject  in spotMerge ")
  }
  if( missing(arrayDataObject) ){
    stop(" arrayDataObject is missing in spotMerge ")
  }
  if( ! class(arrayDataObject) == "arrayData" ){
    stop(" wrong class for object arrayDataObject in spotMerge ")
  }

  if( dim(exprs(exprSetRGObject))[1] != dim(getIntensities(arrayDataObject))[1] ){
    stop(paste(" number of rows of exprSetRGObject:", (dim(exprs(exprSetRGObject))[1]), " and arrayDataObject intensities:", (dim(getIntensities(arrayDataObject))[1])," differ in spotMerge"))
  }
  if(  dim(exprs(exprSetRGObject))[2]/2 != dim(getIntensities(arrayDataObject))[3] ){
    stop(paste(" number of hybridisations of exprSetRGObject:", (dim(exprs(exprSetRGObject))[2]/2)," and of getIntensities(arrayDataObject):",  (dim(getIntensities(arrayDataObject))[3])," differ in spotMerge"))
  }
  if( dim(exprs(exprSetRGObject))[1] != dim(getSpotAttr(arrayDataObject))[1] ){
    stop(paste(" number of rows of exprSetRGObject:", (dim(exprs(exprSetRGObject))[1]), " and getSpotAttr(arrayDataObject):", (dim(getSpotAttr(arrayDataObject))[1])," differ in spotMerge"))
  }

  
  spotAttr <- getSpotAttr(arrayDataObject)
  intensities <- getIntensities(arrayDataObject)
  weights <- getWeights(arrayDataObject)
  
  if( is.null(spotAttr) ){
    stop(" getSpotAttr(arrayDataObject) is null in spotMerge ")
  }
  if( ! (spotIdentifier %in% colnames(spotAttr)) ){
    stop(" no appropriate column in getSpotAttr(arrayDataObject) in function spotMerge")
  }

  ## "reshaping"  
  exprsGreen <- exprs(getExprSetGreen(exprSetRGObject))
  exprsRed <- exprs(getExprSetRed(exprSetRGObject))
  stopifnot( all(dim(exprsGreen) == dim(exprsRed)) )
  exprsData <- array(NA, dim=c(dim(exprsGreen)[1], 2, dim(exprsGreen)[2]))
  dimnames(exprsData) <- list( dimnames(exprsGreen)[[1]], c("green", "red"), dimnames(exprsGreen)[[2]] ) 
  exprsData[,"green",] <- exprsGreen
  exprsData[,"red",] <- exprsRed
  greenIndex <- which( dimnames(exprsData)[[2]] == "green" )
  redIndex <- which( dimnames(exprsData)[[2]] == "red" )
  

  se.exprs <- se.exprs(exprSetRGObject)
  
  ## spot replicas 
  tmpListSpotReplicas <- detectReplicas(spotAttr, spotIdentifier=spotIdentifier, identifiersToBeSkipped=identifiersToBeSkipped)
  nrSpotReplicas <- tmpListSpotReplicas[["nrOfReplicas"]]
  spotReplicas <- tmpListSpotReplicas[["spotReplicas"]]

  if( ! any(is.na(nrSpotReplicas)) & ! is.null(nrSpotReplicas) ){
    
    if( nrSpotReplicas >= 2 ){
      
      ## reshape spotReplicas
      spotReplicasArray <- sapply(spotReplicas, function(x) return(x))

      ## spotReplicasArray[indexes x uniqueIdentifiers]
      ##simpleApply(arrayObject, dimension(s), func, funcResultDimensionality, DEBUG=FALSE)
      exprsDataResult <-simpleApply(spotReplicasArray, 2, function(x)
                                    return(controlledApply(
                                                exprsData[x,,,drop=FALSE],
                                                2:3, mergeFunc, 1)),
                                      c(dim(exprsData)[2],dim(exprsData)[3]))
      dimnames(exprsDataResult) <- list(dimnames(spotReplicasArray)[[2]],
                                         dimnames(exprsData)[[2]],
                                         dimnames(exprsData)[[3]])
      

      if( length(se.exprs) < 1 ){

        ## se.exprs does not exist -- generate one via sd of spot merge, cf. below
        
      }else{

        ## se.exprs already exists -- either generate one or sum up the exisiting ones
        
        if( ! all(dim(se.exprs) == dim(exprs(exprSetRGObject)) ) ){
          stop(" se.exprs dimensions must match dimensions of exprSetRGObject in spotMerge ")
        }
        cat("\n Attention: \n")
        cat(" existing se.exprs of the exprSetRGObject is replaced by the sd\n of the spot merge operation in function spotMerge \n")
        
      }
      newSeExprs <- simpleApply(spotReplicasArray, 2, function(x)
                      return(controlledApply(exprsData[x,,,drop=FALSE],
                                             2:3, sd, 1)),
                                c(dim(exprsData)[2],dim(exprsData)[3]))
      dimnames(newSeExprs) <- list(dimnames(spotReplicasArray)[[2]],
                                   dimnames(exprsData)[[2]],
                                   dimnames(exprsData)[[3]])

      
      ################################################

      if( ! is.null(intensities) ){
        newIntensities <- simpleApply(spotReplicasArray, 2, function(x)
                        return(controlledApply(intensities[x,,,drop=FALSE],
                                               2:3, mergeFunc, 1)),
                                    c(dim(intensities)[2],dim(intensities)[3]))
      dimnames(newIntensities) <- list(dimnames(spotReplicasArray)[[2]],dimnames(intensities)[[2]],dimnames(intensities)[[3]])

      }else{
        newIntensities <- NULL
      }

      if( ! is.null(weights) ){

        ## weights are given
        newWeights <-  simpleApply(spotReplicasArray, 2, function(x)
                           return(controlledApply(weights[x,,drop=FALSE],
                                                  2, mergeFunc,1)),
                                c(dim(weights)[2]))
        dimnames(newWeights) <- list(dimnames(spotReplicasArray)[[2]],dimnames(weights)[[2]])

      }else{

        ## no weights are given
        newWeights <- NULL

      }
      
      ## merging of spot annotation information for the replicas
      newSpotAttr <- spotAttr[spotReplicasArray[1,],]
      for(i in (2:nrSpotReplicas)){
        newSpotAttr <- data.frame(newSpotAttr, spotAttr[spotReplicasArray[i,],], check.names=FALSE)
      }
      if( ! is.null(colnames(spotAttr) ) ){
        columnNames <- colnames(spotAttr)
        allColumnNames <- make.names(rep(columnNames,nrSpotReplicas),unique=TRUE)
        colnames(newSpotAttr) <- allColumnNames
      }
      rownames(newSpotAttr) <- dimnames(spotReplicasArray)[[2]]
     

      ## recreate object
      newArrayDataObject <- new("arrayData", intensities=newIntensities ,weights=newWeights, spotAttr=newSpotAttr , hybAttrList=getHybAttrList(arrayDataObject) )

      
      ## reshape
      exprsArray <- as.matrix(cbind(
                      controlledSubsetting(exprsDataResult,
                                          ranges=list(NA,greenIndex,NA),
                                          drop=2),
                      controlledSubsetting(exprsDataResult,
                                          ranges=list(NA,redIndex,NA),
                                          drop=2)))

      se.exprsMerged <- as.matrix(cbind(
                      controlledSubsetting(newSeExprs,
                                          ranges=list(NA,greenIndex,NA),
                                          drop=2),
                      controlledSubsetting(newSeExprs,
                                          ranges=list(NA,redIndex,NA),
                                          drop=2)))
      
      ## recreate object
      nrSlides <- dim(exprsDataResult[,"green",,drop=FALSE])[3]
      indGreen <- 1:nrSlides
      indRed <- (nrSlides+1):(nrSlides+nrSlides)
      channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
      colnames(channels) <- c("green","red")
      newExprSetRGObject <- new("exprSetRG", exprs=exprsArray, channels=channels, se.exprs=se.exprsMerged, phenoData=phenoData(exprSetRGObject), notes=notes(exprSetRGObject), annotation=annotation(exprSetRGObject))
      description(newExprSetRGObject) <- description(exprSetRGObject)
      
      
    }else{

      cat("\n warning: no spots to merge in spotMerge! \n")
      newExprSetRGObject <- exprSetRGObject
      newArrayDataObject <- arrayDataObject
    }
    
  }else{

    cat("\n warning: no spots to merge in spotMerge! \n")
    newExprSetRGObject <- exprSetRGObject
    newArrayDataObject <- arrayDataObject
  }
    
                               
  return(list(exprSetRGObject=newExprSetRGObject,
              arrayDataObject=newArrayDataObject ))
  
}
