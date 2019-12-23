## note: introduced large width/WIDTH and height/HEIGHT
##       to avoid trouble with writing pdfs ...

# /**
#
# \name{qualityDiagnostics}
#
# \title{Generates quality diagnostic plots}
#
# \alias{qualityDiagnostics}
#
# @usage
#
# \description{Several quality diagnostic plots are generated.
#              The distributions of the (normalised) intensities,
#              as well as overall similarities between hybridisations and
#              several other quality parameters are graphically visualised
#              (cf. function \code{\link{qualityParameters}}).
#             }
#
# \value{The function is called for its side effect.}
#
#
# \arguments{
#  \item{arrayDataObject}{\code{\link{arrayData-class}};
#                         required; default: missing}
#  \item{exprSetRGObject}{\code{\link{exprSetRG-class}};
#                         required, default: missing}
#  \item{qualityParametersList}{the return value of
#                               \code{\link{qualityParameters}}
#                               required; default: missing}
#
#  \item{groupingColumn}{character string; optional; default: missing;
#                         specifies a column of
#                         \code{pDataSlide(exprSetRGObject)} which is
#                         used to indicate the hybridisation groups
#                         in the clustered similarity plots.}
#  \item{slideNameColumn}{character string; optional; default: missing;
#                         specifies a column of
#                         \code{pDataSlide(exprSetRGObject)}.}
#  \item{savePath}{character string; required; default: "."}
#  \item{completeOutput}{required; default: \code{FALSE}}
#  \item{verbose}{ logical; required; default: \code{TRUE}}
#  \item{plotOutput}{character string specifying the output type;
#        either "screen", "pdf" or "win.metafile"; default: "pdf"}
# }
# 
# \details{}
#
# \seealso{\code{\link{qualityParameters}},
#          \code{\link{exprSetRG-class}},
#          \code{\link{arrayData-class}}
#         }
#
# \examples{
#	}
#
# \keyword{hplot}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

qualityDiagnostics <- function(
                               arrayDataObject,
                               exprSetRGObject,
                               qualityParametersList,
                               groupingColumn,
                               slideNameColumn,
                               savePath=".",
                               completeOutput=FALSE,
                               verbose=TRUE,
                               plotOutput="pdf"
                               ){

  suffix <- ""
  if( plotOutput == "pdf" ){
    suffix <- "pdf"
  }else if( plotOutput == "win.metafile" ){
    suffix <- "wmf"
  }
  
  if( ! plotOutput %in% c("screen","pdf", "win.metafile") ){
    stop(" unknown plotOutput in qualityDiagnostics", call.=FALSE)
  }

  if( plotOutput == "win.metafile" ){
    if( .Platform$OS.type != "windows" ){
      stop( paste(plotOutput, " only valid if running under Windows in qualityDiagnostics "), call.=FALSE )
    }
  }
  
  if( missing(exprSetRGObject) |
      missing(arrayDataObject) |
      missing(qualityParametersList) ){
    stop(" missing arguments in qualityDiagnostics ", call.=FALSE)
  }

  if( ! class(arrayDataObject) == "arrayData" ){
    stop(" arrayDataObject of wrong type in qualityDiagnostics ", call.=FALSE)
  }
    
  if( ! class(exprSetRGObject) == "exprSetRG" ){
    stop(" exprSetRGObject of wrong type in qualityDiagnostics ", call.=FALSE)
  }

  
  ## consistency of objects
  nrOfExprRows <- dim(exprs(exprSetRGObject))[1]
  intensities <-  getIntensities(arrayDataObject) 
  nrOfIntensityRows <- dim(intensities)[1]
    
  if( nrOfIntensityRows != nrOfExprRows ){
    stop(paste(" number of rows in arrayDataObject:",nrOfIntensityRows,": and number of rows in exprSetRGObject:",nrOfExprRows,": do not match in qualityDiagnostics \n",sep=""), call.=FALSE)
  }
  
  nrOfExprSlides <- dim(exprs(getExprSetLogRatio(exprSetRGObject)))[2]
  nrOfADSlides <- dim(intensities)[3]
  if( nrOfADSlides != nrOfExprSlides ){
    stop(paste(" number of slides in arrayDataObject:", nrOfADSlides,": and number of slides in exprSetRGObject:",nrOfExprSlides,": do not match in qualityDiagnostics \n",sep=""), call.=FALSE)
  }

  
  if( missing(groupingColumn) ){

    sideColors <- NULL
    
  }else{

    slideAnnotation <- pDataSlide(exprSetRGObject)
    if( ! groupingColumn %in% colnames(slideAnnotation) ){

      stop(paste(":groupingColumn: :",groupingColumn, ": not found in pDataSlide(exprSetRGObject) in qualityDiagnostics"), call.=FALSE)
      
    }else{

      groupingFactor <- as.factor(slideAnnotation[, groupingColumn])
      nrFactors <- nlevels(groupingFactor)
      if( nrFactors <= 6 ){
        hybColours <- c( "#e31a1c", "#377db8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33" )[1:nrFactors]
      }else{
        hybColours <- rainbow(nrFactors)
      }
      names(hybColours) <- as.character(levels(groupingFactor))
      sideColors <- hybColours[as.character(as.vector(groupingFactor))]
      
    }
  }
  

  
  if( missing(slideNameColumn) ){

    slideLabels <- 1:dim(exprs(getExprSetLogRatio(exprSetRGObject)))[2]
    
  }else{

    slideAnnotation <- pDataSlide(exprSetRGObject)
    if( ! slideNameColumn %in% colnames(slideAnnotation) ){

      stop(paste(":slideNameColumn: :",slideNameColumn, ": not found in pDataSlide(exprSetRGObject) in qualityDiagnostics"), call.=FALSE)
      
    }else{

      slideLabels <- slideAnnotation[, slideNameColumn]

    }
  }

  ## parameter overview
  fileName <- paste("visualiseQualityParametersOutput.", suffix, sep="")
  visualiseQualityParameters(qualityParameters=
                             qualityParametersList$qualityParameters,
                             savePath=savePath, plotOutput=plotOutput,
                             fileName=fileName)

  
  WIDTH <- 20
  HEIGHT <- 16
  CEX.MAIN <- 4
  
  colourRamp <- rgb(seq(0,1,l=256),seq(0,1,l=256),seq(1,0,l=256))
  
  if( length(slideLabels) > 2 ){
    
    ## heatmaps and image matrixes (logRatio, green, red)
    myMaps <-list(list(tit="slideDistances",dis="slideDistance"),
                  list(tit="slideDistancesGreen",dis="slideDistanceGreen"),
                  list(tit="slideDistancesRed",dis="slideDistanceRed")
                  )
    ## log raw
    if( completeOutput ){

      myMaps <-c(myMaps,
                 list(list(tit="slideDistancesLogRaw",
                         dis="slideDistanceLogRaw"),
                    list(tit="slideDistancesGreenLogRaw",
                         dis="slideDistanceGreenLogRaw"),
                    list(tit="slideDistancesRedLogRaw",
                         dis="slideDistanceRedLogRaw")
                      )
                 )
      
    }## end of if completeOutput
    
    for( myMap in myMaps ){

      fN <- file.path(savePath, paste(myMap$tit,".",suffix,sep=""))
      plot.imageMatrix(qualityParametersList[[myMap$dis]], labels=slideLabels, main=myMap$tit, plotOutput=plotOutput, fileName=fN, reverseYaxis = FALSE, width=WIDTH, height=HEIGHT, xlab="", ylab="", cex.main=CEX.MAIN)
      
      fN <- file.path(savePath, paste(myMap$tit,"Heatmap.",suffix,sep=""))
      if( plotOutput == "screen" ){
        if( interactive() ){
          x11(width=15,height=15)
        }
      }else if( plotOutput == "pdf" ){
        pdf(width=15,height=15,file=fN)
      }else if( plotOutput == "win.metafile" ){
        win.metafile(width=15,height=15,file=fN)
      }else{
        stop(" unexpected argument for plotOutput in qualityDiagnostics ", call.=FALSE)
      }

      if( is.null(sideColors) ){
        heatmap(qualityParametersList[[myMap$dis]], margins=c(15,25),
                col=colourRamp, scale="none", Colv=NULL, Rowv=NULL,
                main=myMap$tit)
      }else{
        heatmap(qualityParametersList[[myMap$dis]], margins=c(15,25),
                col=colourRamp, scale="none", Colv=NULL, Rowv=NULL,
                ColSideColors=sideColors, RowSideColors=sideColors,
                main=myMap$tit)
      }
        
      if( plotOutput == "pdf" || plotOutput == "win.metafile" ){
        dev.off()
      }

    }## end of for

    ## end of if more than two hybridisations
  }else{

    if( verbose ){
      cat(" all slideDistance* plots are skipped (less than 3 hybridisations) in qualityDiagnostics\n") 
    }
    
  }
    
  ## ###################################
  
  ## normalisation diagnostics
  normalisationInfo <- description(exprSetRGObject)@preprocessing
  if( length(normalisationInfo) > 0 ){
    for( i in (1:length(normalisationInfo)) ){
      baseName <- paste("meanVsSDafterNormalisation",i,".",suffix,sep="")
      fileName <- file.path(savePath, baseName)
      if( class(normalisationInfo[[i]]) == "exprSet" ){
        
        if( plotOutput == "screen" ){
          if( interactive() ){
            x11()
          }
          }else if( plotOutput == "pdf" ){
            pdf(file=fileName)
          }else if( plotOutput == "win.metafile" ){
            win.metafile(file=fileName)
          }else{
            stop(" unexpected case; unknown plotOutput in qualityDiagnostics", call.=FALSE)
          }
        
        meanSdPlot(normalisationInfo[[i]], ranks=TRUE)
        
        if( plotOutput == "pdf" || plotOutput == "win.metafile" ){
          dev.off()
        }
        
      }else{ ## class(normalisationInfo) != "exprSet"
        if(verbose){
          cat(" normalisationInfo is not recognized in qualityDiagnostics\n")
          cat(" (specific normalisation diagnostic plots are only offered for vsn)\n")
        }
      }
    }
  }else{
    if(verbose){
      cat(" no normalisationInfo given in qualityDiagnostics\n")
      cat(" (specific normalisation diagnostic plots are only offered for vsn)\n")
    }
  }
  
  
  ## distributions
  WIDTH  <- 8 
  HEIGHT <- 6
  
  title <- "distributionOfRawDataChannelWise"
  fileName <- paste(title,".",suffix,sep="")
  plotDistributions(dataMatrix=
                    cbind(getIntensities(arrayDataObject)[,"green",],
                          getIntensities(arrayDataObject)[,"red",]),
                    colourVector=c(rep("darkgreen",length(slideLabels)),
                      rep("darkred",length(slideLabels))),
                    main=title,
                    width=WIDTH, height=HEIGHT,
                    fileName=fileName,
                    savePath=savePath,
                    plotOutput=plotOutput,
                    labels=c(slideLabels,slideLabels)) ## i.e. two times slideLabels
  
  

  title <- "distributionOfTopRawDataChannelWise"
  fileName <- paste(title,".",suffix,sep="")
  plotDistributions(dataMatrix=
                    cbind(getIntensities(arrayDataObject)[,"green",],
                          getIntensities(arrayDataObject)[,"red",]),
                    quantiles=c(0.75,0.99),
                    colourVector=c(rep("darkgreen",length(slideLabels)),
                      rep("darkred",length(slideLabels))),
                    main=title,
                    width=WIDTH, height=HEIGHT,
                    fileName=fileName,
                    savePath=savePath,
                    plotOutput=plotOutput,
                    labels=c(slideLabels,slideLabels)) ## i.e. two times slideLabels
  
  
  title <- "distributionOfRawDataSlideWise"
  fileName <- paste(title,".",suffix,sep="")
  plotDistributions(dataMatrix=
                    interweave(getIntensities(arrayDataObject)[,"green",],
                               getIntensities(arrayDataObject)[,"red",]),
                    colourVector=rep(c("darkgreen","darkred"),length(slideLabels)),
                    main=title,
                    width=WIDTH, height=HEIGHT,
                    fileName=fileName,
                    savePath=savePath,
                    plotOutput=plotOutput,
                    labels=interweave(slideLabels,slideLabels))## i.e. two times slideLabels

  
  title <- "distributionOfTopRawDataSlideWise"
  fileName <- paste(title,".",suffix,sep="")
  plotDistributions(dataMatrix=
                    interweave(getIntensities(arrayDataObject)[,"green",],
                               getIntensities(arrayDataObject)[,"red",]),
                    quantiles=c(0.75,0.99),
                    colourVector=rep(c("darkgreen","darkred"),length(slideLabels)),
                    main=title,
                    width=WIDTH, height=HEIGHT,
                    fileName=fileName,
                    savePath=savePath,
                    plotOutput=plotOutput,
                    labels=interweave(slideLabels,slideLabels))## i.e. two times slideLabels
  
  title <- "distributionOfNormalisedData"
  fileName <- paste(title,".",suffix,sep="")
  plotDistributions(dataMatrix=
                      cbind(exprs(getExprSetGreen(exprSetRGObject)),
                            exprs(getExprSetRed(exprSetRGObject))),
                      colourVector=c(rep("darkgreen",length(slideLabels)),
                        rep("darkred",length(slideLabels))),
                      main=title,
                      width=WIDTH, height=HEIGHT,
                      fileName=fileName,
                      savePath=savePath,
                      plotOutput=plotOutput,
                      labels=c(slideLabels,slideLabels)) ## i.e. two times slideLabels

  title <- "distributionOfNormalisedDataLogRatio"
  fileName <- paste(title,".",suffix,sep="")
  plotDistributions(dataMatrix=exprs(getExprSetLogRatio(exprSetRGObject)),
                    colourVector=rep("darkblue",length(slideLabels)),
                    main=title,
                    width=WIDTH, height=HEIGHT,
                    fileName=fileName,
                    savePath=savePath,
                    plotOutput=plotOutput,
                    labels=slideLabels)

  title <- "distributionOfRawDataLogRatio"
  fileName <- paste(title,".",suffix,sep="")
  plotDistributions(dataMatrix=
                      (log(getIntensities(arrayDataObject)[,"green",]) -
                       log(getIntensities(arrayDataObject)[,"red",])),
                    colourVector=rep("darkblue",length(slideLabels)),
                    main=title,
                    width=WIDTH, height=HEIGHT,
                    fileName=fileName,
                    savePath=savePath,
                    plotOutput=plotOutput,
                    labels=slideLabels)
  
  
}## end of qualityDiagnostics
