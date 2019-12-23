## Recall: do not put any comments in the Rd-section below
## comment: arguments are divided into groups somehow separated by
##          extra space, cf. with the comments in the function
##          definition header

# /**
#
# \name{processArrayData}
#
# \title{Automated processing of two colour DNA microarray data}
#
# \alias{processArrayData}
#
# \description{Automated processing of image analyis result files
#              and related annotation information. Combines several
#              steps of microarray data processing, read
#              the package vignette for a detailed example. To access
#              the vignette call openVignette()
#              after calling library(arrayMagic).
#	      }
#
#
#
# \value{
#        A list of objects, i.e.  an "exprSetRGObject" and an
#        "arrayDataObject" with corresponding class types
#        \code{\link{exprSetRG-class}} and
#        \code{\link{arrayData-class}}
#        (cf. the result of \code{\link{processArrayDataObject}}).
#        Side-effects: The result list "resultList" is stored
#        as file \code{objectsFileName} in the directory \code{savePath}
#        if the argument \code{objectsFileName} is supplied.
#        The \code{slideDescriptionFile} is stored
#        with suffix "\_processed"
#        in the directory \code{savePath}.
# }
#
# @usage
#
#
# \arguments{
#
#
#  \item{spotIdentifier}{ character string; required; default "Name".
#                         \code{spotIdentifier} specifies the column
#                         in the image analysis result files which contain
#                         spot or gene identifiers}
#  \item{verbose}{ logical; required; default: \code{TRUE}}
#                             
#
#  \item{loadPath}{ character string; required; default: ".".
#      The path is used for loading of the \code{slideDescriptionFile}
#      and the image analysis result files;
#      note: "." refers to the working directory.
#      }
#
#  \item{slideDescriptionFile}{ character string;
#       required; default "slideDescription.txt".
#       The first line of the tab-deliminated file
#       must contain all column names, i.e. a header
#       line. In particular it must contain the
#       column named \code{fileNameColum} and possibly
#       additionally a column named \code{slideNameColumn}. }
#
#
#
#  \item{deleteBlanks}{ logical; required; default: \code{TRUE}.
#                       If set to \code{TRUE} any blank character (space)
#                       is removed from the text body of the
#                       \code{slideDescriptionFile}. }
#
#
#
#  \item{fileNameColumn}{ character string; required; default: "fileName".
#                         \code{fileNameColumn} specifies the column which
#                         contains the names of all
#                         image analysis result files.}
#  \item{slideNameColumn}{ character string; optional; default missing.
#        If \code{slideNameColumn} is missing the value is set
#        to \code{fileNameColumn}.}
#  \item{channelColumn}{optional; cf. \code{\link{readIntensities}};
#                       default: \code{NULL} }
#
#  \item{type}{character string to characterize
#              the file type like "GenePix" or "generic";
#              note e.g. "generic" requires the arguments \code{dataColumns}
#              and \code{spotAnnoColumns}; default: "GenePix"; cf.
#              \code{\link{readIntensities}} for details}
#  \item{dataColumns}{required for \code{type} "generic";
#                     cf. \code{\link{readIntensities}}; default: \code{NULL} }
#  \item{spotAnnoColumns}{required for \code{type} "generic";
#                     cf. \code{\link{readIntensities}}; default: \code{NULL} }
#  \item{removePatterns}{optional; cf.  \code{\link{readIntensities}};
#                       default: \code{NULL} }
#  \item{skip}{optional; cf.  \code{\link{readIntensities}};
#                       default: \code{NULL} }
#  \item{...}{ further arguments which are passed to
#              \code{\link{readIntensities}} and eventually are
#              passed to \code{\link{read.table}}.
#            }
#
#
#
#
#  \item{normalisationMethod}{character string; required; default: "vsn";
#        cf. argument \code{method} of \code{\link{normalise}} }
#  \item{subtractBackground}{ logical; required; default: \code{FALSE}
#        cf. \code{\link{normalise}} }
#  \item{spotsRemovedBeforeNormalisation}{ vector of character strings;
#       required; default: \code{NULL}.
#       All spots which match the string(s) will be excluded  
#       already before normalisation and will not be present
#       in the resulting objects at all.
#       }
#  \item{spotsRemovedAfterNormalisation}{ vector of character strings;
#       required; default: \code{NULL};
#       cf. argument \code{spotsRemovedBeforeNormalisation}.
#       }
#  \item{subGroups}{character string or \code{NULL};
#                   required; cf. \code{\link{normalise}};
#                   default: \code{NULL} }
#  \item{channelsSeparately}{logical; required;
#                            cf. \code{\link{normalise}};
#                            default \code{FALSE}}
#  \item{hybridisationGroups}{list of numeric vectors;
#                             cf. \code{\link{normalise}};
#                             required; default: \code{NULL}}
# 
#
#
#
#
#  \item{savePath}{ character string; required; default: ".".
#      The directory \code{savePath} used to store
#      the results (cf. argument \code{objectsFileName}
#      and processed files.
#      If the path does not exist a directory is created;
#      note: "." refers to the working directory.
#      }
#  \item{objectsFileName}{character string; optional;
#                         default missing, i.e. no file is created.
#                         \code{objectsFileName} specifies
#                         the name of the file used to store the object
#                         "resultList", i.e. the return value of the function.
#  }
#  \item{plotOutput}{character string; required; default: "screen";
#        Possible values: "screen", "pdf" or "win.metafile". }
#
# }
#
# \seealso{ \code{\link{readpDataSlides}},
#           \code{\link{readIntensities}},
#           \code{\link{processArrayDataObject}},
#           \code{\link{normalise}},
#           \code{\link{exprSetRG-class}},
#           \code{\link{arrayData-class}}
#         }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
#
#
# \examples{
#
#       LOADPATH <- file.path(.path.package("arrayMagic"), "extdata")
#       SAVEPATH <- tempdir()
# 	SLIDEDESCRIPTIONFILE <- "slideDescription"
#  
# 	resultList <- processArrayData(
#                             loadPath=LOADPATH, 
#  	                      savePath=SAVEPATH,
#                             slideDescriptionFile=SLIDEDESCRIPTIONFILE
#                  )
#       writeToFile(arrayDataObject=resultList$arrayDataObject,
#                   exprSetRGObject=resultList$exprSetRGObject,
#                   fileName="normalisedData.txt",
#                   savePath=SAVEPATH)
#
#       summarizedResult <- slideMerge(exprSetRGObject=resultList$exprSetRGObject, slideMergeColumn="replicates" )
#
#        qPL <- qualityParameters(arrayDataObject=resultList$arrayDataObject,
#                                 exprSetRGObject=resultList$exprSetRGObject)
#
#
#        visualiseQualityParameters(qualityParameters=qPL$qualityParameters,
#                                   savePath=tempdir())
#
#        qualityDiagnostics(
#                    arrayDataObject=resultList$arrayDataObject,
#                    exprSetRGObject=resultList$exprSetRGObject,
#                    qualityParametersList=qPL,
#                    groupingColumn="replicates",
#                    slideNameColumn="fileName",
#                    savePath=tempdir(),
#                    plotOutput="pdf")
#
#        unlink(file.path(SAVEPATH, paste(SLIDEDESCRIPTIONFILE,"_processed",sep="")))
# 	 resultListG <- processArrayData(
#                             loadPath=LOADPATH, 
#  	                      savePath=SAVEPATH,
#                             slideDescriptionFile=SLIDEDESCRIPTIONFILE,
#                             plotOutput="pdf",
#                             hybridisationGroups = list((1:4),(5:9))
#                  )
# unlink(file.path(SAVEPATH, paste(SLIDEDESCRIPTIONFILE,"_processed",sep="")))
# resultListG2 <- processArrayData(
#                             loadPath=LOADPATH, 
#  	                      savePath=SAVEPATH,
#                             slideDescriptionFile=SLIDEDESCRIPTIONFILE,
#                             plotOutput="pdf",
#                             objectsFileName = "exprSetRG.RData",
#                             hybridisationGroups = "slideBySlide"
#                                         )
# unlink(file.path(SAVEPATH, paste(SLIDEDESCRIPTIONFILE,"_processed",sep="")))
#
# SLIDEDESCRIPTIONFILE <- "genericChannelsPerFile"
# spotAnnoColumns <- c("Index", "Label" , "Type" , "Name" , "ID" )
# dataColumns <- c("Normalized....","Average....","Normalized....","Average....")
# names(dataColumns) <- c("greenForeground","greenBackground",
# 		      "redForeground","redBackground")
#
# resultGenericChannel <- processArrayData(
#                                spotIdentifier="Index",
#                                loadPath=LOADPATH, 
#                                savePath=SAVEPATH,
#                                slideDescriptionFile=SLIDEDESCRIPTIONFILE,
#                                normalisationMethod="none",
#                                channelColumn="channel",
#                                fileNameColumn="files",
#                                slideNameColumn="name",
#                                type="genericOneFilePerChannel",
#                                spotAnnoColumns=spotAnnoColumns,
#                                dataColumns=dataColumns
#                            )
#
# unlink(file.path(SAVEPATH, paste(SLIDEDESCRIPTIONFILE,"_processed",sep="")))
# SLIDEDESCRIPTIONFILE <- "genericChannelsPerFileTwo"
# dataColumns <- c("Integral..QL.","Bkg..QL.", "Integral..QL.","Bkg..QL.")
# names(dataColumns) <- c("greenForeground","greenBackground",
# 		      "redForeground","redBackground")
#
# resultGenericChannelTwo <- processArrayData(
#                                spotIdentifier="ID",
#                                loadPath=LOADPATH, 
#                                savePath=SAVEPATH,
#                                slideDescriptionFile=SLIDEDESCRIPTIONFILE,
#                                normalisationMethod="vsn",
#                                channelColumn="channel",
#                                fileNameColumn="files",
#                                slideNameColumn="name",
#                                subtractBackground=TRUE,
#                                type="genericOneFilePerChannel",
#                                spotAnnoColumns=spotAnnoColumns,
#                                dataColumns=dataColumns
#                            )
# unlink(file.path(SAVEPATH, paste(SLIDEDESCRIPTIONFILE,"_processed",sep="")))
#
#  \dontshow{
#
#    }     
# }
# 
# \keyword{utilities}
#
# */
processArrayData <- function(
                             spotIdentifier = "Name",                             
                             verbose = TRUE,
                             loadPath = ".",
                             slideDescriptionFile = "slideDescription.txt",

                             ## option used solely in readpDataSlides
                             deleteBlanks = TRUE,


                             ## options used solely in processArrayDataObject
                             normalisationMethod = "vsn",
                             subtractBackground=FALSE,
                             spotsRemovedBeforeNormalisation = NULL,
                             spotsRemovedAfterNormalisation = NULL,
                             subGroups = NULL,
                             channelsSeparately = FALSE,
                             hybridisationGroups = NULL,

                             ## options used solely processArrayData
                             savePath = ".",
                             objectsFileName,
                             plotOutput = "screen",

                             ## options used solely in readIntensities
                             fileNameColumn = "fileName", 
                             slideNameColumn,
                             type = "GenePix",
                             dataColumns = NULL,
                             spotAnnoColumns = NULL,
                             channelColumn = NULL,
                             removePatterns = NULL,
                             skip = NULL,
                             ...
                             
                           ){
  
##################
#### very general checks
  
  ## check paths
  if( any(is.na(file.info(loadPath)$isdir)) ){
    stop(" the load path does not exist \n", call.=FALSE)
  }else{
    stopifnot( file.info(loadPath)$isdir )
  }
  if( any(is.na(file.info(savePath)$isdir)) ){
    if(verbose){
      cat(" the save path does not exist\n")
      cat(" trying to create a new directory\n")
    }
    stopifnot(dir.create(savePath))
    if(verbose){ cat(" directory has been successfully created\n") }
  }else{ if(file.info(savePath)$isdir){
  ## directory exists
  }else{
    stop(paste(savePath," does exist, but is not a directory \n"),call.=FALSE)
  }
       }

  
  if( ! plotOutput %in% c("screen","pdf", "win.metafile") ){
    warning(" unknown plotOutput in processArrayData", call.=FALSE)
    return()
  }

  if( plotOutput == "win.metafile" ){
    if( .Platform$OS.type != "windows" ){
      stop( paste(plotOutput, " only valid if running under Windows in processArrayData "), call.=FALSE )
    }
  }
  

  
  width <- 8
  height <- 6

  ## test graphics device
  if( plotOutput == "screen" ){
    if( interactive() ){
      if( verbose ){
        cat("\n functionality test of your graphical display")
        cat(" graphic window pops up \n")
      }
      x11()
      dev.off()
    }
  }else if( plotOutput == "pdf" || plotOutput == "win.metafile" ){
    testPlot <- file.path(savePath,"testPlotEmpty")
    if( plotOutput == "pdf" ){
      pdf(width=width,height=height,file=testPlot)
    }else if( plotOutput == "win.metafile" ){
      win.metafile(width=width,height=height,file=testPlot)
    }else{
      stop(" unexpected case; unknown plotOutput in processArrayData ", call.=FALSE)
    }
    dev.off()
    unlink(testPlot)
  }else{ 
    stop(" unexpected case; unknown plotOutput in processArrayData ", call.=FALSE)
  }


  
##################
## checks related to the normalisation
  
  
  if( ! normalisationMethod %in% c("loess", "loessScale", "loessQuantile", "quantile", "vsn", "none" ) ){
    stop(" invalid normalisationMethod ",normalisationMethod," in processArrayData ", call.=FALSE)
  }

  if( ! type %in% c("GenePix","ScanAlyze",
                    "generic","genericOneFilePerChannel") ){
    stop(paste(" wrong type:",type," in processArrayData",sep=""), call.=FALSE)
  }

  
##################
#### checks related to slideDescriptionFile 

      
  ## description file 
  tableFile <- file.path(loadPath, slideDescriptionFile)
  if( file.exists(tableFile) ){
    ##if(verbose){ cat(" using description file: ");cat(tableFile);cat("\n") }
  }else{
    stop(paste(" no description file ",tableFile,"found \n"), call.=FALSE)
  }



###################
### end of checks  
###################

  
  if(verbose){cat("\n\n copying description file: ");cat(tableFile);cat("\n")}
  storedTableFile <- file.path(savePath,
                               paste(slideDescriptionFile, "_processed",sep=""))
  if( ! file.exists(storedTableFile) ){

    if(verbose){ cat(" to the result directory\n") }
    file.copy(tableFile, storedTableFile)
    
  }else{

    file.copy(tableFile, storedTableFile, overwrite = TRUE)
    tmpString <- paste(" Attention - the description file (filename + \"_processed\") has been replaced in directory ",savePath,"\n",sep="")
    cat(tmpString)
    
  }


  if( missing( slideNameColumn ) ){
    slideNameColumn <- fileNameColumn
  }

  slideDescription <- readpDataSlides(
                                  loadPath=loadPath, 
                                  slideDescriptionFile=slideDescriptionFile,
                                  deleteBlanks=deleteBlanks,
                                  verbose=verbose
                                  )
  
  arrayDataObject <- readIntensities(
                                     slideDescription = slideDescription,
                                     fileNameColumn=fileNameColumn,
                                     slideNameColumn=slideNameColumn,
                                     channelColumn=channelColumn,
                                     loadPath=loadPath,
                                     type = type,
                                     spotAnnoColumns=spotAnnoColumns,
                                     dataColumns=dataColumns,
                                     removePatterns=removePatterns,
                                     skip=skip,
                                     ...,
                                     spotIdentifier = spotIdentifier,
                                     verbose = verbose
                                     )

  resultList <- processArrayDataObject(
                                       arrayDataObject=arrayDataObject,

                                       verbose = verbose,
                                       spotIdentifier = spotIdentifier,
                                       
                                       normalisationMethod=
                                         normalisationMethod,
                                       subtractBackground=subtractBackground,
                                       spotsRemovedBeforeNormalisation =
                                         spotsRemovedBeforeNormalisation,
                                       spotsRemovedAfterNormalisation =
                                         spotsRemovedAfterNormalisation,
                                       subGroups = subGroups, 
                                       channelsSeparately =
                                         channelsSeparately,
                                       hybridisationGroups =
                                         hybridisationGroups
                                     )

    
  if( ! missing( objectsFileName ) ){

    if( is.character(objectsFileName) && length(objectsFileName) == 1 ){
    
      if(verbose){ cat("\n saving object \"resultList\" for later processing in processArrayData\n\n") }
      save(resultList, file=file.path(savePath,objectsFileName), compress=TRUE)
      if(verbose){ cat(" saving finished \n") }

    }else{
      if(verbose){ cat("\n non valid objectsFileName; saving of object \"resultList\" skipped in processArrayData\n\n") }
      
    }

  }
    
  return(resultList)
  
}
