# /**
#
# \name{readIntensities}
#
# \title{readIntensities}
#
# \alias{readIntensities}
#
# \description{
#              The function takes the \code{data.frame}
#              \code{slideDescription} as input and reads
#              the listed image analysis raw data files of
#              the column \code{slideNameColumn}; cf.
#              function \code{\link{readpDataSlides}}. Each
#              image analysis raw data file must contain
#              tab separated columns and a header line -
#              not necessarily at the top of the file; cf.
#              the arguments \code{removePatterns} and
#              \code{skip}. Each file may only contain the
#              information for one channel, cf. argument
#              \code{channelColumn}.
#              The raw data information is returned as an
#              object of class \code{\link{arrayData}}.
#              Note: All image analysis quantification
#              (=raw data) files have to correspond
#              to the same type of microarray.
#             }
#
# \details{Details on the argument \code{type}:
#     \code{type="GenePix"} defines 
#     \code{spotAnnoColumns =  c("Block", "Column", "Row", "Name", "ID")}
#     and
#     \code{dataColumns = c("F532.Median", "B532.Median",
#                           "F635.Median", "B635.Median")} and corresponding
#     \code{names(dataColumns) = c("greenForeground", "greenBackground",
#      "redForeground", "redBackground")}, a dynamically determined argument 
#     \code{skip =  grep("Block...Column", imageFile) - 1} and
#     \code{removePatterns = NULL},
#     whereas
#     \code{type="ScanAlyze"} defines
#     \code{spotAnnoColumns = c( "HEADER", "SPOT", "GRID", "ROW", "COL" )}
#     and
#     \code{dataColumns = c("CH1I", "CH1B", "CH2I", "CH2B")} and corresponding
#     \code{names(dataColumns) = c("greenForeground", "greenBackground",
#                                  "redForeground", "redBackground")},
#     \code{skip = 0} and
#     \code{removePatterns = c("^REMARK")}.
#     }   
# @usage
#
# \arguments{
#  \item{slideDescription}{\code{data.frame}; required; default: missing.
#         (cf. the return value of the function \code{\link{readpDataSlides}})
#         The \code{data.frame} must contain at least one column;
#         this column has to be named as the argument \code{fileNameColumn}.
#         It may additionally contain a column named \code{slideNameColumn}.
#                         }
#  \item{fileNameColumn}{ character string; required; default: "fileName".
#                         \code{fileNameColumn} specifies the column which
#                         contains all image quantification result files
#                         in the \code{data.frame} of argument
#                         \code{slideDescription}.}
#  \item{slideNameColumn}{ character string; optional; default missing;
#        refers to the \code{data.frame} of argument \code{slideDescription}.
#        If \code{slideNameColumn} is missing the value is set
#        to \code{fileNameColumn}.}
# \item{channelColumn}{ named vector of character strings;
#                       optional; default \code{NULL}. 
#         If the \code{data.frame} of argument
#         \code{slideDescription} contains information
#         for each channel of every slide/hybridisation separately, the
#         \code{channelColumn} vector contains the column name of the
#         \code{data.frame} of argument \code{slideDescription}
#         used to specify the channel.
#         If \code{length(channelColumn) == 1} the character strings
#         "green" and "red" are assumed to be used for the coding,
#         otherwise \code{names(channelColumn)} must contain:
#         \code{c("channelColumnName","green","red")}.
#                     }
#  \item{loadPath}{ character string; required; default: ".".
#      The path is used to load the image quantification result files;
#      note: "." refers to the working directory.
#      }
#  \item{spotIdentifier}{ character string; optional; default missing.
#                         \code{spotIdentifier} specifies the column
#                         in the image analysis result files which contain
#                         spot or gene identifiers.}
#  \item{type}{character string; required;
#              possible values: "GenePix", "ScanAlyze",
#              "generic" and "genericOneFilePerChannel";
#              cf. Details section.
#              Note: value "generic" requires the arguments
#              \code{spotAnnoColumns}, \code{dataColumns}
#              and possibly \code{skip} and \code{removePatterns},
#              value "genericOneFilePerChannel" additionally requires
#              the argument "channelColumn";
#              whereas "GenPix"
#              and "ScanAlyze" use predefined values if not
#              otherwise specified;
#              default: "GenePix".}
#  \item{spotAnnoColumns}{ vector of character strings;
#        the column names of the image analysis data file.
#        The columns must contain the same information for all
#        files, e.g. the information on the layout of the microarray.
#        The argument \code{spotIdentifier} is automatically
#        added to the vector if not already included; default: \code{NULL} }
#  \item{dataColumns}{ named vector of character strings;
#         the column names of the image analysis data file.
#         The columns  contain the raw intensities values of each spot; 
#         \code{names(dataColumns)} must contain:
#         \code{c("greenForeground", "greenBackground",
#                 "redForeground", "redBackground")};
#         default: \code{NULL}
#       }
#  \item{removePatterns}{ vector of character string(s) each defining
#               a regular expression; default: \code{NULL};
#               all lines of each image analysis raw data file,
#               which match any of the regular expression(s) are discarded.
#             }
#  \item{skip}{ integer; default: \code{NULL};
#               number of lines skipped in each image analysis data file;
#               Argument \code{skip} applies after all lines matched by
#               one of the arguments of
#               \code{removePatterns} have been discarded.
#             }
#  \item{...}{ further arguments which are passed to \code{\link{read.table}}.
#              A function which is used inside this function. The arguments
#              must not include \code{sep},  \code{header}, \code{as.is}
#              and \code{skip} (for \code{skip} cf. above).
#            }
#  \item{verbose}{logical; required; default \code{TRUE}}
#
# }
#
# \value{object of class \code{\link{arrayData}}}
#
# \seealso{ \code{\link{readpDataSlides}},
#           \code{\link{arrayData-class}}
#   }
#
# \keyword{IO}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
#
# \examples{
#
#   \dontshow{
#
#       LOADPATH <- file.path(.path.package("arrayMagic"), "extdata")
#
# 	slideDescription <- readpDataSlides(
#                             loadPath=LOADPATH, 
#                             slideDescriptionFile="genericSlidesDescription"
#                           )
#       dataColumns=c("a","b","c","d")
#       names(dataColumns)= c("greenForeground","greenBackground",
#                             "redForeground","redBackground")
#       resultObject <- readIntensities(slideDescription=slideDescription,
#                                       fileNameColumn="files",
#                                       loadPath=LOADPATH,
#                                       type="generic",
#                                       spotAnnoColumns=c("e","f"),
#                                       dataColumns=dataColumns,
#                                       skip=7
#                                      )
#
# 	slideDescriptionB <- readpDataSlides(
#                             loadPath=LOADPATH, 
#                             slideDescriptionFile="genericSlidesTwoColourDescription"
#                           )
#       channelColumn <-  "channel"
#       resultObjectB <- readIntensities(slideDescription=slideDescriptionB,
#                                        fileNameColumn="files",
#                                        loadPath=LOADPATH,
#                                        channelColumn=channelColumn,
#                                        type="generic",
#                                        spotAnnoColumns=c("e","f"),
#                                        dataColumns=dataColumns,
#                                        skip=7
#                                       )
# 	slideDescriptionC <- readpDataSlides(
#                             loadPath=LOADPATH, 
#                             slideDescriptionFile="genericSlidesTwoColourDescription2"
#                           )
#       channelColumn <-  c("channel","xgreen","xred")
#       names(channelColumn) <- c("channelColumnName", "green", "red")
#       resultObjectC <- readIntensities(slideDescription=slideDescriptionC,
#                                        fileNameColumn="files",
#                                        loadPath=LOADPATH,
#                                        channelColumn=channelColumn,
#                                        type="generic",
#                                        spotAnnoColumns=c("e","f"),
#                                        dataColumns=dataColumns,
#                                        skip=7
#                                       )
#
#     stopifnot( all( match(unlist(getHybAttr(resultObjectB)), unlist(getHybAttr(resultObjectC)))))
#      stopifnot( all(unlist(getHybAttrRed(resultObjectB)[,c("files","name","sample")]) ==
#                     unlist(getHybAttrRed(resultObjectC)[c(1,3,2),c("files","name","sample")])))
#
#      stopifnot( all(unlist(getHybAttrGreen(resultObjectB)[,c("files","name","sample")]) ==
#                     unlist(getHybAttrGreen(resultObjectC)[c(1,3,2),c("files","name","sample")])))
#
# 	slideDescriptionD <- readpDataSlides(
#                             loadPath=LOADPATH, 
#                             slideDescriptionFile="genericSlidesTwoColourDescription3"
#                           )
#       channelColumn <-  c("channel","xgreen","xred")
#       names(channelColumn) <- c("channelColumnName", "green", "red")
#       resultObjectD <- readIntensities(slideDescription=slideDescriptionD,
#                                        fileNameColumn="files",
#                                        loadPath=LOADPATH,
#                                        channelColumn=channelColumn,
#                                        type="generic",
#                                        spotAnnoColumns=c("e","f"),
#                                        dataColumns=dataColumns,
#                                        skip=7
#                                       )
#     stopifnot( all( match(unlist(getHybAttr(resultObjectC)), unlist(getHybAttr(resultObjectD)))))
#
# stopifnot( all(getHybAttrRed(resultObjectC)[,c("files","name","sample","channel")] ==
#                getHybAttrRed(resultObjectD)[,c("files","name","sample","channel")]))
#
# stopifnot( all(getHybAttrGreen(resultObjectC)[,c("files","name","sample","channel")] ==
#                getHybAttrGreen(resultObjectD)[,c("files","name","sample","channel")]))
#
#slideDescription <- data.frame(fileName="lc7b048rex2.DAT")
#r <- readIntensities(slideDescription=slideDescription, loadPath=LOADPATH, type="ScanAlyze")
#slideDescription <- data.frame(fileName="lc7b048rex.DAT")
#r2 <- readIntensities(slideDescription=slideDescription, loadPath=LOADPATH, type="ScanAlyze")
# spotAnnoColumns <- c( "HEADER", "SPOT", "GRID", "ROW", "COL" )
# dataColumns <- c("CH1I", "CH1B", "CH2I", "CH2B")
# names(dataColumns) <-  c("greenForeground","greenBackground","redForeground","redBackground")
#  r3 <- readIntensities(slideDescription=slideDescription, loadPath=LOADPATH, spotAnnoColumns = spotAnnoColumns, dataColumns=dataColumns, type="generic")
# stopifnot(  all( getIntensities(r) == getIntensities(r2) ) )
# stopifnot(  all( getIntensities(r) == getIntensities(r3) ) )
#
#
#   }
#
# }
#
# */

readIntensities <- function(slideDescription,
                            fileNameColumn="fileName",
                            slideNameColumn,
                            channelColumn = NULL,
                            loadPath=".",
                            type = "GenePix",
                            spotAnnoColumns = NULL,
                            dataColumns = NULL,
                            removePatterns = NULL,
                            skip = NULL,
                            spotIdentifier,
                            ...,
                            verbose = TRUE){

  ## 
  if( length(fileNameColumn) != 1 ){
    stop(" argument fileNameColumn must be of length 1 in readIntensities \n", call.=FALSE)
  }
  if( ! is.character(fileNameColumn) ){
    stop(" argument fileNameColumn must be of type character in readIntensities \n", call.=FALSE)
  }
  
  ##
  if( ! is.null(skip) ){
    if( length(skip) != 1 ){
      stop(" argument skip must be of length 1 in readIntensities \n", call.=FALSE)
    }
    if( ! is.numeric(skip) ){
      stop(" argument skip must be of type numeric in readIntensities \n", call.=FALSE)
    }
  }
  ## 
 if( ! is.null(removePatterns) ){
    if( length(removePatterns) < 1 ){
      stop(" argument removePatterns must be of length >=1 in readIntensities \n", call.=FALSE)
    }
    if( ! is.character(removePatterns) ){
      stop(" argument removePatterns must be of type character in readIntensities \n", call.=FALSE)
    }
  }
  
  
  ## 1
  
  if( missing(slideDescription) ){
    stop(" no slideDescription in readIntensities \n", call.=FALSE)
  }
  
  if( length(slideDescription) < 1 ){
    stop(" length(slideDescription) < 1 in readIntensities \n", call.=FALSE)
  }

  if( dim(slideDescription)[1] < 1 ){
    stop(" dim(slideDescription)[1] < 1 in readIntensities \n", call.=FALSE)
  }

  ## 2

  if( missing( slideNameColumn ) ){
    slideNameColumn <- fileNameColumn
  }else{
    
      if( length(slideNameColumn) != 1 ){
        stop(" argument slideNameColumn must be of length 1 in readIntensities \n", call.=FALSE)
      }
      if( ! is.character(slideNameColumn) ){
        stop(" argument slideNameColumn must be of type character in readIntensities \n", call.=FALSE)
      }

  }
  ## 3: 1 2

  if( ! fileNameColumn %in% colnames(slideDescription) ){
    stop(paste(":fileNameColumn: :",fileNameColumn,
               ": not found in slideDescription in readIntensities\n",
               "found only: ",
               paste(colnames(slideDescription), collapse=","),
               sep=""), call.=FALSE)
  }
  if( ! slideNameColumn %in% colnames(slideDescription) ){
    stop(paste(":slideNameColumn: :",slideNameColumn,
               ": not found in slideDescription in readIntensities\n",
               "found only: ",
               paste(colnames(slideDescription), collapse=","),
               sep=""), call.=FALSE)
  }
  
  stopifnot(!is.null(slideDescription[,fileNameColumn]))
  stopifnot(!is.null(slideDescription[,slideNameColumn]))


  ## 4: 1 2 3

  if( ! all(is.null(channelColumn)) ){

    if( !all(is.character(channelColumn)) ){
      stop(" argument channelColumn must be of type character in readIntensities\n", call.=FALSE)
    }
    
    if( length(channelColumn) == 1 ){

      channelColumnName <- channelColumn
      greenChannelName <- "green"
      redChannelName <- "red"

    }else{
      if( ! all( c("channelColumnName", "green", "red") %in% names(channelColumn) )){
        stop(" names(channelColumn), i.e.:", paste(names(channelColumn),collapse=", "),": are not valid in readIntensities", call.=FALSE)
      }

      channelColumnName <- channelColumn["channelColumnName"]
      greenChannelName <- channelColumn["green"]
      redChannelName <- channelColumn["red"]

    }
    if( ! (channelColumnName %in% names(slideDescription) )){
      stop(" channelColumnName, i.e.:", channelColumnName,": not found in names(slideDescription), i.e.:", paste(names(slideDescription), collapse=", "), ": in readIntensities", call.=FALSE)
    }
    
  }
  
  
  if( ! all(is.null(channelColumn)) ){
    
    greenIndexes <- which( slideDescription[,channelColumnName] == greenChannelName )
    greenHybAttrUnordered <- slideDescription[greenIndexes, ,drop=FALSE]
    redIndexes <- which( slideDescription[,channelColumnName] == redChannelName )
    redHybAttrUnordered <- slideDescription[redIndexes, ,drop=FALSE]

    tmpGreen <- greenHybAttrUnordered[, slideNameColumn]
    tmpRed <- redHybAttrUnordered[, slideNameColumn]
    names(tmpGreen) <- NULL
    names(tmpRed) <- NULL
    if( ! identical( tmpGreen, tmpRed) ){

      greenHybAttr <- greenHybAttrUnordered[order(greenHybAttrUnordered[,slideNameColumn]),]
      redHybAttr <- redHybAttrUnordered[order(redHybAttrUnordered[,slideNameColumn]),]
      
    }else{

      greenHybAttr <- greenHybAttrUnordered
      redHybAttr <- redHybAttrUnordered
      
    }

    slideDescriptionExtractGreen <- greenHybAttr[, c(fileNameColumn,slideNameColumn)]
    slideDescriptionExtractRed <- redHybAttr[, c(fileNameColumn,slideNameColumn)]

    if( ! all(slideDescriptionExtractGreen[, slideNameColumn] ==
              slideDescriptionExtractRed[, slideNameColumn]      ) ){
      stop(" slideNameColumn is not consistent between both channels ", call.=FALSE)
    }else{
      
      slideNames <-  slideDescriptionExtractGreen[, slideNameColumn]

    }
    if( ! all(slideDescriptionExtractGreen[, fileNameColumn] ==
              slideDescriptionExtractRed[, fileNameColumn]      ) ){

      if(verbose){
        cat(" detected different fileNames for the same slideName, i.e. channel-wise fileNames in readIntensities\n")
      }
      fileNamesOfSlides <- cbind(slideDescriptionExtractGreen[, fileNameColumn],
                                 slideDescriptionExtractRed[, fileNameColumn]   )
      dimnames(fileNamesOfSlides) <- list( dimnames(fileNamesOfSlides)[[1]],
                                           c("green", "red")
                                          )
    }else{

      fileNamesOfSlides <- slideDescriptionExtractGreen[, fileNameColumn]

    }


    
  }else{

    slideNames <-  slideDescription[, slideNameColumn]
    fileNamesOfSlides <- slideDescription[, fileNameColumn]

    greenHybAttr <- slideDescription
    redHybAttr <- slideDescription
    
  }
  fileNamesOfSlides <- as.matrix(fileNamesOfSlides)
  
  ## 5

  if( length(type) != 1 ){
    stop(paste("type:",type," must be of length one in readIntensities",sep=""), call.=FALSE)
  }
  if( ! type %in% c("GenePix","ScanAlyze",
                    "generic", "genericOneFilePerChannel") ){
    stop(paste(" wrong type:",type," in readIntensities",sep=""), call.=FALSE)
  }
  if(type == "genericOneFilePerChannel"  &&
     all(is.null(channelColumn)) ){
    stop(paste(" type:",type," requires argument channelColumn in readIntensities",sep=""), call.=FALSE)
  }
    
  if( verbose ){cat(paste(" type: ",type," \n",sep=""))}
  
  if( type == "GenePix" ){

    if( is.null(spotAnnoColumns) ){
      spotAnnoColumns <-  c("Block", "Column", "Row", "Name", "ID")
    }
    ## note:
    ##         Cy3 - 532 - green
    ##         Cy5 - 635 - red
    if( is.null(dataColumns) ){
      dataColumns <- c("F532.Median", "B532.Median", "F635.Median", "B635.Median")
      names(dataColumns) <- c("greenForeground","greenBackground","redForeground","redBackground")
    }
    
  }else if( type == "ScanAlyze" ){

    ## notes on ScanAlyze
    ## http://rana.lbl.gov/EisenSoftware.htm
    ## http://rana.lbl.gov/manuals/ScanAlyzeDoc.pdf
    ## note: FLAG may differ between hybridisations
    
    if( is.null(spotAnnoColumns) ){
      spotAnnoColumns <- c( "HEADER", "SPOT", "GRID", "ROW", "COL" )
    }
    if( is.null(dataColumns) ){
      ## median values, channel 1 green, channel 2 red
      dataColumns <- c("CH1I", "CH1B", "CH2I", "CH2B")
      names(dataColumns) <- c("greenForeground","greenBackground","redForeground","redBackground")
    }
    if( is.null(removePatterns) ){
      removePatterns <- c("^REMARK")
    }
    
  }else if( type == "generic" ){
    
    if( is.null(spotAnnoColumns) ){
      stop(paste(" spotAnnoColumns are required for type:",type," in readIntensities",sep=""), call.=FALSE)
    }
    if( is.null(dataColumns) ){
      stop(paste(" named dataColumns are required for type:",type," in readIntensities",sep=""), call.=FALSE)
    }
     if( is.null(names(dataColumns)) ){
      stop(paste(" named dataColumns are required for type:",type," in readIntensities",sep=""), call.=FALSE)
    }

  }else if( type == "genericOneFilePerChannel" ){

    if( is.null(spotAnnoColumns) ){
      stop(paste(" spotAnnoColumns are required for type:",type," in readIntensities",sep=""), call.=FALSE)
    }
    if( is.null(dataColumns) ){
      stop(paste(" named dataColumns are required for type:",type," in readIntensities",sep=""), call.=FALSE)
    }
    if( is.null(names(dataColumns)) ){
      stop(paste(" named dataColumns are required for type:",type," in readIntensities",sep=""), call.=FALSE)
    }
    
    greenSelection <- grep( "green", names(dataColumns) )
    redSelection <- grep( "red", names(dataColumns) )
    dataColumns[greenSelection] <- paste("green",dataColumns[greenSelection],sep="")
    dataColumns[redSelection] <- paste("red",dataColumns[redSelection],sep="")
    
  }else{
    stop(paste(" wrong type:",type," in readIntensities",sep=""), call.=FALSE)
  }

  ## 6 : 5
  if( ! all( is.character(spotAnnoColumns) ) ){
    stop(paste(" spotAnnoColumns must be of type character in readIntensities",sep=""), call.=FALSE)
  }
  if( ! all( is.character(dataColumns) ) ){
    stop(paste(" dataColumns must be of type character in readIntensities",sep=""), call.=FALSE)
  }

  
  ## 7 : 5
                                                 
  if( !missing( spotIdentifier ) ){
    
    if( ! length(spotIdentifier) == 1 ){
      stop(" argument spotIdentifier must be of length 1 in readIntensities \n", call.=FALSE)
    }     
    if( ! is.character(spotIdentifier) ){
      stop(" argument spotIdentifier must be of type character in readIntensities \n", call.=FALSE)
    }
    if( ! (spotIdentifier %in% spotAnnoColumns) ){
      spotAnnoColumns <- c(spotAnnoColumns, spotIdentifier)
    }
  }

  ##

  tmpAllNames <- c("greenForeground","greenBackground","redForeground","redBackground")
  if( ! ((all(names(dataColumns) %in%  tmpAllNames)) &&
         (all(tmpAllNames %in% names(dataColumns)))     )){
    stop(paste(" wrong names(dataColumns):",paste(names(dataColumns),collapse=", ")," in readIntensities",sep=""), call.=FALSE)
  }
  if( any(duplicated(names(dataColumns))) ){
    stop(paste(" invalid names(dataColumns):",paste(names(dataColumns),collapse=", ")," , i.e. they must be unique in readIntensities",sep=""), call.=FALSE)
  }
  
  ## check loadPath
  if( any(is.na(file.info(loadPath)$isdir)) ){
    stop(" the loadPath does not exist \n", call.=FALSE)
  }else{
    if( ! file.info(loadPath)$isdir ){
      stop(" the loadPath is not a directory \n", call.=FALSE)
    }
  }

  if(verbose){ cat("\n checking existence of all image data result files\n") }
  if( ! all(fileNamesOfSlides %in% dir(path=loadPath) ) ){
    preText <- " could not find the following image data result files:\n"
    notFoundOnes <- ! (fileNamesOfSlides %in% dir(path=loadPath))
    stop( paste(preText, paste(fileNamesOfSlides[notFoundOnes],
                               collapse="\n"), "\n"),
         call.=FALSE)
  }

  stopifnot( ! is.null( dim(fileNamesOfSlides) ) )
  stopifnot( length(dim(fileNamesOfSlides)) == 2 )
  nrOfFiles <- dim(fileNamesOfSlides)[1]
  stopifnot( length(slideNames) == nrOfFiles )
  stopifnot( dim(fileNamesOfSlides)[2] <= 2 )
  
  ## 
  if( verbose ){
                cat(paste(" using dataColumns:\n"))
                cat(" ", paste(names(dataColumns), collapse=",\t"), "\n")
                cat(" ", paste(     (dataColumns), collapse=",\t"), "\n")
                cat(paste(" using spotAnnoColumns: ", 
                    paste(spotAnnoColumns, collapse=", "),"\n"))
                if( ! is.null(removePatterns) ){
                  cat(paste(" using removePatterns: ",
                            paste(removePatterns, collapse=","),
                            "\n"))
                }
               }
  ##

  if( dim(fileNamesOfSlides)[2] == 2 ){

    if( type != "genericOneFilePerChannel" ){
      stop( paste(" type: ", type, " requires one file per hybridisation in readIntensities "), call.=FALSE)
    }
    
  }
  
  for (i in (1:nrOfFiles)){

    fn <- file.path(loadPath, fileNamesOfSlides[i,])
    names(fn) <- colnames(fileNamesOfSlides[i,,drop=FALSE])
    
    if( verbose ){cat(" reading", fn, "\n")}
    
    ## read file(s) fn and create data.frame dat
    ############################################
    if( type == "GenePix" ){

      ## required by skip
      txt <- scan(file=fn,what="character",sep="\n", allowEscapes=FALSE)

      if( ! is.null(removePatterns) ){
        removeLineNumbers <- as.vector(unlist(sapply(removePatterns,
                                            function(p) grep(p, txt))))
        if( length(removeLineNumbers) > 0 ){
          txt <- txt[-removeLineNumbers]
        }
      }
      
      if( is.null(skip) ){
        skipLines <- grep("Block...Column", txt) 
        if( ! (is.numeric(skipLines) && length(skipLines)==1) ){
          stop(" could not dynamically determine skip for type=\"GenPix\" in readIntensities\n specify the skip argument manually", call.=FALSE)
        }
        skipLines <- skipLines - 1
      }else{
        skipLines <- skip
      }

      ## textConnections appeared to be slow ?? hence
      ## I use a direct connection to the file
      if( ! is.null(removePatterns) ){
        txtCon <- textConnection(txt) 
        dat <- read.table(txtCon, sep='\t', header=TRUE,
                          skip=skipLines, as.is=TRUE, ...)
        close(txtCon)
      }else{
        dat <- read.table(fn, sep='\t', header=TRUE,
                          skip=skipLines, as.is=TRUE, ...)
      }
      
    }else if( type == "generic" || type == "ScanAlyze" ){


      if( is.null(skip) ){
        skipLines <- 0
      }else{
        skipLines <- skip
      }

      ## textConnections appeared to be slow ?? hence
      ## I use a direct connection to the file
      if( ! is.null(removePatterns) ){
        txt <- scan(file=fn,what="character",sep="\n",allowEscapes=FALSE)
        removeLineNumbers <- as.vector(unlist(sapply(removePatterns,
                                            function(p) grep(p, txt))))
        if( length(removeLineNumbers) > 0 ){
          txt <- txt[-removeLineNumbers]
        }
        txtCon <- textConnection(txt) 
        dat <- read.table(file=txtCon, sep="\t", header=TRUE,
                          skip=skipLines, as.is=TRUE, ...)
        close(txtCon)
      }else{
        dat <- read.table(file=fn, sep="\t", header=TRUE,
                          skip=skipLines, as.is=TRUE, ...)
      }
        
    }else if( type == "genericOneFilePerChannel" ){

      if( length(fn) != 2 ){
        stop("require one file per channel cf. argument channelColumn in readIntensities", call.=FALSE)
      }
      
      if( is.null(skip) ){
        skipLines <- 0
      }else{
        skipLines <- skip
      }

      ## textConnections appeared to be slow ?? hence
      ## I use a direct connection to the file
      if( ! is.null(removePatterns) ){
        txtGreen <- scan(file=fn["green"],what="character",sep="\n",allowEscapes=FALSE)
        removeLineNumbers <- as.vector(unlist(sapply(removePatterns,
                                          function(p) grep(p, txtGreen))))
        if( length(removeLineNumbers) > 0 ){
          txtGreen <- txtGreen[-removeLineNumbers]
        }
        txtGreenCon <- textConnection(txtGreen) 
        datGreen <- read.table(file=txtGreenCon, sep="\t", header=TRUE,
                               skip=skipLines, as.is=TRUE, ...)
        close(txtGreenCon) 
      }else{
        datGreen <- read.table(file=fn["green"], sep="\t", header=TRUE,
                               skip=skipLines, as.is=TRUE, ...)
      }

      datGreenSpotanno <- datGreen[colnames(datGreen) %in% spotAnnoColumns]
      colnames(datGreen) <- paste("green", colnames(datGreen),sep="")
      datGreenData <- datGreen[colnames(datGreen) %in% dataColumns]

      ## textConnections appeared to be slow ?? hence
      ## I use a direct connection to the file
      if( ! is.null(removePatterns) ){
        txtRed <- scan(file=fn["red"],what="character",sep="\n",allowEscapes=FALSE)
        removeLineNumbers <- as.vector(unlist(sapply(removePatterns,
                                            function(p) grep(p, txtRed))))
        if( length(removeLineNumbers) > 0 ){
          txtRed <- txtRed[-removeLineNumbers]
        }
        txtRedCon <- textConnection(txtRed) 
        datRed <- read.table(file=txtRedCon, sep="\t", header=TRUE,
                             skip=skipLines, as.is=TRUE, ...)
        close(txtRedCon)
      }else{
        datRed <- read.table(file=fn["red"], sep="\t", header=TRUE,
                             skip=skipLines, as.is=TRUE, ...)
      }
      
      datRedSpotanno <- datRed[colnames(datRed) %in% spotAnnoColumns]
      colnames(datRed) <- paste("red", colnames(datRed),sep="")
      datRedData <- datRed[colnames(datRed) %in% dataColumns]

      ## possibly you may offer a more complex merge operation ...
      if( ! (all(datGreenSpotanno==datRedSpotanno, na.rm=TRUE)) ){
        inconsist <- which( ! datGreenSpotanno ==
                              datRedSpotanno , arr.ind=TRUE)
        stop(paste(" found inconsistency between annotation",
                   " information given for the same slide ",
                   paste(fn, collapse=", "),
                   " (one file per channel) in readIntensities \n\n",
                   " first occurence in row: ", inconsist[1,1], "\n",
                   " and column: ", inconsist[1,2], " i.e. ",
                   colnames(datGreenSpotanno)[inconsist[1,2]], "\n",
                   " (some) further rows ",
                  paste(inconsist[1:min(10,dim(inconsist)[1]),1],collapse=","),
                   "\n and columns ",
                  paste(inconsist[1:min(10,dim(inconsist)[1]),2],collapse=","),
                   "\n", sep=""), call.=FALSE)
      }
      
      dat <- cbind(datGreenSpotanno, datGreenData, datRedData)
      
    }else{
      
      stop(paste(" wrong type:",type," in readIntensities",sep=""), call.=FALSE)
        
    }
            
    nc <- spotAnnoColumns[!(spotAnnoColumns %in% colnames(dat))]
    if(length(nc)>0){
      stop(paste("spotAnnoColumns", paste(nc,collapse=", "), "not found in data file: ", paste(fn,collapse=", "), " containing ", paste(colnames(dat), collapse=", ")), call.=FALSE)
    }
    nc <- dataColumns[!(dataColumns %in% colnames(dat))]
    if(length(nc)>0){
      stop(paste("dataColumns", paste(nc,collapse=", "), "not found in data file: ",paste(fn,collapse=", "), " containing ", paste(colnames(dat), collapse=", ")), call.=FALSE)
    }
    
    if(i==1){
      ## array dimensions of qua [number of genes, 4, number of hybridisations]
      qua <- array(NA, dim=c(nrow(dat), 4, nrOfFiles))
      dimnames(qua) <- list( NULL, c("green", "greenBackground", "red", "redBackground"), slideNames ) 
      # annotation information for all genes
      spotanno <-  dat[,spotAnnoColumns]
    }else{
      ## check consistency of available information (NAs may differ)
      ## across slides (confirmation of the same slide type)
      if( ! (all(dat[,spotAnnoColumns]==spotanno, na.rm=TRUE)) ){
        inconsist <- which( ! dat[,spotAnnoColumns] == spotanno, arr.ind=TRUE)
        stop(paste(" found inconsistency between spot annotation",
                   " information of different slides in readIntensities \n",
                   " first occurence in row: ", inconsist[1,1], "\n",
                   " and column: ", inconsist[1,2], " i.e. ",
                   colnames(spotanno)[inconsist[1,2]], "\n",
                   " (some) further rows ",
                  paste(inconsist[1:min(10,dim(inconsist)[1]),1],collapse=","),
                   "\n and columns ",
                  paste(inconsist[1:min(10,dim(inconsist)[1]),2],collapse=","),
                   "\n", sep=""), call.=FALSE)
      }
    }
      
    qua[,"green",i] <- dat[,dataColumns["greenForeground"]] 
    qua[,"greenBackground",i] <-dat[,dataColumns["greenBackground"]] 
    qua[,"red",i] <- dat[,dataColumns["redForeground"]] 
    qua[,"redBackground",i] <- dat[,dataColumns["redBackground"]] 
    
  } ## end of for i in nrOfFiles


  arrayDataObject <- new("arrayData",
                         intensities=qua,
                         spotAttr=spotanno,
                         hybAttr=list(green=greenHybAttr, red=redHybAttr)
                         )
  
  return(arrayDataObject)
  
}## end of readIntensities
