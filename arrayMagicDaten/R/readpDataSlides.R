# /**
#
# \name{readpDataSlides}
#
# \title{Reads the description of all slides or channels}
#
# \alias{readpDataSlides}
#
# \keyword{IO}
#
# \description{The function reads the \code{slideDescriptionFile},
#              which contains all information row-wise
#              for each slide/microarray
#              or for each channel in a tab-deliminated text file.
#              The file is read up to the first line
#              containing solely "white space".
#	}
#
# \value{ \code{\link{data.frame}} }
#
# @usage
#
# \arguments{
#
#  \item{slideDescriptionFile}{ character string;
#       required; default "slideDescription.txt".
#       The first line of the file must contain all column names,
#       in particular the column named "fileName" which contains
#       image analyis result file names and possibly
#       additionally a column which contains the slide names;
#       cf. the arguments \code{fileNameColumn} and
#                         \code{slideNameColumn}
#           of \code{\link{readIntensities}}.
#       Note: Column names must not contain underlines or spaces.
#       }
#  \item{loadPath}{ character string; required; default: ".".
#       The path is used to load the \code{slideDescriptionFile};
#       note: "." refers to the working directory.
#       }
#  \item{deleteBlanks}{ logical; required; default: \code{TRUE}.
#                       Any blank character is removed from
#                       the text body of the
#                       \code{slideDescriptionFile}.
#                       Thus no item/name should contain spaces.
#                     }
#  \item{verbose}{ logical; required; default: \code{TRUE} }
#
# }
#
# \examples{
#       LOADPATH <- file.path(.path.package("arrayMagic"), "extdata")
# 	SLIDEDESCRIPTIONFILE <- "slideDescription"
#  
# 	resultObject <- readpDataSlides(
#                             loadPath=LOADPATH, 
#                             slideDescriptionFile=SLIDEDESCRIPTIONFILE
#                  )
# }
#
# \seealso{\code{\link{readIntensities}},
#          \code{\link{processArrayData}}
#          }
# }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
#
# */
readpDataSlides <- function(
                            slideDescriptionFile = "slideDescription.txt",
                            loadPath = ".",
                            deleteBlanks = TRUE,
                            verbose = TRUE
                             ){
  
  
  ## check loadPath
  if( any(is.na(file.info(loadPath)$isdir)) ){
    stop(" the loadPath does not exist \n", call.=FALSE)
  }else{
    if( ! file.info(loadPath)$isdir ){
      stop(" the loadPath is not a directory \n", call.=FALSE)
    }
  }

  ## description file 
  descriptionFile <- file.path(loadPath, slideDescriptionFile)
  if( file.exists(descriptionFile) ){
    if(verbose){cat(" using description file: ");cat(descriptionFile);cat("\n")}
  }else{
    stop(paste(" no description file ",descriptionFile ,"found \n"), call.=FALSE)
  }

  ## description file infos
  if(verbose){
    cat("\n count fields information for description file:\n")
    cat(descriptionFile);cat("\n")
    cat("\n")
    cat(" the number of detected columns=fields is shown in first line \n")
    cat(" of the output and the number of such rows in the second line \n")
    cat(" (only one pair of columns\n")
    cat("                     rows   should appear)\n")
  }

  txt <- scan(file=descriptionFile,what="character",
              blank.lines.skip = FALSE, sep="\n", allowEscapes=FALSE)
  emptyLines <- grep("^[[:space:]]+$",txt)
  if( length(emptyLines) >= 1 ){
    txt <- txt[1:(emptyLines[1]-1)]
  }

  descriptionConnection <- textConnection(txt)
  cF <- count.fields(descriptionConnection, sep="\t", quote="\"")
  if( is.null(cF)  | all(is.na(cF)) ){
    stop(" no information found in file \n", call.=FALSE)
  }
  tmp <- table(cF)
  if(verbose){
    cat(format(dimnames(tmp)));cat("\n")
    cat(tmp);cat("\n")
  }
  close(descriptionConnection)
  
  descriptionConnection <- textConnection(txt)
  description <- read.table(descriptionConnection, sep='\t',
                            header=TRUE, as.is=TRUE)
  close(descriptionConnection)
  

  if( deleteBlanks ){
    if(verbose){
      cat("\n automatic deletion of spaces/blanks in the body of the description file\n")
      cat(" thus any element/item/name should not contain spaces\n")
    }
    for (cn in colnames(description)) {
      if(is.character(description[, cn])) {
        description[, cn] = I(gsub(" ", "", description[, cn]))
      } ## if
    } ## for
  }
  
  
  return(description)
  
}## end of readpDataFiles
