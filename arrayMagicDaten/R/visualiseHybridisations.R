# /**
#
# \name{visualiseHybridisations}
#
# \title{Microarray intensity data are visualised with respect
#        to their spatial layout}
#
# \alias{visualiseHybridisations}
#
# @usage
#
# \description{Two colour microarray intensity data are visualised
#              with respect to their spatial layout.
#              Missing values (\code{NA}) as well as
#              minus infinity (\code{-Inf})
#              etc. are not visualised.
#              They appear to be white, i.e. in the colour of the background.
#              \code{-Inf} might be a result of the (log-)transformation
#              of the data; cf. the argument \code{transFunc}.}
#
# \value{None, but a plot is generated.}
#
# \arguments{
#
#  \item{arrayDataObject}{object of class \code{\link{arrayData}};
#                         required; default: missing}
#  \item{exprSetRGObject}{object of class \code{\link{exprSetRG}};
#                         only required,
#                         if \code{type = "normalised"} or
#                         \code{type = "normalisedLogRatio"};
#                         default: missing}
#  \item{type}{vector of character strings; required; default "raw";
#              valid strings are "raw", "rawLogRatio",
#              "rawLogRatioBackgroundSubtracted", "normalised"
#              or "normalisedLogRatio"}
#  \item{hybridisations}{vector of integers; optional; default: missing,
#                        i.e. all microarray slides}
#  \item{slideNameColumn}{character string specifying the column
#                         \code{getHybAttr(arrayDataObject)}
#                         which contains the names
#                         of the hybridisations; optional;
#                         default: missing}
#  \item{numberOfSpots}{integer; optional; default: missing;
#                       but required if the values in \code{arrayDataObject}
#                       do not cover the whole microarray slide, i.e.
#                       do not contain all spots;
#                       cf. \code{\link{spatialLayout}}.}
#  \item{nrOfBlocksPerRow}{integer; cf. \code{\link{spatialLayout}};
#                          required; default: 4}
#  \item{mappingColumns}{list; required; the list elements named
#                        "Block", "Column", "Row" define the
#                        corresponding column names given in
#                        \code{getSpotAttr(arrayDataObject)};
#                        the "Block" element is optional, if
#                        missing a single block is assumed;
#                        default: \code{list(Block="Block",
#                        Column="Column", Row="Row")};
#                        cf. \code{\link{spatialLayout}}
#                       }
#  \item{transFunc}{transformation function
#                   applied to the raw data,
#                   in particular in combination with \code{type = "raw"}
#                   you may for example use \code{log} or \code{rank};
#                   (note: \code{type = "rawLogRatio"} and
#                          \code{type = "rawLogRatioBackgroundSubtracted"}
#                          require log );
#                   required; default: log}
#  \item{savePath}{character string; required; default: "."}
#  \item{plotOutput}{character string; required; either "screen", "pdf"
#                    or "win.metafile"; default: "pdf"}
#
# }
# 
# \examples{
#	}
#
# \seealso{
#           \code{\link{spatialLayout}},
#           \code{\link{exprSetRG-class}},
#           \code{\link{arrayData-class}}
#         }
#
# \keyword{hplot}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */
visualiseHybridisations <- function(arrayDataObject,
                                    exprSetRGObject,
                                    type="raw",
                                    hybridisations,
                                    slideNameColumn,
                                    numberOfSpots,
                                    nrOfBlocksPerRow=4,
                                    mappingColumns=
                                       list(Block="Block", Column="Column", Row="Row"),
                                    transFunc=log,
                                    savePath=".",
                                    plotOutput="pdf"){

  
  suffix <- ""
  if( plotOutput == "pdf" ){
    suffix <- "pdf"
  }else if( plotOutput == "win.metafile" ){
    suffix <- "wmf"
  }
  
  if( ! plotOutput %in% c("pdf", "screen", "win.metafile") ){
    stop(paste("invalid argument:",plotOutput,": for plotOutput in visualiseHybridisations",sep=""), call.=FALSE)
  }

  
  if( plotOutput == "win.metafile" ){
    if( .Platform$OS.type != "windows" ){
      stop( paste(plotOutput, " only valid if running under Windows in qualityDiagnostics "), call.=FALSE )
    }
  }
  
  if( ! all( c("Column","Row") %in% names(mappingColumns) ) ){
    stop(paste("invalid argument for mappingColumns\n names(mappingColumns): ", names(mappingColumns), "\n do not match the requirements\n in visualiseHybridisations", sep="")) 
  }
  NOBLOCK <- FALSE
  
  if( ! ( "Block" %in% names(mappingColumns) ) ){
    NOBLOCK <- TRUE
    nrOfBlocksPerRow = 1
  }
  
  if( ! all( is.character(type)  ) ){
    stop(" all type elements must be of type character in visualiseHybridisations ")
  }
    
  for( typeItem in type ){
  
  
    if( ! typeItem %in% c("raw","rawLogRatio", "rawLogRatioBackgroundSubtracted",
                          "normalised", "normalisedLogRatio") ){
      stop(paste(" invalid typeItem: ", typeItem, "in visualiseHybridisations ", sep=""))
    }
  
    if( typeItem %in% c("normalised", "normalisedLogRatio") &
       missing( exprSetRGObject ) ){
      stop(paste(" exprSetRGObject is missing if typeItem = ", typeItem, " in visualiseHybridisations ", sep=""))
    }
    
    
    if( missing( arrayDataObject ) ){
      stop("arrayDataObject is missing in visualiseHybridisations")
    }
    
    intensities <- getIntensities(arrayDataObject)
    spotAttr <- getSpotAttr(arrayDataObject)
    hybAttr <- getHybAttr(arrayDataObject)
  
    ## consistency check
    if( dim(intensities)[1] != dim(spotAttr)[1] ){
      stop("inconsistent arrayDataObject in visualiseHybridisations ")
    }
    if( ! missing( exprSetRGObject ) ){
      
      if( dim(exprs(getExprSetRed(exprSetRGObject)))[1] != dim(intensities)[1] ){
        stop("inconsistent arrayDataObject and exprSetRGObject with respect to number of rows (intensity values) in visualiseHybridisations ")
      }
      if( dim(exprs(getExprSetLogRatio(exprSetRGObject)))[2] != dim(intensities)[3] ){
        stop("inconsistent arrayDataObject and exprSetRGObject with respect to number hybridisations in visualiseHybridisations ")
      }
    }
    
    if( missing(hybridisations) ){
      hybridisations <- (1:dim(intensities)[3])
    }
  
    if( ! all( hybridisations %in% (1:dim(intensities)[3]) ) ){
      stop(" hybridisations out of range in visualiseHybridisations ")
    }
    
    if( ! missing(slideNameColumn) ){
      
      if( length(slideNameColumn) != 1 ){
        stop("length of slideNameColumn must equal one in visualiseHybridisations")
      }
      
      if(! slideNameColumn %in% colnames(hybAttr)  ){
        stop(" slideNameColumn not in colnames(hybAttr) in visualiseHybridisations")
      }
      hybridisationNames <- hybAttr[hybridisations, slideNameColumn]
      
    }else{
      
      hybridisationNames <- as.character(hybridisations)
      
    }
    
    if( !  (mappingColumns[["Row"]] %in% colnames(spotAttr) ) ){
      stop(paste("mappingColumns[[\"Row\"]] :", mappingColumns[["Row"]],": not in colnames(spotAttr) in visualiseHybridisations",sep=""))
    }
    if( !  (mappingColumns[["Col"]] %in% colnames(spotAttr) ) ){
      stop(paste("mappingColumns[[\"Col\"]] :", mappingColumns[["Col"]],": not in colnames(spotAttr) in visualiseHybridisations",sep=""))
    }
    
  
  for( counter in 1:length(hybridisations) ){
    
    hybridisation <- hybridisations[counter]
    
    myRow <- spotAttr[, mappingColumns[["Row"]]]
    myCol <- spotAttr[, mappingColumns[["Column"]]]
    if( NOBLOCK ){
      myBlock <- rep(1, dim(spotAttr)[1] )
    }else{
      myBlock <- spotAttr[, mappingColumns[["Block"]]]
    }
    
    if( !missing(numberOfSpots) ){
      
      
      if( typeItem %in% c("normalised", "normalisedLogRatio") ){
        
        greenf <-  spatialLayout(exprs(getExprSetGreen(exprSetRGObject))[,hybridisation],
                                 row=myRow,myCol,myBlock,
                                 numberOfValues=numberOfSpots,
                                 nrOfBlocksPerRow=nrOfBlocksPerRow
                                 )
        
        redf <-  spatialLayout(exprs(getExprSetRed(exprSetRGObject))[,hybridisation],
                               row=myRow,myCol,myBlock,
                               numberOfValues=numberOfSpots,
                               nrOfBlocksPerRow=nrOfBlocksPerRow
                               )
        
      }else if( typeItem %in% c("raw","rawLogRatio", "rawLogRatioBackgroundSubtracted") ){
        
        greenf <- spatialLayout(intensities[,"green",hybridisation],
                                row=myRow,myCol,myBlock,
                                numberOfValues=numberOfSpots,
                                nrOfBlocksPerRow=nrOfBlocksPerRow
                                )
        
        redf <- spatialLayout(intensities[,"red",hybridisation],
                              row=myRow,myCol,myBlock,
                              numberOfValues=numberOfSpots,
                              nrOfBlocksPerRow=nrOfBlocksPerRow
                              )
        
        if( typeItem %in% c("raw", "rawLogRatioBackgroundSubtracted") ){
        
          greenb <- spatialLayout(intensities[,"greenBackground",hybridisation],
                                  row=myRow,myCol,myBlock,
                                  numberOfValues=numberOfSpots,
                                  nrOfBlocksPerRow=nrOfBlocksPerRow
                                  )
        
          redb <- spatialLayout(intensities[,"redBackground",hybridisation],
                                row=myRow,myCol,myBlock,
                                numberOfValues=numberOfSpots,
                                nrOfBlocksPerRow=nrOfBlocksPerRow
                                )
        }
        
      }else{
        stop(" unexpected case I in visualiseHybridisations ")
      }
      
    }else{
      
      if( typeItem %in% c("normalised", "normalisedLogRatio") ){
        
        greenf <-  spatialLayout(exprs(getExprSetGreen(exprSetRGObject))[,hybridisation],
                                 row=myRow,myCol,myBlock,
                                 nrOfBlocksPerRow=nrOfBlocksPerRow
                                 )
        
        redf <-  spatialLayout(exprs(getExprSetRed(exprSetRGObject))[,hybridisation],
                               row=myRow,myCol,myBlock,
                               nrOfBlocksPerRow=nrOfBlocksPerRow
                               )
        
      }else if( typeItem %in% c("raw","rawLogRatio", "rawLogRatioBackgroundSubtracted") ){
        
        
        greenf <- spatialLayout(intensities[,"green",hybridisation],
                                row=myRow,myCol,myBlock,
                                nrOfBlocksPerRow=nrOfBlocksPerRow
                                )
        
        redf <- spatialLayout(intensities[,"red",hybridisation],
                              row=myRow,myCol,myBlock,
                              nrOfBlocksPerRow=nrOfBlocksPerRow
                              )
      
        if( typeItem %in% c("raw", "rawLogRatioBackgroundSubtracted") ){
          
          greenb <- spatialLayout(intensities[,"greenBackground",hybridisation],
                                  row=myRow,myCol,myBlock,
                                  nrOfBlocksPerRow=nrOfBlocksPerRow
                                  )
          
          redb <- spatialLayout(intensities[,"redBackground",hybridisation],
                                row=myRow,myCol,myBlock,
                                nrOfBlocksPerRow=nrOfBlocksPerRow
                                )

        }
          
      }else{
        stop(" unexpected case II in visualiseHybridisations ")
      }
      
      
      
    }
    
    WIDTH <- 6
    CEX.MAIN <- 1
    if( typeItem == "raw" | typeItem == "normalised" ){
      WIDTH <- 20
      CEX.MAIN <- 2
    }
    
    hybName <- hybridisationNames[counter]
    fileName <- paste("hybridisation_",typeItem,"_",hybName,".", suffix,sep="")



    if( plotOutput == "screen" ){
      if( interactive() ){
        x11(width=WIDTH)
      }
    }else if( plotOutput == "pdf" ){
      pdf(file=file.path(savePath, fileName), width=WIDTH)
    }else if( plotOutput == "win.metafile" ){
      if( .Platform$OS.type == "windows" ){
        win.metafile(file=file.path(savePath, fileName), width=WIDTH)
      }else{
        stop( paste(plotOutput, " only valid if running under Windows in visualiseHybridisations "), call.=FALSE )
      }
    }else{
      stop(" unexpected case; unknown plotOutput argument in visualiseHybridisations ", call.=FALSE)
    }

    
    
    if( typeItem == "raw" ){
      
      ## the number of columns appears to be limited in layout ...
      layout(matrix(c(1,1,2, 3,3,4, 5,5,6, 7,7,8),nrow=1))
      ##layout(matrix(c(1,1,1,2,3,3,4,5,5,5,6,7,7,8),nrow=1))
      
      m <- transFunc(greenf)
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main="green foreground", separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", axes=FALSE, ann=FALSE)
        plot(0,type="n", axes=FALSE, ann=FALSE)
      }
      mtext(hybName)
      
      m <- transFunc(greenb)
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main="green background", separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", axes=FALSE, ann=FALSE)
        plot(0,type="n", axes=FALSE, ann=FALSE)
      }
      
      mtext(hybName)
      
      m <- transFunc(redf)
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main="red foreground", separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", axes=FALSE, ann=FALSE)
        plot(0,type="n", axes=FALSE, ann=FALSE)
      }
      mtext(hybName)
      
      m <- transFunc(redb)
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main="red background", separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", axes=FALSE, ann=FALSE)
        plot(0,type="n", axes=FALSE, ann=FALSE)
      }
      mtext(hybName)
      
      layout(1)

    }else if( typeItem == "normalised" ){
      
      layout(matrix(c(1,1,1,2,2, 3,3,3,4,4, 5,5,5,6,6),nrow=1))

      m <- greenf - redf
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main="normalised log ratio", separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", axes=FALSE, ann=FALSE)
        plot(0,type="n", axes=FALSE, ann=FALSE)
      }
      mtext(hybName)
      
      m <- greenf
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main="normalised green channel", separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", axes=FALSE, ann=FALSE)
        plot(0,type="n", axes=FALSE, ann=FALSE)
      }
      mtext(hybName)
      
      m <- redf
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main="normalised red channel", separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", axes=FALSE, ann=FALSE)
        plot(0,type="n", axes=FALSE, ann=FALSE)
      }
      mtext(hybName)
      
      layout(1)

    }else if( typeItem == "rawLogRatio" | typeItem == "normalisedLogRatio" ){
      
      layout(matrix(c(1,1,1,1,2,2),nrow=1))
      
      if( typeItem == "rawLogRatio" ){

        if( ! identical(transFunc, log) ){
          stop(" if typeItem rawLogRatio transFunc has to be log ")
        }
        
        mG <- transFunc(greenf) ## log
        mR <- transFunc(redf)   ## log
        m <- mG - mR

        
      }else if( typeItem ==  "normalisedLogRatio" ){

        m <- greenf - redf
        
      }else{
        stop(" unexpected case III in visualiseHybridisations ")
      }
      
      m[ which(is.infinite(m)) ] <- NA
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main=typeItem, separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", main=typeItem, axes=FALSE, ann=FALSE)
        plot(0,type="n", main=typeItem, axes=FALSE, ann=FALSE)
      }
      mtext(hybName)

      layout(1)
      
    }else if( typeItem == "rawLogRatioBackgroundSubtracted" ){

      layout(matrix(c(1,1,1,1,2),nrow=1))
      
      if( ! identical(transFunc, log) ){
        stop(" if typeItem rawLogRatioBackgroundSubtracted transFunc has to be log ")
      }
      ##mG <- transFunc(greenf-greenb)
      ##mR <- transFunc(redf-redb)
      ## better avoid warnings in visualiseHybridisations
      green <- greenf-greenb
      red <- redf-redb

      if( any( green < 0 ) ){
        green[ green<0 ] <- NA
      }
      if( any( red < 0 ) ){
        red[ red<0 ] <- NA
      }
      mG <- transFunc(green) ## log
      mR <- transFunc(red)   ## log
      m <- mG - mR
      m[ which(is.infinite(m)) ] <- NA 
      if( ! all( is.na(m) ) ){
        plot.imageMatrix(m, main=typeItem, separateZScale=TRUE,  zScale=TRUE, xlab="", ylab="", cex.main=CEX.MAIN)
      }else{
        plot(0,type="n", main=typeItem, axes=FALSE, ann=FALSE)
        plot(0,type="n", main=typeItem, axes=FALSE, ann=FALSE)
      }
      mtext(hybName)

      layout(1)
      
    }else{
      stop(" unknown typeItem in visualiseHybridisations " )
    }
    
    if( plotOutput == "pdf" || plotOutput == "win.metafile" ){
      dev.off()
    }

    
  }## end of for

  }## end of for typeItem in type
  
}## end of function



