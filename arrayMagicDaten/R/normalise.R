# /**
#
# \name{normalise}
#
# \title{Normalisation of microarray data}
#
# \alias{normalise}
#
# \keyword{utilities}
#
# @usage
#
# \description{An object of class \code{\link{exprSetRG}} is generated
#              which contains the resulting normalised data.}
#
# \arguments{
#  \item{arrayDataObject}{object of class \code{\link{arrayData}};
#                         required; default missing.
#                 \code{arrayDataObject} must contain the raw data, i.e.
#                 a three dimensional array
#                 (spot x channel x hybridisation),
#                 cf. the class methods \code{\link{getIntensities}}
#                 and \code{\link{intensities<-}},
#                 and must contain information
#                 on the hybridisations,
#                 cf. class methods \code{\link{getHybAttr}} and
#                 \code{{hybAttrList<-}}.
#                 If the argument \code{subGroups} (see below)
#                 is specified it must also contain 
#                 information on the spots, cf. the class methods
#                 \code{\link{getSpotAttr}} and
#                 \code{\link{spotAttr<-}}.
#                 Note: Weights are only used by loess-type normalisations.
#                }
#  \item{subtractBackground}{logical; default: \code{FALSE}}
#  \item{method}{character string; required; default: "\code{vsn}";
#        possible values:
#        "\code{none}", "\code{vsn}", "\code{quantile}",
#        "\code{loess}", "\code{loessScale}", "\code{loessQuantile}".
#        Note: "\code{quantile}" and  "\code{loess*}" normalisation
#              transform the data to the (natural) logarithmic scale.
#        Note: Weights are only used by loess-type normalisations.
#        Note: "\code{loessScale}" and "\code{loessQuantile}" refer
#              to a loess normalisation followed by a
#              between slide/ \code{subGroups} normalisation, cf. the function
#              \code{normalizeBetweenArrays} of the
#              \code{limma} package.
#        Note: "\code{quantile}" and  "\code{loess*}" normalisations
#              do call the corresponding methods of the \code{limma} package
#              and "\code{vsn}" uses the \code{vsn} package.
#        }
#  \item{subGroups}{character string or \code{NULL};
#                   required; default: \code{NULL};
#                   \code{subGroups} allows to define subgroups
#                   of spots of each hybridisation
#                   which are normalised separately like a 
#                   print-tip normalisation
#                   (Note: the between slide/ \code{subGroups} normalisation of
#                    "\code{loessScale}" and "\code{loessQuantile}"
#                    is separately applied for each subgroup). 
#                   The list must contain a column name refering to the
#                   \code{data.frame} of \code{getSpotAttr(arrayDataObject)}.
#                   In case of GenePix data you may for example
#                   specify the column name "Block".
#                   The column itself must contain integer values.
#                   Note: In case of \code{method == "vsn"},
#                   \code{vsn} is called with
#                   the argument \code{strata}, which differs
#                   from a separate normalisation of each subgroup
#                   with \code{vsn} itself. 
#                   }
#  \item{channelsSeparately}{logical; required; default: \code{FALSE};
#                            If \code{channelsSeparately} ist set to \code{TRUE}
#                            each channel is normalised separately. Only meaningful
#                            for single channel normalisation methods like
#                            "\code{vsn}" and
#                            "\code{quantile}" normalisation
#                            but not for ratio based
#                            normalisation like "\code{loess}".}
#  \item{hybridisationGroups}{list of vectors of indexes or
#                             the character string "slideBySlide";
#                             optional; default: missing.
#                             Each group of hybridisations
#                             is normalised separately. If
#                             missing all hybridisations are
#                             taken as one group. Only meaningful for
#                             normalisation methods like "vsn" and "quantile".
#                             The indexes must refer to the third
#                             dimension of \code{getIntensities(arrayDataObject)}
#                             and have to contain all hybridisations.
#                             }
#  \item{spotIdentifier}{ character string; optional; default missing.
#                         \code{spotIdentifier} specifies the column
#                         of \code{getSpotAttr(arrayDataObject)} which must
#                         contain non-unique spot or gene identifiers.
#                         The identifiers are used as names for the
#                         resulting \code{exprSetRG}-object; cf. the function
#                         \code{geneNames} of the \code{exprSetRG-class}. }
#  \item{verbose}{ logical; required; default: \code{TRUE}}
#  \item{...}{ further arguments which are passed to \code{vsn} if
#              "\code{vsn}" is specified for normalisation.
#              The arguments must not include \code{strata} and \code{verbose}.
#            }
#
# }
#
# \value{object of class \code{\link{exprSetRG}} }
#
# \seealso{ 
#           \code{\link{exprSetRG-class}},
#           \code{\link{processArrayData}},
#           \code{\link{processArrayDataObject}}
#         }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# \examples{
#
#       intensities <- array(data=runif(720),dim=c(120,2,3))
#       dimnames(intensities) <- list(NULL, c("green","red"), NULL)
#       hybAttr <- data.frame(Name=I(c("hx","hy","hz")), Index=c(1:3))
# 	arrayDataObject <- new("arrayData",
#                              intensities=intensities,
#                              hybAttrList=list(red=hybAttr,green=hybAttr)
#                             )
#       exprSetRGObject <- normalise(arrayDataObject = arrayDataObject,
#                               subtractBackground = FALSE,
#                               method = "none",
#                               verbose = TRUE
#                               )
#       nRed <- exprs(getExprSetRed(exprSetRGObject))
#       nGreen <- exprs(getExprSetGreen(exprSetRGObject))
#       stopifnot( all.equal.numeric( as.vector(nRed), as.vector(intensities[,"red",] )))
#       stopifnot( all.equal.numeric( as.vector(nGreen), as.vector(intensities[,"green",] )) ) 
#
#   \dontshow{
#       exprSetRGObject <- normalise(arrayDataObject = arrayDataObject,
#                               subtractBackground = FALSE,
#                               method = "none",
#                               channelsSeparately = TRUE,
#                               verbose = TRUE
#                               )
#       nRed <- exprs(getExprSetRed(exprSetRGObject))
#       nGreen <- exprs(getExprSetGreen(exprSetRGObject))
#       stopifnot( all.equal.numeric( as.vector(nRed), as.vector(intensities[,"red",] ) ))
#       stopifnot( all.equal.numeric( as.vector(nGreen), as.vector(intensities[,"green",] ) ))
#
#       exprSetRGObject <- normalise(arrayDataObject = arrayDataObject,
#                               subtractBackground = FALSE,
#                               method = "none",
#                               hybridisationGroups= list(c(1,3),2),
#                               verbose = FALSE
#                               )
#       nRed <- exprs(getExprSetRed(exprSetRGObject))
#       nGreen <- exprs(getExprSetGreen(exprSetRGObject))
#       stopifnot( all.equal.numeric( as.vector(nRed), as.vector(intensities[,"red",] ) ))
#       stopifnot( all.equal.numeric( as.vector(nGreen), as.vector(intensities[,"green",] ) ) )
#
#       exprSetRGObject <- normalise(arrayDataObject = arrayDataObject,
#                               subtractBackground = FALSE,
#                               method = "none",
#                               channelsSeparately = TRUE,
#                               hybridisationGroups= list(c(2,3),1),
#                               verbose = TRUE
#                               )
#       nRed <- exprs(getExprSetRed(exprSetRGObject))
#       nGreen <- exprs(getExprSetGreen(exprSetRGObject))
#       stopifnot( all.equal.numeric( as.vector(nRed), as.vector(intensities[,"red",] ) ))
#       stopifnot( all.equal.numeric( as.vector(nGreen), as.vector(intensities[,"green",] ) ) )
#
#       intensities <- array(data=runif(1440),dim=c(120,4,3))
#       dimnames(intensities) <- list(NULL,
#                                     c("green","red","greenBackground","redBackground"),
#                                     NULL)
#       intensities[,c("greenBackground","redBackground"),] <- -1
#       hybAttr <- data.frame(Name=I(c("hx","hy","hz")), Index=c(1:3))
# 	arrayDataObject <- new("arrayData",
#                              intensities=intensities,
#                              hybAttrList=list(red=hybAttr,green=hybAttr)
#                             )
#       exprSetRGObject <- normalise(arrayDataObject = arrayDataObject,
#                               subtractBackground = TRUE,
#                               method = "none",
#                               channelsSeparately = TRUE,
#                               hybridisationGroups= list(c(2,3),1),
#                               verbose = TRUE
#                               )
#       nRed <- exprs(getExprSetRed(exprSetRGObject))
#       nGreen <- exprs(getExprSetGreen(exprSetRGObject))
#       stopifnot( all.equal.numeric( as.vector(nRed), as.vector((intensities[,"red",] + 1) ) ))
#       stopifnot( all.equal.numeric( as.vector(nGreen), as.vector((intensities[,"green",] + 1) ) ))
#
#   }      
# }
#
# */

normalise <- function(arrayDataObject,
                      subtractBackground = FALSE,
                      method = "vsn",
                      subGroups = NULL,
                      channelsSeparately = FALSE,
                      hybridisationGroups,
                      spotIdentifier,
                      verbose = TRUE,
                      ...
                      ){

  
  if( channelsSeparately == TRUE & ( method == "loess" |
                                     method == "loessScale"  |
                                     method == "loessQuantile" 
        )){
    stop(paste("method:",method,"\n", " invalid combination:",
               " method of type loess and channelsSeparately=TRUE",
               " in normalise\n", sep=""),
               call.=FALSE)
  }
  
  if( missing(arrayDataObject) ){
    stop("object arrayDataObject is missing in normalise", call.=FALSE)
  }
  if( ! class(arrayDataObject) == "arrayData" ){
    stop(" wrong class of object arrayDataObject in normalise", call.=FALSE)
  }


  hybAttrGreen <- getHybAttrGreen(arrayDataObject)
  hybAttrRed <- getHybAttrRed(arrayDataObject)
  
  rawData <- getIntensities(arrayDataObject)
  nrspot <- dim(rawData)[1]
  nrhyb  <- dim(rawData)[3]
  
  ## validity and consistency of arrayDataObject
  if( is.null(hybAttrGreen) ||  is.null(hybAttrRed) ){
    stop("invalid arrayDataObject, i.e. getHybAttrGreen() and/or getHybAttrRed() are NULL in normalise", call.=FALSE)
  }
  if( subtractBackground ){
    channelNames = c("green", "greenBackground", "red", "redBackground")
  }else{
    channelNames = c("green", "red")
  }
  if( !all(channelNames %in% dimnames(rawData)[[2]] ) ){
    stop("channelNames :",paste(channelNames,collapse=","),": not found in getIntensities(arrayDataObject) :", paste(dimnames(rawData)[[2]], collapse=","),": in normalise", call.=FALSE)
  }
  if( ( dim(hybAttrGreen)[1] != nrhyb ) ||  (dim(hybAttrRed)[1] != nrhyb) ){
    stop("inconsistent arrayDataObject, i.e. getIntensities() and getHybAttr() are inconsistent in normalise", call.=FALSE)
  }
  if( ! all(colnames(hybAttrGreen) %in% colnames(hybAttrRed)) ||
      ! all(colnames(hybAttrRed) %in% colnames(hybAttrGreen))   ){
    stop("inconsistent arrayDataObject, i.e. getHybAttrGreen() and getHybAttrRed() have inconsistent colnames in normalise", call.=FALSE)
  }
    
  if( ! method %in% c("none","vsn","quantile", "loess","loessScale","loessQuantile") ){
    stop(paste("method:", method,"\n",
               "invalid method specified in normalise", sep=""),
         call.=FALSE)
  }


  ## groups, i.e. subsets of hybridisations/slides which
  ## are normalised (and possibly analysed) separately as group

  
  if( missing(hybridisationGroups) ){                ####### if not missing #######
    
    ## same case as below !
    hybridisationGroups <- vector(mode="list", length=1)
    hybridisationGroups[[1]] <- 1:nrhyb     
    
  }else{                                             ####### if not missing #######

    if( is.null(hybridisationGroups) ){
      
      ## same case as above !
      hybridisationGroups <- vector(mode="list", length=1)
      hybridisationGroups[[1]] <- 1:nrhyb    
      
    }else if( identical(hybridisationGroups,"slideBySlide") ){

      ## particular case "slideBySlide"
      hybridisationGroups <- vector(mode="list", length=nrhyb)
      for( i in (1:nrhyb) ){
        hybridisationGroups[[i]] <- i
      }
      
    }else{
      
      ## predefined hybridisationGroups
      
    }

  }

  ## check on hybridisationGroups
  if( ! all.equal(sort(unlist(hybridisationGroups)),(1:dim(rawData)[3])) ){
    stop("not all hybridisations are given in hybridisationGroups in normalise", call.=FALSE)
  }

  ## check subGroups
  if( ! is.null( subGroups ) ){

    
    if( !is.character(subGroups) ){
      stop(paste("subGroups :",subGroups,": is.character(subGroups) must be TRUE in  normalise",sep=""), call.=FALSE)
    }
    if( length(subGroups) != 1 ){
      stop(paste("subGroups :",subGroups,": must have length 1 in normalise",sep=""), call.=FALSE)
    }
    spotAttr <- getSpotAttr(arrayDataObject)    
    if( ! subGroups %in%  colnames(spotAttr) ){
      stop(paste("subGroups :",subGroups,": not found in colnames(getSpotAttr(arrayDataObject)) in normalise",sep=""), call.=FALSE)
    }
    if( ( dim(spotAttr)[1] != nrspot ) ){
      stop("inconsistent arrayDataObject, i.e. getIntensities() and getSpotAttr() are inconsistent in normalise", call.=FALSE)
    }
    tmpFactor <- as.factor(spotAttr[,subGroups])
    subGroupInteger <- as.integer(tmpFactor)
    stopifnot( max(subGroupInteger) == nlevels(tmpFactor) )

    subGroupNote <- paste(max(subGroupInteger), " different subGroups are used", sep="")
    if( verbose ){
      cat( " ", subGroupNote, " in normalise\n", sep="")
    }
    
  }else{

    subGroupNote <- "single group"
    subGroupInteger <- as.integer(rep(1, nrspot))
    
    
  }
  
  pqua   <- array(NA, dim=c(nrspot, 2, nrhyb))
  dimnames(pqua) <- list(NULL, c("greenChannel", "redChannel"), NULL)

  ## background subtraction ?
  if(subtractBackground){
    pqua[,"greenChannel",] <- rawData[,"green",] - rawData[,"greenBackground",]
    pqua[,"redChannel",]   <- rawData[,"red",]   - rawData[,"redBackground",]
  }else{
    pqua[,"greenChannel",] <- rawData[,"green",]
    pqua[,"redChannel",]   <- rawData[,"red",]
  }



  
  ## result objects
  normGroupResults <- vector(mode="list", length=length(hybridisationGroups))
  nqua   <- array(NA, dim=c(nrspot, 2, nrhyb))
  dimnames(nqua) <- list(NULL, c("green", "red"), NULL)
  
  for(i in 1:length(hybridisationGroups)){
    
    group <- hybridisationGroups[[i]]

    if( length(hybridisationGroups) > 1 ){
      if( verbose ){
        cat(" the following hybridisation index(es) are used in normalisation step",i,":\n")
        cat( paste(group,collapse=", "), "\n" )
      }
    }

    ## one or two dimensional ... but do not use drop == FALSE
    xGreenAll <- as.matrix(pqua[,"greenChannel",group])  
    yRedAll <-   as.matrix(pqua[,"redChannel",group])
    
    if( method == "vsn" ){
      ## note:
      ## different treatment of subGroups/strata for vsn and all other normalisation methods
      
      xGreen <- xGreenAll
      yRed <- yRedAll
      
      if( verbose ){cat(" vsn normalisation \n")}
      
      if( channelsSeparately == FALSE ){
        
        norm <- vsn(cbind(xGreen,yRed), strata=subGroupInteger,
                    verbose=verbose, ...)
        normGroupResults[[i]] <- norm
        firstHalf <- dim(exprs(norm))[2]/2
        nxGreen <- exprs(norm)[,1:firstHalf,drop=FALSE] 
        nyRed <- exprs(norm)[,(firstHalf+1):(2*firstHalf),drop=FALSE]
        
      }else{

        normXGreen <- vsn(xGreen, strata=subGroupInteger,
                          verbose=verbose, ...)
        normYRed <- vsn(yRed, strata=subGroupInteger,
                        verbose=verbose, ...)
        normGroupResults[[i]] <- list(greenChannel = normXGreen,
                                      redChannel   = normYRed   )
        nxGreen <- exprs(normXGreen)
        nyRed <- exprs(normYRed)
        
      }

      nqua[,"green",group] <- nxGreen
      nqua[,"red",group] <- nyRed
    
      ## end of method vsn
    }else{
      ## note:
      ## different treatment of subGroups/strata for vsn and all other normalisation methods

      for( s in 1:max(subGroupInteger) ){
        
         selection <- which(subGroupInteger == s)

         if( length( selection ) < 100 ){
           cat(paste("warning: normalisation is run on less than 100 values, i.e. ", length(selection), "\n"))
         }
         
         xGreen <- xGreenAll[selection,]
         yRed <- yRedAll[selection,]

         if( method == "none" ){
           
           if( verbose ){cat(" no normalisation \n")}
           
           nxGreen <- xGreen
           nyRed <- yRed
           
           
         }else if( method == "quantile" ){
           
           
           if( verbose ){cat(" quantile normalisation \n")}
           
           if( channelsSeparately == FALSE ){
             
             norm <- normalizeQuantiles(cbind(xGreen,yRed))
             normGroupResults[[i]] <- list(NULL)
             firstHalf <- dim(norm)[2]/2
             nxGreen <- norm[,1:firstHalf,drop=FALSE] 
             nyRed <- norm[,(firstHalf+1):(2*firstHalf),drop=FALSE]
          
             ## natural log transformation of the data
             nxGreen <- log(nxGreen)
             nyRed <- log(nyRed)
             
           }else{
          
             normXGreen <- normalizeQuantiles(xGreen)
             normYRed <- normalizeQuantiles(yRed)
             normGroupResults[[i]] <- list(greenChannel=list(NULL),redChannel=list(NULL))
             nxGreen <- normXGreen
             nyRed <- normYRed
             
             ## natural log transformation of the data
             nxGreen <- log(nxGreen)
             nyRed <- log(nyRed)
             
           }
           
         }else if( method == "loess" | method == "loessScale" | method == "loessQuantile"  ){
           
           if(verbose){ cat(paste(" loess normalisation :", method," \n")) }
           
           ## M and A versus R and G, cf also with reverse conversion below
           
           ## M: numeric matrix containing the M-values or log-2
           ## expression ratios. Rows correspond to spots and columns to arrays.
           ## A: numeric matrix containing the A-values or average
           ##    log-2 expression values 
           ## taken from the function MA.RG:
           ##    R <- log(R, 2)
           ##    G <- log(G, 2)
           ##    object$M <- as.matrix(R - G)
           ##    object$A <- as.matrix((R + G)/2)
           ## and therefore
           ## log(G,2) <- A - 0.5 M
           ## log(R,2) <- A + 0.5 M
           
           G <- xGreen
           R <- yRed
           Gb <- array(data=0,dim=dim(G))
           Rb <- array(data=0,dim=dim(R))
           rgListObject <- list(G=G,R=R,Gb=Gb,Rb=Rb)
           maListObject <- MA.RG(rgListObject)
        
           if(!is.null(getWeights(arrayDataObject)) ){
             weights <- getWeights(arrayDataObject)[selection,,drop=FALSE]
           }else{
             weights <- array(1, dim=dim(G))
           }
           
           narrays <- ncol(maListObject$M)
           for (j in 1:narrays){
             
             y <- maListObject$M[,j]
             x <- maListObject$A[,j]
             w <- weights[,j]
             maListObject$M[,j] <- loessFit(y,x,w,span=0.3,iterations=4)$residuals
             
           }
           
           ## limma:normalizeBetweenArrays may be called, methods: none, scale, quantile
           ##
           ## `object=MAList, method=="scale"' M-values are scaled to have the same
           ## median-absolute values across arrays, A-values are scaled to have
           ## the same medians across arrays
           ##
           ## `object=MAList, method=="quantile"' M- and A-values are normalized so
           ## that the single channel intensities R and G have the same
           ## empirical distribution across channels and across arrays
           
           if( method == "loessScale" ){
             if( verbose ){cat(" normalizeBetweenArrays scale is called on a MAList object\n")}
             maListObject <- normalizeBetweenArrays(maListObject,method="scale")
           }
           if( method == "loessQuantile" ){
             if( verbose ){
               cat(" normalizeBetweenArrays quantile is called normalisation on a MAList object\n")
             }
             maListObject <- normalizeBetweenArrays(maListObject,method="quantile")
           }
           
           normGroupResults[[i]] <- list(NULL)
           
           M <- maListObject[["M"]]
           A <- maListObject[["A"]]
           
           nxGreen <- A - 0.5 * M    
           nyRed <- A + 0.5 * M      

           ## convert log2 to ln, i.e. change log base
           nxGreen <- log(2) * nxGreen
           nyRed <- log(2) * nyRed
           
         }else{
           stop(paste(" unexpected case; unknown method :", method, " in normalise ",sep=""), call.=FALSE)
         }

         nqua[selection, "green", group] <- nxGreen
         nqua[selection, "red",group] <- nyRed

       }## end of for subGroups
         
    }## not method vsn
    
  }## end of for i in 1:length(hybridisationGroups)


  notes <- paste(" subtractBackground :",
                 as.character(subtractBackground), ":, ",
                 " normalisation method :", method, ":, ",
                 " subGroups:", as.character(subGroups), 
                 " (", subGroupNote, ")", ":, ",
                 " channelsSeparately :",
                 as.character(channelsSeparately), ":, ",
                 " hybridisationGroups :",
                 as.character(hybridisationGroups), ":, ",
                 sep="")
  
  phenoDataObject <- new("phenoData",
                         pData=rbind(hybAttrGreen,hybAttrRed),
                         varLabels=as.list(colnames(hybAttrGreen)))

  indGreen=1:nrhyb
  indRed=(nrhyb+1):(nrhyb+nrhyb)
  channels <- matrix( c(indGreen,indRed), nrow=length(indGreen), byrow=FALSE )
  colnames(channels) <- c("green","red")
  
  exprsMatrix <- as.matrix(cbind(nqua[,"green",],nqua[,"red",]))
  colnames(exprsMatrix) <- rownames(pData(phenoDataObject))
  
  annotation <- ""
  ## adding of (gene-)names to the exprSetRGObject
  if( ! missing( spotIdentifier ) ){

    if(! is.character(spotIdentifier) ){
      stop(" spotIdentifier must be of type character in normalise ",
           call.=FALSE)
    }
    if( ! (length(spotIdentifier) == 1) ){
      stop(" spotIdentifier must be one element in normalise ", call.=FALSE)
    }
    if(is.null(spotIdentifier) | is.na(spotIdentifier) ){
      stop(" spotIdentifier is NULL or NA in normalise ", call.=FALSE)
    }
   
    spotAttr <- getSpotAttr(arrayDataObject)
    if( ! is.null(spotAttr) &  ! (length(spotAttr) < 1) ){

      if( ! spotIdentifier %in% colnames(spotAttr) ){
        stop(" spotIdentifer not found in colnames(getSpotAttr(arrayDataObject)) in normalise ", call.=FALSE)
      }else{
        
        spotIds <- spotAttr[,spotIdentifier]
        if( ! any(duplicated(spotIds)) ){ 
          rownames(exprsMatrix) <- spotIds
          ## only add annotation if geneNames are set
          annotation <- paste("geneNames correspond to the column :",spotIdentifier,": of getSpotAttr(arrayDataObject) and have been set in function normalise", sep="")
        }else{
          extraNotes <- paste(" non-unique identifiers found in",
                             " getSpotAttr(arrayDataObject)[,spotIdentifier],",
                             " no spot/gene names have been added to",
                             " the exprSetRG in normalise ", sep="")
          notes <- paste(notes, extraNotes, sep=";")
          if(verbose){
            cat("\n");cat(extraNotes);cat("\n")
          }## if verbose
        }
      }
    }## if valid spotAttr
  }## if not missing spotIdentifier
  
  exprSetRGObject <- new("exprSetRG",
                         exprs=exprsMatrix,
                         channels=channels,
                         phenoData=phenoDataObject,
                         annotation=annotation,
                         notes=notes)
  
  ## ensure existence of the description
  description(exprSetRGObject) <- description(exprSetRGObject)
  exprSetRGObject@description@preprocessing <-
    append(exprSetRGObject@description@preprocessing, normGroupResults)
  
  return(exprSetRGObject)
  
}
