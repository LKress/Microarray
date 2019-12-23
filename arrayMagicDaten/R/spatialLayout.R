# /**
#
# \name{spatialLayout}
#
# \title{Calculation of two dimensional coordinates}
#
# \alias{spatialLayout}
#
# @usage
#
# \description{All \code{value}s are mapped on a matrix representing the
#              the spatial layout defined by the function arguments, mainly
#              by \code{row}, \code{column} and \code{block}. The result can be 
#              visualised with the function
#              \code{\link{plot.imageMatrix}}.}
#
# \value{A matrix representing the spatial layout of all \code{value}s.
#        The matrix is labelled as class \code{imageMatrix} and \code{matrix}. }
#
#
# \arguments{
#  \item{value}{numeric vector; required; default: missing}
#  \item{row}{integer vector; required; default: missing}
#  \item{col}{integer vector; required; default: missing}
#  \item{block}{integer vector; required; default: missing}
#  \item{numberOfValues}{integer; optional; default: missing.
#        The argument \code{numberOfValues} may be used 
#        if not all values for all coordinates are
#        passed to the function. An additional consistency
#        check is offered.}
#  \item{nrOfBlocksPerRow}{integer; required; default: 4.
#                          Useful if there is a block structured layout
#                          with columns and rows assigned within each block
#                          as common for microarrays. See examples for a
#                          simple case with only one block.}
#  \item{mapping}{integer; either zero or one; default: zero,
#                 Mapping toggles between counting blocks "by row" or "by column".
#                 Zero corresponds to "ScanAlyze" and "GenePix", i.e. counting
#                 blocks "by row".}
#  \item{nrRow}{integer; optional; default: missing.
#        The argument \code{nrRow} (number of rows per block) may be used to
#        supersede \code{max(row)} which is calculated automatically.}
#  \item{nrCol}{integer; optional; default: missing.
#        The argument \code{nrCol} (number of columns per block)  may be used to
#        supersede \code{max(col)} which is calculated automatically.}
#
# }
# 
# \examples{
#
# value <- rep(c(1,rep(0,49)),10)
# block <- as.integer(gl(10,50))
# col <- rep(c(1:10),50)
# row <- rep(as.integer(gl(5,10)),10)
# sL <- spatialLayout(value=value,row=row,col=col,block=block,nrOfBlocksPerRow=2)
# plot.imageMatrix(sL)
# value <- value[-(201:250)]
# block <- block[-(201:250)]
# col <- col[-(201:250)]
# row <- row[-(201:250)]
# sL <- spatialLayout(value=value,row=row,col=col,block=block,nrOfBlocksPerRow=2,numberOfValues=500)
# plot.imageMatrix(sL)
#
# value <- 1:200
# block <- rep(1, 200)
# col <- rep(1:20, 10)
# row <- as.integer(gl(10,20))
# sLOne <- spatialLayout(value=value,block=block,col=col,row=row,nrOfBlocksPerRow=1)
#	}
#
# \keyword{dplot}
#
# \seealso{ \code{\link{plot.imageMatrix}} }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */


spatialLayout <- function (value, row, col, block, numberOfValues, 
                           nrOfBlocksPerRow = 4, mapping = 0,
                           nrRow, nrCol) 
{

  if( missing(value) ){
    stop(" argument value is missing in spatialLayout ", call.=FALSE)
  }
  if( ! is.numeric(value) ){
    stop(" argument value must be of type numeric in spatialLayout ", call.=FALSE)
  }
  if( missing(row) ){
    stop(" argument row is missing in spatialLayout ", call.=FALSE)
  }
  if( ! is.numeric(row) ){
    stop(" argument row must be of type numeric in spatialLayout ", call.=FALSE)
  }
  if( missing(col) ){
    stop(" argument col is missing in spatialLayout ", call.=FALSE)
  }
  if( ! is.numeric(col) ){
    stop(" argument col must be of type numeric in spatialLayout ", call.=FALSE)
  }
  if( missing(block) ){
    stop(" block is missing in spatialLayout ", call.=FALSE)
  }
  if( ! is.numeric(block) ){
    stop(" argument block must be of type numeric in spatialLayout ", call.=FALSE)
  }
  if( ! length(nrOfBlocksPerRow) == 1 ){
    stop(" argument nrOfBlocksPerRow must be of length 1 in spatialLayout ", call.=FALSE)
  }
  if( ! is.numeric(nrOfBlocksPerRow) ){
    stop(" argument nrOfBlocksPerRow must be of type numeric in spatialLayout ", call.=FALSE)
  }
  if( ! length(mapping) == 1 ){
    stop(" argument mapping must be of length 1 in spatialLayout ", call.=FALSE)
  }
  if( ! is.numeric(mapping) ){
    stop(" argument mapping must be of type numeric in spatialLayout ", call.=FALSE)
  }
  
  
  ## ensure equal length of value, row, col, block
  if( !(length(row) == length(col))      ||
      !(length(col) == length(value))    ||
      !(length(value) == length(block))    ){
    stop(" value, row, col, and block must be of same length in spatialLayout ", call.=FALSE)
  }


  if( missing(nrRow) ){
    nrRow <- max(row)
  }
  if( missing(nrCol) ){
    nrCol <- max(col)
  }
   
  if( mapping == 0 ){
    ib = (block - 1) %/% nrOfBlocksPerRow
    jb = (block - 1)  %% nrOfBlocksPerRow
  } else if( mapping == 1 ){
    ib = (block - 1)  %% nrOfBlocksPerRow
    jb = (block - 1) %/% nrOfBlocksPerRow
  }else{
    stop("unknown mapping in spatialLayout", call.=FALSE)
  }
  
  i = nrRow * ib + row
  j = nrCol * jb + col
  
  nrRowSlide = nrRow * (max(ib)+1)
  nrColSlide = nrCol * (max(jb)+1)
  

  

  if( ! missing(numberOfValues) ){
    
    if( ! length(numberOfValues) == 1 ){
      stop(" argument numberOfValues must be of length 1 in spatialLayout ", call.=FALSE)
    }
    if( ! is.numeric(numberOfValues) ){
      stop(" argument numberOfValues must be of type numeric in spatialLayout ", call.=FALSE)
    }

    totalNumberOfValues <- numberOfValues

    if( totalNumberOfValues%%(max(col)*nrOfBlocksPerRow) != 0 ){
      stop(" the information in spatialLayout appears to be inconsistent (numberOfValues) ", call.=FALSE)
    }

    if( (nrRowSlide * nrColSlide) > totalNumberOfValues ){
      cat(" Attention: the information in spatialLayout appears to be inconsistent w.r.t numberOfValues) \n")
    }
      
  }
    


    
  mat = rep(as.numeric(NA), nrRowSlide*nrColSlide)
  idx = i + (j-1)*nrRowSlide
  if(any(duplicated(idx)))
    stop("Spot coordinates are not unique!", call.=FALSE)
  mat[idx] = value
  dim(mat) = c(nrRowSlide, nrColSlide)
  class(mat) <- c("imageMatrix", "matrix")
  return(mat)
}
