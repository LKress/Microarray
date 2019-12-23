# /**
#
# \name{controlledSubsetting}
#
# \title{Subsetting of Arrays}
#
# \alias{controlledSubsetting}
#
# \description{Finer control on the drop argument as in []}
#
# \value{subsetted array}
#
# @usage
#
# \arguments{
#  \item{a}{array; required; default: missing.}
#  \item{ranges}{list of vectors; list length must match
#                dimensionality of a; each element of the
#                numeric vector specifies
#                the subset of the corresponding dimension of a;
#                if is.na(element) the whole corresponding dimension is kept;
#                required; default: missing.}
#  \item{drop}{optional; specifies each dimension of array a which will
#              be dropped, the corresponding subset must specify exact one
#              element; if missing all such subsets are dropped;
#              default: missing.}
# }
# 
# \details{}
#
# \seealso{\code{\link{controlledApply}}}
#
# \examples{
#            a <- array(1:400, dim=c(2,3,5,6))
#            r <- controlledSubsetting(a=a, ranges=list(1,2,3,2), drop=2)
#            stopifnot(length(dim(r)) == 3 )
#            stopifnot(r == a[1,2,3,2])
#            r <- controlledSubsetting(a=a, ranges=list(2,1:2,2,2:3), drop=3)
#            stopifnot( all(r[1,,,drop=TRUE] == a[2,1:2,2,2:3]) )
#            r <- controlledSubsetting(a=a, ranges=list(1:2,1:2,2,3), drop=3)
#            stopifnot( all(r[,,1,drop=TRUE] == a[1:2,1:2,2,3]) )
#            r <- controlledSubsetting(a=a, ranges=list(1:2,1:2,2,3), drop=c(3,4))
#            stopifnot( all(r == a[1:2,1:2,2,3]) )
#            r <- controlledSubsetting(a=a, ranges=list(1:2,1:2,1:5,3), drop=c(4))
#            stopifnot( all(r == a[1:2,1:2,1:5,3]) )
#            r <- controlledSubsetting(a=a, ranges=list(NA,NA,1:5,3), drop=c(4))
#            stopifnot( all(r == a[,,1:5,3]) )
#
#	}
#
# \keyword{utilities}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

controlledSubsetting <- function(a, ranges, drop){

  if( missing(a) ){
    stop(" argument a is required ", call.=FALSE)
  }
  if( ! is.array(a) ){
    stop(" argument a must be of type array ", call.=FALSE)
  }
  if( missing(ranges) ){
    stop(" argument ranges is required ", call.=FALSE)
  }
  if( ! is.list(ranges) ){
    stop(" argument a must be of type list ", call.=FALSE)
  }
  if( length(dim(a)) != length(ranges) ){
    stop(" dimensionality of a must match length of list ranges ")
  }

  nas <- which( is.na(ranges) )
  if( length(nas) > 0 ){
    for(i in nas){
      ranges[[i]] <- 1:(dim(a)[i])
      }
  }
  
  if( missing(drop) ){

    return(do.call("[", c(list(a),ranges,drop=TRUE)) )
           
  }
  if( length(drop) > length(dim(a)) ){
    stop(" drop dimensions must not extend dimensionality of a ")
  }
  if( !is.vector(drop) ){
    stop(" drop must be of type vector ")
  }
  if( !is.numeric(drop) ){
    stop(" drop must be numeric ")
  }

  if( ! all( sapply(ranges, length)[drop] == 1 ) ){
    stop(" dimensions to be dropped must correspond to subsets of one ")
  }
  sa <- do.call("[", c(list(a),ranges,drop=FALSE))
  newDim <- dim(sa)[-drop]
  newA <- array(sa, dim=newDim)
  dimnames(newA) <- dimnames(sa)[-drop]
  
  return(newA)
  
}## end of function controlledSubsetting
