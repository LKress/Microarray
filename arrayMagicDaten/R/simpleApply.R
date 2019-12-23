# /**
#
# \name{simpleApply}
#
# \title{simpleApply}
#
# \alias{simpleApply}
#
# @usage
#
# \keyword{iteration}
#
# \description{Note: very slow.
#   \code{func} is applied to all subsets
#   of \code{arrayObject} defined by \code{dimensions}, i.e.
#   for every element i of \code{arrayObject}[\code{dimensions}]
#   the function \code{func} is applied to \code{arrayObject}[i].
#   \code{func} must be unary.
#   Due to the recursive definition, the function might
#   not only be slow but also very memory intensive.
#   The function aims to give you more control on the dimensionality
#   of the return value in contrast to \code{\link{apply}};
#   cf. the examples.
#   Attention: Be careful with \code{funcResultDimensionality} !
#   }
#
# \value{\code{array} of \code{dim=c(dim( arrayObject[dimensions] ),
#                              funcResultDimensionality     )};
#        possibly use \code{\link{aperm}} to rearrange the dimensions}
#
#
# \arguments{
#  \item{arrayObject}{object of type \code{array}; required; default: missing}
#  \item{dimensions}{increasing numeric vector; required; default: missing}
#  \item{func}{unary function; required; default: missing}
#  \item{funcResultDimensionality}{numeric (vector) specifying the
#                      dimensionality of the result value of \code{func}}
#  \item{DEBUG}{logical; required; default: \code{FALSE};
#               to trace the recursive calling you may use \code{DEBUG=TRUE}}
#
# }
# 
# \seealso{ \code{\link{controlledApply}}}
#
# \examples{
#
#a <- array(c(1:30),dim=c(3,2,5))
#r <- simpleApply(a, 1, function(x){return(x[2,5])}, 1)
#stopifnot( all(r == matrix(data=c(28:30))))
#
#r <- simpleApply(a, 2, function(x){return(x[,])}, c(3,5))
#stopifnot( all( a == aperm(r,c(2,1,3)) ) )
#
#vec <- 1:10; dim(vec) <- c(10,1)
#mat <- matrix(data=rep(1:10,4),nrow=10,ncol=4,byrow=FALSE)
#r <- simpleApply(mat,1,function(y){return(mean(y))},1)
#stopifnot(all(r==vec))
#
#r <- simpleApply(mat, 1:2, function(x) return(x), 1)
#stopifnot( all(r[,,1] == mat) )
#
#r <- simpleApply(a, c(1,3) , function(x) return(x), dim(a)[2])
#stopifnot( all(aperm(r[,,],c(1,3,2)) == a) )
#
#r <- simpleApply(a, 1:2, function(x) return(x[2]), 1)
#stopifnot( all(r[,,] == a[,,2]) )
#
#r <- simpleApply(a, 1, function(x) return(x), c(dim(a)[2],dim(a)[3]))
#stopifnot( all( r== a ) )
#
# \dontshow{
#
#r <- simpleApply(matrix(c(1, 2)), 1, sum, 1)
#stopifnot( all(r == matrix(c(1,2))) )
#
#a <- array(c(1:30),dim=c(3,2,5))
#r <- simpleApply(a, 1, function(x){return(x[1])}, 1)
#stopifnot( all(r == matrix(data=c(1:3))))
#
#r <- simpleApply(a, 1, function(x){return(x[1,3])}, 1)
#stopifnot( all(r == matrix(data=c(13:15))))
#
#r <- simpleApply(a, 1, function(x){return(x[,])}, c(2,5))
#stopifnot( all( a == r))
#
#r <- simpleApply(a, 3, function(x){return(x[,])}, c(3,2))
#stopifnot( all( a == aperm(r,c(2,3,1)) ) )
#
#r2 <- simpleApply(a, 1, function(x){ return(as.vector(x))}, 10)
#
#r <- simpleApply(a, 1:3, function(x) return(x), 1)
#stopifnot( all(r[,,,1] == a) )
#
#r <- simpleApply(a, 1:2, function(x) return(x), dim(a)[3])
#stopifnot( all(r[,,] == a) )
#
#r <- simpleApply(a, 1:2, function(x) return(x[1]), 1)
#
#r <- simpleApply(a, 1:2, function(x) return(x[1]), 1)
#stopifnot( all(r[,,] == a[,,1]) )
#
#a <- array(1, dim=c(2,3,2))
#b <- simpleApply(a, c(1,2), function(x){ return(x)}, 2)
#stopifnot( all(a == b) )
#a <- array(1, dim=c(2,1,2))
#b <- simpleApply(a, c(1,2), function(x){ return(x)}, 2)
#
# }
#
#	}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */

simpleApply <- function(arrayObject, dimensions, func, funcResultDimensionality, DEBUG=FALSE){
  
  
  if( missing(funcResultDimensionality) ){
    stop(" funcResultDimensionality is missing in simpleApply ")
  }
  if( ! is.numeric(funcResultDimensionality) ){
    stop(" funcResultDimensionality must be numeric in simpleApply ")
  }
  if( length(funcResultDimensionality) < 1 ){
    stop(" length(funcResultDimensionality) must be greater or equal than 1 in simpleApply ")
  }
  if( any(funcResultDimensionality < 1) ){
    stop(" each element of funcResultDimensionality must be greater or equal than 1 in simpleApply ")
  }
  if( missing(func) ){
    stop(" func is missing in simpleApply ")
  }
  if( missing(arrayObject) ){
    stop(" arrayObject is missing in simpleApply ")
  }
  if( ! is.array(arrayObject) ){
    stop(" arrayObject must be of type array in simpleApply ")
  }
  if( missing(dimensions) ){
    stop(" dimensions is missing in simpleApply ")
  }
  if( ! all(is.numeric(dimensions)) ){
    stop(" dimensions must be of type numeric in simpleApply ")
  }
  if( ! all( sort(dimensions) == dimensions ) ){
    stop(" dimensions must be of increasing order in simpleApply ")
  }


  
  dimension <- dimensions[1]

  
  if( ! is.numeric(dimension) ){
    stop(" dimension must be of type numeric in simpleApply ")
  }
  if( (dimension < 1) | (dimension > length(dim(arrayObject)))    ){
    stop(" dimension must be valid/meaningful in simpleApply ")
  }

  result <- array(data=NA, dim=c(dim(arrayObject)[dimensions],funcResultDimensionality))
  
  nrOfDimensions <- length(dim(arrayObject))
  
  resultPreString <- "result["
  resultPostString <- paste(paste(rep(",",length(funcResultDimensionality)),collapse=""),"] <- resultI",sep="")
  extraResultPostString <- paste(rep(",",length(dimensions)-1),collapse="")
  resultPostString <- paste(extraResultPostString,resultPostString,sep="")

  
  if(DEBUG) print(" next run : ")
  
  
  for( i in (1:dim(arrayObject)[dimension]) ){

    rangeVec <- as.list(c(rep(NA,dimension-1),i,
                          rep(NA,nrOfDimensions-dimension))
                        )
    arrayObjectI <- controlledSubsetting(a=arrayObject,
                                         ranges=rangeVec,
                                         drop=dimension)

    ## recursion
    if( length(dimensions) > 1 ){
      if( !is.array(arrayObjectI) ){
        arrayObjectI <- array(arrayObjectI)
      }
      resultI <- simpleApply(arrayObject=arrayObjectI, dimensions=(dimensions[-1]-1), func=func, funcResultDimensionality=funcResultDimensionality, DEBUG=DEBUG)
    }else{
      resultI <- func( arrayObjectI )
    }
    
    tmpResultText <- paste(resultPreString,i,resultPostString,sep="")
    if(DEBUG) print(tmpResultText)
    eval(parse(text=tmpResultText))
    
  }
  
  return(result)

}## end of simpleApply
