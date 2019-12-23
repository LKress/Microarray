# /**
#
# \name{controlledApply}
#
# \title{controlledApply}
#
# \alias{controlledApply}
#
# @usage
#
# \keyword{iteration}
#
# \description{Same functionality as \code{\link{simpleApply}} but
#              possibly faster. In certain circumstances the function
#              \code{apply} is used instead of \code{\link{simpleApply}}
#              which improves the performace.}
#
# \value{An array of
#        \code{dim=c(dim(arrayObject[dimensions]),funcResultDimensionality)}.
#        You may want to use \code{\link{aperm}} to rearrange the dimensions.}
#
#
# \arguments{
#  \item{arrayObject}{object of class \code{array}}
#  \item{dimensions}{increasing numeric vector}
#  \item{func}{unary function}
#  \item{funcResultDimensionality}{numeric (vector) specifying
#                           the dimensionality of the return value of \code{func}}
#  }
#
# \seealso{ \code{\link{simpleApply}}  }
#
# \examples{
#
#a <- array(c(1:30),dim=c(3,2,5))
#r <- controlledApply(a, 1, function(x){return(x[2,5])}, 1)
#stopifnot( all(r == matrix(data=c(28:30))))
#
#r <- controlledApply(a, 2, function(x){return(x[,])}, c(3,5))
#stopifnot( all( a == aperm(r,c(2,1,3)) ) )
#
#vec <- 1:10; dim(vec) <- c(10,1)
#mat <- matrix(data=rep(1:10,4),nrow=10,ncol=4,byrow=FALSE)
#r <- controlledApply(mat,1,function(y){return(mean(y))},1)
#stopifnot(all(r==vec))
#
#r <- controlledApply(mat, 1:2, function(x) return(x), 1)
#stopifnot( all(r[,,1] == mat) )
#
#r <- controlledApply(a, c(1,3) , function(x) return(x), dim(a)[2])
#stopifnot( all(aperm(r[,,],c(1,3,2)) == a) )
#
#r <- controlledApply(a, 1:2, function(x) return(x[2]), 1)
#stopifnot( all(r[,,] == a[,,2]) )
#
#r <- controlledApply(a, 1, function(x) return(x), c(dim(a)[2],dim(a)[3]))
#stopifnot( all( r== a ) )
#
# \dontshow{
#
#     nrOfDimensions <- as.integer(runif(n=1,min=1,max=7))
#     allDimensions <- as.integer(runif(n=nrOfDimensions,min=4,max=8))
#     arrayObject <- array(data=runif(prod(allDimensions)),dim=allDimensions)
#     dimensionsLength <- as.integer(runif(n=1,min=1,max=nrOfDimensions))
#     dimensions <- unique(as.integer(
#                      runif(n=dimensionsLength,min=1,max=nrOfDimensions)))
#     func <- mean
#     result <- simpleApply(arrayObject=arrayObject,
#                           dimensions=dimensions,
#                           func=func,
#                           funcResultDimensionality = 1)
#
#     result2 <- simpleApply(arrayObject=arrayObject,
#                            dimensions=dimensions,
#                            func=func,
#                            funcResultDimensionality = 1)
#     stopifnot( identical(result, result2))
#    
#
#r <- controlledApply(matrix(c(1, 2)), 1, sum, 1)
#stopifnot( all(r == matrix(c(1,2))) )
#
#a <- array(c(1:30),dim=c(3,2,5))
#r <- controlledApply(a, 1, function(x){return(x[1])}, 1)
#stopifnot( all(r == matrix(data=c(1:3))))
#
#r <- controlledApply(a, 1, function(x){return(x[1,3])}, 1)
#stopifnot( all(r == matrix(data=c(13:15))))
#
#r <- controlledApply(a, 1, function(x){return(x[,])}, c(2,5))
#stopifnot( all( a == r))
#
#r <- controlledApply(a, 3, function(x){return(x[,])}, c(3,2))
#stopifnot( all( a == aperm(r,c(2,3,1)) ) )
#
#r2 <- controlledApply(a, 1, function(x){ return(as.vector(x))}, 10)
#
#r <- controlledApply(a, 1:3, function(x) return(x), 1)
#stopifnot( all(r[,,,1] == a) )
#
#r <- controlledApply(a, 1:2, function(x) return(x), dim(a)[3])
#stopifnot( all(r[,,] == a) )
#
#r <- controlledApply(a, 1:2, function(x) return(x[1]), 1)
#
#r <- controlledApply(a, 1:2, function(x) return(x[1]), 1)
#stopifnot( all(r[,,] == a[,,1]) )
#
#       }
#
# }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
#
# */


controlledApply <- function(arrayObject, dimensions, func, funcResultDimensionality){
  
  if( missing(dimensions) ){
    stop(" dimensions is missing in controlledApply ")
  }
  if( ! all(is.numeric(dimensions)) ){
    stop(" dimensions must be of type numeric in controlledApply ")
  }
  if( ! all( sort(dimensions) == dimensions ) ){
    stop(" dimensions must be of increasing order in controlledApply ")
  }
  dimension <- dimensions[1]
  

  if( missing(funcResultDimensionality) ){
    stop(" funcResultDimensionality is missing in controlledApply ")
  }
  if( ! is.numeric(funcResultDimensionality) ){
    stop(" funcResultDimensionality must be numeric in controlledApply ")
  }
  if( length(funcResultDimensionality) < 1 ){
    stop(" length(funcResultDimensionality) must be greater or equal than 1 in controlledApply ")
  }
  if( any(funcResultDimensionality < 1) ){
    stop(" each element of funcResultDimensionality must be greater or equal than 1 in controlledApply ")
  }
  if( missing(func) ){
    stop(" func is missing in controlledApply ")
  }
  if( missing(arrayObject) ){
    stop(" arrayObject is missing in controlledApply ")
  }
    if( ! is.array(arrayObject) ){
    stop(" arrayObject must be of type array in controlledApply ")
  }
  if( ! is.numeric(dimension) ){
    stop(" dimension must be of type numeric in controlledApply ")
  }
  if( (dimension < 1) | (dimension > length(dim(arrayObject)))    ){
    stop(" dimension must be valid/meaningful in controlledApply ")
  }

  expectedDim <- c(dim(arrayObject)[dimensions], funcResultDimensionality)

  ## to deal with funcResultDimensionality > 1
  ## appeared to be rather difficult
  if(funcResultDimensionality == 1         &&
     all(dim(arrayObject)[dimensions] > 1)   ){
    
    result <- apply(arrayObject, dimensions, func)
    if( is.null(dim(result)) ){
      result <- as.array(result)
    }
    dim(result) <- c(dim(result),1)
    
    
    
  }else{

    result <- simpleApply(arrayObject, dimensions, func, funcResultDimensionality, DEBUG=FALSE)
    
  }
  
  stopifnot( all(expectedDim == dim(result)) )

  
  return(result)
  
}## end of controlledApply
