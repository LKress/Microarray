# /**
#
# \name{interweave}
#
# \title{interweave}
#
# \alias{interweave}
#
# \description{
#          The first object is "interweaved" with the second object
#          and the resulting object with the following object and
#          continued as long as the last object is reached.
#          Note: \code{AsIs} class objects are coerced to a vector
#                before interweaving starts.}
#
# \value{An "interweaved" object, cf. example.}
#
# @usage
#
# \arguments{
#  \item{...}{vectors, matrices, one- and two-dimensional arrays;
#             an \code{AsIs} class object is coerced to a vector.}
# }
# 
# \examples{
#    x <- seq(1,100,2)
#    y <- seq(2,100,2)
#    z <- interweave(x,y)
#    stopifnot( all.equal(z,1:100) )
#	}
#
# \keyword{manip}
#
# \author{Andreas Buness <a.buness@dkfz.de>}
# */
interweave <- function(...){

  dataList <- list(...)
  A <- dataList[[1]]
                               
  for(B in dataList[2:length(dataList)]){

    if( ! is.null(class(A)) ){
      if( class(A) == "AsIs" ){
        A <- as.vector(A)
      }
    }
    if( ! (is.vector(A) | is.matrix(A) | is.array(A)) ){
      stop(" first/current object of wrong type in interweave \n")
    }
    if( ! is.null(class(B)) ){
      if( class(B) == "AsIs" ){
        B <- as.vector(B)
      }
      if( ! (is.vector(B) | is.matrix(B) | is.array(B)) ){
        stop(" second/next object of wrong type in interweave \n")
      }
    }
    
    if(( is.vector(A) & is.vector(B) ) |
       ( is.array(A) & length(dim(A))==1 &
        is.array(B) & length(dim(B))==1   )){
      VECTOR <- TRUE
      A <- t(as.matrix(A))
      B <- t(as.matrix(B))
    }else{
      VECTOR <- FALSE
    }
    
    ## note: two-dimensional arrays appeared to be matrices
    if( ! is.matrix(A) ){
      stop(" first/current object casting failed in interweave \n")
    }
    if( ! is.matrix(B) ){
      stop(" second/next object casting failed in interweave \n")
    }
    
    if( ! (ncol(A) == ncol(B)) ){
      stop(" dimension mismatch between objects in interweave \n")
    }
    l <- ncol(A)
    generatePairs <- function(x,l){
      return(c(x,x+l))
    }
    indexes <- as.vector(sapply(1:l,function(x) generatePairs(x,l)))
    stopifnot( all(sort(indexes) == 1:(2*l)) )
    if( VECTOR ){
      A <- ( as.vector(cbind(A,B)[,indexes]) )
    }else{
      A <- ( cbind(A,B)[,indexes] )
    }
    
  }## end of for

  return(A)
  
}
  
