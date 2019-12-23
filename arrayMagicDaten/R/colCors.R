colCors = function(x, y) {

  sqr = function(x) x*x
  
  if(!is.matrix(x)||!is.matrix(y)||any(dim(x)!=dim(y)))
    stop("Please supply two matrices of equal size.")
  
  x   = sweep(x, 2, colMeans(x))
  y   = sweep(y, 2, colMeans(y))
  cor = colSums(x*y) /  sqrt(colSums(sqr(x))*colSums(sqr(y)))
}

rowCors = function(x, y) {

  sqr = function(x) x*x

  if(!is.matrix(x)||!is.matrix(y)||any(dim(x)!=dim(y)))
    stop("Please supply two matrices of equal size.")
  
  x   = sweep(x, 1, rowMeans(x))
  y   = sweep(y, 1, rowMeans(y))
  cor = rowSums(x*y) /  sqrt(rowSums(sqr(x))*rowSums(sqr(y)))
}


