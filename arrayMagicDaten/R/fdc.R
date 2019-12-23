##----------------------------------------------------------------
## one- or two-sample t-test for matrix x and naive estimation of
## no. of false positives
##----------------------------------------------------------------
fdc <- function(x, fac, 
  teststatfun = "rowFtests",
  nrperm      = 100,
  nrgenesel   = c(10, 20, 40, 60, 80, 100, 200),
  ...) {

  if( teststatfun == "rowFtests" ){
    if(!require("genefilter")){
      stop("Required package 'genefilter' cannot be loaded")
    }
  }
  stopifnot(ncol(x)==length(fac), is.matrix(x), is.factor(fac))
  stopifnot(is.numeric(nrperm), length(nrperm)==1, !is.na(nrperm), nrperm>0)
  stopifnot(is.numeric(nrgenesel), all(!is.na(nrgenesel)), all(nrgenesel>0))
  

  ## perms is a nrgenes x nrperm matrix (where nrgenes=nrow(x)).
  ## each column corresponds to a permutation
  perms <- sapply(1:nrperm, function(i) sample(length(fac)))

  ## the test statistic
  stat <- do.call(teststatfun, list(x=x, fac=fac, ...))$statistic

  ## a matrix of permuted test statistics, its rows correspond to the probes (rows of x),
  ##   columns correspond to permutations
  pstat <- apply(perms, 2, function(p)
        do.call(teststatfun, list(x=x, fac=fac[p], ...))$statistic )

  ## Sort each column. After this, the last row contains a permutation distribution
  ## of largest values, the second-last row that of second-largest values etc.
  spstat = apply(pstat, 2, sort)

  ## Calculate the median of each row, i.e. the expected largest value, expected
  ## second-largest value etc.
  mpstat = apply(spstat, 1, median)

  fdc <- thresh <- numeric(length(nrgenesel))
  names(thresh) <- names(fdc) <- paste(nrgenesel)
  for(i in 1:length(nrgenesel)) {
    ## thresh[i] is the threshold that corresponds to selecting the top nrgenesel[i] probes
    thresh[i] <- sort(abs(stat), decreasing=TRUE)[nrgenesel[i]]
    ## fdc[i] is the median number of probes that exceed this threshold in the permuted data
    fdc[i]    <- median(apply(pstat, 2, function(x) length(which(abs(x)>=thresh[i]))))
  }
  return(list(stat=stat, mpstat=mpstat, fdc=fdc, thresh=thresh, nrgenesel=nrgenesel))
}
