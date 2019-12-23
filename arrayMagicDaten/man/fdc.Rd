\name{fdc}
\alias{fdc}

\title{FDC (false discovery count)}
\description{Estimate the FDC (false discovery count) through permutations}

\usage{fdc(x, fac,
  teststatfun = "rowFtests",
  nrperm      = 100,
  nrgenesel   = c(10, 20, 40, 60, 80, 100, 200),
  ...)
} 

\arguments{
  \item{x}{Matrix.}
  \item{fac}{Factor, with \code{length(fac)=ncol(x)}.}
  \item{teststatfun}{Character. Name of a function that takes arguments
    \code{x} and \code{fac}, and returns a list with component
    \code{statistic}. See for example \code{rowFtests} in package genefilter.}
  \item{nrperm}{Numeric. Number of permutations.}
  \item{nrgenesel}{Numeric. A vector with the 'number of genes' for which
    the FDC is to be calculated.}
  \item{...}{Further arguments passed to code{teststatfun}.}
}

\value{  
  A list with elements
  \code{stat}: the test statistics;
  \code{mpstat}: median permuted test statistics;
  \code{fdc}: estimated false discovery counts;
  \code{thresh}: the threshholds associated with \code{nrgenesel};
  \code{nrgenesel}
}

\author{Wolfgang Huber <w.huber@dkfz.de>}
\seealso{\code{rowFtests}}
\examples{
   ## data matrix: 2000 genes, 16 samples
   x  <- matrix(runif(2000*16), ncol=16)
   ## 8 blue and 8 red samples
   fac <- factor(c(rep("blue", 8), rep("red", 8)))
   ## implant differential signal into the first 50 genes
   x[1:50, fac=="blue"] <- x[1:50, fac=="blue"] + 1

   library(genefilter)
   res <- fdc(x, fac)
   plot(res$nrgenesel, res$fdc, pch=16, col="blue",
       xlab="Number of genes selected",
       ylab="Expected number of false discoveries")
   abline(a=0, b=1, col="red", lwd=2)

   qqplot(res$stat, res$mpstat, pch=".")
   abline(a=0, b=1, col="red", lwd=2)

}
 
\keyword{math}
