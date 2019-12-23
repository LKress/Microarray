\name{colCors}
\alias{rowCors}
\alias{colCors}

\title{Correlation coefficients between the corresponding columns (rows)
  of two matrices that have the same size.}
\description{Correlation coefficients between the corresponding columns (rows)
  of two matrices that have the same size.}

\usage{
rowCors(x,y)
colCors(x,y)
}
\arguments{
  \item{x}{Matrix}
  \item{y}{Matrix, same size as \code{x}}
}

\details{The implementation is naive.}

\value{  
A vector with the correlation coefficients for each row (\code{rowWiseSds}),
or column  (\code{rowWiseSds})
}

\seealso{\code{\link{colSums}}}

\examples{
x  = matrix(runif(1e6), ncol=100)
y  = matrix(runif(1e6), ncol=100)

commands = c(
  "c1 <<- colCors(x,y)",
  "c2 <<- sapply(1:ncol(x), function(i) cor(x[,i], y[,i]))",
  "c3 <<- rowCors(x,y)",
  "c4 <<- sapply(1:nrow(x), function(i) cor(x[i,], y[i,]))")
  
times = sapply(commands, function(text) system.time(eval(parse(text=text)))[1])
print(t(times))

stopifnot(all(abs(c1-c2) < 1e-3))
stopifnot(all(abs(c3-c4) < 1e-3))

}

\keyword{math}


