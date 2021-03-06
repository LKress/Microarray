%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file was automatically generated by Rdoc on 2006-04-12  8:38:17.
% Do NOT modify this file, instead modify the source (/buness/R/TestingOfarrayMagic/Rpacks/arrayMagic/R/plotDistributions.R).
% Rdoc is copyright (C) 2001, Henrik Bengtsson, henrikb@braju.com.
% Download the Rdoc compiler at http://www.braju.com/R/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 \name{plotDistributions}

 \title{Visualisation of distributions}

 \alias{plotDistributions}

 \usage{plotDistributions(dataMatrix, transFunc, quantiles, main, labels, xlab, ylab, colourVector, width=8, height=6, fileName="plotDistributionsOutput", savePath, plotOutput="standard", automaticSubtitle=TRUE)}

 \keyword{hplot}

 \description{Boxplot like visualisation of distributions, only the boxes,
   i.e. the median and the second and third quartile are plotted
   (cf. \code{boxplot.stats} for the details on the calculation).
   The plots may help to identify shortcomings of the raw data
   or normalised data. The argument \code{quantiles} can be used
   to visualize two or three arbitrary quantiles with boxes.}

 \value{The function is called for its side effect.}


 \arguments{
  \item{dataMatrix}{numeric data matrix,
                    where columns represent distributions, e.g.
                    raw array data or normalised data;
                    required; default missing.} 
  \item{transFunc}{unary function; optional; default missing.
                   Data transformation function, e.g. log}
  \item{quantiles}{missing by default; a vector of two or
          three increasing quantiles used to determine the boxes to be drawn}
  \item{main}{plot title; type character string; optional; default missing}
  \item{labels}{vector of names, may substitute column names
                of \code{dataMatrix}; optional; default missing}
  \item{xlab}{label for x axis; type character string; optional; default missing}
  \item{ylab}{label for y axis; type character string; optional; default missing}
  \item{colourVector}{vector of colours; optional; default missing}
  \item{width}{graphics window width; required; default: 8}
  \item{height}{graphics window height; required; default: 6}
  \item{fileName}{optional; default: "plotDistributionsOutput"}
  \item{savePath}{optional; default: missing}
  \item{plotOutput}{character string specifying
        either "standard", "screen", "pdf", "win.metafile";
        default: "standard"}
  \item{automaticSubtitle}{logical; required; default: \code{TRUE};
                           The quantiles values are added
                           as subtitle to the plot.}

 }
 
 \details{Default of \code{transFunc} is no transformation, i.e. identity.
          If \code{labels} are supplied at first the column names or
          secondly a numbering  are used instead.
          By default the \code{colourVector} is defined as alternating
          "darkred" and "darkgreen".
          A data matrix which only consists of \code{NA, NaN, Inf, -Inf}
          will cause a cryptical error.}

 \seealso{\code{boxplot.stats}}

 \examples{
      plotDistributions(cbind(rnorm(100),rnorm(100)),
             main="Random Gaussians", labels = c("N1","N2"), ylab="scale")
      plotDistributions(as.matrix(1:100), quantiles=c(0.25,0.85),
             main = "Random Gaussians", labels = c("N1"), ylab = "scale")
      plotDistributions(as.matrix(1:100), transFunc=log,
             quantiles=c(0.1,0.95), main = "Random Gaussians",
             labels = c("N1"), ylab = "scale")
           }
 \author{Andreas Buness <a.buness@dkfz.de>}
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% End of File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
