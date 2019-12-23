# /**
#
# \name{detectReplicas}
#
# \title{detectReplicas}
#
# \alias{detectReplicas}
#
# \keyword{utilities}
#
# @usage
#
# \description{The function returns for each unique item all corresponding indexes.}
#
# \arguments{
#  \item{arrayDescription}{an object of class \code{data.frame}
#                          which contains a column named as
#                          \code{spotIdentifier}; required; default missing
#                          }
#  \item{spotIdentifier}{character string; required; default: "ID"}
#  \item{identifiersToBeSkipped}{vector of character strings;
#                                required; default: \code{NULL};
#                                items of the column \code{spotIdentifier}
#                                for which no replica detection takes place}
#  \item{verbose}{ logical; required; default: \code{TRUE}}
# }
#
# \value{A list which contains \code{nrOfReplicas} and
#        \code{spotReplicas}.  \code{nrOfReplicas}: one
#        integer characterizing the number of spot replicas
#        given for each identifier if existing or otherwise  \code{NA}.
#        \code{spotReplicas}: a list of the length of the unique
#        identifiers where each element contains a vector of
#        indexes corresponding to the given identifier (i.e. the
#        name of the list element) otherwise \code{NA}.}
#
# \examples{
#
#aD <- data.frame(ID=c("z", "x", "x", "x", "y", "z", "z", "y", "y"))
#re <- detectReplicas(aD, identifiersToBeSkipped = c("Blank", "Control1", "Control2"))
#stopifnot(re[["nrOfReplicas"]] == 3 )
#stopifnot( re$spotReplicas[["z"]] == c(1,6,7) )
#stopifnot( re$spotReplicas[["x"]] == c(2,3,4) )
#stopifnot( re$spotReplicas[["y"]] == c(5,8,9) )
#
#aD <- data.frame(ID=c("Blank", "Control1", "Blank", "Control2"))
#re <- detectReplicas(aD, identifiersToBeSkipped = c("Blank", "Control1", "Control2"))
#stopifnot(is.na(re[["nrOfReplicas"]]))
#
# \dontshow{
# aD <- data.frame(ID=c("Blank", "Blank", "x", "x", "Control1", "y", "x", "z", "y", "Control2"))
# re <- detectReplicas(aD, identifiersToBeSkipped = c("Blank", "Control1", "Control2"))
# stopifnot(is.na(re[["nrOfReplicas"]]))
# stopifnot( re$spotReplicas[["y"]] == c(6,9) )
# stopifnot( re$spotReplicas[["x"]] == c(3,4,7) )
# stopifnot( re$spotReplicas[["z"]] == c(8) )

# aD <- data.frame(ID=c("z", "x", "Control1", "y"))
# re <- detectReplicas(aD, identifiersToBeSkipped = c("Control1"))
# stopifnot(re[["nrOfReplicas"]] == 1 )
# stopifnot( re$spotReplicas[["x"]] == c(2) )
# stopifnot( re$spotReplicas[["y"]] == c(4) )
# stopifnot( re$spotReplicas[["z"]] == c(1) )

# aD <- data.frame(ID=c("Blank", "x", "x", "Control1", "y", "z", "z", "y", "Control2"))
# re <- detectReplicas(aD, identifiersToBeSkipped = c("Blank", "Control1", "Control2"))
# stopifnot(re$nrOfReplicas == 2 )
# stopifnot( re$spotReplicas[["x"]] == c(2,3) )
# stopifnot( re$spotReplicas[["y"]] == c(5,8) )
# stopifnot( re$spotReplicas[["z"]] == c(6,7) )
# }
#  }
#
# \author{Andreas Buness <a.buness@dkfz.de>}
#
# */
detectReplicas <- function(arrayDescription,
                           spotIdentifier = "ID",
                           identifiersToBeSkipped = NULL,
                           verbose = TRUE
                           ){

  if( missing( arrayDescription ) ){
    stop(" arrayDescription is missing in detectReplicas ")
  }
  if( !is.data.frame(arrayDescription) ){
    stop(" arrayDescription has to be of class data.frame in detectReplicas ")
  } 

  
  # initialization
  nrOfReplicas <- NA
  spotReplicas <- NA
  
  if(is.null(arrayDescription[,spotIdentifier])){
    tmpString <- paste("spotIdentifier",spotIdentifier," is null in data.frame arrayDescription")
    if(verbose){ cat(tmpString) }
    warning(tmpString)
    return(list(nrOfReplicas=nrOfReplicas,spotReplicas=spotReplicas))
  }

  spotIDs <- arrayDescription[,spotIdentifier]
  if( ! is.null(identifiersToBeSkipped) ){
    selNonValid <- spotIDs %in% identifiersToBeSkipped
  }else{
    selNonValid <- rep(FALSE, length(spotIDs))
  }
  validIdentifiers <- unique(spotIDs[!selNonValid]) 
  nrOfSkipped <- sum(selNonValid)
  if(verbose){ 
    cat(" number of items to be skipped: ", nrOfSkipped, "\n")
  }

  if(length(spotIDs) > nrOfSkipped ){

    spotReplicas <- split(1:length(spotIDs), spotIDs)
    spotReplicas <- spotReplicas[validIdentifiers]
    
    distributionOfReplicas <- sapply(spotReplicas, length)
    maxReplicas <-  max(distributionOfReplicas)
    minReplicas <-  min(distributionOfReplicas)
    if( maxReplicas == minReplicas ){
      nrOfReplicas <- maxReplicas
      if( verbose ){
        cat("\n every item is spotted ");cat(nrOfReplicas);cat(" times \n")
      }
    }else{
      if( verbose ){
        cat("\n no single/duplicate/triplcate ...")
        cat(" spotting has been detected \n")
        cat(paste(" the number of spot replicas varies between ",
                  minReplicas, " and ", maxReplicas, "\n",sep=""))
        cat(" the distribution is given by\n")
        cat(" (first line type, e.g. duplicate = 2, triplicate= 3 \n")
        cat("   second line number of cases )\n")
        distTable <- table(factor(distributionOfReplicas))
        cat(format(dimnames(distTable)));cat("\n")
        cat(distTable);cat("\n")
      }
    }
  }else{
    if(verbose){ cat(" no spot identifiers given\n") }
  }
    
  return(list(nrOfReplicas=nrOfReplicas,spotReplicas=spotReplicas))
  
}
