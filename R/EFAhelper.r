#' get harmonics
#'
#' get harmonvics
#'
#' @param x \code{\link{Coe}} object
#' @param nh integer (vector): number of harmonics to extract
#' @return list with harmonics (an, bn, cn, dn)
#' @export getHarm
getHarm <- function(x,nh=NULL)
  {
    an <- grep("A",names(x))
    bn <- grep("B",names(x))
    cn <- grep("C",names(x))
    dn <- grep("D",names(x))
    
    if (!is.null(nh))
      {
        an <- an[nh]
        bn <- bn[nh]
        cn <- cn[nh]
        dn <- dn[nh]
      }
    return(list(an=x[an],bn=x[bn],cn=x[cn],dn=x[dn]))
  }
#' convert harmonics to 2D-coordinates
#'
#' convert harmonics to 2D-coordinates
#' @param x vector, matrix or Coe object
#' @param nh integer (vector): number of harmonics to use
#' @param nb.pts number of coordinats
#' @return matrix with 2D coordinates
#' @export ef2coo
ef2coo <- function(x,nh=NULL,nb.pts=300)
  {
    if (class(x) == "Coe")
      x <- x@coe
    if (is.vector(x))
      {
        xnam <- names(x)
        x <- matrix(x,1,length(x));colnames(x) <- xnam
      }
    harms <- apply(x,1,getHarm)
    
    outharms <- harms
    if(!is.null(nh))
      outharms <- lapply(outharms,function(x){x <- lapply(x,function(x){x[-nh] <- 0;return(x)})})
    
    coords <- lapply(outharms,efourier.i,nb.pts=nb.pts)
    coo.out <- lapply(coords,function(x){x <- matrix(unlist(x),nb.pts,2)})
    coo.out <- Coo(coo=coo.out)
    coo.out@coo.closed <- sapply(coo.out@coo.closed,function(x){x <- TRUE})
    return(coo.out)
  }

#' get direction of closed 2D-outline coordinates
#'
#' get direction of closed outline coordinates by comparing signed angles between first and k/4 coordinate
#' @param coords k x 2 matrix containing 2D-coordinates
#' @return sign of the angle
#' @export checkDir
checkDir <- function(coords)
    {
        centcoo <- apply(coords,2,scale, scale=FALSE)
        k <- dim(coords)[1]
        v1 <- centcoo[1,]/sqrt(crossprod(centcoo[1,]))
        v2 <- centcoo[floor(k/4),]/sqrt(crossprod(centcoo[floor(k/4),]))
        angle <- atan2(v1[2],v2[1])-atan2(v2[2],v2[1])
        print(angle)
        angsign <- sign(angle)
        return(angsign)
    }
#' make sure all coordinates of outlines stored in an object of class "Coo" are oriented coherently
#'
#' make sure all coordinates of outlines stored in an object of class "Coo" are oriented coherently
#' @param Coo object of class "Coo"
#' @return input with outlines oriented coherently
#' @export checkDirCoo


checkDirCoo <- function(Coo)
    {
        n <- length(Coo@coo)
        refdir <- checkDir(Coo@coo[[1]])
        if (n > 1)
            {
                for (i in 2:n)
                    {
                        tmp <- checkDir(Coo@coo[[i]])
                        if (sign(refdir*tmp) < 0)
                           { Coo@coo[[i]] <- Coo@coo[[i]][dim(Coo@coo[[i]])[1]:1,]
                             cat(paste("changed Direction for outline",i,"\n"))
                         }
                    }
            }
        return(Coo)
    }
                

        

