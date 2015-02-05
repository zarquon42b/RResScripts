#' get harmonics
#'
#' get harmonvics
#'
#' @param x \code{\link{Coe}} object
#' @param nh integer (vector): number of harmonics to extract
#' @param type type="list" returns a list of coefficients, type="matrix" a matrix, and type="index" the column indices.
#' @return list or matrix with harmonics (an, bn, cn, dn), or list with column indices
#' @export getHarm
getHarm <- function(x,nh=NULL,type=c("list","indices","matrix")) {
    type <- type[1]
    
    an = grep("A",colnames(x),ignore.case = T)
    bn = grep("B",colnames(x),ignore.case = T)
    cn = grep("C",colnames(x),ignore.case = T)
    dn = grep("D",colnames(x),ignore.case = T)
    
    
    if (!is.null(nh))
      {
        an <- an[nh]
        bn <- bn[nh]
        cn <- cn[nh]
        dn <- dn[nh]
      }
    clist <- list(
        an= an,
        bn = bn,
        cn = cn,
        dn = dn
    )
    clist <- lapply(clist,function(x) {if (!length(x)) x <- NULL;return(x)}) 
    if (type == "list")
        return(list(an=x[,clist$an,drop=FALSE],bn=x[,clist$bn,drop=FALSE],cn=x[,clist$cn,drop=FALSE],dn=x[,clist$dn,drop=FALSE]))
    else if (type== "matrix") {
        outmat <- cbind(an=x[,clist$an,drop=FALSE],bn=x[,clist$bn,drop=FALSE],cn=x[,clist$cn,drop=FALSE],dn=x[,clist$dn,drop=FALSE])
        rownames(outmat) <- rownames(x)
        return(outmat)
        
    } else
        return(list(an,bn,cn,dn))
  }
#' convert harmonics to 2D-coordinates
#'
#' convert harmonics to 2D-coordinates
#' @param x vector, matrix or Coe object
#' @param nh integer (vector): number of harmonics to use
#' @param nb.pts number of coordinats
#' @return matrix with 2D coordinates
#' @export ef2coo
ef2coo <- function(x,nh=NULL,nb.pts=300,type=c("e","t","r")) {
    type <- type[1]
    if (inherits(x , "Coe"))
        x <- x$coe
    if (is.vector(x)) {
        xnam <- names(x)
        x <- matrix(x,1,length(x));colnames(x) <- xnam
    }
    harms <- getHarm(x)#apply(x,1,getHarm,drop=F)
    ll <- lapply(harms,length)
    if (!length(harms$cn) && !length(harms$dn)) {
        if (type == "t")
            revfun <- tfourier_i
        else
            revfun <- rfourier_i
    }
    else
        revfun <- efourier_i

    
    idnames <- rownames(harms[[1]])
    outharms <- harms
    if(!is.null(nh))
        outharms <- lapply(outharms,function(x){ x[,-nh] <- 0;return(x)})
        
                                        #outharms <- lapply(outharms,function(x){x <- lnction(x){x[-nh] <- 0;return(x)})})
    outcoords <- list()
    for (i in 1:dim(outharms[[1]])[1])
         outcoords[[i]] <- revfun(list(an=outharms$an[i,],bn=outharms$bn[i,],cn=outharms$cn[i,],dn=outharms$dn[i,]),nb.pts=nb.pts)
    names(outcoords) <- idnames
    coo.out <- Out(outcoords)
    return(coo.out)
  }


signedAngle2d <- function(x,y) {
     x <- as.vector(x)/sqrt(sum(x^2))
     y <- as.vector(y)/sqrt(sum(y^2))
     perpDot <- x[1] * y[2] - x[2] * y[1]
     return(atan2(perpDot,crossprod(x, y)))
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
        #print(angle)
        angsign <- sign(signedAngle2d(v1,v2))
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
        n <- length(Coo$coo)
        #refdir <- checkDir(Coo$coo[[1]])
        if (n > 1)
            {
                for (i in 2:n)
                    {
                        tmp <- checkDir(Coo$coo[[i]])
                        if (sign(tmp) < 0)
                           { Coo$coo[[i]] <- Coo$coo[[i]][dim(Coo$coo[[i]])[1]:1,]
                             cat(paste("changed Direction for outline",i,"\n"))
                         }
                    }
            }
        return(Coo)
    }
                
#' run efourier_norm on object of class Coe
#'
#' run efourier_norm on object of class Coe and report values
#' @param efa object of class Coe
#' @param output if "size" only a list with the size of the first ellipse is returned, the complete output of efourier_norm otherwise.
#' 
#' @export       
normEFA <- function(efa,output="size") {
    out <- list()
    for(i in 1:nrow(efa$coe)) {
        tmp <- getHarm(efa$coe[i,,drop=F])
        nn <- efourier_norm(tmp)
        if (length(output) == 1) {
            gg <- which(names(nn) == output)
            out[[i]] <- nn[[gg]]
        } else
            out[[i]] <- nn
    }
    names(out) <- rownames(efa$coe)
    return(out)
}

#' set starting pointi n a list o outlines
#'
#' set starting pointi n a list o outlines
#' @param x list of coordinate matrices
#' @return a list of coordinates close to the starting point
setLM <- function(x) {
    out <- list()
    for(i in 1:length(x)) {
        plot(x,i)
        tmp <- locator(n=1,type="p")
        #points(unlist(tmp),col=2,pch=19)
        out[[i]] <- t(as.matrix(unlist(tmp)))
    }
    return(out)
}

#' get index of starting point given a close estimate
#'
#' get index of starting point given a close estimate determined by setLM
#' @param lms list of starting points (as 1 x 2 matrices)
#' @param coords object of class Coo
#' @return vector containing indices of starting points
getStart <- function(lms,coords) {
    ind <- NULL
    for(i in 1:length(coords$coo)) {
        ind[i] <- mcNNindex(coords$coo[[i]][,1:2],lms[[i]],k=1)
    }

    return(ind)
}
#' resort coordinates based on indices of starting points
#'
#' resort coordinates based on indices of starting points (determined by getStart)
#' 
#' @param coords object of class Coo
#' @param vector of indices
#' @return Coo object with resorted coordinates
resortCoord <- function(coords,ind) {
    for (i in 1:length(coords$coo)) {
        tmp <- coords$coo[[i]][,1:2]
        if (ind[[i]] > 1) {
            lower <- unique(ind[[i]]:nrow(tmp))
            upper <- unique(1:(ind[[i]]-1))
            tmp1 <- rbind(tmp[lower,],tmp[upper,])
            coords$coo[[i]] <- tmp1
        }
    }
    return(coords)
}
