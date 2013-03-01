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





