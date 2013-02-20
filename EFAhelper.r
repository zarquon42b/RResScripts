getHarm <- function(x,nh=NULL)
  {
    namefun <- colnames
    if (is.vector(x))
      namefun <- names
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
    if (is.matrix(x))
     harms <- apply(x,1,getHarm,nh=nh)
    else
      stop("please provide matrix")

    coords <- lapply(harms,efourier.i,nb.pts=nb.pts)
    coo.out <- lapply(coords,function(x){x <- matrix(unlist(x),nb.pts,2)})
    coo.out <- Coo(coo=coo.out)
    coo.out@coo.closed <- sapply(coo.out@coo.closed,function(x){x <- TRUE})
    return(coo.out)
  }
                      
    
      
