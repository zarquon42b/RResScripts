triangvol <- function(x,y)
  {
    allvol <- rbind(x,y[c(3,1,2),])
    if (dim(allvol)[2] ==3 )
      allvol <- cbind(allvol,1)
    p0 <- c(0,0,0,1)
    V <- tetravol(allvol[1:4,])+tetravol(allvol[c(1,2,4,5),])+tetravol(allvol[c(2,4,5,6),])
    return(V)

  }

tetravol <- function(x)
  {
    volume <- 1/6*abs(det(x))
    return(volume)
  }

trianintersect <- function(x,y)
  {
    normal <- crossp(x[1,]-x[2,],x[1,]-x[3,])
    p0 <- x[1,]
    signdist <- sign(apply(y,1,function(z){z <- crossprod(z-p0,normal)}))
    grt <- prod(signdist >=0)
    less <- prod(signdist <= 0)
    
    if (grt == 1 && less == 1)

    
    
    return(signdist)
  }
    
trivolfort <- function(mesh1,mesh2)
  {
    #mesh2 <- conv2backf(mesh2)
    vb1 <- mesh1$vb
    vb2 <- mesh2$vb
    it <- mesh1$it; storage.mode(it) <- "integer"
    V <- 0; storage.mode(V) <- "double"
    out <- .Fortran("trianvol",vb1,vb2,it,ncol(vb1),ncol(it),V)
    return(out)
  }
