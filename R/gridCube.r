gridCube <- function(matrix, tarmatrix=NULL, ngrid=10)
{
    if (is.null(tarmatrix))
        tarmatrix <- matrix
    x2<-x1<-x3<-c(0:(ngrid-1))/ngrid;x0<-as.matrix(expand.grid(x1,x2,x3))
    cent.mat<-apply(matrix,2,scale,scale=F)
    mean.mat<-apply(matrix,2,mean)
    xrange <- diff(range(matrix[,1]))
      yrange <- diff(range(matrix[,2]))
      zrange <- diff(range(matrix[,3]))
      xrange1 <- diff(range(tarmatrix[,1]))
      yrange1 <- diff(range(tarmatrix[,2]))
      zrange1 <- diff(range(tarmatrix[,3]))
      maxi <- max(c(xrange,yrange,zrange,xrange1,yrange1,zrange1))
      maxi <- maxi+0.02*maxi
      x0 <- maxi*x0
      x0 <- apply(x0,2,scale,scale=FALSE)
      space <- eigen(crossprod(cent.mat))$vectors
      x0 <- t(t(x0%*%space)+mean.mat)
      #x0 <- tps3d(x0,matrix,tarmatrix)

    outmesh <- list(vb = rbind(t(x0),1))
    class(outmesh) <- "mesh3d"
    iq <- matrix(NA, 4, ngrid*(ngrid-1)^2)

    xinit0 <- xinit <- (c(1,2,2+ngrid,1+ngrid))
    print(xinit)
    for( i in 1:(ngrid-2))
       xinit <- cbind(xinit,(xinit0+i))
    #iq[,1:((ngrid-1))] <- xinit
    #j <- 2*(ngrid-1)
    #print(dim(xinit))
    xinit0 <- xinit
    for (i in 1:(ngrid-2))
        xinit <- cbind(xinit,xinit0+(i*ngrid))
     #   {
     #       iq[,(j+1):(2*j)] <- iq[,1:j]+ngrid
     #       j <- 2*j
     #   }
    xinit0 <- xinit
             #print(dim(xinit))
    for (i in 1:(ngrid-1))
        xinit <- cbind(xinit,xinit0+(i*ngrid^2))
 #print(dim(xinit))
   yinit0 <- yinit <- c(ngrid,ngrid+ngrid^2, 2*ngrid+ngrid^2, 2*ngrid)
    for( i in 1:(ngrid-2))
        yinit <- cbind(yinit,yinit0+i*ngrid)
    yinit0 <- yinit
   for (i in 1:(ngrid-2))
      yinit <- cbind(yinit,yinit0+i*ngrid^2)
    yinit0 <- yinit
     for (i in 1:(ngrid-1))
         yinit <- cbind(yinit,yinit0-i)
    outmesh$ib <- cbind(xinit,yinit)
    return(list(x0=x0,ib=iq,mesh=outmesh))
      
}
