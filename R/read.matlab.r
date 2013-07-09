read.matlab <- function(name)
  {
    namevb <- paste(name,".node",sep="")
    nameface <- paste(name,".face",sep="")
    mesh <- list();class(mesh) <- "mesh3d"
    vn <- as.integer(strsplit(readLines(namevb,n=1),split=" ")[[1]][1])
    fn <- as.integer(strsplit(readLines(nameface,n=1),split=" ")[[1]][1])
    vb <- scan(namevb,what=double(),skip=1,nlines=vn)
    vb <- matrix(vb,4,length(vb)/4)[-1,]
    vb <- rbind(vb,1)
    it <- scan(nameface,what=double(),skip=1,nlines=fn)
    it <- matrix(it,5,length(it)/5)[-c(1,5),]
    it <- it+1
    mesh$vb <- vb
    mesh$it <- it
    mesh <- conv2backf(mesh)
    mesh <- adnormals(mesh)
    return(mesh)
  }
    
    
