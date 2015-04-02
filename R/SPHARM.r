
#' A simple wrapper for the command line tools of SPHARM-PDM
#'
#' A simple wrapper for the command line tools of SPHARM-PDM, starting from an object of class mesh3d
#' @export
spharmCompute <- function(x,imagename,imagetype="mha",clean=TRUE,flipTemplate=NULL,spacing=rep(1,3),outfolder="./",iter=100, subdivLevel=20,GenArgs=NULL,ParaArgs=NULL) {
    outimage <- paste(imagename,imagetype,sep=".")
    if (! require(RvtkStatismo))
        stop("please install RvtkStatismo from https://github.com/zarquon42b/RvtkStatismo")
    vtkMesh2Image(x,spacing=spacing, filename = outimage, col=1)
    
    cmd <- ""
    if (clean) {
        imclean <- paste0(imagename,"_clean.mha")
        cmd <- paste(cmd,"SegPostProcessCLP",outimage, imclean,"&&")
        outimage <- imclean
    }
    dir.create("parasurf",showWarnings = F)
    dir.create(outfolder, showWarnings = F) 
    paraname <- paste0("parasurf/",imagename,"_para.vtk")
    surfname <- paste0("parasurf/",imagename,"_surf.vtk")
    cmd <- paste(cmd,"GenParaMeshCLP --label 1 --iter",iter,GenArgs,outimage,paraname,surfname, "&&")
    if (! is.null(flipTemplate))
        flipTemplate <- paste("--flipTemplate", flipTemplate,"--flipTemplateOn")
    cmd <- paste(cmd,"ParaToSPHARMMeshCLP --subdivLevel", subdivLevel, flipTemplate,ParaArgs,paraname, surfname, paste0(outfolder,"/",imagename,"_"))
    system(cmd)
}

#' a wrapper for ParaToSPHARMMeshCLP
#' @export
spharmParaToMesh <- function(paraname, surfname,args=NULL,outname="default_",helpargs=FALSE) {
    if (helpargs)
        system("ParaToSPHARMMeshCLP -h")
    else {
        cmd <- paste("ParaToSPHARMMeshCLP",paraname, surfname,args,outname)
        system(cmd)
    }
}

#' read SPHARM coef files
#' @export
read.coef <- function(x) {
    raw <- readLines(x,warn=F)
    rawlist <- strsplit(raw,split = "\\{")
   
    rawlist <- lapply(rawlist,function(x) x <- x[-1])
    rawlist <- lapply(rawlist,function(x) x <- gsub("\\}","",x))
    nrowC <- as.numeric(gsub(",","",unlist(rawlist)[1]))
    rawlist <- paste((unlist(rawlist)[-1]),collapse = "")
    rawlist <- as.numeric(unlist(strsplit(rawlist,",")))
    out <- matrix(rawlist,nrowC,length(rawlist)/nrowC,byrow = TRUE)
    return(out)
}

#' write SPHARM coef files
#' @export
write.coef <- function(x,filename){
    bulk <- x
    nmax <- ncol(bulk)
    if (nmax != 3)
        stop("x must be a 3-column matrix")
    rmax <- nrow(bulk)
    bulk[,-nmax] <- paste0(bulk[,-nmax],",")
    bulk[,1] <- paste0("{",bulk[,1])
    bulk[,nmax] <- paste0(bulk[,nmax],"}")
    bulk[-rmax,nmax] <- paste0(bulk[-rmax,nmax],",")
    bulk[rmax,nmax] <- paste0(bulk[rmax,nmax],"}")
    bulk[1,1] <- paste0("{ ",rmax,",",bulk[1,1])
    bulk <- apply(bulk,1,paste,collapse=" ")
    write.table(bulk,filename,quote=FALSE,row.names=FALSE,col.names = FALSE)
}
