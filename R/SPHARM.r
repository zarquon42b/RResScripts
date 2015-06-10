
#' A simple wrapper for the command line tools of SPHARM-PDM
#'
#' A simple wrapper for the command line tools of SPHARM-PDM, starting from an object of class mesh3d
#' @export
spharmCompute <- function(x,imagename,imagetype="mha",clean=TRUE,flipTemplate=NULL,spacing=rep(1,3),outfolder="./",iter=100, subdivLevel=20,GenArgs=NULL,ParaArgs=NULL,readvtk=TRUE,silent=FALSE) {
    outimage <- paste(imagename,imagetype,sep=".")
    if (! require(RvtkStatismo))
        stop("please install RvtkStatismo from https://github.com/zarquon42b/RvtkStatismo")
    vtkMesh2Image(x,spacing=spacing, filename = outimage, col=1)
    pref <- NULL
    if (silent)
        pref <- " > /dev/null"
    cmd <- ""
    if (clean) {
        imclean <- paste0(imagename,"_clean.mha")
        cmd <- paste(cmd,"SegPostProcessCLP",outimage, imclean,pref,"&&")
        outimage <- imclean
    }
    dir.create("parasurf",showWarnings = F)
    dir.create(outfolder, showWarnings = F) 
    paraname <- paste0("parasurf/",imagename,"_para.vtk")
    surfname <- paste0("parasurf/",imagename,"_surf.vtk")
   
    cmd <- paste(cmd,"GenParaMeshCLP --label 1 --iter",iter,GenArgs,outimage,paraname,surfname,pref, "&&")
    if (! is.null(flipTemplate))
        flipTemplate <- paste("--flipTemplate", flipTemplate,"--flipTemplateOn")
    cmd <- paste(cmd,"ParaToSPHARMMeshCLP --subdivLevel", subdivLevel, flipTemplate,ParaArgs,paraname, surfname, paste0(outfolder,"/",imagename,"_"),pref)
    system(cmd,ignore.stdout=silent,ignore.stderr=silent)
    if (readvtk) {
        out <- list()
        out$SPHARM_ellalign <- read.vtk(paste0(outfolder,"/",imagename,"_SPHARM_ellalign.vtk"))
        out$SPHARM <- read.vtk(paste0(outfolder,"/",imagename,"_SPHARM.vtk"))
        
        return(out)
    }
        
}

#' a wrapper for ParaToSPHARMMeshCLP
#' @export
spharmParaToMesh <- function(paraname, surfname,args=NULL,outname="default_",helpargs=FALSE,readvtk=TRUE) {
    if(nargs() == 0)
        helpargs <- T
    if (helpargs)
        system("ParaToSPHARMMeshCLP -h")
    else {
        cmd <- paste("ParaToSPHARMMeshCLP",paraname, surfname,args,outname)
        system(cmd)
    }
    if (readvtk) {
        out <- list()
        out$SPHARM_ellalign <- read.vtk(paste0(outname,"SPHARM_ellalign.vtk"))
        out$SPHARM <- read.vtk(paste0(outname,"SPHARM.vtk"))
        return(out)
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
    cat(paste0("\n   Degree of SPHARMs = ", sqrt(nrowC)-1,"\n\n"))
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
