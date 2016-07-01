read.viewbox <- function(file) {
    tree <- xmlParse(file)
    patients <- getNodeSet(tree,"//patients/patient")
    patientChild <- lapply(patients,xmlChildren)
    datasets <- lapply(patientChild,function(x) x <- x$dataset)
    datasetsChild <- lapply(datasets,xmlChildren)
    ptfun <- function(x) {
        out <- NULL
        pts <- which(names(x) == "points")
        if (length(pts)) {
            out <- as.numeric(unlist(lapply(x[pts],function(x) lapply(xmlChildren(x),xmlAttrs))))
            out <- t(matrix(out,4,length(out)/4))
            rownames(out) <- out[,1]
            out <- out[,2:4]
        }
        return(out)
    }
    cvsfun <- function(x) {
        out <- NULL
        pts <- which(names(x) == "curves")
        if (length(pts)) {
            outchild<- xmlChildren(x$curves)
        }
        cc <- which(names(outchild) == "curve")
        if (length(cc)) {
            out <- as.numeric(unlist(lapply(outchild[cc],function(x) lapply(xmlChildren(x),xmlAttrs))))
            out <- t(matrix(out,3,length(out)/3))
        }
        return(out)    
    }
    dspts <- lapply(datasetsChild,ptfun)
    cvpts <- lapply(datasetsChild,cvsfun)
    return(list(dspts=dspts,cvpts=cvpts))
}
