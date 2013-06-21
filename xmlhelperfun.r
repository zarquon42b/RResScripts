copyfun <- function(from,to,zahnID,sliceID,images)
{
    for (i in 1:length(zahnID))
    {
        pathdir <- paste(to,"/",zahnID[i],sep="")
        if (!file.exists(pathdir))
            dir.create(pathdir,recursive = T)
        slicedir <- paste(pathdir,"/",sliceID[i],sep="")
       
        if (!file.exists(slicedir))
            dir.create(slicedir,recursive = T)

        fromfiles <- paste(from,"/",images[[i]],sep="")
        #if (i == 1)
            file.copy(fromfiles,slicedir)            
    }
}

getValueFun <- function(x)
    {
        tmp <- sapply(x,function(x) xmlGetAttr(x,"Value"))
        return(tmp)
    }
