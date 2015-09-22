read.ckpt <- function(file) {
    input <- readLines(file)
    landmarks <- grep("\\[Landmarks\\]",input)
    nlm <- as.integer(gsub("NumberOfPoints:","",input[landmarks+1]))
    lmarr <- read.table(file,nrows=nlm,skip=landmarks+2)
    lmarr <- as.matrix(lmarr[,5:7])
    
    singlepoints <- grep("\\[SinglePoints\\]",input)
    nfix <- as.integer(gsub("NumberOfSinglePoints:","",input[singlepoints+1]))
    fix <- read.table(file,nrows=nfix,skip=singlepoints+1)[,2]+1

    Curves <- grep("\\[Curves\\]",input)
    curvelist <- list()
    ncurves <- as.integer(gsub("NumberOfCurves:","",input[Curves+1]))
    if (ncurves > 0) {
        for (i in 1:ncurves) {
            tmp <- input[Curves+1+i]
            nameind <- strsplit(tmp,split="\"")[[1]][2]
            inds <- as.integer(unlist(strsplit(strsplit(strsplit(tmp,split="\"")[[1]][1],split=":")[[1]][2],split=" ")))
            curvelist[[nameind]] <- inds[-which(is.na(inds))]
        }
    }
    Patches <- grep("\\[Patches\\]",input)
    patchlist <- list()
    npatches <- as.integer(gsub("NumberOfPatches:","",input[Patches+1]))
    if (npatches > 0) {
        for (i in 1:npatches) {
            tmp <- input[Patches+1+i]
            nameind <- strsplit(tmp,split="\"")[[1]][2]
            inds <- as.integer(unlist(strsplit(strsplit(strsplit(tmp,split="\"")[[1]][1],split=":")[[1]][2],split=" ")))
            patchlist[[nameind]] <- inds[-which(is.na(inds))]
        }
    }
           
    return(list(landmarks=lmarr,fix=fix,curves=curvelist,patchlist=patchlist))
}
