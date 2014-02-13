#' @import Morpho
#' @export
nafun <- function(x) {
    ret <- apply(x,1,function(x){
        out <- prod(is.na(x)); return(out)
    })
    return(ret)
}

#' for a 3D-array get a list of vectors indicating lines with NAs 0=data, 1=NA
#' @export
nafunArray <- function(x) {
    nalist <- list()
    for (i in 1:(dim(x)[3]))
        nalist[[i]] <- nafun(x[,,i])
    return(nalist)
}

#' check if there are pairwise complementary data missing
checkmatArray <- function(nalist) {
    nn <- length(nalist)
    checkmat <- matrix(NA,nn,nn)
    for (i in 1:(nn-1)) {
        for (j in (i+1):nn) {
            print(paste(i,j))
            checkmat[i,j] <- sum(nalist[[i]]*nalist[[j]])
        }
    }
    return(checkmat)
}
#' get a list with all rows from an nalist (output from nafunArray) that are actually NAs
#' @export
getNAlistBool <- function(nalist) {
    out <- lapply(nalist,function(x) which(as.logical(x)))
    return(out)
}

#' based on an array and the result from getNAlistBool map each specimen onto all others using TPS
#' @export
pairwiseTPS <- function(bilatfix,nalistBool) {
    fixlist <- list()
    nn <- dim(bilatfix)[3]
    for (i in 1:nn) {
        tmparr <- bilatfix[,,]
        reference <- bilatfix[,,i]
        tarint <- (1:nn)[-i]
        for (j in 1:(nn-1)) {
            target <- bilatfix[,,tarint[j]]
            tarmissing <- nalistBool[[ tarint[j] ]]
            bothmissing <- sort(unique(append(nalistBool[[i]],tarmissing)))
            tmparr[,,tarint[j]] <- tps3d(reference,reference[-bothmissing,], target[-bothmissing,])
            tmparr[-tarmissing,,tarint[j] ] <- target[-tarmissing,]
            
        }
        fixlist[[i]] <- tmparr
    }
    return(fixlist)
}

#' run all functions above and select the best version of each specimen (i.e. with the least missing landmarks
#' @export
fixitall <- function(bilatfix) {
    nalist <- nafunArray(bilatfix)
    nalistBool <- getNAlistBool(nalist)
    fixlist <- pairwiseTPS(bilatfix,nalistBool)
    check2 <- lapply(fixlist,nafunArray)
    check2Bool <- unlist(lapply(check2,getNAlistBool),recursive = F)
    allarr <- fixlist[[1]]
    for (i in 2:length(fixlist))
        allarr <- bindArr(allarr,fixlist[[i]],along=3)
    dimnames(allarr)[[3]] <- rep(dimnames(bilatfix)[[3]],length(fixlist))
    check2Bool <- unlist(lapply(check2,getNAlistBool),recursive = F)
    check2BoolLength <- unlist(lapply(check2Bool,length))
    nn <- dim(bilatfix)[3]
    ## now we select the best version of each specimen (least missing landmarks)
    bestSelect <- bilatfix
    for (i in 1:dim(bilatfix)[3]){
        subsel <- which((1:dim(allarr)[3]) %%nn  == i%%nn)
        best <- subsel[which(check2BoolLength[subsel]==min(check2BoolLength[subsel]))][1]
        bestSelect[,,i] <- allarr[,,best]
    }
    return(bestSelect)
}
