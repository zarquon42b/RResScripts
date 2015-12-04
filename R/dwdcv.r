#' leaving one out cross-validation for DWD
#'
#' leaving one out cross-validation for DWD
#' @param x predictor
#' @param y binary coded variable for classification
#' @param ... arguments passed to kerndwd and predict.kerndwd
#' @return vector of cross-validated predictions
#' @export
dwdcv <- function(x,y,...) {
    require(kerndwd)
    n <- length(y)
    cvfun <- function(i) {
        xtmp <- x[-i,,drop=FALSE]
        ytmp <- y[-i]
        dwdtmp <- kerndwd(xtmp,ytmp,...)
        out <- predict.kerndwd(dwdtmp,x=xtmp,newx=x[i,,drop=FALSE],...)
        return(out)
    }
    
    allout <- unlist(parallel::mclapply(1:n,cvfun))
    print(table(allout,y))
    print(prop.table(table(allout,y),1)*100)
    return(allout)
    
}
    
