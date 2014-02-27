#' pairwise mahalnobis distance
#'
#' pairwise mahalnobis distance
#' @param x data matrix
#' @return pairwise squared mahalanobis distances
#' @note copied from \url{http://stats.stackexchange.com/a/81710}
#' @export
cholMaha <- function(X) {
 dec <- chol( cov(X) )
 tmp <- forwardsolve(t(dec), t(X) )
 dist(t(tmp))
}
