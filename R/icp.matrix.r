#' ICP matching for two matrices
#'
#' ICP matching for two matrices
#' @param x matrix
#' @param y matrix containing target coordinates
#' @param maxdist maximum distance to consider
#' @param weighting logical if TRUE, each point is weighted by the inverse distance to the closest point
#' @param iterations integer: iterations to run
#' @return returns scaled, translated and rotated version of \code{x}
#' @examples
#' require(Morpho)
#' data(nose)
#' matchpoint <- icp.matrix(longnose.lm,shortnose.lm, maxdist = 5)
#' @export icp.matrix 
#' 
icp.matrix <- function(x, y, maxdist=1, weighting=TRUE, iterations=3)
    {
        require(RANN)
        i <- 0
        while (i < iterations) {
            clost <- nn2(y, x, k=1)
            clostInd <- clost$nn.idx
            good <- which(clost$nn.dists <= maxdist)
            if (weighting) {
                weights <- 1/clost$nn.dists[good]
                chk <-  sapply(weights,is.infinite)
                if (TRUE %in% chk)
                    weights[which(chk)] <- 1e12
        } else
                weights <- NULL
            xtmp <- rotonmat(x, x[good, ], y[clostInd[good], ], weights=weights)
            i <- i+1
        }
        return(xtmp)
    }
