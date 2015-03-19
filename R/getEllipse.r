#' compute ellipse fitting four points as the extrema
#'
#' compute ellipse fitting four points as the extrema
#' @param points 4 x 3 matrix with rows containing the extrema of an ellipse in clockwise or counterclockwise fashion
#' @param npt integer: amount of points on the ellipse to generate
#' @return returns a matrix with points showing an allipse
#' @export
getEllipse <- function(points,npt) {# takes extrema of ellipse in clockwise fashion
    
    a <- norm(points[1,]-points[3,],"2")/2
    b <- norm(points[2,]-points[4,],"2")/2
    a0 <- c(0,a,0)
    b0 <- c(b,0,0)
    ref <- rbind(b0,a0,-b0,-a0)
    out <- matrix(0,npt,3)
    angles <- seq(from=0,to=2*pi,length.out=npt)
    for (i in 1:npt) {
        out[i,1] <- a*cos(angles[i])
        out[i,2] <- b*sin(angles[i])
    }
    rot <- rotonmat(out,refmat = ref,tarmat = points,reflection = TRUE,scale = FALSE)
    
    return(rot)
}
