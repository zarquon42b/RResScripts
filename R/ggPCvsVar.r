#' create Plot for each column of PCs against var
#'
#' create Plot for each column of PCs against var
#' @param PCs Matrix for y-Axis
#' @param var vector containing data for x-axis
#' @param xlab x-axis label
#' @param ncol layout columns
#' @param group grouping variable
#' @param layout layout matrix
#' @param legend.title custom legend title
#' @param pt.size point size
#' @param lwd line width of regression line
#' @rdname ggMatvsVar
#' @export ggMatvsVar
ggMatvsVar <- function(PCs, var, xlab=xname,ncol=2, group=NULL, layout=NULL, legend.title=NULL,pt.size=1)
{
    require(ggplot2)
    require(grid)
    xname <- deparse(substitute(var))
    dims <- ncol(PCs)
    if (is.null(legend.title) && ! is.null(group))
        legend.title <- deparse(substitute(group))
    if (is.null(layout))
        layout <- matrix(1:(ceiling(dims/ncol)*ncol) , ceiling(dims/ncol),ncol,byrow = T)
    #print(layout)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:ncol(PCs))
        {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print((
                qplot(x=var,y=PCs[,i], geom="line",group=group,colour=group)
                + labs(colour=legend.title,linetype=legend.title,shape=legend.title)
                + geom_point(size=pt.size)+ylab(colnames(PCs)[i])
                + xlab(xlab)
                ), vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
        }
}
#' @rdname ggMatvsVar
#' @export
ggMatvsVar2 <- function(PCs, var, xlab=xname,ncol=2, group=NULL, layout=NULL, legend.title=NULL,pt.size=1,lwd=1)
{
    require(ggplot2)
    require(grid)
    xname <- deparse(substitute(var))
    dims <- ncol(PCs)
    if (is.null(legend.title) && ! is.null(group))
        legend.title <- deparse(substitute(group))
    if (is.null(layout))
        layout <- matrix(1:(ceiling(dims/ncol)*ncol) , ceiling(dims/ncol),ncol,byrow = T)
    #print(layout)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:ncol(PCs))
        {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print((
                qplot(x=var,y=PCs[,i], group=group,colour=group)
                + labs(colour=legend.title,linetype=legend.title,shape=legend.title)
                + geom_smooth(method=lm,size=lwd) 
                + geom_point(size=pt.size)+ylab(colnames(PCs)[i])
                + xlab(xlab)
                ), vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
        }
}
