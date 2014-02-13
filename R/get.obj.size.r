#' analyse memory usage of objects in current workspace
#'
#' analyse memory usage of objects in current workspace
#' @param units decimal power to show used space e.g. 2=1024^2 bytes = Megabytes.
#' @return data frame of objects and space used
#' @examples get.obj.size()
#' @export get.obj.size
get.obj.size <- function(units=2)
  {
    units <- units[1]
    
    allob <- 0
    wspace <- ls(.GlobalEnv)
    #print(wspace)
    for (i in 1:length(wspace))
      {
        allob[i] <- (object.size(get(wspace[i])))
      }
    allob <- round(allob/1024^units,digits=2)
    allob <- data.frame(allob,wspace)
    allob <- allob[order(allob[,1],decreasing=TRUE),]
    return(allob)    
  }
