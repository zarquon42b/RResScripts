ff2csv <- function(x) {
     tab <- with(x, data.frame(df, exVarSS, c(nPC, NA), c(nBU,NA), c(exVarPC, NA), c(exVarBU, NA), c(pValues, NA)))
     tab <- with(x, data.frame(df, exVarSS, c(nPC, NA), c(nBU, 
        NA), c(exVarPC, NA), c(exVarBU, NA), c(pValues, NA)))
    dimnames(tab) <- list(c(x$termNames, "Residuals"), c("Df", 
        "exVarSS", "nPC", "nBU", "exVarPC", "exVarBU", "p-Value"))
    tab <- tab[-1, ]
     return(tab)
 }
