bal.table.ps <- function(x, digits = digits){
	if(!class(x) == "ps") stop("Function 'bal.table.ps' is only defined for objects of class ps.")
	lapply(x$desc, function(x){return(round(x$bal.tab$results, digits))})
}