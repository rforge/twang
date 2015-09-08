get.weights.unstab <- function(x){
	if(class(x) != "iptw") stop("\"get.weights.unstab\" is only defined for iptw objects.")
	prodWt <- rep(1, length(x$psList[[1]]$treat))
	for(i in 1:length(x$psList)){
		hdWt <- x$psList[[i]]$ps * x$psList[[i]]$treat + (1-x$psList[[i]]$ps) * (1-x$psList[[i]]$treat)
		prodWt <- hdWt * prodWt
	}	
	return(1/prodWt)
}