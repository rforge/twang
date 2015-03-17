
iptw <- function(formula, data, timeInvariant = NULL, estimand = "ATE", n.trees = 10000, stop.method = "es.max", cumulative = TRUE, timeIndicators = NULL, ID = NULL, priorTreatment = FALSE,  ...){
	if(!is.list(formula) & is.null(timeIndicators)) stop("\"formula\" must be a list with length equal to the number of time points (wide data format), or timeIndicators must be specified (long data format).")
	
	isLong <- !is.list(formula)
	
	if(isLong){
		if(length(grep(".time.", attr(terms(formula), "term.labels"))) > 0){
			stop("For long-format data specifications, \".time.\" is not allowed in variable names in the \n
		right-hand side of the formula. Please rename: \n", attr(terms(formula), "term.labels")[grep(".time.", attr(terms(formula), "term.labels"))])
		}
	}
	else{
		wideDat <- NULL
		if(priorTreatment) warning("The 'priorTreatment' argument only applies for long data formats.")
	}
	
	if(isLong){
		if(is.null(ID)) warning("Using long data format specification without IDs. The function is assuming \n that the ordering of subjects in 'data' is the same at all time points.")
		tvCov <- attr(terms(formula), "term.labels")
		if(!is.null(timeInvariant)) tiCov <- attr(terms(timeInvariant), "term.labels")
		tb <- table(timeIndicators)
		tb <- tb[tb > 0]
		if(!(max(tb) == min(tb))) stop("Outcomes must be observed at all times for all subjects.")
		unqTimes <- sort(unique(timeIndicators))
		formList <- vector(mode = "list", length = length(unqTimes))
		hldDat <- subset(data, timeIndicators == unqTimes[1])
		trtVar <- all.vars(formula)[1]
		hldDat <- hldDat[,names(hldDat) %in% c(tvCov, trtVar)]
		hdNm <- names(hldDat)
		names(hldDat) <- paste(hdNm, ".time.", unqTimes[1], sep = "")
		trtVarLong <- paste(trtVar, ".time.", unqTimes[1], sep = "")
		tvCovLong <- paste(tvCov, ".time.", unqTimes[1], sep = "")
		formList[[1]] <- as.formula(paste(trtVarLong, paste(tvCovLong, collapse = " + "), sep= "~"))
		wideDat <- hldDat
		if(priorTreatment) hdNm <- c(attr(terms(formula), "variables")[[2]], hdNm)
		for(i in 2:length(unqTimes)){
			hldDat <- subset(data, timeIndicators == unqTimes[i])
			hldDat <- hldDat[,names(hldDat) %in% c(tvCov, trtVar)]
			names(hldDat) <- paste(hdNm, ".time.", unqTimes[i], sep = "")
			wideDat <- cbind(wideDat, hldDat)
			trtVarLong <- paste(trtVar, ".time.", unqTimes[i], sep = "")
			tvCovLong <- paste(tvCov, ".time.", unqTimes[i], sep = "")
			formList[[i]] <- as.formula(paste(trtVarLong, paste(tvCovLong, collapse = " + "), sep= "~"))
		}
		dt2 <- wideDat
		
	}
	else{
		unqTimes <- 1:length(formula)
		formList <- formula
		dt2 <- data
	}
	
	if(! is.null(timeInvariant)){
		invTerms <- attr(terms(timeInvariant), "term.labels")
		for(i in 1:length(formula)){
			currTerms <- union(attr(terms(timeInvariant), "term.labels"), invTerms)
			formList[[i]] <- as.formula(paste(all.vars(formList[[i]])[1], paste(invTerms, collapse = " + "), sep = "~"))
		}
	}


	nFits <- length(formList)
	
	if(cumulative){
		for(i in 2:nFits){
			oldTerms <- attr(terms(formList[[i-1]]), "term.labels")
			allTerms <- union(oldTerms, attr(terms(formList[[i]]), "term.labels"))
			formList[[i]] <- as.formula(paste(all.vars(formList[[i]])[1], paste(allTerms, collapse = " + "), sep = "~"))
		}
	}
	
	formula <- formList	
	
	psList <- vector(mode = "list", length = length(formula))
		
	for(i in 1:nFits){
		psList[[i]] <- ps(formula[[i]], data = dt2, n.trees = n.trees, stop.method = stop.method, ...)	
	}
	
	outObj <- list(psList = psList, estimand = estimand, stop.methods = stop.method, nFits = nFits, 
	uniqueTimes = unqTimes)
	
	class(outObj) <- "iptw"
	return(outObj)
	
}