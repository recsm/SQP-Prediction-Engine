# Various helper functions used in predict.R

# Inverse logit for back-converting predictions
invlogit <- function (x) {
    1/(1 + exp(-x))
}

# Obtain importance scores in order of importance from an randomForest
getImp <- function(rf, order.it=TRUE, which=c(1,2)) {
    imp <- importance(rf)[,which]
    if(order.it) 
		imp <- imp[order(importance(rf, dec=TRUE)[, 1]), 1]
    imp
}

# Calculate a prediction for each of the levels of a particular variable
# If the variable is numeric, calculate the prediction for each quintile
# An aggregation function is applied to the
# predictions, by default this is the mean (fastest because it is cached rf output).
get.conditional.effects <- function(rf, xname, newdata=nd, xlevs,
				   aggregate.func=NULL, idx=c(1)) {

    lvs  <- unique(xlevs[[xname]]$levels)

    f <- function(xval) {
	tmpdat <- newdata
	if(xlevs[[xname]]$is.factor){
	    tmpdat[[xname]] <- factor(xval, levels=lvs, ordered=xlevs[[xname]]$ordered)
	}
	else {
	    tmpdat[[xname]] <- xval
	}
	if (is.null(aggregate.func)) {
	    return(predict(rf, newdata=tmpdat, predict.all=FALSE))
	}
	pred <- predict(rf, newdata=tmpdat, predict.all=TRUE)$individual
	apply(pred, 1, aggregate.func)
    }
    agg.pred <- mapply(f, lvs, SIMPLIFY=FALSE)

    if(!xlevs[[xname]]$is.factor) names(agg.pred) <- lvs
    unlist(lapply(agg.pred, function(x) x[[idx]]))
}

# Variable recoding function from car library
recode <- function (var, recodes, as.factor.result, as.numeric.result = TRUE, 
    levels) 
{
    recodes <- gsub("\n|\t", " ", recodes)
    recode.list <- rev(strsplit(recodes, ";")[[1]])
    is.fac <- is.factor(var)
    if (missing(as.factor.result)) 
        as.factor.result <- is.fac
    if (is.fac) 
        var <- as.character(var)
    result <- var
    if (is.numeric(var)) {
        lo <- min(var, na.rm = TRUE)
        hi <- max(var, na.rm = TRUE)
    }
    for (term in recode.list) {
        if (0 < length(grep(":", term))) {
            range <- strsplit(strsplit(term, "=")[[1]][1], ":")
            low <- eval(parse(text = range[[1]][1]))
            high <- eval(parse(text = range[[1]][2]))
            target <- eval(parse(text = strsplit(term, "=")[[1]][2]))
            result[(var >= low) & (var <= high)] <- target
        }
        else if (0 < length(grep("^else=", squeezeBlanks(term)))) {
            target <- eval(parse(text = strsplit(term, "=")[[1]][2]))
            result[1:length(var)] <- target
        }
        else {
            set <- eval(parse(text = strsplit(term, "=")[[1]][1]))
            target <- eval(parse(text = strsplit(term, "=")[[1]][2]))
            for (val in set) {
                if (is.na(val)) 
                  result[is.na(var)] <- target
                else result[var == val] <- target
            }
        }
    }
    if (as.factor.result) {
        result <- if (!missing(levels)) 
            factor(result, levels = levels)
        else as.factor(result)
    }
    else if (as.numeric.result && (!is.numeric(result))) {
        result.valid <- na.omit(result)
        opt <- options(warn = -1)
        result.valid <- as.numeric(result.valid)
        options(opt)
        if (!any(is.na(result.valid))) 
            result <- as.numeric(result)
    }
    result
}


# Needed by recode
squeezeBlanks <- function (text) {
    gsub(" *", "", text)
}


# Not used: not faster than just calling repeatedly from python
get.all.conditional <- function(model, nd, xlevs) {
    print("Called get.all.conditional")
    print(system.time(
	res <- mapply(function(xname) {
		invlogit(get.conditional.effects(model, xname, nd, xlevs))
	    }, names(model$forest$xlevels))
    ))
    print(res)
    res
}




if(FALSE) { 
    # Some helper code that creates the cached list of xlevels
    #   Does not need to be run every time
    get.levels <- function(x) {
	if(is.factor(x)) {
	    return(levels(x))
	} 
	if(length(uniq <- unique(x)) > 5) {
	    return(quantile(x, na.rm=TRUE))
	} 
	sort(uniq)
    }
    lvs <- list()
    for(i in names(data)) 
	try(lvs[[i]] <- list('is.factor'=is.factor(data[,i]), 
			    'ordered'=is.ordered(data[,i]),
			     'levels'=get.levels(data[,i])
			     ), silent=TRUE)
    xlevels <- lvs
    xlevels.fre <- xlevels[-grep('^(rel|val|study|item_name)$', names(xlevels))]
    save(xlevels.fre, file="~/Documents/sqp_project/sqp/predict/xlevels-fre.Rdata")
}
