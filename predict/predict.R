library(randomForest)

# Load in helper functions
source('sqp/predict/helpers.R')

# Load in levels of original training covariates
load(file = "sqp/predict/xlevels_cat.Rdata")
load(file = "sqp/predict/xlevels_fre.Rdata")

# Load prediction models
#########################################################################
# RAIMforest is not a typo: it stands for RAndom IMputation Forests
print(system.time( {
    load('sqp/predict/raimforest.cat-rel.Rdata')
    load('sqp/predict/raimforest.fre-rel.Rdata')
}
))
print(system.time({
    load('sqp/predict/raimforest.cat-val.Rdata')
    load('sqp/predict/raimforest.fre-val.Rdata')
}))

# Set default number of digits for rounding, is also set from python
digits  <- 3

# Set debug or not
django.debug <- FALSE

# Define output functions

# Standard deviation over predictions
get.sd <- function(pre) 
    round(sd(pre), digits)

# First and third quartiles
get.iqr <- function(pre) 
    round(quantile(pre, c(1/4, 3/4)), digits)

# Prediction interval, size should be given in percentage
get.pred.interval <- function(pre, size=95) 
    round(quantile(pre, c((1/2) - size/200, (1/2) + size/200)), digits)


#DEBUG print and save the obtained codes
#print("sqp.codes is")
#print(sqp.codes)
#save(sqp.codes, file='sqp/predict/test-codes.Rdata')

#########################################################################
# Create a dataset for prediction from the SQP codes passed by Django   #
#########################################################################

get.nd.from.choices <- function(choices, var.names) {

    sqp.codes <- unlist(choices)
    names(sqp.codes) <- var.names

    if(!"scale_basic" %in% names(sqp.codes)){
	print(sqp.codes)
	stop("get.nd.from.choices() got called but no scale_basic in choices.")
    }
    xlevels <- if(sqp.codes['scale_basic'] == '2') { xlevels.fre } else { xlevels.cat }

    # Select variables
    varnames <- c(#'fixrefpoints', 'ncategories',  
		  'domain', 'concept', 'socdesir',
		  'scale_trange', 'scale_urange')
    # categorical variables
    factor.names <- varnames[which(mapply(function(vn) xlevels[[vn]]$is.factor, varnames))]
	    
    nd <- as.list(sqp.codes[varnames])

    nd$computer.assisted <- sqp.codes['comp_assist']


    # Recode SQP codings to prediction variables
    #########################################################################

    # Copy the codes than can be copied (if they are in the xlevels list and also
    # in the SQP codes provided)
    for(xname in intersect(names(xlevels), names(sqp.codes))) {
	nd[[xname]] <- sqp.codes[xname]
    }

    # Domain: national politics is split according to subchar
    #  (Code 1 should guarantee that natpoldomain is available, otherwise an error)
    # TODO: Should the missing ones be coded "Other"?
    nd$domain[nd$domain == 1] <- recode(sqp.codes['natpoldomain'],"
	    '11'=101;
	    '13'=103;
	    '15'=105;
	    '17'=107;
	    else=120")
    # Recode Family and EU politics into Other
    nd$domain[nd$domain %in% c(9,10)]  <- 120
	    
    # Concept: other simple is split according to subchar
    #  (Code 7 should guarantee that conc_simple is available, otherwise an error)
    nd$concept[nd$concept == 7] <- paste("7", sqp.codes['conc_simple'], sep="")

    nd$future  <- recode(sqp.codes['ref_period'], "1=1; 2=0; 3=0;")
    nd$past  <- recode(sqp.codes['ref_period'], "1=0; 2=0; 3=1;")

    # Numeric characteristics
    numeric.chars <-
	names(sqp.codes)[grep('^n(syll|words|abst|nouns|sents|umsub|sub)',
					   names(sqp.codes))]
    numcodes <- numeric()
    for(xname in numeric.chars) 
	numcodes[xname] <- as.numeric(sqp.codes[xname])

    nd$nsyll_total    <- numcodes['nsyll_ans']  + numcodes['nsyll_quest'] 
    nd$nwords_total   <- numcodes['nwords_quest']  # Ans is all NA
    nd$avgsy_total <- ifelse(nd$nwords_total == 0, 0, 
			     nd$nsyll_total / nd$nwords_total )   
    nd$nabst_total    <- numcodes['nabst_ans']  + numcodes['nabst_quest']
    nd$nnouns_total   <- numcodes['nnouns_ans'] + numcodes['nnouns_quest']

    nd$avgwrd_total   <- ifelse(numcodes['nsents_quest'] == 0, 0, 
				nd$nwords_total/numcodes['nsents_quest'])

    # These branches need to be dealt with by hand
    nd$nsents_intro <- ifelse(nd$intropresent == 1, numcodes['nsents_intro'], 0)
    nd$nwords_intro <- ifelse(nd$intropresent == 1, numcodes['nwords_intro'], 0)
    nd$numsub_intro <- ifelse(nd$intropresent == 1, numcodes['numsub_intro'], 0)

    nd$avgwrd_intro   <- ifelse(nd$nsents_intro == 0, 0, 
				nd$nwords_intro/nd$nsents_intro)

    nd$avgabs_total <- ifelse(nd$nnouns_total==0,0, 
			      nd$nabst_total/nd$nnouns_total)

    nd$nsub_quest <- numcodes['nsub_quest']


    # Impute median as missing value for now
    nd$nnouns_intro <- ifelse(nd$intropresent == 1, 3, 0) # from original data
    nd$nabst_intro <- ifelse(nd$intropresent == 1, 2, 0) # from original data

    # avgabs_intro
    nd$avgabs_intro <- ifelse(nd$intropresent == 1, 0.67, 0) # from original data

    # repetition is always 0
    nd$repetition <- 0



    # Set missings from branches to 0, set other missings to median
    #########################################################################

    # form_basic
    # scale_basic
    # (usedshowcard) OK
    # (intropresent)

    for (needed.name in names(xlevels)) {
	default.value  <- 0
	if (!needed.name %in% names(nd)) { 
	    # Needed varaible not found (due to branching)
	    #   Set it to the default value
	    if(needed.name == 'labels') default.value <- 1 # Exception
	    if((needed.name == 'ncategories') && (sqp.codes['scale_basic'] == 1)) {
		default.value <- 2 # Exception: Yes/No qustions
	    }
	    if(needed.name == 'ncategories' && !(sqp.codes['scale_basic'] %in% c(0,1))) {
		default.value  <- sqp.codes['nfrequencies']
	    }
	    nd[[needed.name]] <- default.value
	}
    }


    nd$range.correspondence  <- 
	recode(paste(sqp.codes['scale_trange'], sqp.codes['scale_urange']),
	   "'0 0' = 1; '1 0' = 2; '1 1' = 3; '0 1' = 1; '0 NA' = 1; 'NA NA' = 1")


    #print('nd is now:')
    #print(nd)
    #save(nd, file='~/Documents/sqp_project/sqp/predict/nd_temp.Rdata')

    rf.xlevels <- raimforest.cat.rel$forest$xlevels
    # Convert to proper types, using the information in xlevels
    #########################################################################
    convert.factor <- function(xname) {
	if(!(xname %in% names(rf.xlevels))) {
	    print(sprintf("Characteristic '%s' is converted but not used in the model.",
		    xname))
	}
	else if(!(nd[[xname]] %in% rf.xlevels[[xname]])) {
	    print(rf.xlevels)	
	    print(sqp.codes)
	    stop(sprintf("Could not predict because the value '%s' of
		characteristic '%s' was not present in the training data set.
	    Allowed are %s.",
		nd[[xname]], xname, 
		paste(rf.xlevels[[xname]], collapse=",")))
	}
	factor(nd[[xname]], levels=rf.xlevels[[xname]],
	       ordered=xlevels[[xname]]$ordered)
    }

    for (xname in names(xlevels)) {
	# Convert categorical variables
	if(xname %in% names(nd)) {
	    if(xlevels[[xname]]$is.factor)
		nd[[xname]] <- convert.factor(xname)
	    # Convert rest to numerical
	    else
		nd[[xname]] <- as.numeric(nd[[xname]])
	}
    }

    return(nd)
}


#########################################################################
#  Predict using the rf.* objects and the newly created dataset		#
#########################################################################
prophesize <- function(choices, var.names) {

    nd  <- get.nd.from.choices(choices, var.names)
    #print('nd is now:')
    #print(nd)
    #save(nd, file='~/Documents/sqp_project/sqp/predict/nd_temp.Rdata')

    
    ## The following is DEBUG code, comment out or remove in production:
    #ndlevs  <- lapply(nd, function(x) c(is.factor(x), is.ordered(x)))
    #com  <- lapply(sort(names(xlevels)), function(n) cbind(xlevels[[n]][1:2], ndlevs[[n]]))
    #names(com) <- names(xlevels)
    #print(com)

    # Models are different for frequencies or other types of questions
    if(nd$scale_basic == 2) {
	predictor.rel  <- raimforest.fre.rel
	predictor.val  <- raimforest.fre.val
    }
    else {
	predictor.rel  <- raimforest.cat.rel
	predictor.val  <- raimforest.cat.val
    }

    # Predict reliability coefficient
    est.rel.list <- predict(predictor.rel, newdata=nd, predict.all=TRUE)
    # The point estimate for reliability coefficient
    est.rel  <- invlogit(est.rel.list$aggregate[[1]])
    est.rel2  <- est.rel^2
    # The vector of estimates for reliability coefficient
    pre.rel <-  invlogit(est.rel.list$individual)
    pre.rel2 <- pre.rel^2

    # Predict validity coefficient
    est.val.list <- predict(predictor.val, newdata=nd, predict.all=TRUE)
    # The point estimate for validity coefficient
    est.val  <- invlogit(est.val.list$aggregate[[1]])
    est.val2  <- est.val^2
    # The vector of estimates for validity coefficient
    pre.val <-  invlogit(est.val.list$individual)
    pre.val2 <- pre.val^2

    # Calculate quality coefficient
    est.qual  <- est.rel * est.val # Quality coefficient point est
    est.qual2  <- est.qual^2  # Quality point est

    pre.qual  <- pre.rel * pre.val # Quality coefficients vector
    pre.qual2  <- pre.qual^2 # Quality vector

    #  method effect "coefficient", i.e. "m"
    est.met <- sqrt(1 - est.val2)
    pre.met <- sqrt(1 - pre.val2)

    # common method variance / method-induced correlation, i.e. m^2 * r^2
    est.cmv <- (1 - est.val2) * est.rel2
    pre.cmv <- (1 - pre.val2) * pre.rel2

    return(list(
	est.rel  =est.rel  ,
	est.rel2  =est.rel2  ,
	pre.rel =pre.rel ,
	pre.rel2 =pre.rel2 ,

	est.val  =est.val  ,
	est.val2 =est.val2 ,
	pre.val =pre.val ,
	pre.val2 =pre.val2 ,

	est.qual  =est.qual  ,
	est.qual2 =est.qual2 ,
	pre.qual  =pre.qual  ,
	pre.qual2 =pre.qual2 ,

	est.met=est.met,
	pre.met=pre.met,

	est.cmv =est.cmv ,
	pre.cmv =pre.cmv 
    ))
}

# Conditional effects for different coefficients
# Calculated is the predicted value of the parameter in question for the
# different values of a particular variable (which values those are is 
# determined by the xlevels object).
# For example, conditional.rel('ncategories') will give, for each of the factor
#   levels of the ncategories variable, the predicted reliability coefficient,
#   _given the values chosen (codings) by the user_ for the other variables.

# Reliability coefficient and reliability
conditional.rel <- function(xname, choices, var.names) {
    newdata  <- get.nd.from.choices(choices, var.names)

    # Models are different for frequencies or other types of questions
    if(newdata$scale_basic == 2) {
	predictor.rel  <- raimforest.fre.rel
	xlevs  <- xlevels.fre
    }
    else {
	predictor.rel <- raimforest.cat.rel
	xlevs  <- xlevels.cat
    }
    invlogit(get.conditional.effects(predictor.rel, xname, newdata,
				     xlevs=xlevs))
}
conditional.rel2 <- function(xname, choices, var.names) {
    conditional.rel(xname,choices, var.names) ^ 2
}


# Reliability coefficient and valiability
conditional.val <- function(xname, choices, var.names) {
    newdata  <- get.nd.from.choices(choices, var.names)
    # Models are different for frequencies or other types of questions
    if(newdata$scale_basic == 2) {
	predictor.val  <- raimforest.fre.val
	xlevs  <- xlevels.fre
    }
    else {
	predictor.val <- raimforest.cat.val
	xlevs  <- xlevels.cat
    }
    invlogit(get.conditional.effects(predictor.val, xname, newdata, xlevs=xlevs))
}
conditional.val2 <- function(xname, choices, var.names) {
    conditional.val(xname,choices, var.names) ^ 2
}

# Quality coefficient and quality
conditional.qual <-function(xname, choices, var.names) {
    conditional.rel(xname,choices,var.names) *
    conditional.val(xname,choices,var.names)
}
conditional.qual2 <-function(xname, choices,var.names) {
    conditional.qual(xname,choices,var.names) ^ 2
}


# Common method variance
conditional.cmv <-function(xname, choices,var.names) {
    (1-conditional.val2(xname,choices,var.names)) *
    conditional.rel2(xname,choices,var.names)
}


# 
# Not used: not faster than just calling repeatedly from python
conditional.rel.all <- function(choices, var.names) {
    newdata  <- get.nd.from.choices(choices, var.names)

    # Models are different for frequencies or other types of questions
    if(newdata$scale_basic == 2) {
	predictor.rel  <- raimforest.fre.rel
	xlevs  <- xlevels.fre
    }
    else {
	predictor.rel <- rf.rel
	xlevs  <- xlevels
    }
    get.all.conditional(predictor.rel, nd=newdata, xlevs=xlevs)
}
