#' Calculate an L score
#' 
#' @param data (data.frame) Just the data to L score in the wide format
#' @param lambda (numeric) a vector specifying the lambda weights to produce the L scores in the same order as the columns in data
#' @export
LScore <- function(data,lambda) {
  if (is.vector(data)) {data <- t(as.matrix(data))}
  rowSums(data*matrix(lambda,nrow=nrow(data),ncol=ncol(data),byrow=TRUE))
}
NULL

#' Calculate an r score
#' 
#' Calculate the correlations (row by row) between sets of scores and a set of lambda weights
#' @param data (data.frame) Just the data to L score in the wide format
#' @param lambda (numeric) a vector specifying the lambda weights to produce the L scores in the same order as the columns in data
#' @export
rScore <- function(data,lambda) {
  if (is.vector(data)) {data <- t(as.matrix(data))}
  apply(data,1,FUN=function(x) {cor(x,lambda)})
}
NULL

#' Convert r to a t
#'
#'
#' @param r The correlation coefficient value
#' @param df the df for the correlation coefficient.  This is typically the number of pairs minus 2.
#' @export
r2t <- function(r,df) {
  if ("package:psych" %in% search()) {
    message("This is the russmisc version of r2t.  The version in package:psych uses different arguments!  To use that version, call psych::r2t.")
  }
  return(unname( (r/sqrt(1-r^2))*sqrt(df)))
}
NULL

#' Convert t to a r
#'
#' @param t The t statistic
#' @param df The df for the t statistic
#' @export
t2r <- function(t,df) {return(unname(sqrt(t^2/(t^2+df))))}
NULL

#' Calcualte the counter null of a correlation coefficient
#'
#' @param r The r statistic
#' @return (numeric) The (unsigned) value of the r statistic that is as likely as r=0.
#' @references Rosenthal, R., Rosnow, R.  L., & Rubin, D. B. (2000). \emph{Contrasts and Effect Sizes in Behavioral Research}. New York: Cambridge University Press. p. 127.
#' @examples
#' rCounternull(.833)
#' @export
rCounternull <- function(r) {
  return(unname(sqrt((4*r^2)/(1+3*r^2))))
}

#' Calculate the median absolute deviation (MAD)
#' @param x A set of numeric scores
#' @export
#' 
MAD <- function(x) {return(median(abs(x-median(x))))}
NULL

#' Calculate the Iglewicz and Hoaglin (1993) Z modified
#' @param x A set of numeric scores
#' @export
Zmodified <- function(x) {.6745*(x-median(x)) / MAD(x)}
NULL

#' Calculate the population standard deviation of x
#' Usage
#' popsd(x)
#' @param x       A numeric vector
#' @param nomiss  A numeric vector specifying the proporiton of valid cases in x
#'         (i.e. data that must not be NA) for the sd to be returned
#' @note R's built-in sd function divides the sum of the squared deviations
#' from the mean by the number of observations minus 1 (N-1). However,
#' there are times where one would prefer to use the formula with N
#' in the denominator (e.g. if one is working with the entire population
#' of scores). This function does just that.
#' @author Borrowed from Ryne Sherman on 2012/09/06
#' @export
popsd <- function(x, nomiss=.8) {
  miss <- sum(is.na(x))
  comp <- (length(x) - miss) / length(x)
  out <- ifelse(comp >= nomiss, sqrt((var(x, na.rm=T)*(length(x)-miss-1)) / (length(x)-miss)), NA)
  return(out)
}
NULL

#' Population scale/Z scores
#' 
#' The built-in R function scale uses the sample standard deviation when
#' its scale option is set to TRUE. This function uses the population
#' standard deviation instead.
#' @param x       A numeric vector, matrix, or data.frame
#' @param center  A logical indicating whether the scores in the columns in x should
#'         have their column means subtracted
#' @param scale   A logical indicating whether the scores in the columns in x should
#'         be divided by their column standard deviations
#' @author Borrowed from Ryne Sherman on 2012/09/06, his function is named scale2
#' @export

popscale <- function(x, center = TRUE, scale = TRUE) {
  x <- data.frame(x)
  miss <- colSums(is.na(x))
  valid <- nrow(x) - miss
  x.means <- colMeans(x, na.rm=T)
  x.sds <- sqrt((apply(x, 2, sd, na.rm=T)^2) * (valid-1) / (valid))
  
  if(center==T & scale==T) {
    out <- t((t(x) - x.means) / x.sds)
  }
  if(center==T & scale==F) {
    out <- t(t(x) - x.means)
  }
  if(center==F & scale==T) {
    out <- t(t(x) / x.sds)
    warning("Standardizing without centering is unconventional.")
  }
  if(center==F & scale==F) {
    out <- x
    warning("You realize you didn't do anything to your data right?")
  }
  return(out)
}
NULL

#' Calculate the winsorized variance of x
#' Usage
#' winvar(x, tr=.2, na.rm=TRUE)
#' This function is borrowed directly from Rand Wilcox's function and is
#' kept with this set of functions for capability with a large number
#' of functions.
#' @param x       A numeric vector of which to get the winsorized variance
#' @param tr      The proportion of scores to winsorize
#' @param na.rm   A logical indicating whether missing values should be removed prior
#' to calculation.
#' @author Rand Wilcox, borrowed from Ryne Sherman on 2012/09/06
#' @export

winvar <- function(x, tr=.2, na.rm=TRUE) {
  if(na.rm) x <- x[!is.na(x)]
  y <- sort(x)
  n <- length(x)
  ibot <- floor(tr*n)+1
  itop <- length(x)-ibot+1
  xbot <- y[ibot]
  xtop <- y[itop]
  y <- ifelse(y <= xbot, xbot, y)
  y <- ifelse(y >= xtop, xtop, y)
  winvar <- var(y)
  return(winvar)
}
NULL

#' Trim values
# Returns the trimmed values of x
#' Usage
#' trimval(x, tr=.2, na.rm=TRUE)
#' @param x       A numeric vector to be trimmed
#' @param tr      The proportion of values in x to be trimmed
#' @param na.rm   A logical indicating whether missing values should be removed
#'         prior to trimming

#' @author borrowed from Ryne Sherman on 2012/09/06.  Modified to return NA for removed values by Russell S. Pierce
#' @export
trimval <- function(x, tr=.2, na.rm=TRUE) {
  if(na.rm==T) {x <- x[!is.na(x)]}
  if(tr < 0 | tr >= .5) {stop("Trimming must be between 0% (.00) and 50% (.50)")}
  if(tr==0) {return(x)}
  n <- length(x) - sum(is.na(x))
  ibot <- floor(tr*n)
  itop <- n-ibot+1
  ranks <- rank(x, na.last="keep", ties.method="first")
  lows <- which(ranks <= ibot)
  highs <- which(ranks >= itop)
  x[c(lows,highs)] <- NA
  return(x)
}
NULL

#' Degrees of Freedom and Effects
#' 
#' Calculate the effects and degrees of freedom 
#' for a set of \emph{fully crossed} effects.
#' 
#' @param vars (character vector) Independent Variables
#' @param levels (integer vector) Levels for each of those indpendent variables respectively
#' @return data.frame with two columns, effect (the name of the effect) and df (the df associated with that effect)
#' @references Rosenthal & Rosnow. (2008). \emph{Essentials of Behavioral Research}. Boston: Mc Graw Hill.
#' @export
#' @examples
#' #Table 18.24
#' #Calculate the within effects
#' within <- dfEffects(c("Expectancy","Pupil Sex"),c(2,2))
#' #Specify the between effects (these can't be calculated using dfEffects because they aren't fully crossed)
#' btwn <- data.frame(effect=c("Sex of Teacher","Teachers (within sex)"),df=c(1,8))
#' #Now we rbind these two and use grid.expand to explore all possible combinations
#' 
#' table.18.24 <- rbind(btwn,within,data.frame(effect=apply(expand.grid(btwn$effect,within$effect),1,paste,collapse=":"),df=apply(expand.grid(btwn$df,within$df),1,prod)))
#' # Table 18.30 uses the same between subjects effects but has a more complex within structure that we create using this function
#' 
#' within <- dfEffects(c("Expectancy","Pupil Sex","Diagnosis"),c(2,2,2))
#' rbind(btwn,within,data.frame(effect=apply(expand.grid(btwn$effect,within$effect),1,paste,collapse=":"),df=apply(expand.grid(btwn$df,within$df),1,prod)))
#' # Note: Obviously the order of effects is different here than in R&R.
dfEffects <- function(vars,levels) {
  effect.names <- NULL
  effect.df <- NULL
  for (i in 1:length(vars)) {
    effect.names <- c(effect.names,apply(combn(vars,i),2,paste,collapse=":"))
    effect.df <- c(effect.df,apply(combn(levels-1,i),2,prod))
  }
  return(data.frame(effect=effect.names,df=effect.df))
}
NULL

#' Extract the main effects and interactions from an aov object
#' 
#' @family AggregatingErrorTerms
#' @param aov.obj An object of class aov representing an ANOVA calculation
#' @param strata.to.extract (vector) A vector of the names of the error strata from which to extract effects. As elsewhere, note that these are the names embedded in the aov summary.  It may be best to extract them from a use of the \code{\link{errorTerms}} function.
#' @return data.frame 
#' @examples
#' data(EBR.Table.18.25)
#' aov.EBR.Table.18.25 <- ezANOVA.EBR.Table.18.25$aov
#' effectTerms(aov.EBR.Table.18.25)
#' @export
effectTerms <- function(aov.obj,strata.to.extract=names(summary(aov.obj))) {
  errorTerms.names <- strata.to.extract
  extractTermNames <- function(x) {
    res <- rownames(summary(aov.obj)[[x]][[1]])
    res <- res[-length(res)] #the last term is Residuals
    return(res)
  }
  extractrow <- function(x,extract) {
    res <- summary(aov.obj)[[x]][[1]]
    return(res[rownames(res) %in% extract,])
  }
  extractdf <- function(x) {summary(aov.obj)[[x]][[1]]["Residuals","Df"]}
  term.names <- unlist(sapply(errorTerms.names,extractTermNames))
  res <- NULL
  for (i in errorTerms.names) {
    res <- rbind(res,extractrow(i,term.names))
  }	
  return(res)
}
NULL

#' Extract the error terms from an aov object
#' 
#' @family AggregatingErrorTerms
#' @param aov.obj An object of class aov representing an ANOVA calculation
#' @return data.frame
#' @examples
#' data(EBR.Table.18.25)
#' aov.EBR.Table.18.25 <- ezANOVA.EBR.Table.18.25$aov
#' errorTerms(aov.EBR.Table.18.25)
#' @export
errorTerms <- function(aov.obj) {
  errorTerms.names <- names(summary(aov.obj))
  extractMS <- function(x) {summary(aov.obj)[[x]][[1]]["Residuals","Mean Sq"]}
  extractdf <- function(x) {summary(aov.obj)[[x]][[1]]["Residuals","Df"]}
  res.MS <- sapply(errorTerms.names,extractMS)
  res.df <- sapply(errorTerms.names,extractdf)
  res.SS <- res.MS*res.df
  res <- data.frame(Df=res.df,"Sum Sq"=res.SS,"Mean Sq"=res.MS,"F value"=NA,"Pr(>F)"=NA,check.names=FALSE)
  return(res)
}
NULL

#' Calculate the ratio of error terms from an aov object
#' 
#' When the ratio of error terms is low, it may be reasonable to aggregate them.  This function lets us visual inspect the ratios of the error terms from an aov object as a precursor to deciding whether to perform error term aggregation.
#' 
#' @family AggregatingErrorTerms
#' @param aov.obj An object of class aov representing an ANOVA calculation
#' @return data.frame
#' @references Rosenthal & Rosnow. (2008). \emph{Essentials of Behavioral Research}. Boston: Mc Graw Hill.
#' @examples
#' data(EBR.Table.18.25)
#' aov.EBR.Table.18.25 <- ezANOVA.EBR.Table.18.25$aov
#' errorTermRatios(aov.EBR.Table.18.25)
#' @export
errorTermRatios <- function(aov.obj) {
  et <- errorTerms(aov.obj)
  et.names <- rownames(et)
  rownames(et) <- et.names
  et.n <- length(et.names)
  res <- matrix(NA,nrow=et.n,et.n)
  rownames(res) <- et.names
  colnames(res) <- et.names
  for (i in et.names) {
    for (j in et.names) {
      res[i,j] <- as.numeric(et[i,"Mean Sq"]/et[j,"Mean Sq"])
    }
  }
  return(res)
}
NULL

#' Calculate the aggregated error term from an aov object
#' 
#' @family AggregatingErrorTerms
#' @param aov.obj An object of class aov representing an ANOVA calculation
#' @param terms.to.combine (vector) A character vector of the names of the error terms to combine.  Note that these are the names embedded in the aov summary.  It may be best to extract them from a use of the \code{\link{errorTerms}} function.
#' @return data.frame
#' @references Rosenthal & Rosnow. (2008). \emph{Essentials of Behavioral Research}. Boston: Mc Graw Hill.
#' @examples
#' data(EBR.Table.18.25)
#' aov.EBR.Table.18.25 <- ezANOVA.EBR.Table.18.25$aov
#' et <- errorTerms(aov.EBR.Table.18.25)
#' errorTermRatios(aov.EBR.Table.18.25)
#' et.names <- rownames(et)
#' terms.to.combine <- et.names[c(2,3,4)]
#' aggregateErrorTerms(aov.EBR.Table.18.25,terms.to.combine)
#' @export
aggregateErrorTerms <- function(aov.obj,terms.to.combine) {
  et <- errorTerms(aov.obj)
  et.names <- rownames(et)
  res <- colSums(et[rownames(et) %in% terms.to.combine,c("Sum Sq","Df")])
  res <- data.frame("Df"=res["Df"],"Sum Sq"=res["Sum Sq"],"Mean Sq"=res["Sum Sq"]/res["Df"],"F value"=NA,"Pr(>F)"=NA,check.names=FALSE)
  rownames(res) <- "Aggregated Error Term"
  return(res)
}
NULL

#' Calculate new F and p-values using an aggregated error term from an aov object
#' 
#' This function calls others of the same family to aggregate the error terms selected in \code{terms.to.combine} extract the related effects, and calculate new F ratios and p-values.
#' 
#' @family AggregatingErrorTerms
#' @param aov.obj An object of class aov representing an ANOVA calculation
#' @param terms.to.combine (vector) A character vector of the names of the error terms to combine.  Note that these are the names embedded in the aov summary.  It may be best to extract them from a use of the \code{\link{errorTerms}} function.
#' @return data.frame
#' @references Rosenthal & Rosnow. (2008). \emph{Essentials of Behavioral Research}. Boston: Mc Graw Hill.
#' @examples
#' data(EBR.Table.18.25)
#' aov.EBR.Table.18.25 <- ezANOVA.EBR.Table.18.25$aov
#' et <- errorTerms(aov.EBR.Table.18.25)
#' errorTermRatios(aov.EBR.Table.18.25)
#' et.names <- rownames(et)
#' terms.to.combine <- et.names[c(2,3,4)]
#' useAggregateErrorTerms(aov.EBR.Table.18.25,terms.to.combine)
#' @export
useAggregateErrorTerms <- function(aov.obj,terms.to.combine) {
  new.error <- aggregateErrorTerms(aov.obj,terms.to.combine)
  old.effect <- effectTerms(aov.obj,strata.to.extract=terms.to.combine)
  new.F <-  old.effect[["Mean Sq"]]/new.error[["Mean Sq"]]
  new.p <- df(new.F,old.effect[["Df"]],new.error[["Df"]])
  res <- rbind(old.effect,new.error)
  res[["New F value"]] <- c(new.F,NA)
  res[["New Pr(>F)"]] <- c(new.p,NA)
  return(res)
}
NULL

#' minFprime
#' 
#' Calculates min F' from Clark (1973)
#' 
#' @param F1 F value from the first test
#' @param F2 F value from the second test
#' @param numdf df of the numerator (the variable of interest)
#' @param F1denomdf The df denominator of the first test
#' @param F2denomdf The df denominator of the second test
#' @param round Whether to round the p value to three places
minFprime <- function(F1, F2, numdf, F1denomdf, F2denomdf,round=FALSE)
{
  Fprimemin = F1*F2/(F1+F2)
  i <- numdf
  j <- (F1+F2)^2 / ((F1^2/F2denomdf)+(F2^2/F1denomdf))
  p <- pf(Fprimemin,i,j,lower.tail=FALSE)
  if (round) {p <- round(p,3)}
  cat("min F'(",i,", ",j,") = ",Fprimemin," p = ",droplead0(p),"\n",sep="")
  return(c(Fprimemin=Fprimemin,numdf=i,denomdf=j,p=p))
}
NULL