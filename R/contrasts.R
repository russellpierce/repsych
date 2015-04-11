#' Conduct a t-contrast
#'
#' Returns a t-value based on the given contrast weights
#' Same as yuen.contrast (see Ryne's code, not implemented in russmisc) only no trimming is allowed.
#' Created largely for sanity checks, yuen.contrast should generally be used.
#'
#' @section Details:
#' This function computes a t-value for a DV given a set of
#' contrast weights following Rosenthal, Rosnow, & Rubin (2000). If EQVAR=FALSE
#' then degrees of freedom are calculated using Welch's method. The wgt option
#' allows one to specify contrast weights to test hypotheses with more than
#' 2 levels of an IV. By default it tests the hypothesis that the sum of the wgt
#' vector times the DV means for each IV is greater than 0.
#' To get a two-tailed p-value or one predicting a negative contrast,
#' change the \code{alternative} parameter.
#' @references 
#' Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000). \emph{Contrasts and effect sizes in behavioral research: A correlational approach}. New York: Cambridge University Press.\cr\cr
#' Rosenthal, R., Rosnow, R.L., & Rubin, D.B. (2000). Contrasts and correlations in effect-size estimation. \emph{Psychological science}. \emph{11}(6), 446-453.
#' @author Ryne Sherman <rsherm13@@fau.edu>; modifications by Russell S. Pierce <rpier001@@ucr.edu>
#' 
#' @export
#' @rdname tContrast
#' @exportClass tContrast
#' @param DV    A numeric vector of the same length as IV containing the measured values.
#' @param IV    A factor of the same length as DV containing the independent variable codes.
#' @param wgt   A numeric vector containing the contrast weights corresponding to each successive level of the IV.  If this value is left NULL, the function will try to use the \code{\link{contrasts}} set on the IV to conduct the test(s)
#' @param alpha A numeric element > .00 and < 1.00 specifying the Type I error rate.
#' @param EQVAR A logical indicating whether equal variances amongst the groups should be assumed.
#' @param alternative A character vector specifying the alternative hypothesis. Must be one of "unequal", "greater", or "less".

#' @examples
#' # To test a single contrast...
#' tContrast(warpbreaks$breaks,warpbreaks$tension,wgt=c(-1,0,1))
#' # To test multiple contrasts...
#' # We make a matrix with our desired contrasts
#' my.contr <- cbind(
#'  I = c(-1,0,1),
#'  II = c(1,0,-1),
#'  III = c(1,-2,1)
#' )
#' # Now we set the contrasts along side the variable
#' # Now we have to change how.many to match the number
#' # of contrasts we have 
#' contrasts(warpbreaks[,"tension"],how.many=3) <- my.contr # set the contrasts for the 'group' factor
#' tContrast(warpbreaks$breaks,warpbreaks$tension)
#' 
tContrast <- function(DV, IV, wgt=NULL, alpha=.05, EQVAR=FALSE, alternative="greater") {  
  if (is.null(wgt)) {
    wgts <- attr(IV,"contr")
    if (is.null(wgts)) stop("In tContrast: No contrasts in IV and no weights provided")
    res <- vector("list",ncol(wgts))
    for (i in 1:ncol(wgts)) {
      #message("Testing contrast ",i," ",colnames(wgts)[i])
      res[[i]] <- tContrast(DV,IV,wgts[,i],alpha=alpha, EQVAR=EQVAR, alternative=alternative)
    }
    names(res) <- colnames(wgts)
    return(res)
  }
  if(length(unique(IV))!=length(wgt)) {stop("Each level of the IV must have a specified contrast weight")}
  z <- data.frame(DV, IV)
  Z <- z[complete.cases(z),]
  GrpNs <- tapply(Z[,1], Z[,2], length)
  GrpMs <- tapply(Z[,1], Z[,2], mean)
  GrpVars <- tapply(Z[,1], Z[,2], var)
  oDF <- sum(GrpNs-1)
  if(EQVAR==F) {
    GrpQs <- GrpVars / GrpNs
    DF <- sum(GrpQs)^2 / sum(GrpQs^2 / (GrpNs-1))
    Num <- sum(GrpMs*wgt)
    Den <- sqrt(sum((GrpVars*wgt^2)/GrpNs))
    teststat <- Num / Den
  }
  if(EQVAR==T) {
    DF <- sum(GrpNs-1)
    PoolVar <- sum((GrpNs - 1)*GrpVars) / sum(GrpNs-1)
    Num <- sum(GrpMs*wgt)
    Den <- sqrt(sum(wgt^2/GrpNs)*PoolVar)
    teststat <- Num / Den
  }
  crit <- ifelse(alternative=="unequal", qt(1-alpha/2,DF), qt(1-alpha,DF))
  if(alternative=="unequal") {
    p <- 2*(1-pt(abs(teststat),DF))
  }
  if(alternative=="greater") {
    p <- ifelse(teststat > 0, 1-pt(abs(teststat),DF), ifelse(teststat < 0, pt(abs(teststat),DF), .5))
  }
  if(alternative=="less") {
    p <- ifelse(teststat > 0, pt(abs(teststat),DF), ifelse(teststat < 0, 1-pt(abs(teststat),DF), .5))
  }
  r.contrast <- sqrt(teststat^2 / (teststat^2 + sum(GrpNs-1)))
  r.effectsize <- cor(DV,wgt[as.numeric(IV)])
  r.alerting <- cor(GrpMs,wgt)
  out <- list(GrpNs, GrpMs, wgt, teststat, DF, oDF, crit, p, r.contrast,GrpVars,r.alerting,r.effectsize,EQVAR)
  names(out) <- c("Group Ns", "Group Means", "Contrast Weights", "T-Value", "df", "unadjusted df", "Critical Value", "p-value", "r contrast","Group Vars","r alerting","r effect size","equal variances assumed")
  attr(out, "class") <- "tContrast"
  return(out)
}
NULL
#' @rdname tContrast
#' @export
#' @method print tContrast
#' @param x an object used to select the method.
#' @param ...  further arguments passed to or from other methods.



print.tContrast <- function(x,...) {
  print(data.frame(GroupNs=x[["Group Ns"]],
                   GroupMs=x[["Group Means"]],
                   GroupVars=x[["Group Vars"]],
                   ContrastWeights=x[["Contrast Weights"]]
  )
  )
  cat("\nt critical:  ",x[["Critical Value"]],"\n",sep="")
  cat("t(",x[["unadjusted df"]],") = ",x[["T-Value"]],", p=",x[["p-value"]],"\n",sep="")
  if (x[["equal variances assumed"]]) {
    cat("Equal variances assumed.\n")
  } else {
    cat("Equal variances not assumed.  Adjusted df",round(x[["df"]],2),"\n")
  }
  cat("r_contrast: ",x[["r contrast"]],"\n",sep="")  
  cat("r_alerting: ",x[["r alerting"]],"\n",sep="")
  cat("r_effectSize: ",x[["r effect size"]],"\n",sep="")
}
NULL


#Rosnow, R.L., & Rosenthal, R. (1996). Computing contrasts, effect sizes, and counternulls
#on other people’s published data: General procedures for research consumers. Psychological Methods, 1, 331–340.
