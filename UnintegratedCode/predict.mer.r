# Banished here.
#' Refresh the russmisc package
#'
#' This is just a convenience to unload an reload the russmisc package, useful for rapid development
#'
#' @export

repsychRefresh <- function() {
  detach("package:russmisc",unload=TRUE)
  library(russmisc)
}
NULL

#require(russmisc) #
#' Not in
#' 
#' Logical operator opposite of \%in\%
#' @rdname notin
#' @name notin
#' @param x vector or NULL: the values to be matched.
#' @param y vector or NULL: the values to be matched against.
#' @return logical vector of length x
#' @export notin %!in%
#' @aliases notin %!in%
#' @examples
#' ## Example not run
#' #1:5 %in% c(2,4)
#' #1:5 %!in% c(2,4)
notin <- function(x,y) {return(x %!in% y)}
"%!in%" <- function(x, y) match(x, y, nomatch = 0) == 0
NULL

require(ggplot2)
require(lme4)

#ToDo:  allow selective mean substitution
predict.mer <- function(object,newdata,...) {
  message("predict.mer is adhoc code for fixed effects only.  Only tested for models produced using lme4::lmer")
  if (length(as.list(substitute(list(...)))[-1L])>0) {warning("In predict.mer: Options other than object and newdata are ignored.")}
  coefs <- fixef(object)
  mains <- names(coefs)[!grepl(":",names(coefs))]
  if ("(Intercept)" %in% mains) {
    newdata <- cbind("(Intercept)"=1,newdata)
  }
  if (!all(mains %in% names(newdata))) {
    #if (ctSubstitute == FALSE) {
      message("In predict.mer: effects for ",paste(mains[!(mains %in% names(newdata))],collapse=", ")," are all missing from newdata, assuming values are 0")
      newdata[,mains[!(mains %in% names(newdata))]] <- 0
      #     } else {
      #       ctReplace <- names(object@frame)[(lapply(object@frame,is.factor)==FALSE) & lapply(object@frame,mode)=="numeric"]
      #       factReplace <- names(object@frame)[(lapply(object@frame,is.factor)==TRUE) & lapply(object@frame,mode)=="numeric"]
      #       otherReplace <- names(object@frame)[lapply(object@frame,mode)!="numeric"]
      #       factReplace[!factReplace %in% names(newdata)]
      #       message("In predict.mer: effects for ",paste(mains[!(mains %in% names(newdata))],collapse=", ")," are all missing from newdata")      
      # 
      #       message("In predict.mer: effects for "
      #       
      #       
      #       ls.str(object@frame[,!(mains %in% names(newdata))])[[1]]
      #       newdata[,mains[!(mains %in% names(newdata))]] <- colMeans(object@frame[,!(mains %in% names(newdata))])
      #     }
  }
  #synthesize full newdata frame
  for (i in names(coefs)[!(names(coefs) %in% mains)]) {
    cols <- strsplit(i,":")[[1]]
    newdata[,i] <- apply(newdata[,cols],1,prod)
  }
  if (any(names(newdata) %!in% names(coefs))) {
    message("In predict.mer: values calculated or provided for ",paste(names(newdata)[names(newdata) %!in% names(coefs)],collapse=" ")," but this (these) value(s) is(are) not in the fixef of the provided model")  
  }
  coef.mat <- matrix(coefs,byrow=TRUE,nrow=nrow(newdata),ncol=length(coefs))
  newdata <- newdata[,names(coefs)] #restructure in correct order
  res <- rowSums(newdata*coef.mat)
  attr(res,"names") <- rownames(newdata)
  return(res)
}

eg.data <- data.frame(SubjID=rep(1:5,each=5),DV=rnorm(200),IV=1:5,cov=rnorm(200))
eg.data$DV <- with(eg.data,IV+cov+cov*(IV-3)+rnorm(200))
lmer.model <- lmer(DV~IV*cov+(1|SubjID),data=eg.data)
newdata.eg <- expand.grid(IV=1:5,cov=-1:1)
predict(lmer.model,newdata.eg)
newdata.eg$Prediction <- predict(lmer.model,newdata.eg)
qplot(IV,Prediction,data=newdata.eg,col=factor(cov),geom="line")


## languageR example ##
#############################################################
# a model with an interaction involving numeric predictors
#############################################################
require(languageR)
require(rms)
require(lme4)
danish$Sex=relevel(danish$Sex, "F")
danish.lmer = lmer(LogRT ~ PC1 + PC2 + PrevError + Rank +
                     ResidSemRating + ResidFamSize + LogWordFreq*LogAffixFreq*Sex +  
                     poly(LogCUP, 2, raw=TRUE) + LogUP + LogCUPtoEnd + 
                     (1|Subject) + (1|Word) + (1|Affix), data = danish)
danish.lmerA = lmer(LogRT ~ PC1 + PC2 + PrevError + Rank +
                      ResidSemRating + ResidFamSize + LogWordFreq*LogAffixFreq*Sex +  
                      poly(LogCUP, 2, raw=TRUE) + LogUP + LogCUPtoEnd + 
                      (1|Subject) + (1|Word) + (1|Affix), data = danish,
                    subset=abs(scale(resid(danish.lmer)))<2.5)
#My solutions can't plot the HPD intervals as written and this takes a long time
#mcmc = pvals.fnc(danish.lmerA, nsim=10000, withMCMC=TRUE)
#mcmc$fixed[,1:5]
# # plot for reference level of Sex
# plotLMER.fnc(danish.lmerA, pred = "LogAffixFreq", 
#              intr=list("LogWordFreq", round(quantile(danish$LogWordFreq),3), "beg",
#                        list(c("red", "green", "blue", "yellow", "purple"), rep(1,5))), ylimit=c(6.5,7.0),
#              mcmcMat=mcmc$mcmc, xlabel = "log affix frequency", ylabel = "log RT auditory lexical decision")

# this model has a significant three-way interaction
# for visualization, we can either relevel Sex and refit,
# or make use of the control option. First releveling:

danish$Sex=relevel(danish$Sex, "F")
danish.lmerF = lmer(LogRT ~ PC1 + PC2 + PrevError + Rank +
                      ResidSemRating + ResidFamSize + LogWordFreq*LogAffixFreq*Sex +  
                      poly(LogCUP, 2, raw=TRUE) + LogUP + LogCUPtoEnd + 
                      (1|Subject) + (1|Word) + (1|Affix), data = danish)
danish$Sex=relevel(danish$Sex, "M")
danish.lmerM = lmer(LogRT ~ PC1 + PC2 + PrevError + Rank +
                      ResidSemRating + ResidFamSize + LogWordFreq*LogAffixFreq*Sex +  
                      poly(LogCUP, 2, raw=TRUE) + LogUP + LogCUPtoEnd + 
                      (1|Subject) + (1|Word) + (1|Affix), data = danish)

# Next preparing for using the control option:
#
# names(fixef(danish.lmer))[10]  # SexM
# unique(danish.lmer@X[,10])     # 1 0

par(mfrow=c(2,2))

plotLMER.fnc(danish.lmerF, pred="LogWordFreq", ylimit=c(6.5,7.0),
             intr=list("LogAffixFreq", quantile(danish$LogAffixFreq), "end"),
             control=list("SexM", 0))
mtext("females", line=1.5, cex=0.9)

plotLMER.fnc(danish.lmerF, pred="LogWordFreq", ylimit=c(6.5,7.0),
             intr=list("LogAffixFreq", quantile(danish$LogAffixFreq), "end"),
             control=list("SexM", 1))
mtext("males", line=1.5, cex=0.9)

plotLMER.fnc(danish.lmerF, pred="LogWordFreq", ylimit=c(6.5,7.0), 
             intr=list("LogAffixFreq", quantile(danish$LogAffixFreq), "end"))
mtext("females", line=1.5, cex=0.9)

plotLMER.fnc(danish.lmerM, pred="LogWordFreq", ylimit=c(6.5, 7.0),
             intr=list("LogAffixFreq", quantile(danish$LogAffixFreq), "end"))
mtext("males", line=1.5, cex=0.9)

par(mfrow=c(1,1))

# Or my way, notice we don't have to caluclate lmer twice, but we do have to manually specify the 
#control levels for all non 0 variables not being manipulated
colMeans(danish[,4:5],na.rm=TRUE)
mean(danish[,8],na.rm=TRUE)
colMeans(danish[,10:16],na.rm=TRUE)
danish.lmer = lmer(LogRT ~ PC1 + PC2 + PrevError + Rank +
                     ResidSemRating + ResidFamSize + LogWordFreq*LogAffixFreq*Sex +  
                     poly(LogCUP, 2, raw=TRUE) + LogUP + LogCUPtoEnd + 
                     (1|Subject) + (1|Word) + (1|Affix), data = danish)
newdata.eg <- with(danish,expand.grid(LogWordFreq=0:10,LogAffixFreq=quantile(danish$LogAffixFreq),
                                      SexF=0:1,
                                      PC1=mean(PC1),PC2=mean(PC2),PrevErrorERROR=0,Rank=mean(Rank),
                                      ResidSemRating=mean(ResidSemRating),ResidFamSize=mean(ResidFamSize),
                                      'poly(LogCUP, 2, raw = TRUE)1'=mean(poly(LogCUP, 2, raw = TRUE)[,1]),
                                      'poly(LogCUP, 2, raw = TRUE)2'=mean(poly(LogCUP, 2, raw = TRUE)[,2]),
                                      LogUP=mean(LogUP),
                                      LogCUPtoEnd=mean(LogCUPtoEnd)))
#predict(danish.lmer,newdata.eg,meanSubstitute=TRUE)
newdata.eg$LogRT <- predict(danish.lmer,newdata.eg)
newdata.eg$SexF <- factor(newdata.eg$Sex,levels=c(0,1),labels=c("Male","Female"))
qplot(LogWordFreq,LogRT,data=newdata.eg,col=factor(LogAffixFreq),geom="line") + facet_grid(SexF ~ .)
