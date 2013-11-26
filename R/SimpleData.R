#' Vectorized lookup
#' 
#' This function does a vectorized lookup on a data.frame.  That is, each individual row is matched with a 
#' particular column.
#' 
#' @param df A data.frame
#' @param rowValues A vector of the row values to lookup, matched element by element with colValues
#' @param colValues A vector of the column values to lookup, matched element by element with rowValues
#' @return A vector from df[x,y] where x and y are pulled in matched pairs
#' @export
#' @examples
#' df.eg <- as.data.frame(matrix(1:100,10,10))
#' vlookup(df.eg,c(1:6,6:1),rep(c("V1","V2"),each=6))
vlookup <- function(df,rowValues,colValues) {
  if (!is.data.frame(df)) {stop("In vlookup: The value provided for the argument df was not a data.frame!")}
  grab <- function(df,x,y) {df[rownames(df)==x,colnames(df)==y]}
  mapply(grab,x=rowValues,y=colValues,MoreArgs=list(df=df))
}
NULL

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

#' Convert Excel Column ID
#' 
#' This function converts R numeric column IDs to Excel letter IDs and vice versa.
#' @param ID Either a character or numeric value specifying the ID to convert.
#' @export
#' @return character or numeric; the reverse of what was specified by ID
#' @note This function is not properly vectorized, it can only handle a single ID

convertID <- function(ID) {
  ID <- as.character(ID)
  getLetterPos <- function(letters) {sapply(letters,getLetterPos.worker)}
  getLetterPos.worker <- function(letter) {seq(1:26)[letter==LETTERS]}
  getNumberLetter <- function(number) {
    if (suppressWarnings(!is.na(as.numeric(number)))) 
    {
      number <- as.numeric(number)
      thirdLetter <- number %/% 676
      secondLetter <- (number - thirdLetter * 676) %/% 26
      firstLetter <- number - thirdLetter * 676 - secondLetter * 26
      return(paste(LETTERS[c(thirdLetter,secondLetter,firstLetter)],collapse=""))
    } else {
      return(NA)
    }
  }
  numericIDProvided <- suppressWarnings(!is.na(as.numeric(ID)))
  splitID <- strsplit(ID,"")
  elementLength <- lapply(splitID,length)
  decodedAlpha <- lapply(splitID,getLetterPos)
  decodedAlpha <- ifelse(!numericIDProvided,decodedAlpha,NA)
  pad <- function(x) {
    length.x <- length(x)
    x <- x-1
    if (length.x < 3) {x <- c(rep(0,3-length.x),x)}
    x <- sum(x*c(26^2,26,1))+sum(c(676,26,0)[names(x)!=""])+1
    return(x)
  }
  numeric.vals <- lapply(decodedAlpha,pad)
  char.vals <- sapply(ID,getNumberLetter)
  return(ifelse(numericIDProvided,char.vals,numeric.vals))
}
NULL
#' Debackslash
#' 
#' Take the contents of the clipboard and replace backslashes with forward slashes
#' @export
debackslash <- function() {
  clip <- file("clipboard")
  x <- suppressWarnings(readLines(con=clip))
  if (sum(grepl("\\",x,fixed=TRUE)) < 1) {
    message("In repsych:debackslash:  No backslashes detected in clipboard")
  }
  x <- gsub("\\","/",x,fixed=TRUE)
  if (length(x)==1) {sep <- ""} else {sep <- "\n"}
  writeLines(text=x,con=clip,sep=sep)
  close(clip)
}
NULL

#' Calculate the mean squared error
#' 
#' @param x Numeric data series
#' @param y Numeric data series
#' @param ... further arguments for mean
#' @return numeric
#' @export
MSE <- function(x,y,...) {
  res <- mean((x-y)^2,...)
  return(res)
  }
NULL

#' Calculate the sum of squared errors
#' 
#' @param x Numeric data series
#' @param y Numeric data series
#' @param ... further arguments for mean
#' @return numeric
#' @export
SSE <- function(x,y,...) {
  res <- sum((x-y)^2,...)
  return(res)
  }
NULL

#' Center a variable
#'
#' By centering I mean take the numeric variable and subtract the mean from each value.  Then return each value.
#'
#' @export
#' @param x Variable to be centered
#' @return Numeric x - mean(x)
#' @examples 
#' center(c(1,2,3))
center <- function(x) {
    x <- x[!is.na(x)]
    if (!all(is.numeric(x))) stop("In repsych:center:  A non-numeric value of x was detected")
    x.is.finite <- is.finite(x)
    if (!all(x.is.finite)) {
      print("In repsych:center: Non-finite values detected while centering after removing NAs, removed from calculation")
      print(table(x[!x.is.finite],exclude=NULL))
      x <- x[x.is.finite]
    }
    if (length(x)==0) stop("In repsych:center: No finite numeric values in x")
    return(x-mean(x))
  }
NULL


#' Write out the variables from X in a MPLUS MEANS STD CORR format
#'
#' Take data.frame x which only has the variables of interest and convert to MEAN STD CORR format.  Note this has not been tested at all. Also note that this will overwrite any pre-existing file.
#'
#' @export
#' @param x The data.frame with the variables to export
#' @param filename The filename to export to.
#' @examples
#' x <- data.frame(X1=rnorm(20),X2=rnorm(20),X3=rnorm(20)) #example data
#' MEANS.STD.CORR(x)
MEANS.STD.CORR <- function(x,filename="output.dat") {
    cat(apply(x, 2, mean), "\n", sep = " ", append = FALSE, file = filename)
    cat(apply(x, 2, sd), "\n", sep = " ", append = TRUE, file = filename)
    mat <- cor(x)
    lower.tri.conv <- function(mat) {
        mat[upper.tri(mat)] <- ""
        mat <- apply(mat, 1, FUN = function(x) {return(paste(paste(x, collapse = " "), "\n", collapse = ""))})
        return(mat)
    }
    cat(lower.tri.conv(mat), append = TRUE, file = filename,sep="")
} 
NULL

#' Generate Names for a Wide Stuctured Dataset
#'
#' This function attempts to be flexible, but may not cover all usage cases.
#' See the examples for known-supported usage cases.
#' 
#' @export
#' @param varnames This can be a vector of character values, a list of character values, or a data.frame of factors.
#' @param varlevels This can be a vector of numeric (specifying the number of levels) or a list of character (specifying the factor levels) when varnames is a vector of characters.  When varnames is a data.frame, it must be a list specifying either the numeric id of the desired factor levels or the names of the factors themselves.
#' @seealso \code{\link{longframe}}
#' @examples
#' widenames(c("Age","Workload"),c(2,2))
#' widenames(c("Age","Workload"),list(c("Young","Old"),c("High","Low")))
#' widenames(list("Age","Workload"),list(c("Young","Old"),c("High","Low")))
#' widenames(list("Age","Workload"),c(2,2))
#' 
#' df <- data.frame(Var1=factor(LETTERS[1:5]),Var2=factor(10:14))
#' widenames(df)
#' widenames(df,list(Var1=c("A","B"),Var2=1:2))
#' widenames(df,list(Var1=c("A","B"),Var2=NULL))
#' widenames(df,list(Var1=NULL,Var2=1:2))
#' widenames(df,list(Var1=NULL,Var2=c("11","13")))
#' widenames(df,list(Var1=c("A","B"),Var2=c("10","11","12")))
#' # widenames(df,c(2,3)) #Not supported
#' data(examples213)
#' widenames(sec213.1.I.L[c("Prep","Day")])

widenames <- function(varnames,varlevels=vector("list",length(varnames))) {
  #process the data.frame to get it to work
  if (is.data.frame(varnames)) {
    df <- varnames
    varnames <- names(df)
    if (!all(unlist(lapply(df,is.factor)))) {stop("In repsych::widenames:  A data.frame was provided for varnames, but one of the columns was not a factor!")}
    for (i in 1:ncol(df))
    {
      if (is.null(varlevels[[i]])) {
        varlevels[[i]] <- levels(df[,i])
      } else {
        if (is.numeric(varlevels[[i]])) {varlevels[[i]] <- levels(df[,i])[varlevels[[i]]]}
        if (is.character(varlevels[[i]])) {varlevels[[i]] <- levels(df[,i])[levels(df[,i]) %in% varlevels[[i]]]}
      }
    }
  }
  if (!is.list(varlevels)) {varlevels <- as.list(varlevels)}
  varlevels.expanded <- lapply(varlevels,function(x) {
    if ((is.numeric(x) & (length(x) == 1))) {return(1:x)} else {return(x)}
  }
  )
  tmpl <- unlist(lapply(varlevels.expanded,length))
  tmpn <- varnames
  varlevels.expanded <- lapply(varlevels.expanded,as.character)
  wn <- vector("character",prod(tmpl))
  
  for (i in 1:length(tmpn))
  {
    if (i < length(tmpn))
    {
      each <- prod(tmpl[(i+1):length(tmpn)])
    } else {each <- 1}
    wn <- paste(wn,
                rep(
                  paste(
                    tmpn[i],"_",varlevels.expanded[[i]],sep=""
                  )
                  ,each=each,
                  length.out=prod(tmpl)),sep=".")
  }
  wn <- substr(wn,2,nchar(wn))
  return(wn)
}

#' Transform a wide dataset into a long dataset.
#'
#' There is almost certainly a better way to do this.  The aim here is to turn a wide dataset into a long dataset.

#' @export
#' @importFrom reshape2 melt
#' @importFrom foreach foreach %do%
#' @param wide.data The wide dataset
#' @param btwnsubnames The between subject identifiers
#' @param wide.var.names The variables that are in the wide format
#' @param value.name unknown
#' @param dropcol Columns to drop from the long format
#' @seealso \code{\link{widenames}}
#' @examples
#' widenames(c("Age","Workload"),c(2,2))
longframe <- function(wide.data,btwnsubnames,wide.var.names,value.name=NA,dropcol=c())
{
  i <- NULL
  tmpn <- wide.var.names
  wd <- wide.data
  wd <- wd[,!(names(wd) %in% dropcol)]
  wd.melt <- melt(wd,id.vars=btwnsubnames) 
  varids <- strsplit(unfactor(wd.melt$variable),".",fixed=TRUE)
  getvals <- function(varid)
  {
    return(as.numeric(diag(sapply(varid,substring,first=sapply(tmpn,nchar)+1,last=nchar(varid)))))
  }
  longres <- foreach(i=1:length(varids),.combine=rbind) %do% getvals(varids[[i]]) 
  longres <- as.data.frame(longres) 
  names(longres) <- tmpn
  rownames(longres) <- c()
  res <- cbind(wd.melt,longres)
  names(res)[names(res)=="variable"] <- "orig.var.name"
  if (!is.na(value.name))
  {
    names(res)[names(res)=="value"] <- value.name
  }
  return(res)
}
NULL

