#' Unfactor a vector
#'
#' Take a factor vector and return a value a vector of either type character or numeric by replacing the factors with their associated labels.  If all label names are numeric, a numeric vector will be returned, otherwise a character vector will be returned.
#'
#' @export
#' @param factors The vector of factors to be unfactored
#' @param ignore (character) Strings formatted to be used by \link{gsub} in the pattern argument to be removed from the character vector
#' @param ... Additional arguments passed to gsub
#' @return character or numeric
#' @examples 
#' unfactor(factor(c(3,2,1)))
#' unfactor(factor(c("1,000","2,123,123")))
unfactor <- function(factors,ignore=",",...)
{
  if (!is.factor(factors)) {
    message("In repsych::unfactor: the vector provided is not a factor; no values changed")
    return(factors)
  }
  char.ret <- as.character(factors) #make use of S3 versions of as.character
  #Get rid of those things in ignore
  for (pattern in ignore) {
    char.ret <- gsub(pattern,"",char.ret,...)
  }
  #try to convert to numeric
  num.try <- suppressWarnings(as.numeric(char.ret))
  num.n.fail <- sum(is.na(num.try))
  if (num.n.fail == 0) {
    #none failed to convert, must want numeric
    ret <- num.try
  }
  if (num.n.fail == length(factors)) {
    #none converted, they must want text
    return(char.ret) 
  } else {
    num.ret <- tryCatch(as.numeric(char.ret),
                        warning=function (w) {
                          #see how many we failed to convert    
                          #names(table(as.character(my.ugly.factor)[is.na(})))]))
                          #nameNAvalues(as.numeric(as.character(not.a.factor)))
                        }
    )
  }#end non-numeric error handling
  if (!is.null(num.ret)) return(num.ret) else return(char.ret)
  stop("In repsych::unfactor: no return value was provided, coding error")
}
NULL
