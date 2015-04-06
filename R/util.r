#' Catch errors and warnings and store them for subsequent evaluation
#'
#' Factory modified from a version written by Martin Morgan on Stack Overflow (see below).  
#' Factory generates a function which is appropriately wrapped by error handlers.  
#' If there are no errors and no warnings, the result is provided.  
#' If there are warnings but no errors, the result is provided with a warn attribute set.
#' If there are errors, the result retutrns is a list with the elements of warn and err.
#' This is a nice way to recover from a problems that may have occurred during loop evaluation or during cluster usage.
#' Check the references for additional related functions.
#' I have not included the other factory functions included in the original Stack Overflow answer because they did not play well with the return item as an S4 object.
#' @export
#' @param fun The function to be turned into a factory
#' @return The result of the function given to turn into a factory.  If this function was in error "An error as occurred" as a character element.  factory-error and factory-warning attributes may also be set as appropriate.
#' @references
#' \url{http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function}
#' @author Martin Morgan; Modified by Russell S. Pierce
#' @examples 
#' f.log <- factory(log)
#' f.log("a")
#' f.as.numeric <- factory(as.numeric)
#' f.as.numeric(c("a","b",1))
#' # Ad Hoc utility functions available
#' # .has <- function(x, what) {
#' #   !is.null(attr(x,what))
#' # }
#' # hasWarning <- function(x) .has(x, "factory-warning")
#' # hasError <- function(x) .has(x, "factory-error")
#' # isClean <- function(x) !(hasError(x) | hasWarning(x))
#' # Tests not run:
#' # bothErrorAndWarning <- function() {warning("A warning");stop("Really an error")}
#' # ErrorOnly <- function() {stop("Really an error")}
#' # WarningOnly <- function() {warning("A warning")}
#' # glibrary(assertthat)
#' # assert_that(hasWarning(factory(bothErrorAndWarning)())==TRUE)
#' # assert_that(hasError(factory(bothErrorAndWarning)())==TRUE)
#' # assert_that(hasWarning(factory(ErrorOnly)())==FALSE)
#' # assert_that(hasError(factory(ErrorOnly)())==TRUE)
#' # assert_that((hasWarning(factory(WarningOnly)()))==TRUE)
#' # assert_that(hasError(factory(WarningOnly)())==FALSE)

factory <- function (fun) {
  errorOccurred <- FALSE
  library(data.table)
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(tryCatch(fun(...), error = function(e) {
      err <<- conditionMessage(e)
      NULL
    },finally=errorOccurred <<- TRUE), warning = function(w) {
      warn <<- append(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    if (errorOccurred) {
      res <- "An error occurred in the factory function"
    } 
    
    if (is.character(warn)) {
      data.table::setattr(res,"factory-warning",warn)
    } else {
      data.table::setattr(res,"factory-warning",NULL) 
    }
    
    if (is.character(err)) {
      data.table::setattr(res,"factory-error",err)
    } else {
      data.table::setattr(res, "factory-error", NULL)
    }  
    return(res)
  }
}
NULL
