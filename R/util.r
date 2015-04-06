#' Catch errors and warnings and store them for subsequent evaluation
#'
#' Factory modified from a version written by Martin Morgan on Stack Overflow (see below).  
#' Factory generates a function which is appropriately wrapped by error handlers.  
#' If there are no errors and no warnings, the result is provided.  
#' If there are warnings but no errors, the result is provided with a warn attribute set.
#' If there are errors, the result retutrns is a list with the elements of warn and err.
#' This is a nice way to recover from a loop problems that may have occured during loop evaluation or during cluster usage.
#' Check the references for additional related functions.
#' I have not included the other factory functions included in the original Stack Overflow answer because they did not play well with the return item as an S4 object.
#' @export
#' @param fun The function to be turned into a factory
#' @return list[3]: item 1 is the result from the function and $warn and $err contain warning and error messages respectively.
#' @references
#' \url{http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function}
#' @author Martin Morgan; Packaged by Russell S. Pierce
#' @examples 
#' f.log <- factory(log)
#' f.log("a")
#' f.as.numeric <- factory(as.numeric)
#' f.as.numeric(c("a","b",1))
factory <- function(fun)
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    return(list(res, warn=warn, err=err))
  }
NULL
