#' Catch errors and warnings and store them for subsequent evaluation
#'
#' Factory is not my code.  Factory generates a function which is appropriately wrapped by error handlers.  The result produced is a list.
#' This is a nice way to recover from a loop problems that may have occured during loop evaluation.  Check the references for additional related functions.
#' I have not included the other factory functions because they did not play well with the return item as an S4 object.
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

#' Refresh the russmisc package
#'
#' This is just a convenience to unload an reload the russmisc package, useful for rapid development
#'
#' @export

russmisc.refresh <- function() {
  detach("package:russmisc",unload=TRUE)
  library(russmisc)
}
NULL