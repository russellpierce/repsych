#' Deprecated function(s) in the russmisc package
#' 
#' These functions are provided for compatiblity with older version of
#' the russmisc package and russfnc scripts.  They may eventually be completely
#' removed.
#' @rdname russmisc-deprecated
#' @name russmisc-deprecated
#' @param ... Parameters to be passed to the modern version of the function
#' @docType package
#' @export  latinsquare.digram Conv3Dto2D Conv2Dto3D dist3D.l product
#' @aliases latinsquare.digram Conv3Dto2D Conv2Dto3D dist3D.l product
#' @section Details:
#' \tabular{rl}{
#'   \code{latinsquare.digram} \tab now a synonym for \code{\link{latinSquareDigram}}\cr
#'   \code{Conv3Dto2D} \tab now a synonym for \code{\link{conv3Dto2D}}\cr
#'   \code{Conv2Dto3D} \tab now a synonym for \code{\link{conv2Dto3D}}\cr
#'   \code{dist3D.l} \tab now a synonym for \code{\link{dist3D}}\cr
#'   \code{product} \tab now a synonym for \code{\link{prod}} in base\cr
#' }
#'  
latinsquare.digram <- function(...) {
  .Deprecated("latinSquareDigram",package="russmisc")
  latinSquareDigram(...)
}
Conv3Dto2D <- function(...) {
  .Deprecated("conv3Dto2D",package="russmisc")
  conv3Dto2D(...)
}
Conv2Dto3D <- function(...) {
  .Deprecated("conv2Dto3D",package="russmisc")
  conv2Dto3D(...)
}
dist3D.l <- function(...) {
  .Deprecated("dist3D",package="russmisc")
  dist3D(...)
}
product <- function(...)
{
  .Deprecated("prod",package="base")
  return(prod(...))
}
NULL

NULL

#' Defunct function(s) in the russmisc package
#' 
#' These functions were in older versions of the russmisc package
#' or russfnc scripts, but now have been completely removed.
#' Where possible they will retain a call to .Defunct.
#' 
#' @rdname russmisc-defunct
#' @name russmisc-defunct
#' @aliases t.contrast
#' @section Details:
#' \tabular{rl}{
#'   \code{t.contrast} \tab conflicted with the packaging and has been removed, see \code{\link{tContrast}} for a replacement\cr
#' }
NULL