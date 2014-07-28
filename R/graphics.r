#' Save current plot as a bmp
#'
#' @export
#' @param filename The filename for the saved plot
#' @param ... options passed to dev.copy
bmp.save <- function(filename,...)
{
  if (substr(filename,nchar(filename)-3,nchar(filename)) != ".bmp") {filename <- paste(filename,".bmp",collapse="",sep="")}
	dev.copy(bmp,...,filename=filename)
	dev.off()
}
NULL

#' Save current plot as a jpeg
#'
#' @export
#' @param filename The filename for the saved plot
#' @param ... options passed to dev.copy
jpeg.save <- function(filename)
{
  if (substr(filename,nchar(filename)-3,nchar(filename)) != ".jpg") {filename <- paste(filename,".jpg",collapse="",sep="")}
  dev.copy(jpeg,filename=filename)
  dev.off()
}
NULL