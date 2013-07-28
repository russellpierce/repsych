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