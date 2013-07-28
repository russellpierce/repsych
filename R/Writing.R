#Print LaTeX Table Directly to an Image (PNG or other): http://stackoverflow.com/questions/9298765/print-latex-table-directly-to-an-image-png-or-other

#' Print LaTex Table Direction to an Image
#' 
#' @param obj The matrix to turn into a table
#' @param name The name of the png to produce (without extention)
#' @author Found here:  http://stackoverflow.com/questions/9298765/print-latex-table-directly-to-an-image-png-or-other
#' @export
#' @importFrom xtable xtable
#' @importFrom tools texi2dvi
table.png <- function(obj, name) {
  first <- name
  name <- paste(name, ".tex", sep = "")
  sink(file = name)
  cat("\n\\documentclass{report}\n\\usepackage[paperwidth=5.5in,paperheight=7in,noheadfoot,margin=0in]{geometry}\n\\begin{document}\\pagestyle{empty}\n")
  print(xtable(obj))
  cat("\n\\end{document}\n")
  sink()
  tools::texi2dvi(file = name)
  cmd <- paste("dvipng -T tight", shQuote(paste(first, ".dvi", sep = "")))
  invisible(system(cmd))
  cleaner <- c(".tex", ".aux", ".log", ".dvi")
  invisible(file.remove(paste(first, cleaner, sep = "")))
} 
NULL


#' Add a word to the end of an aspell dictionary
#'
#' Actually this adds the value in the 'word' parameter to the end of any file specified by the 'file' argument.
#'
#' @export
#' @param word The word to be added to the dictionary
#' @param dict The location of the dictionary
#' @examples 
#' #mydictloc <- "c:\\rsp\\tools\\aspell\\bin\\aspell.exe"
#' #aspell_add_word("pythagorean",mydictloc)
aspell_add_word <- function(word,dict) {
  tmp <- readLines(dict) #read in old dict
  new <- c(tmp,word)
  writeLines(new,dict)
}
NULL

#' Update a .tex file from .rnw file if needed
#'
#' Check the file dates.  If .Rnw is newer than .Tex, then re-weave.
#'
#' @export
#' @param filename character the root of the filename, e.g. "writing" when refering to "Writing.Rnw"
updateTex <- function(filename) {
  Rnw.name <- paste(filename,".Rnw",sep="")
  Tex.name <- paste(filename,".tex",sep="")
  if ((file.info(Rnw.name)$mtime > (file.info(Tex.name)$mtime))) {
    cat("Reweaving",filename,"\n")
    Sweave("FileProcesses.Rnw")  
  }
} #end updateTex
NULL

#' Insert a figure in LaTex
#'
#' @export
#' @param file The name of the picture to insert
#' @param placementSpecifier Options for LaTex that go in brackets when starting a figure.  Tells LaTex where to put the picture. Note that
#' the placement specifier here is the latex default, tdp.  When using many images other packages, e.g. floats, morefloats may be useful.  Alternatively,
#' a \\clearpage can clear out the floats.
#' @param clearpage Whether to trigger a \\clearpage after the figure.
#' @param caption (optional) The caption for the figure in LaTex
latexFigure <- function(file,caption=NULL,placementSpecifier="tdp",clearpage=FALSE) {
  cat("\n% Figure created by russmisc::latexFigure\n")
  cat("\\begin{figure}[",placementSpecifier,"]\n",sep="")
  cat("\\includegraphics{", file, "}\n", sep="")
  if (!is.null(caption)) {
    cat("\\caption{",as.character(caption),"}\n")
  }
  cat("\\end{figure}\n")
  if (clearpage) {cat("\\clearpage\n")}
  cat("\n")
}
NULL

#' droplead0
#'
#' Drop a leading zero from a number and convert to a character string, as in reporting p-values.
#' @export
#' @param val The numeric value from which to drop the lead 0
droplead0 <- function(val)
{
  val <- as.character(val)
  leadchar <- substr(val,1,1)
  if (leadchar == "0") {val <- substr(val,2,nchar(val))}
  return(val)
}
NULL


#' Format a p-value for LaTex
#'
#' All ps greater than .01 will be reported exactly to two decimal places.  All ps greater than .001 but less than 
#' .01 will be reported exactly to 3 decimal places.  All other ps will be reported at p < .001.
#' @export
#' @param p.val A numeric p value
latex.p <- function(p.val)
{
  if (p.val < .001) {p.val <- paste("\\\\\\emph{p} < .001")} else
    if (p.val < .01) {p.val <- paste("\\\\\\emph{p} = ",droplead0(signif(p.val,3)),sep="")} else
    {p.val <- paste("\\\\\\emph{p} = ",droplead0(signif(p.val,2)),sep="")}
  return(p.val)
}
NULL