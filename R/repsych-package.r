#'A repository for a variety of useful functions.
#'
#'A respository for a variety of useful functions which may be of interest or otherwise reused.  Unless otherwise stated in the function description, all functions are authored by Russell S. Pierce.
#'
#' @name repsych-package
#' @aliases repsych
#' @docType package
#' @title russmisc-package placeholder
#'
NULL
.onAttach <- function(libname,pkgname) {
    packageStartupMessage(paste(
      'Starting:','repsych',
      '
	Version:',utils::packageVersion("repsych"),
      '
	Package Date:',utils::packageDescription("repsych")$Date))
    #options(contrasts=c(unordered="contr.sum",ordered="contr.poly"))
    #packageStartupMessage("Note:  russmisc DOES NOT change the default contrast options!\n")
}
NULL
#' Essentials of Behavioral Research: Methods and Data Analysis, pg 555
#'
#' "Suppose our subjects are five female and five male teachers, each assigned a different set of four pupils to teach in a brief instructional situation.  Of each of these 10 sets of four pupils, two are female and two are male. Furthermore, one female and one male pupil are designated (at random) to the particular teacher as showing special intellectual promise (high expectancy), but nothing is said of the ramining puipils (low expectancy)".
#' data.EBR.Table.18.25 is the example displayed in Table 18.25 in the refered text is provided her in the long format.
#' ezANOVA.EBR.Table.18.25 is the result of the following \code{ezANOVA} function from the package \code{ez}: \code{ezANOVA.EBR.Table.18.25 <- ezANOVA(data=pg555,dv=.(Score),within=.(Expectancy,StudentGender),between=.(TeacherGender),wid=.(TeacherID),return_aov=TRUE,type=3)}
#' 
#' Included in the package russmisc with the permission of Robert Rosenthal.
#' 
#' @name EBR.Table.18.25
#' @aliases data.EBR.Table.18.25 ezANOVA.EBR.Table.18.25
#' 
#' @docType data
#' @references Rosenthal & Rosnow. (2008). \emph{Essentials of Behavioral Research}. Boston: Mc Graw Hill. p 555.
#' @keywords data
NULL

#' Examples for Psych 213 taught at University of California, Riverside Winter of 2013.
#' 
#' @name examples213
#' @aliases hw213.1.I.L hw213.1.I.W sec213.1.I.L sec213.1.I.W sec213.1.II.W sec213.2.L sec213.2.W sec213.4.L data.names
#' 
#' @docType data
#' @keywords data
NULL
