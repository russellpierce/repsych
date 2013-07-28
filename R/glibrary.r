#' Try to load a library, if that fails, install it, then load it.
#'
#' glibrary short for (get)library.
#' The primary aim of this function is to make loading packages more transparent.  Given that we know we want to load a given package, actually fetching it is a formality.  glibrary skims past this formality to install the requested package.
#'
#' @export
#' @param ... comma seperated package names
#' @param lib.loc See \code{\link{require}}
#' @param quietly See \code{\link{require}}
#' @param warn.conflicts See \code{\link{require}}
#' @param pickmirror If TRUE, glibrary allows the user to select the mirror, otherwise it auto-selects on the basis of the country code
#' @param countrycode This option is ignored and the first mirror with the substring "Cloud", e.g. the RStudio cloud, is selected.  If no mirrors with that substring are identified, glibrary compares this value to results from getCRANmirrors() to select a mirror in the specified country.
#' @return logical; TRUE if glibrary was a success, an error if a package failed to load
#' @note keep.source was an arguement to require that was deprecated in R 2.15
#' @note This warning \code{Warning in install.packages: InternetOpenUrl failed: 'The operation timed out'} indicates that the randomly selected repository is not available.  Check your internet connection.  If your internet connection is fine, set pickmirror=TRUE and manually select an operational mirror.
#' @examples
#' #glibrary(lattice,MASS) #not run to prevent needless dependency
glibrary <- function(..., lib.loc = NULL, quietly = FALSE, warn.conflicts = TRUE, pickmirror = FALSE, countrycode = "us") {

  character.only <- TRUE  #this value is locked to TRUE so that the function passes the character value to require and not the variable name thislib
  librarynames <- unlist(lapply(as.list(substitute(.(...)))[-1],as.character))
  success <- vector("logical", length(librarynames))
  needToInstall <- !librarynames %in% installed.packages()[,"Package"]
  warningHandle <- function(w) {
    if (grepl("there is no package called",w$message,fixed=TRUE)) {
      return(FALSE) #not-loadable
    } else {
      return(TRUE) #loadable
    }
  }
  if (any(needToInstall)) {
    if (pickmirror) {chooseCRANmirror()}
    if (getOption("repos")[["CRAN"]] == "@CRAN@") {
      #Select the first "Cloud" if available
      m <- getCRANmirrors(all = FALSE, local.only = FALSE)
      URL <- m[grepl("Cloud",m$Name),"URL"][1] #get the first repos with "cloud" in the name
      if (is.na(URL)) { #if we did not find the cloud,
        #Fall back and use the previous method
        message("\nIn repsych:glibrary:  Now randomly selecting a CRAN mirror. You may reselect your CRAN mirror with chooseCRANmirror().\n")
        #if there is no repository set pick a random one by country code
        getCRANmirrors.res <- getCRANmirrors()
        foundone <- FALSE  #have we found a CRAN mirror yet?
        #is it a valid country code?
        if (!countrycode %in% getCRANmirrors.res$CountryCode) {
          stop("In repsych::glibrary:  Invalid countrycode argument")
        }
        ticker <- 0
        while (!foundone) {
          ticker <- ticker + 1
          URL <- getCRANmirrors.res$URL[sample(grep(countrycode, getCRANmirrors.res$CountryCode), 1)]
          host.list <- strsplit(URL, "/")
          host.clean <- unlist(lapply(host.list, FUN = function(x) {return(x[3])}))
          #make sure we can actually access the package list
          if (nrow(available.packages(contrib.url(URL)))!=0) {foundone <- TRUE}        
          if (ticker > 5) {stop("In repsych::glibrary:  Unable to access valid repository.  Is the internet connection working?")}
        } #end while
      } #end else
      repos <- getOption("repos")
      repos["CRAN"] <- gsub("/$", "", URL[1L])
      options(repos = repos)
    } #done setting CRAN mirror
    for (i in librarynames[needToInstall]) {
      thislib <- i  #oddly, require won't take librarynames[i], so we do it this way      
      message("\nIn repsych::glibrary:  Attempting to install and load missing package ",thislib,"\n")
      install.packages(thislib)
    }
  } #done installing packages
  needToInstall <- !librarynames %in% installed.packages()[,"Package"]
  if (any(needToInstall)) {
    stop("In repsych::glibrary: Could not download and/or install ",librarynames[needToInstall],", glibrary stopped.")
  }
  message("In repsych::glibrary:  Attempting to load requested packages...\n")
  for (i in 1:length(librarynames)) {
    thislib <- librarynames[i]  #oddly, require won't take librarynames[i], so we do it this way      
    success[i] <- tryCatch(
      require(thislib, lib.loc = lib.loc, quietly = FALSE, warn.conflicts = warn.conflicts, character.only = TRUE),
      warning=warningHandle)
    if (success[i]) {
      require(thislib, lib.loc = lib.loc, quietly = FALSE, warn.conflicts = warn.conflicts, character.only = TRUE)  
    }    
  }
  #Done handing cases where at least one package did not install
  if (all(success)) {
    message("In repsych::glibrary:  Success!")
    return(invisible(TRUE))
  } else {
    stop(paste("\nIn repsych::glibrary, unable to load: ", paste(librarynames[!success], collapse = " "), 
               collapse = " "))
  }
}
NULL