# Farm has been banished here because it likely violates 
# CRAN's rules in regards to where it saves files and is very
# windows specific.  Also, the darn thing is buggy.

#' Create a farm
#'
#' A farm is an external self-terminating instance of R to solve a time consuming problem in R.  
#' Think of it as a (very) poor-person's multi-core.
#' For a usage example, see checkFarm.
#' Known issues:  May have a problem if the library gdata has been loaded.//
#' If a farm produces warnings or errors you won't see them
#' If a farm produces an error... it never will produce a result.
#'
#' @export
#' @param commands A text string of commands including line breaks to run.  
#' This must include the result being saved in the object farmName in the file farmResult (both are variables provided by farm() to the farm).
#' @param farmName This is the name of the farm, used for creating and destroying filenames.  One is randomly assigned that is plausibly unique.
#' @param Rloc The location of R.exe.  The default loads the version of R that is stored in the windows registry as being \"current\".
#' @return The farm name is returned to be stored in an object and then used in checkFarm()
#' @seealso \code{\link{checkFarm}} \code{\link{waitForFarm}}
farm <- function(commands,farmName=paste("farm-",as.integer(Sys.time())+runif(1),sep=""),Rloc = NULL)
{
  if (is.null(Rloc)) {Rloc <- paste('\"',readRegistry(paste("Software\\R-core\\R\\",readRegistry("Software\\R-core\\R\\",maxdepth=100)$`Current Version`,"\\",sep=""))$InstallPath,"\\bin",sep="")}
  Rloc <- paste(Rloc,"\\R.exe\"",sep="")
  farmRda <- paste(farmName,".Rda",sep="")
	farmRda.int <- paste(farmName,".int.Rda",sep="") #internal .Rda
	farmR <- paste(farmName,".R",sep="")
	farmResult <- paste(farmName,".res.Rda",sep="") #result .Rda
	unlink(c(farmRda,farmR,farmResult,farmRda.int))
	farmwd <- getwd()
	cat("setwd(\"",farmwd,"\")\n",file=farmR,append=TRUE,sep="")
	#loading the internals to get them, then loading the globals, then reloading the internals to make sure they have haven't been overwritten
  cat("
load(\"",farmRda.int,"\")
load(farmRda)
load(\"",farmRda.int,"\")
		",file=farmR,append=TRUE,sep="")
	cat("library(russmisc)\n",file=farmR,append=TRUE)
	cat("glibrary(",paste(c(names(sessionInfo()$loadedOnly),names(sessionInfo()$otherPkgs)),collapse=","),")\n",file=farmR,append=TRUE)
	cat(commands,file=farmR,append=TRUE)
	cat("
		unlink(farmRda)
		unlink(farmRda.int)
	",file=farmR,append=TRUE,sep="")
	save(list = ls(all.names=TRUE,envir=.GlobalEnv), file = farmRda,envir=.GlobalEnv)
	save(list = ls(all.names=TRUE), file = farmRda.int)
	#have to drop the escaped quotes for file.exists to find the file
  if (file.exists(gsub('\"','',Rloc))) {
		cmd <- paste(Rloc," --file=",getwd(),"/",farmR,sep="")
	} else {
		stop(paste("Error in russmisc:farm: Unable to find R.exe at",Rloc))
	}
	print(cmd)
	shell(cmd,wait=FALSE)
	return(farmName)
}
NULL

#' Check a farm
#'
#' See farm() for details on farms.  This function checks for a file based on the farmName parameter called farmName.res.Rda.
#' If that file exists it loads it and returns the object stored by the farm in the object farmName.  If that file does not exist,
#' then the farm is not done processing, and a warning and NULL are returned.  Note that a rapid loop through checkFarm() without Sys.sleep produced an error during development.
#'
#' @export
#' @param farmName This is the name of the farm, used for creating and destroying filenames.  This should be saved from when the farm() is created
#' @seealso \code{\link{farm}} \code{\link{waitForFarm}}
#' @examples 
#' #Example not run
#' #.tmp <- "This is a test of farm()"
#' #exampleFarm <- farm("
#' #print(.tmp)
#' #helloFarm <- 10+2
#' #farmName <- helloFarm
#' #save(farmName,file=farmResult)
#' #")
#' #example.result <- checkFarm(exampleFarm)
#' #while (is.null(example.result)) {
#' #	example.result <- checkFarm(exampleFarm)
#' #	Sys.sleep(1)
#' #}
#' #print(example.result)
checkFarm <- function(farmName) {
  farmResult <- paste(farmName,".res.Rda",sep="")
  farmR <- paste(farmName,".r",sep="")
  if (!file.exists(farmR)) {
    message(paste("Warning in russmisc:checkFarm:  There is no evidence that the farm '",farmName,"' exists (no .r file found).\n",sep=""))
  }
	if (file.exists(farmResult)) {
		load(farmResult)
    unlink(farmResult) #delete the farmResult file
    unlink(farmR)      #delete the script file
		return(farmName)
	} else {
		warning(paste("Warning in russmisc:checkFarm:  The farm '",farmName,"' is not ready.\n",sep=""))
		return(invisible(NULL))
	}
}
NULL

#' Wait for a farm result
#'
#' This function repeatedly checks for a farm, when the farm is found it returns the harvest (the farm result object).
#' If the farm terminated with an error or there is some other sort of coding error, waitForFarm will be an infinate loop. As
#' \code{checkFarm} produces errors on checks when the harvest is not ready, waitForFarm hides these errors in the factory error-catching wrapper.
#'
#' @export
#' @param farmName This is the name of the farm, used for creating and destroying filenames.  This should be saved from when the farm() is created
#' @param noCheck If this value is TRUE the check for the farm's .r is skipped.  If it is FALSE, the existance of the appropriate .r is checked for before entering a potentially unending while loop.
waitForFarm <- function(farmName,noCheck=FALSE) {
  f.checkFarm <- factory(checkFarm)
  farmR <- paste(farmName,".r",sep="")
  if (!file.exists(farmR) & !noCheck) {
    stop(paste("Error in russmisc:checkFarm:  There is no evidence that the farm '",farmName,"' exists (no .r file found).\n",sep=""))
  }
  repeat {
    harvest <- f.checkFarm(farmName)
    if (!is.null(harvest[[1]])) {break}
    Sys.sleep(1)
  }
	return(harvest[[1]])
}
NULL

#' Create a one-line simple farm
#'
#' This is a convience wrapper function that uses farm to create a single farm appropriate for processing single line commands.
#'
#' @export
#' @param command A single command
#' @param farmName This is the name of the farm, used for creating and destroying filenames.  One is randomly assigned that is plausibly unique.
#' @param Rloc The location of R.exe.  The default loads the version of R that is stored in the windows registry as being \"current\".
#' @return The farm name is returned to be stored in an object and then used in checkFarm()
#' @seealso \code{\link{farm}}, \code{\link{checkFarm}}, and \code{\link{waitForFarm}}
#' @examples
#' #Example not run
#' #a <- 5
#' #b <- 10
#' #farmID <- simpleFarm("a + b")
#' #waitForFarm(farmID)
simpleFarm <- function(command,farmName=paste("farm-",as.integer(Sys.time())+runif(1),sep=""),Rloc = NULL) {
  return(farm(paste("farmName <- (",command,");save(farmName,file=farmResult)",collapse=""),farmName=paste("farm-",as.integer(Sys.time())+runif(1),sep=""),Rloc = NULL))
}
NULL