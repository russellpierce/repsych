repsych
=======

R Function for Reproducible Research in Psychology

## Accessing
I think the good old days of the repository are gone.  Soon (by the end of 2013) the current stable version of repsych
will be on CRAN.  Until that time, or if you want to work on the development version, there are several approaches
availble.

1. The official github method:  a pain for Windows users, but fine for most people
2. Manual:  this is probably what you want if you are only interested in a few specific functions from repsych
3. Private distribution from source
4. On-the-fly script environment

### Official github method
After you complete the steps below you can use the following code to update to the current version:
```
library(devtools)
install_github('repsych','drknexus')
library(repsych)
```
If you want to only temporarily update set dev_mode TRUE first...
```
library(devtools)
dev_mode(TRUE)
install_github('repsych','drknexus')
library(repsych)
#stuff you want to do here
dev_mode(FALSE)
```
#### Mac/Linux Setup
Install the devtools library ```(install.packages('devtools')```
#### Windows Setup
* Install Rtools, http://cran.r-project.org/bin/windows/Rtools/ to match your version of R
* Install devtools via ```install.packages('devtools',type='source'))```
* 

### Manual
```
source("https://raw.github.com/drknexus/repsych/master/R/glibrary.r")
```
Where you replace glibrary.r with your desired repsych script.

### Private distribution
This method isn't updated and probably only works for 2.15.x at the moment.  However, this is the only method other than githubs that brings in the function documentation along with the functions.
```
download.file("https://raw.github.com/drknexus/repsych/master/repos/src/contrib/repsych_3.0.0.2.tar.gz",
  "./repsych-current.tar.gz",
  mode="wb"
)
install.packages("repsych-current.tar.gz",repos=NULL,type="source")
```

### Attach as environment
This method sources all of the files on github under the 'master' branch into an environment called repsych.tmp and then attaches to that environment.
```
source('https://raw.github.com/drknexus/repsych/master/allplain.r')
```

## Notice
Not all code is in here is mine.  Not all code in here works.

## Style Guide
Suggestions accepted.
### Function Names
* Functions will be provided in camelCaseLikeThis except where it harms readablity or makes no sense.
* For example km2mi is not benefited by being turned into km2Mi.
* Abreviations are all uppercase unless they start the function name.  For example aicCompare versus compareAIC.
* As a general matter, functions will be defined in verb-noun order as in "I want to <verb> <noun>".
* Nouns for these purposes will remain in their singular form, e.g. compareAIC not compareAICs.

### Variable Names
* Variables that are used inside functions will be all lower case except where it harms readablity and may be seperated by a period except which this would yield an S3 class-like name.

### Dataset Names
* Root dataset names will be a capital letters.
* Subsets/Modifications of those dataset names will be denoted by a period and then the nature of the modification.




