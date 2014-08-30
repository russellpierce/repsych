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
Install the devtools library ```install.packages('devtools')```
#### Windows Setup
* Install Rtools, http://cran.r-project.org/bin/windows/Rtools/ to match your version of R
* Install devtools via ```install.packages('devtools',type='source')```


### Manual
```
source("https://raw.github.com/drknexus/repsych/master/R/glibrary.r")
```
Where you replace glibrary.r with your desired repsych script.

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

## Disclaimer 
This package is distributed in the interest of information exchange.
The opinions, findings, and conclusions expressed in this package are those of the authors and not necessarily 
those of employers, significant others, friends, or pets.  Use at your own risk.  
No party assumes liability for its contents or use thereof. 
If trade or manufacturers' names or products are mentioned, 
   it is because they are considered essential to the object of the package and 
   should not be construed as an endorsement.  
Communications made through this website in no way constitute legal or official
notice or comment to any individual or organization for any purpose.

# Version History
## 1.0
### 1.0.1
#### 1.0.1.1
* Added missing ggplot import
* Updated glibrary to drop some messages and hopefully speed things up
* Added padChar
* Added countif
* Added createSquareMatrix
* Added jpeg.save
* Added minFprime
`
# Other Sources
* ggplot2:::interleave: Interleave (or zip) multiple vectors into a single vector.


`