repsych
=======

R Function for Reproducible Research in Psychology

The repository will be going back up soon (tm).  In the mean time, you can install in this way (make sure you get the entire line):

`
download.file("https://raw.github.com/drknexus/repsych/master/repos/src/contrib/repsych_3.0.0.2.tar.gz",
  "./repsych-current.tar.gz",
  mode="wb"
)
install.packages("repsych-current.tar.gz",repos=NULL,type="source")
`

## Notice ##
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




