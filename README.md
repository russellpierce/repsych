repsych
=======

R Function for Reproducible Research in Psychology

## Accessing
The repository will be going back up soon (tm).  In the mean time, there are several ways to access the files none of which are great.

### Manual
For now the best way is to probably manually source from github, e.g.
```
source("https://raw.github.com/drknexus/repsych/master/R/glibrary.r")
```
But you'd have to pick and choose the items you wanted to load with that approach.

## Load 'source' package
This method isn't updated and probably only works for 2.15.x at the moment.  However, this is the only method that brings in the function documentation along with the functions.
```
download.file("https://raw.github.com/drknexus/repsych/master/repos/src/contrib/repsych_3.0.0.2.tar.gz",
  "./repsych-current.tar.gz",
  mode="wb"
)
install.packages("repsych-current.tar.gz",repos=NULL,type="source")
```

## Attach as environment
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




