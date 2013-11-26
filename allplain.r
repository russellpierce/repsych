
repsych.tmp <- attach(NULL, name = 'repsych.tmp')
tmp <- tempfile()


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/contrasts.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/counterbalance.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/deprecated.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/glibrary.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/graphics.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/SimpleData.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/SimpleStats.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/TimeSeries.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/util.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/VisualAngleCalculations.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/master/R/Writing.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


attach(repsych.tmp)

