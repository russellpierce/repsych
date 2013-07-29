
repsych.tmp <- attach(NULL, name = 'repsych.tmp')
tmp <- tempfile()


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/contrasts.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/counterbalance.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/deprecated.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/glibrary.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/graphics.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/SimpleData.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/SimpleStats.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/TimeSeries.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/util.r',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/VisualAngleCalculations.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/Writing.R',warn=FALSE),tmp)
sys.source(tmp,repsych.tmp)


attach(repsych.tmp)

