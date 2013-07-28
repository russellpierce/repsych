
repsych.tmp <- attach(NULL, name = 'repsych.tmp')
tmp <- tempfile()


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/contrasts.r'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/counterbalance.r'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/deprecated.r'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/glibrary.r'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/graphics.r'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/SimpleData.R'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/SimpleStats.R'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/TimeSeries.r'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/util.r'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/VisualAngleCalculations.R'),tmp)
sys.source(tmp,repsych.tmp)


writeLines(readLines('https://raw.github.com/drknexus/repsych/tweaks/R/Writing.R'),tmp)
sys.source(tmp,repsych.tmp)


attach(repsych.tmp)

