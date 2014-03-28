#' Save current plot as a bmp
#'
#' @export
#' @param filename The filename for the saved plot
#' @param ... options passed to dev.copy
bmp.save <- function(filename,...)
{
  if (substr(filename,nchar(filename)-3,nchar(filename)) != ".bmp") {filename <- paste(filename,".bmp",collapse="",sep="")}
	dev.copy(bmp,...,filename=filename)
	dev.off()
}
NULL

#' Bin data by group.
#'
#' Missing values are currently silently dropped.
#' 
#' @importFrom proto proto
#' @importFrom scales fullseq
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("stat", "bin")}
#'

#' @param binwidth Bin width to use. Defaults to 1/30 of the range of the
#' data
#' @param origin Origin of first bin
#' @param breaks Actual breaks to use. Overrides bin width and origin
#' @param mapping The aesthetic mapping, usually constructed with aes or aes_string. Only needs to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position  The position adjustment to use for overlappling points on this layer
#' @param width Width of bars when used with categorical data/The width of the tiles.
#' @param right If \code{TRUE}, right-closed, left-open, if \code{FALSE},
#" the default, right-open, left-closed.
#' @param drop If TRUE, remove all bins with zero counts
#' @param ... other arguments passed on to layer
#' @return New data frame with additional columns (reference by ...y...):
#' \item{count}{number of data points in bin}
#' \item{scaledcount}{count, scaled such that the maximum value of for a bar in each group is 1}
#' \item{groupprop}{proportion of points in the x-axis bin relative to the number of points in the group}
#' @export
#' @examples
#' \donttest{
#'  test <- data.frame(
#'     test1 = factor(sample(letters[1:2], 100, replace = TRUE,prob=c(.25,.75)),ordered=TRUE,levels=letters[1:2]), 
#'     test2 = factor(sample(letters[3:8], 100, replace = TRUE),ordered=TRUE,levels=letters[3:8])
#'  )
#' ggplot(test, aes(x=test2))+ 
#'   geom_bar(aes(group=test1,fill=test1,y = ..groupprop..),stat="bingroup",position="dodge")
#' }
stat_bingroup <- function (mapping = NULL, data = NULL, geom = "bar", position = "stack",
                           width = 0.9, drop = FALSE, right = FALSE, binwidth = NULL, origin = NULL, breaks = NULL, ...) {
  StatBingroup$new(mapping = mapping, data = data, geom = geom, position = position,
                   width = width, drop = drop, right = right, binwidth = binwidth, origin = origin, breaks = breaks, ...)
}
NULL


#' StatBingroup
#' 
#' Analogous to ggplot2:::StatBin
StatBingroup <- proto(ggplot2:::Stat, {
  objname <- "bin"
  informed <- FALSE
  
  calculate_groups <- function(., data, ...) {
    if (!is.null(data$y) || !is.null(match.call()$y)) {
      # Deprecate this behavior
      gg_dep("0.9.2", paste(sep = "\n",
                            "Mapping a variable to y and also using stat=\"bin\".",
                            " With stat=\"bin\", it will attempt to set the y value to the count of cases in each group.",
                            " This can result in unexpected behavior and will not be allowed in a future version of ggplot2.",
                            " If you want y to represent counts of cases, use stat=\"bin\" and don't map a variable to y.",
                            " If you want y to represent values in the data, use stat=\"identity\".",
                            " See ?geom_bar for examples."))
    }
    
    .$informed <- FALSE
    .super$calculate_groups(., data, ...)
  }
  
  calculate <- function(., data, scales, binwidth=NULL, origin=NULL, breaks=NULL, width=0.9, drop = FALSE, right = FALSE, ...) {
    range <- scale_dimension(scales$x, c(0, 0))
    
    if (is.null(breaks) && is.null(binwidth) && !is.integer(data$x) && !.$informed) {
      message("stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.")
      .$informed <- TRUE
    }
    
    bingroup(data, data$weight, binwidth=binwidth, origin=origin, breaks=breaks, range=range, width=width, drop = drop, right = right)
  }
  
  default_aes <- function(.) aes(y = ..count..)
  required_aes <- c("x")
  default_geom <- function(.) GeomBar
})
NULL

#' bingroup
#' 
#' Analogous to ggplot2:::bin
#' @param x data
#' @param weight Left over code, don't trust it.
#' @param binwidth   Bin width to use. Defaults to 1/30 of the range of the data
#' @param origin  Origin of first bin
#' @param breaks	Actual breaks to use. Overrides bin width and origin
#' @param width width of bars when used with categorical data
#' @param right	If TRUE, right-closed, left-open, if FALSE,
#' @param drop	If TRUE, remove all bins with zero counts
#' @param range no idea

bingroup <- function(x, weight=NULL, binwidth=NULL, origin=NULL, breaks=NULL, range=NULL, width=0.9, drop = FALSE, right = TRUE) {
  original.data <- x
  x <- x$x #what the original bin expected in x
  if (length(na.omit(x)) == 0) return(data.frame())
  if (is.null(weight)) weight <- rep(1, length(x))
  weight[is.na(weight)] <- 0
  
  if (is.null(range)) range <- range(x, na.rm = TRUE, finite=TRUE)
  if (is.null(binwidth)) binwidth <- diff(range) / 30
  
  if (is.integer(x)) {
    bins <- x
    x <- sort(unique(bins))
    width <- width
  } else if (diff(range) == 0) {
    width <- width
    bins <- x
  } else { # if (is.numeric(x))
    if (is.null(breaks)) {
      if (is.null(origin)) {
        breaks <- scales::fullseq(range, binwidth, pad = TRUE)
      } else {
        breaks <- seq(origin, max(range) + binwidth, binwidth)
      }
    }
    
    # Adapt break fuzziness from base::hist - this protects from floating
    # point rounding errors
    diddle <- 1e-07 * stats::median(diff(breaks))
    if (right) {
      fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
    } else {
      fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle)
    }
    fuzzybreaks <- sort(breaks) + fuzz
    
    bins <- cut(x, fuzzybreaks, include.lowest=TRUE, right = right)
    left <- breaks[-length(breaks)]
    right <- breaks[-1]
    x <- (left + right)/2
    width <- diff(breaks)
  }
  count <- as.numeric(tapply(weight, bins, sum, na.rm=TRUE))
  results <- data.frame(
    count = count,
    #groupcount = sum(count),
    groupprop= count/sum(count),
    x = x,
    width = width
  )
  
  if (sum(results$count, na.rm = TRUE) == 0) {
    return(results)
  }
  
  res <- within(results, {
    count[is.na(count)] <- 0
    density <- count / width / sum(abs(count), na.rm=TRUE)
    scaledcount <- count / max(abs(count), na.rm=TRUE)
    ndensity <- density / max(abs(density), na.rm=TRUE)
  })
  if (drop) res <- subset(res, count > 0)
  res
}
NULL