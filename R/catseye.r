#' Vectorized lookup
#' 
#' This function does a vectorized lookup on a data.frame.  That is, each individual row is matched with a 
#' particular column.
#' 
#' @param DV The variable to be plotted
#' @param grp a vector of group identifiers 
#' @param grp.names a character vector of group names
#' @param plotFUN The summary statistics for central tendency
#' @param errFUN The function that defines the error for plotting (these are internal functions to catseye)
#' @param conf The proportion that represents the desired confidence interval
#' @param tick A logicial indicating if you want tick marks on the axis
#' @param ylim To limit the bounds of the y axis
#' @param col Set the color
#' @param ... other arguments passed to graphics::plot
#' @return Generates a plot in R graphics

#' @author Ryne Sherman
#' @export
#' @examples
#' v <- rnorm(50)
#' catseye(v, conf=.95, xlab="", ylab="DV", las=1)
#' catseye(v, conf=.99, xlab="", ylab="DV", las=1, col="light green")
catseye <- function(DV, grp = NULL, plotFUN = mean, errFUN = c("ci", "se", "sd"),
                    conf = .95, xpoints = NULL, grp.names = NULL, tick = FALSE,
                    ylim = NULL, col = "gray", ...)
{
  se <- function(x) {
    x <- na.omit(x)
    res <- sqrt(var(x)/length(x))
    res
  }
  ci <- function(x) {
    x <- na.omit(x)
    alpha <- 1 - (1 - conf)/2
    res <- qt(alpha, length(x) - 2, lower.tail = T) * se(x)
    res
  }
  if (!is.null(errFUN)) {
    errFUN <- match.arg(errFUN)
  }
  if (is.null(grp)) {
    DV <- na.omit(DV)
    res <- do.call(plotFUN, list(DV))
    if (is.null(errFUN)) {
      plot(res, pch = 19, xaxt = "n", ylim = ylim, ...)
    }
    if (!is.null(errFUN)) {
      e <- do.call(errFUN, list(DV))
      e <- ifelse(res <= 0, -e, e)
      Q <- qnorm(seq(.001, .999, by = .001), se(v))
      Qscale <- Q * se(v) + mean(v)
      if (is.null(ylim)) {
        lims <- c(min(Qscale), max(Qscale))
      }
      else (lims <- ylim)
      plot(res, ylim = lims, xaxt = "n", ...)
      Qscale2 <- sort(c(Qscale, res - e, res + e))
      poly1 <- which(Qscale2 == res - e)
      poly2 <- which(Qscale2 == res + e)
      polygon(x = c(c(1 - dnorm(Q))[poly1:poly2], c(1 + dnorm(Q))[poly2:poly1]), y = c(Qscale2[poly1:poly2], Qscale2[poly2:poly1]), border = NA, col = col)
      points(x = 1, y = res, pch = 19)
      arrows(1, res + e, 1, res - e, angle = 90, code = 3, length = 0.08)
      lines(x = 1 - dnorm(Q), y = Qscale)
      lines(x = 1 + dnorm(Q), y = Qscale)
    }
    if (is.null(grp.names)) {
      grp.names <- ""
    }
    axis(1, at = 1, labels = grp.names, tick = tick)
  }
}
NULL
