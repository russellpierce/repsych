#' catseye
#' 
#' Draw a catseye plot per idea from Cumming's book 
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
#' @param xpoints Create visual space
#' @param ... other arguments passed to graphics::plot
#' @return Generates a plot in R graphics
#' @note Check CRAN for 'multicon' on CRAN for Author's newest version
#' @author Ryne A. Sherman
#' @export
#' @examples
#' # A Single Group
#' f <- rnorm(50)
#' catseye(f, conf=.95, xlab="", ylab="DV", las=1)
#' catseye(f, conf=.99, xlab="", ylab="DV", las=1, col="light green")
#' 
#' # Two Groups
#' f2 <- rnorm(100)
#' g <- rep(1:2, each=50)
#' catseye(f2, grp=g, xlab="Conditions", ylab="DV", grp.names=c("Control", "Experimental"), las=1)
#' catseye(f2, grp=g, conf=.8, xlab="Conditions", ylab="DV", grp.names=c("Control", "Experimental"), las=1, col="cyan", main="Two Group Mean Comparison")
#' 
#' # Three Groups
#' f3 <- c(rnorm(10), rnorm(10, mean=.5), rnorm(10, mean=1, sd=2))
#' g2 <- rep(1:3, each=10)
#' catseye(f3, grp=g2, conf=.95, xlab="Conditions", ylab="DV", grp.names=c("Group 1", "Group 2", "Group 3"), las=1, col="cyan", main="Three Group Mean Comparison")
#' 
#' # A 2 x 2 Design
#' f4 <- rnorm(200)
#' fac1 <- rep(1:2, each=100)
#' fac2 <- rep(3:4, 100)
#' catseye(f4, list(fac1, fac2), xlab="Conditions", ylab="DV", grp.names=c("High/High", "High/Low", "Low/High", "Low/Low"), las=1, col="orange", main="A 2 x 2 Experiment Comparison")
#' # Using the xpoints argument to create visual space
#' catseye(f4, list(fac1, fac2), xlab="Conditions", ylab="DV", grp.names=c("High/High", "High/Low", "Low/High", "Low/Low"), xpoints=c(1,2,4,5), las=1, col="orange", main="A 2 x 2 Experiment Comparison")
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
      Q <- qnorm(seq(.001, .999, by = .001), se(DV))
<<<<<<< HEAD
      Qscale <- Q * se(DV) + res
=======
      Qscale <- Q * se(DV) + mean(DV)
>>>>>>> 45812eb6dbf594512f596f108978445bd097e2fa
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
  if (!is.null(grp) & !is.list(grp)) {
    dat <- cbind(DV, grp)
    dat <- dat[complete.cases(dat), ]
    res <- tapply(dat[, 1], dat[, 2], plotFUN)
    if (is.null(xpoints)) {
      places <- 1:length(res)
    }
    else places <- xpoints
    if (is.null(errFUN)) {
      plot(res ~ places, pch = 19, xaxt = "n", xlim = c(0.4, 
                                                        0.4 + places[length(places)]), ylim = ylim, ...)
    }
    if (!is.null(errFUN)) {
      e <- tapply(dat[, 1], dat[, 2], errFUN)
      e <- ifelse(res <= 0, -e, e)
      SEs <- tapply(dat[, 1], dat[, 2], se)
      Q <- matrix(unlist(tapply(dat[, 1], dat[, 2], function(x) qnorm(seq(.001, .999, by = .001), se(x)))), nrow = 999)
      Qscale <- Q * matrix(SEs, nrow = 999, ncol = ncol(Q), byrow = TRUE) + matrix(res, nrow = 999, ncol = ncol(Q), byrow = TRUE)
      if (is.null(ylim)) {
        lims <- c(min(Qscale), max(Qscale))
      }
      else (lims <- ylim)
      plot(res ~ places, pch = 19, xaxt = "n", xlim = c(0.4,
                                                        0.4 + places[length(places)]), ylim = lims, ...)
      Qscale2 <- apply(rbind(Qscale, res - e, res + e), 2, sort) 
      poly1 <- rep(NA, ncol(Qscale2))
      poly2 <- rep(NA, ncol(Qscale2))
      for(i in 1:ncol(Qscale2)) {
        poly1[i] <- which(Qscale2[, i] == res[i] - e[i])
        poly2[i] <- which(Qscale2[, i] == res[i] + e[i])
        polygon(x = c(c(places[i] - dnorm(Q[, i]))[poly1[i]:poly2[i]], c(places[i] + dnorm(Q[, i]))[poly2[i]:poly1[i]]), 
                y = c(Qscale2[poly1[i]:poly2[i], i], Qscale2[poly2[i]:poly1[i], i]), border = NA, col = col)
        lines(x = places[i] - dnorm(Q[, i]), y = Qscale[, i])
        lines(x = places[i] + dnorm(Q[, i]), y = Qscale[, i])
      }
      points(x = places, y = res, pch = 19)
      arrows(places, res + e, places, res - e, angle = 90, code = 3, length = .08)
    }
    if (is.null(grp.names)) {
      grp.names <- 1:length(places)
    }
    axis(1, at = places, labels = grp.names, tick = tick)
  }
  if (is.list(grp)) {
    if (length(unique(unlist(lapply(grp, length)))) != 1) {
      stop("Grouping variables must be the same length.")
    }
    if (length(DV) != lapply(grp, length)[[1]]) {
      stop("DV must be the same length as the grouping variables.")
    }
    dat <- cbind(DV, matrix(unlist(grp), nrow = length(DV), byrow = FALSE))
    if (sum(is.na(dat)) > 0 ) {
      stop("Please remove missing values in DV and IV first.")
    }
    res <- as.vector(tapply(DV, grp, plotFUN))
    if (is.null(xpoints)) {
      places <- 1:length(res)
    }
    else places <- xpoints
    if (is.null(errFUN)) {
      plot(res ~ places, pch = 19, xaxt = "n", xlim = c(0.4,
                                                        0.4 + places[length(places)]), ylim = ylim, ...)
    }
    if (!is.null(errFUN)) {
      e <- as.vector(tapply(DV, grp, errFUN))
      e <- ifelse(res <= 0, -e, e)
      SEs <- as.vector(tapply(DV, grp, se))
      Q <- matrix(qnorm(seq(.001, .999, by = .001), SEs), nrow = 999, ncol = length(SEs))
      Qscale <- Q * matrix(SEs, nrow = 999, ncol = ncol(Q), byrow = TRUE) + matrix(res, nrow = 999, ncol = ncol(Q), byrow = TRUE)
      if (is.null(ylim)) {
        lims <- c(min(Qscale), max(Qscale))
      }
      else (lims <- ylim)
      plot(res ~ places, pch = 19, xaxt = "n", xlim = c(0.4,
                                                        0.4 + places[length(places)]), ylim = lims, ...)
      Qscale2 <- apply(rbind(Qscale, res - e, res + e), 2, sort) 
      poly1 <- rep(NA, ncol(Qscale2))
      poly2 <- rep(NA, ncol(Qscale2))
      for(i in 1:ncol(Qscale2)) {
        poly1[i] <- which(Qscale2[, i] == res[i] - e[i])
        poly2[i] <- which(Qscale2[, i] == res[i] + e[i])
        polygon(x = c(c(places[i] - dnorm(Q[, i]))[poly1[i]:poly2[i]], c(places[i] + dnorm(Q[, i]))[poly2[i]:poly1[i]]), 
                y = c(Qscale2[poly1[i]:poly2[i], i], Qscale2[poly2[i]:poly1[i], i]), border = NA, col = col)
        lines(x = places[i] - dnorm(Q[, i]), y = Qscale[, i])
        lines(x = places[i] + dnorm(Q[, i]), y = Qscale[, i])
      }
      points(x = places, y = res, pch = 19)
      arrows(places, res + e, places, res - e, angle = 90, code = 3, length = .08)
    }
    if (is.null(grp.names)) {
      grp.names <- 1:length(places)
    }
    axis(1, at = places, labels = grp.names, tick = tick)
  }
}
NULL
