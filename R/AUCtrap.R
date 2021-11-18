#' Calculate AUC Using Trapezoidal Rule
#'
#' \code{AUCtrap} implements several methods for calculating the area
#' under a piecewise linear function defined by longitudinally measured
#' values of a variable.
#'
#' The implemented methods are total AUC ("AUC"), incremental AUC measured
#' from the first as baseline ("iAUC"), net AUC ("netAUC"), and AUC from the
#' minimum as baseline ("minAUC").
#'
#' @param x numeric vector giving the measurement times (x-values)
#' @param y numeric vector with the measurements corresponding to \code{x}
#' @param method character value defining the calculation method. Defaults
#' to "AUC", which is the total area under the curve
#' @return an object of class \code{auctrap} with the following elements
#' \describe{
#'   \item{value}{calculated area}
#'   \item{x}{sorted x values}
#'   \item{y}{y values sorted with x}
#'   \item{method}{calculation method}
#' }
#'
#' @examples
#' AUCtrap(0:3, c(3,4,1,2))
#' @export
#' @importFrom utils head tail
#'
AUCtrap <- function(x, y, method=c("AUC", "iAUC","netAUC","minAUC")){

  if (length(x) != length(y)) stop("Input vectors should have the same lengths")

  method <- match.arg(method)

  res <- switch(method,
    AUC = totalAUC(x,y),
    iAUC = iAUC(x,y),
    netAUC = netAUC(x,y),
    minAUC = minAUC(x,y)
  )

  res
}

#' @keywords internal
#' @inheritParams AUCtrap
totalAUC <- function(x,y){
  ord <- order(x)
  xo <- x[ord]
  yo <- y[ord]

  auc <- sum(diff(xo) * (head(yo,-1) + tail(yo, -1))/2)

  auclist <- list(value = auc,
                  x = xo,
                  y = yo,
                  method = "AUC")
  class(auclist) <- "auctrap"
  auclist
}

#' Methods for 'auctrap' class
#' @rdname auctrap_class
#' @param x object of class \code{auctrap}
#' @param digits minimal number of significant digits
#' @export
print.auctrap <- function(x, digits = getOption("digits"), ...){
  cat(format(x$value, digits = digits), "using method", x$method)
  invisible(x)
}

#' @rdname auctrap_class
#' @export
#' @param fill.pos fill color of areas counted positively in the AUC calculation
#' @param fill.neg fill color of areas counted negatively in the AUC calculation
#' @param pch shape of observed data points
plot.auctrap <- function(x, fill.pos="lightblue", fill.neg="pink", pch=1, ...){
  if (x$method == "AUC"){
    ylim <- range(c(x$y, 0))
    plot(x$x, x$y, ylim=ylim, type="n", ...)
    abline(h=0, col="gray")
    polygon(x=c(x$x, max(x$x), x$x[1], x$x[1]),
            y=c(x$y, 0, 0, x$y[1]), col=fill.pos)
    points(x$x,x$y, pch=pch)
    title(main = paste("AUC =", format(x$value, digits = 2)),
          sub = paste("Calculated using the", x$method, "method"))

  }
}


