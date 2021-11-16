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
#' @return numeric value with the calculated area
#'
#' @examples
#' AUCtrap(0:3, c(3,4,1,2))
#' @export
#' @importFrom utils head tail
#'
AUCtrap <- function(x, y, method=c("AUC", "iAUC","netAUC","minAUC")){
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
totalAUC <- function(x,y){
  ord <- order(x)
  xo <- x[ord]
  yo <- y[ord]

  auc <- sum(diff(xo) * (head(yo,-1) + tail(yo, -1))/2)
  auc
}
