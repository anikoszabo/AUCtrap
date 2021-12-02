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
#' @param ... further arguments passed to methods
#' @export
AUCtrap <- function(x,...){
  UseMethod("AUCtrap")
}

#' @name AUCtrap
#' @param y numeric vector with the measurements corresponding to \code{x}
#' @param method character value defining the calculation method. Defaults
#' to "AUC", which is the total area under the curve
#'
#'
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

AUCtrap.default <- function(x, y, method=c("AUC", "iAUC","netAUC","minAUC"),...){

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

#' @name AUCtrap
#' @param formula a formula of the form `y ~ x` where `y` is the measured response
#' and `x` are the measurement times
#' @param data an optional matrix or data frame containing the variables in
#' `formula`. By default, the variables are taken from `environment(formula)`.
#' @param subset an optional vector specifying a subset of observations to be taken
#' @param na.action a function which indicates what should happen when the data
#' contain `NA`s. Defaults to `getOption("na.action")`.
#' @export
AUCtrap.formula <- function(formula, data, subset, na.action, ...){
  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  if (ncol(mf) != 2L)
    stop("The formula should have the form y~x with one term on each side")
  response <- attr(attr(mf, "terms"), "response")
  y <- mf[[response]]
  x <- mf[[-response]]
  if (!is.vector(x, mode="numeric"))
    stop("The predictor should be a single numeric vector")

  res <- AUCtrap.default(x=x, y=y, ...)
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

#' @keywords internal
#' @inheritParams AUCtrap
iAUC <- function(x,y) {

  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  auc<-c()
  for (i in 2:length(x)) {
    if (y[i] >= y[1] & y[i-1] >= y[1]) {
      auc[i-1] <- (((y[i]-y[1])/2) + (y[i-1]-y[1])/2) * (x[i]-x[i-1])

    } else if (y[i] >= y[1] & y[i-1] < y[1]) {
      auc[i-1] <- ((y[i]-y[1])^2/(y[i]-y[i-1])) * (x[i]-x[i-1])/2

    }  else if (y[i] < y[1] & y[i-1] <= y[1]) {
      auc[i-1] <- 0

    } else if (y[i] < y[1] & y[i-1] > y[1]) {
      auc[i-1] <- ((y[i-1]-y[1])^2/(y[i-1]-y[i])) * (x[i]-x[i-1])/2

    }
  }
  auclist <-list(value=sum(auc),x=x, y=y, method="iAUC")
  class(auclist) <- "auctrap"
  auclist

}


#' @keywords internal
#' @inheritParams AUCtrap

netAUC <- function(x,y){
  ord <- order(x)
  xo <- x[ord]
  yo <- y[ord]

  auc <- sum(diff(xo) * (head(yo,-1) + tail(yo, -1))/2)-y[1]*(xo[length(x)]-xo[1])

  auclist <- list(value = auc,
                  x = xo,
                  y = yo,
                  method = "netAUC")
  class(auclist) <- "auctrap"
  auclist
}


#' Methods for 'auctrap' class
#' @rdname auctrap_class
#' @name auctrap_class
#' @param x object of class \code{auctrap}
#' @param digits minimal number of significant digits
#' @param ... for `plot.auctrap` graphical option arguments passed to `plot`;
#' not used for `print.auctrap`.
#' @export
print.auctrap <- function(x, digits = getOption("digits"), ...){
  cat(format(x$value, digits = digits), "using method", x$method)
  invisible(x)
}

#' @name auctrap_class
#' @export
#' @param fill.pos fill color of areas counted positively in the AUC calculation
#' @param fill.neg fill color of areas counted negatively in the AUC calculation
#' @param pch shape of observed data points
#' @importFrom graphics abline points polygon title lines
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
  } else if (x$method == "iAUC"){
    p=x$x
    q=x$y
    ylim <- range(c(q, 0))
    plot(p, q,ylim=ylim, type="n",...)
    abline(h=q[1], col="gray")
    for (i in 2:length(p)) {
      if (q[i] >= q[1] & q[i-1] >= q[1]) {
        polygon(x=c(p[i-1],p[i] , p[i], p[i-1],p[i-1]),
                y=c(q[i-1], q[i], q[1], q[1],q[i-1]), col=fill.pos,border = NA)
      } else if (q[i] >= q[1] & q[i-1] < q[1]) {
        intercect=(p[i]-p[i-1])*(q[1]-q[i])/(q[i]-q[i-1])+p[i]
        polygon(x=c(intercect,p[i] , p[i], intercect),
                y=c(q[1], q[i], q[1], q[1]), col=fill.pos,border = NA)

      } else if (q[i] < q[1] & q[i-1] >= q[1]) {
        intercect=(p[i]-p[i-1])*(q[1]-q[i])/(q[i]-q[i-1])+p[i]
        polygon(x=c(p[i-1], intercect,p[i-1] , p[i-1]),
                y=c(q[i-1],q[1], q[1], q[i-1]), col=fill.pos,border = NA)
      }
    }
    lines(p,q,type="l")
    points(p,q,pch=pch)
    title(main = paste("Incremental AUC =", format(x$value, digits = 2)),
          sub = paste("Calculated using the", x$method, "method"))
  } else if (x$method == "netAUC"){
    p=x$x
    q=x$y
    ylim <- range(c(q, 0))
    plot(p, q,ylim=ylim, type="n",...)
    abline(h=q[1], col="gray")
    for (i in 2:length(p)) {
      if (q[i] >= q[1] & q[i-1] >= q[1]) {
        polygon(x=c(p[i-1],p[i] , p[i], p[i-1],p[i-1]),
                y=c(q[i-1], q[i], q[1], q[1],q[i-1]), col=fill.pos,border = NA)
      } else if (q[i] >= q[1] & q[i-1] < q[1]) {
        intercect=(p[i]-p[i-1])*(q[1]-q[i])/(q[i]-q[i-1])+p[i]
        polygon(x=c(intercect,p[i] , p[i], intercect),
                y=c(q[1], q[i], q[1], q[1]), col=fill.pos,border = NA)
        polygon(x=c(p[i-1],intercect,q[i-1],q[i-1]),
                y=c(q[1],q[1],p[i-1],q[1]), col=fill.neg,border = NA)

      } else if (q[i] < q[1] & q[i-1] >= q[1]) {
        intercect=(p[i]-p[i-1])*(q[1]-q[i])/(q[i]-q[i-1])+p[i]
        polygon(x=c(p[i-1], intercect,p[i-1] , p[i-1]),
                y=c(q[i-1],q[1], q[1], q[i-1]), col=fill.pos,border = NA)
        polygon(x=c(intercect,p[i],p[i],intercect),
                y=c(q[1],q[1],q[i],q[1]), col=fill.neg,border = NA)
      }else if (q[i] < q[1] & q[i-1] < q[1]){
        polygon(x=c(p[i-1],p[i] , p[i], p[i-1],p[i-1]),
                y=c(q[i-1], q[i], q[1], q[1],q[i-1]), col=fill.neg,border = NA)
      }
    }
    lines(p,q,type="l")
    points(p,q,pch=pch)
    title(main = paste("net AUC =", format(x$value, digits = 2)),
          sub = paste("Calculated using the", x$method, "method"))
  } else if (x$method == "minAUC"){
    stop("Plotting for this method is not implemented yet")
  }
}



