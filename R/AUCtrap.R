
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

totalAUC <- function(x,y){
  ord <- order(x)
  xo <- x[ord]
  yo <- y[ord]

  auc <- sum(diff(xo) * (head(yo,-1) + tail(yo, -1))/2)
  auc
}
