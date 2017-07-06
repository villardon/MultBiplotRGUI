
ggcircle <- function(x=0, y=0, r=1, color="black", size = 1/4, alpha = 1) {
theta <- c(seq(-pi, pi, length = 100), seq(pi, -pi, length = 100))
circle <- data.frame(xvar = x + r * cos(theta), yvar = y + r * sin(theta))
g=geom_path(data = circle, aes(x=xvar, y=yvar), color = color, size = size, alpha = 1)
return(g)
}


ggplotConcEllipse <- function(X, confidence=0.95, color="black", size = 1/2, alpha = 1) {
  elip=ConcEllipse(X, confidence=confidence)
  ell=as.data.frame(elip$ellipse)
  names(ell) <- c("xvar", "yvar")
  g=geom_path(data = ell, aes(x=xvar, y=yvar), color = color, size = size, alpha = alpha)
  return(g)
}

ggplotConvHull <- function(X, color="black", size = 1/2, alpha = 1) {
  hpts = chull(X)
  hpts <- c(hpts, hpts[1])
  hpts=as.data.frame(X[hpts,])
  names(hpts) <- c("xvar", "yvar")
  g=geom_path(data = hpts, aes(x=xvar, y=yvar), color = color, size = size, alpha = alpha)
  return(g)
}

ggplotStar <- function(X, centre=c(0, 0),  color="black", size = 1/2, alpha = 1) {
  X=as.data.frame(X)
  n=dim(X)[1]
  names(X) <- c("xvar", "yvar")
  X$c1=rep(centre[1],n)
  X$c2=rep(centre[2],n)
  print(X)
  g=geom_segment(data = X, aes(x = c1, y = c2, xend=xvar, yend=yvar), color = color, size = size, alpha = alpha)
  return(g)
}
