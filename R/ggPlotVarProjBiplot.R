ggPlotVarProjBiplot <- function(d, A , size=0.2, color="black"){
  d=as.numeric(d)
  nn = t(d) %*% d
  scal <- (A %*% d)/nn[1, 1]
  Dscal <- diag(as.vector(scal))
  Fpr <- Dscal %*% matrix(rep(1, nrow(A)), ncol = 1) %*% t(d)
  nrFpr <- nrow(Fpr)
  Data=as.data.frame(cbind(A,Fpr))
  colnames(Data)=c( "xo", "yo", "xvar", "yvar")
  p = geom_segment(data = Data, aes(x = xo, y = yo, xend=xvar, yend=yvar), size =size, linetype=3, color=color)
  return(p)
}

