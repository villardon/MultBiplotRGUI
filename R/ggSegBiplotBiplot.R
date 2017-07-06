ggSegBiplotBiplot <- function(B, b0=NULL, xmin = -3, xmax = 3, ymin = -3, ymax = 3, mode = "a") {
  p=dim(B)[1]
  INI=matrix(0, p, 2)
  FINAL=matrix(0, p, 2)

  for (i in 1:p){
    b1 = B[i,1]/(B[i,1]^2 + B[i,2]^2)
    b2 = B[i,2]/(B[i,1]^2 + B[i,2]^2)
    b = b2/b1
    x1 = xmin
    y1 = b * xmin
    if ((y1 > ymin - 0.001) & (y1 < ymax + 0.001))
      if ((x1 * b1 + y1 * b2) < 0)
        ini = c(x1, y1)
    else {
      final = c(x1, y1)
    }
    x1 = xmax
    y1 = b * xmax
    if ((y1 > ymin - 0.001) & (y1 < ymax + 0.001))
      if ((x1 * b1 + y1 * b2) < 0)
        ini = c(x1, y1)
    else {
      final = c(x1, y1)
    }
    x1 = ymin/b
    y1 = ymin
    if ((x1 > xmin) & (x1 < xmax))
      if ((x1 * b1 + y1 * b2) < 0)
        ini = c(x1, y1)
    else {
      final = c(x1, y1)
    }
    x1 = ymax/b
    y1 = ymax
    if ((x1 > xmin) & (x1 < xmax))
      if ((x1 * b1 + y1 * b2) < 0)
        ini = c(x1, y1)
    else {
      final = c(x1, y1)
    }

    if ((mode=="a") | (mode=="ah") | (mode=="h")) ini=c(0,0)
    if (mode=="a") final=c(B[i,1], B[i,2])

    FINAL[i,]=final
    INI[i,]=ini

  }

  C=as.data.frame(cbind(INI, FINAL))
  colnames(C)=c( "xo", "yo", "xvar", "yvar")
  rownames(C) = rownames(B)
  return(C)
}

