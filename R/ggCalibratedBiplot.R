ggCalibratedBiplot <- function(C, B, b0=NULL, Scales, Colors = NULL,  TypeScale="Complete", tl = 0.03, xmin = -3, xmax = 3, ymin = -3, ymax = 3, varname.adjust=2) {

  p= dim(C)[1]
  if (is.null(b0)) b0 = rep(0,p)
  Coord=list()

  for (i in 1:p){
    ang = atan(B[i,2]/B[i,1]) * 180/pi
    ticks=Scales$Ticks[[i]]
    ticklabels=Scales$Labels[[i]]
    k = length(ticks)
    M = cbind((ticks - b0[i]) * B[i,1], (ticks - b0[i]) * B[i,2])
    deltax <- tl * sin(ang * pi/180)
    deltay <- tl * cos(ang * pi/180)
    Mn <- cbind(M[, 1] + deltax, M[, 2] - deltay)
    IsIn=logical()
    for (j in 1:k)
      IsIn[j] = InBox(M[j, 1], M[j, 2], xmin, xmax, ymin, ymax)

    M=M[IsIn,]
    Mn=Mn[IsIn,]
    ticklabels=ticklabels[IsIn]
    Coord[[i]]=as.data.frame(cbind(M,Mn))
    rownames(Coord[[i]])=ticklabels
    colnames(Coord[[i]])=c( "xo", "yo", "xvar", "yvar")
    Coord[[i]]$Labels=as.character(ticklabels)
  }

  #   if (TypeScale == "BoxPlot") {
  #     points(M[3, 1], M[3, 2], pch = 16, col = Color, ...)
  #     lines(rbind(M[2, ], M[4, ]), col = Color, lwd = 2, lty = 1, ...)
  #   }
  #
  #
  #
  # if (Position == 'Angle'){
  #   angle=atan(bi2/bi1) * 180/pi
  #   if (bi1 < 0)
  #     markerpos = 2
  #   else markerpos = 4
  #  }
  names(Coord)=rownames(B)

  return(Coord)
}
