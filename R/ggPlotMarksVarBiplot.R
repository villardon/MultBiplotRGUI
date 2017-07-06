ggPlotMarksVarBiplot <- function(Calibration, size=0.2, color="black"){
  g= geom_segment(data = Calibration, aes(x = xo, y = yo, xend=xvar, yend=yvar), size =size)
}
