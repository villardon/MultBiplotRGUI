ggTextMarksVarBiplot <- function(Calibration, size=3, color="black", angle = 0, hjust = 0.5, vjust=1){
  g= geom_text(data = Calibration, aes(label=Labels, xend=xvar, yend=yvar), size =size, color=color, angle = angle, hjust = hjust, vjust=vjust)
}
