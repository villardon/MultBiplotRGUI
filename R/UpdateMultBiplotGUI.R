UpdateMultBiplotGUI <- function(){
  update.packages(c("gWidgets", "gWidgetsRGtk2", "RGtk2", "cairoDevice", "foreign", "rattle", "ggrepel", "Hmisc" ))
  install.packages("http://biplot.usal.es/classicalbiplot/multbiplot-in-r/multbiplotrgui.gz", repos = NULL, type="source")
}
