GeneralTable <- function(Title="General Table", Data=matrix(1, 10,3), Toolkit = "tcltk", ...) {
  options(guiToolkit = Toolkit)
  window <- gwindow(Title, width = 800, height = 300)
  Container <- ggroup(container = window, label = gettext("Data"), horizontal = FALSE, expand=TRUE)
  Table <- gdf(Data, container = Container, expand = TRUE, width = 600, height = 300)

  SaveResButt <- gbutton("Save Table", container = Container, handler = function(h,...){
    setfilename <- paste(gfile(text="table.rda", type="save"), ".rda", sep="")
    save(Table[,], file=setfilename)
  })

  #font(SaveResButt) <- list(family="times",size=12, weight="bold", color="red")
}
