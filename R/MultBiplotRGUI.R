MultBiplotRGUI <- function(Toolkit = "tcltk", ...) {


  options(guiToolkit = Toolkit)

  FillData = gaction(label="Open R Data", Toolkit=Toolkit, handler =function(X, ...){
    EditorMultBiplotRGUI(X, Toolkit=Toolkit)
  	})

  window <- gwindow("MultBiplotR", width = 600, height = 600)
  notebook <- gnotebook(container = window, Expand=TRUE)


  GraphTab = ggroup(container = notebook, label = gettext("MultBiplot"), horizontal = FALSE)
  ls=gseparator(horizontal = TRUE, container = GraphTab)
  lb=glabel("MultBiplotR", container = GraphTab)
  font(lb) <- list(family="serif",size=60, color="red")
  if (Toolkit == "RGtk2") size(lb) <- c(490, 100)
  ls1=gseparator(horizontal = TRUE, container = GraphTab)
  if (Toolkit == "RGtk2") size(ls1) <- c(490, 50)
  lb=glabel("Multivariate Analysis using Biplots (with R)", container = GraphTab)
  font(lb) <- list(family="sans",size=20, style="italic", color="green")
  if (Toolkit == "RGtk2") size(lb) <- c(490, 100)
  ls2=gseparator(horizontal = TRUE, container = GraphTab)
  if (Toolkit == "RGtk2") size(ls2) <- c(490, 50)

  # Grafico<-ggraphics(container=GraphTab)
  # data(geometry)
  # grid::grid.raster(geometry)

  WorkSpaceTab = ggroup(container = notebook, label = gettext("WorkSpace"), horizontal = FALSE)

  ReadSystem = gframe(text = "Double click to use a frame from the workspace", container = WorkSpaceTab)
  size(ReadSystem) <- c(500, 400)
  workspace_browser <- gvarbrowser(container=ReadSystem, handler=function(h, ...){
    EditorMultBiplotRGUI(X, Toolkit=Toolkit)
  })

    #ReadSystem = glabel(text = "No var bowser fot tcltk toolkit. Use EditorMultBiplotRGUI(data) on the command line", container = WorkSpaceTab)

  OpenTab = ggroup(container = notebook, label = gettext("Open"), horizontal = FALSE)
  ReadFileFrame = gframe(text = "Use a file containing R data", markup = FALSE, horizontal = FALSE, container = OpenTab)
  aOpenR <-  gaction(label="Open R Data", Toolkit=Toolkit,   icon="open",  handler = function(h,...){
    file <- gfile("Open an R data file",
                  type="open" ,
                  filter=list ("R data file" = list(
                    patterns = c("*.rda" , "*.Rda" , "*.RDA"  , "*.Rdata")
                  ),
                  "All files" = list(patterns = c("*"))
                  ))
    if ( !is.na(file)){
      xx=load(file)
      eval(parse(text=paste("X =",as.character(xx))))
      EditorMultBiplotRGUI(X, Toolkit=Toolkit, GraphWindow=FALSE)}
  })

  ReadButt <- gbutton(action=aOpenR, container = ReadFileFrame)

  ImportFrame = ggroup(container = notebook, label = gettext("Import"), horizontal = FALSE)

  ImportTabFrame = gframe(text = "Tab Delimited Data", markup = FALSE, horizontal = FALSE, container = ImportFrame)
  Header <- gcheckbox("The text file has a Header", container = ImportTabFrame)
  svalue(Header) <- TRUE

  tbl3 = glayout(container = ImportTabFrame)
  size(tbl3) <- c(400, 30)
  tbl3[1, 1] <- "The text file has row labels in column: "
  tbl3[1, 2] <- (Rowlabels <- gspinbutton(from = 0, to = 40000, by = 1, value = 1, container = tbl3))

  aOpenTab <-  gaction(label="Import Tab Delimited Data",   icon="open",  handler = function(h,...){
    file <- gfile("Open text data file",
                  type="open" ,
                  filter=list ("R data file" = list(
                    patterns = c("*.txt" , "*.dat")
                  ),
                  "All files" = list(patterns = c("*"))
                  ))
    if ( !is.na(file)){
      Head=svalue(Header)
      RL=svalue(Rowlabels, index=TRUE)
      if (RL>0)
        X=read.table(file, header=Head, row.names = RL)
      else
        X=read.table(file, header=Head)
      EditorMultBiplotRGUI(X, Toolkit=Toolkit)}
  })
  ReadButt <- gbutton(action=aOpenTab, container = ImportTabFrame)

  ImportSPSSFrame = gframe(text = "SPSS Data", markup = FALSE, horizontal = FALSE, container = ImportFrame)
  aOpenSPSS <-  gaction(label="Import SPSS Data",   icon="open",  handler = function(h,...){
    file <- gfile("Open SPSS file",
                  type="open" ,
                  filter=list ("SPSS data file" = list(
                    patterns = c("*.sav")
                  ),
                  "All files" = list(patterns = c("*"))
                  ))
    if ( !is.na(file)){
      X=read.spss(file, to.data.frame=TRUE)
      EditorMultBiplotRGUI(X, Toolkit=Toolkit)
      }
  })

  SPSSButt <- gbutton(action=aOpenSPSS, container = ImportSPSSFrame)

  ImportExcelFrame = gframe(text = "Excel Data", markup = FALSE, horizontal = FALSE, container = ImportFrame)
  aOpenExcel <-  gaction(label="Import Excel Data",   icon="open",  handler = function(h,...){
    file <- gfile("Open Excel file",
                  type="open" ,
                  filter=list ("Ecel data file" = list(
                    patterns = c("*.xls", "*.xlsx")
                  ),
                  "All files" = list(patterns = c("*"))
                  ))
    if ( !is.na(file)){
      X=read.xls(file)
      EditorMultBiplotRGUI(X, Toolkit=Toolkit)}
  })

  SPSSButt <- gbutton(action=aOpenExcel, container = ImportExcelFrame)

  svalue(notebook) <- 1
}
