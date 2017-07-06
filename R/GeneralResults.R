GeneralResults <- function(title="Analysis Results", text="Results", Toolkit = "tcltk", ...) {
  options(guiToolkit = Toolkit)

  if (class(text)=="character")
    text=list(Analysis=text)

  ntabs=length(text)
  Tabs=list()
  Names=names(text)
  Texts=list()

  window <- gwindow(title, width = 900, height = 600)
  notebook <- gnotebook(container = window, Expand=TRUE)
  texto=character()
  # Group for the original data
  for (i in 1:ntabs){
    Tabs[[i]] <- ggroup(container = notebook, label = Names[i], horizontal = FALSE)
    Texts[[i]] <- gtext("", container=Tabs[[i]], font.attr =c(sizes="small"), width = 800, height = 600)
    insert(Texts[[i]],text[[i]], font.attr=c(family="monospace", sizes="small"))

    texto=c(texto, text[[i]])
  }

  aQuit  <- gaction(label="Quit",   icon="quit",  handler=function(h,...) dispose(window))
  aCut <-   gaction(label="Cut",    icon="cut",   handler=NULL)
  aCopy <-  gaction(label="Copy",   icon="copy",  handler=NULL)
  aPaste <- gaction(label="Paste",  icon="paste", handler=NULL)

  aSave<- gaction(label="Save Results",  icon="save", handler=function(h,...){
    setfilename <- paste(gfile(text="Results.txt", type="save"), ".txt", sep="")
    write(svalue(texto), file=setfilename)
  }
  )

  ml <- list(File = list(
    Savegr= aSave,
    sep = list(separator = TRUE), # must be named component
    quit = aQuit),
    Edit = list(
      cut= aCut,
      copy = aCopy,
      paste = aPaste)
  )
  mismenus=gmenu(ml, container = window)




  # SaveResButt <- gbutton("Save Results", container = window, handler = function(h,...){
  #   setfilename <- paste(gfile(text="Results.txt", type="save"), ".txt", sep="")
  #   write(svalue(texto), file=setfilename)
  # })
}
