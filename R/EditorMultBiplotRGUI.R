EditorMultBiplotRGUI <- function(X, ppi=12, GraphWindow=FALSE, Toolkit = "tcltk", ...) {
  options(guiToolkit = Toolkit)

  n = dim(X)[1]
  p = dim(X)[2]

  ChangeTable <- function(...){
    rownames(TableInd)=rownames(Table)
    rownames(TableVar)=names(Table)
  }

  ChangeIndInfo <- function(...){
    rownames(Table)<-TableInd[,1]
  }

  ChangeVarInfo <- function(...){
      names(Table) <- TableVar[,1]
  }

  window <- gwindow("MultBiplotR Data Manager", width = 600, height = 600)
  notebook <- gnotebook(container = window, expand=TRUE)

  # Group for the original data
  DataFrame <- ggroup(container = notebook, label = gettext("Initial Data"), horizontal=FALSE)
  Edit <-gbutton(container = DataFrame, text = "Edit Data", handler=function(...){
    X=Table[,]
    if (Toolkit == "RGtk2") {require(RGtk2Extras)
      X=dfedit(X)}
    else X=edit(X)
    Table[,]=X
  })
  Table <- gdf(X, container = DataFrame)
  if (Toolkit == "RGtk2") size(Table)<-c(500,500)

  IndFrame <- ggroup(container = notebook, label = gettext("Individual's Information"), horizontal = FALSE)

  RowInfo=as.data.frame(rownames(X))
  names(RowInfo)="Individuals"
  RowInfo$Color=rep("blue", n)
  RowInfo$Symbol=rep(1,n)

  EditRowInfo <-gbutton(container = IndFrame, text = "Edit Individuals Info", handler=function(...){
    RI=TableInd[,]
    if (Toolkit == "RGtk2") {require(RGtk2Extras)
      RI=dfedit(RI)}
    else RI=edit(RI)
    TableInd[,]=RI
  })

  TableInd <- gdf(RowInfo, container = IndFrame)
  addHandlerChanged(TableInd, handler=ChangeIndInfo)

  VarFrame <- ggroup(container = notebook, label = gettext("Variable's Information"), horizontal = FALSE)
  Classes=sapply(X,class)
  ColInfo=as.data.frame(Classes)
  ColInfo$Color=rep("black", p)
  ColInfo$Symbol=rep(0,p)
  EditVarInfo <-gbutton(container = VarFrame, text = "Edit Variables Info", handler=function(...){
    CI=TableVar[,]
    if (Toolkit == "RGtk2"){require(RGtk2Extras)
      CI=dfedit(CI)}
    else CI=edit(CI)
    TableVar[,]=CI
  })
  TableVar <- gdf(ColInfo, container = VarFrame)
  addHandlerChanged(TableVar, handler=ChangeVarInfo)

  if (GraphWindow){
  GraphTab = ggroup(container = notebook, label = gettext("Plot"), horizontal = FALSE)
  Grafico<-ggraphics(ps=ppi,container=GraphTab)
  addHandlerClicked ( Grafico , handler = function ( h , ... ) {
    cat ( sprintf ( "You clicked %.2f x %.2f\n" , h$x , h$y ) )
  } )}

  aOpen <-  gaction(label="Open",   icon="open",  handler=NULL)
  aTabDel <-  gaction(label="Tab Delimited",   icon="open",  handler=NULL)
  aSPSS <-  gaction(label="SPSS",   icon="open",  handler=NULL)
  aClose <- gaction(label="Close",  icon="close", handler=NULL)

  aSaveGraph <- gaction(label="Save Grapg",  icon="save", handler=function(h,...){
    setfilename <- paste(gfile(text="Graph.pdf", type="save"), ".pdf", sep="")
    svalue(Grafico, setfilename)
  }
    )

  aQuit  <- gaction(label="Quit",   icon="quit",  handler=function(h,...) dispose(window))
  aCut <-   gaction(label="Cut",    icon="cut",   handler=NULL)
  aCopy <-  gaction(label="Copy",   icon="copy",  handler=NULL)
  aPaste <- gaction(label="Paste",  icon="paste", handler=NULL)
  aAdd <- gaction(label="Add", handler=NULL)

  aEditData<-gaction(label="Edit Data", handler=function(...){
    X=Table[,]
    if (Toolkit == "RGtk2") X=dfedit(X)
    else X=edit(X)
    Table[,]=X
  })

  aColors <- gaction(label="List of Colors", handler=function(...){
    Colores=colors()
    text=capture.output(print(Colores))
    GeneralResults(title="List of Colors", text=text)
  })

  aSummary <- gaction(label="Quick Summary", handler=function(...){
    X=Table[,]
    text=capture.output(summary(X))
    GeneralResults(title="Summary of Data", text=text)
  })


  aExplore <- gaction(label="Univariate Exploration", handler=function(...){
    X=Table[,]
    UnivariateGUI(X, Tab=4, Toolkit = Toolkit)
  })

  aCompare <- gaction(label="Means Comparison", handler=function(...){
    X=Table[,]
    UnivariateGUI(X, Tab=5, Toolkit = Toolkit)
  })

  aScatter <- gaction(label="Univariariate Scattergrams", handler=function(...){
    X=Table[,]
    ScatterGUI(X, Toolkit = Toolkit)
  })

  aMultPlot<- gaction(label="Matrix Plot", handler=function(...){
    X=Table[,]
    Classes=sapply(X,class)
    Numeric=which(Classes=="numeric")
    panel.hist <- function(x, ...){
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(usr[1:2], 0, 1.5) )
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks; nB <- length(breaks)
      y <- h$counts; y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)}

    panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- abs(cor(x, y))
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste0(prefix, txt)
      if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex.cor * r)}

    pairs(X[,Numeric], diag.panel = panel.hist, main="Simple Scatterplot Matrix", upper.panel = panel.cor)
  })

  aManova <- gaction(label="MANOVA", handler=function(...){
    X=Table[,]
    CanonicalBiplotGUI(X, Toolkit = Toolkit)
  })

  aIRTBin <- gaction(label="IRT - Binary Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aIRTOrd <- gaction(label="IRT - Ordinal Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aClassical <- gaction(label="Classical Biplot - PCA", key.accel = "B", handler=function(...){
    X=Table[,]
    PCA.BiplotGUI(X, Toolkit = Toolkit)
  })

  aHJBiplot <- gaction(label="HJ Biplot", key.accel = "H", handler=function(...){
    X=Table[,]
    PCA.BiplotGUI(X, HJ=TRUE, Toolkit = Toolkit)
  })

  aLogisticBin <- gaction(label="Logistic Biplot - Binary Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aLogisticNom <- gaction(label="Logistic Biplot - Nominal Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aLogisticOrd <- gaction(label="Logistic Biplot - Ordinal Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aLogisticMixed <- gaction(label="Logistic Biplot - Mixed Data", handler=function(...){
    gmessage("Will be available soon")
  })


  aFactorBiplot <- gaction(label="Factor Analysis Biplot", key.accel = "F", handler=function(...){
    gmessage("Will be available soon")
  })

  aSimpleCorrespondences <- gaction(label="Simple Corresponedences (Frequencies/Abundances)", handler=function(...){
    gmessage("Will be available soon")
  })

  aMultipleCorrespondences <- gaction(label="Multiple Corresponedences", handler=function(...){
    gmessage("Will be available soon")
  })

  aCanonical <- gaction(label="One-Way Canonical/MANOVA Biplot", handler=function(...){
    X=Table[,]
    CanonicalBiplotGUI(X, Toolkit = Toolkit)
  })

  aCanonical2 <- gaction(label="Two-Way Canonical/MANOVA Biplot", handler=function(...){
    gmessage("Will be available soon")
  })

  aCanonicalGeneral <- gaction(label="General Canonical/MANOVA Biplot", handler=function(...){
    gmessage("Will be available soon")
  })

  aPrincipalBin <- gaction(label="PCoA - Binary Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aPrincipalNom <- gaction(label="PCoA - Nominal Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aPrincipalOrd <- gaction(label="PCoA - Ordinal Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aPrincipalCon <- gaction(label="PCoA - Continuous Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aPrincipalMixed <- gaction(label="PCoA - Mixed Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aMDSBin <- gaction(label="MDS - Binary Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aMDSNom <- gaction(label="MDS - Nominal Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aMDSOrd <- gaction(label="MDS - Ordinal Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aMDSCon <- gaction(label="MDS - Continuous Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aMDSMixed <- gaction(label="MDS - Mixed Data", handler=function(...){
    gmessage("Will be available soon")
  })

  aUnfolSMACOF <- gaction(label="Unfolding - SMACOF", handler=function(...){
    gmessage("Will be available soon")
  })

  aGenefold <- gaction(label="Unfolding - Genefold", handler=function(...){
    gmessage("Will be available soon")
  })

  aRedunContCont <- gaction(label="Continuous/Continuous", handler=function(...){
    gmessage("Will be available soon")
  })

  aRedunBinCont <- gaction(label="Binary/Continuous", handler=function(...){
    gmessage("Will be available soon")
  })

  aCCA <- gaction(label="Canonical Corespondence Analysis", handler=function(...){
    gmessage("Will be available soon")
  })

  aCoinertia <- gaction(label="Coinertia", handler=function(...){
    gmessage("Will be available soon")
  })

  aCanonCorr <- gaction(label="Canonical Correlation Analysis", handler=function(...){
    gmessage("Will be available soon")
  })

  aPLSContCont <- gaction(label="Continuous/Continuous", handler=function(...){
    gmessage("Will be available soon")
  })

  aPLSBinCont <- gaction(label="Binary/Continuous", handler=function(...){
    gmessage("Will be available soon")
  })

  aSTATIS <- gaction(label="STATIS-ACT (with Biplot)", handler=function(...){
    gmessage("Will be available soon")
  })



  ml <- list(File = list(
    open = aOpen,
    Import=list(
      tabdel=aTabDel,
      spss=aSPSS
    ),
    close = aClose,
    Savegr= aSaveGraph,
    sep = list(separator = TRUE), # must be named component
    quit = aQuit),
    Edit = list(
      copy = aCopy,
      paste = aPaste),
    Data=list(Add=aAdd,
              Colors=aColors,
              Data_Edition = aEditData),
    Univariate=list(Summary=aSummary,
                    Explore=aExplore,
                    Compare=aCompare,
                    Scatter=aScatter),
    Multivariate=list(MultPlot=aMultPlot,
                      Manova=aManova,
                      Item_Response_IRT=list(Binary=aIRTBin,
                                             Ordinal=aIRTOrd)
                      ),
    Biplot=list(Classical=aClassical,
                HJBip=aHJBiplot,
                Factor=aFactorBiplot,
                Logistic_Biplot=list(Binary=aLogisticBin,
                                                  Nominal=aLogisticNom,
                                                  Ordinal=aLogisticOrd,
                                                  Mixed=aLogisticMixed),
                Correspondences=list(Simple=aSimpleCorrespondences,
                                     Multiple=aMultipleCorrespondences),
                Canonical_Biplot = list(OneWay=aCanonical,
                                        TwoWay=aCanonical2,
                                        General=aCanonicalGeneral),
                Principal_Coordinates_Biplot=list(Binary=aPrincipalBin,
                                          Nominal=aPrincipalNom,
                                          Ordinal=aPrincipalOrd,
                                          Continuous=aPrincipalCon,
                                          Mixed=aPrincipalMixed),
                MDS_Biplot=list(Binary=aMDSBin,
                                Nominal=aMDSNom,
                                Ordinal=aMDSOrd,
                                Continuous=aMDSCon,
                                Mixed=aMDSMixed),
                Unfolding=list(SMACOF=aUnfolSMACOF,
                               Genefold=aGenefold),
                Two_Table_Ordination=list(Redundancy_Analysis=list(Cont_Cont=aRedunContCont,
                                                                   Bin_Cont=aRedunBinCont),
                                          CCA=aCCA,
                                          Coinertia=aCoinertia,
                                          CanonCorr=aCanonCorr
                                            ),
                PLS_Biplot=list(Cont_Cont=aPLSContCont,
                                Bin_Cont=aPLSBinCont),
                Multitable=list(STATIS=aSTATIS)
    )
  )
  mismenus=gmenu(ml, container = window)
  svalue(notebook) <- 1
}
