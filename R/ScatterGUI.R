ScatterGUI <- function(X,Toolkit = "tcltk", ...) {
  options(guiToolkit = Toolkit)

  DoExplore <- function(...){
    XX= X2[,svalue(CS, index=TRUE)]

    FacSel=svalue(CS1, index=TRUE)
    if (length(svalue(CS1, index=TRUE))==1)
    { groups=X3[,svalue(CS1, index=TRUE)]}
    else {
      if (length(svalue(CS1, index=TRUE))==0)
      {groups=as.factor(rep(1, dim(XX)[1]))
      levels(groups)<- "Complete Sample"}
      else
        groups=as.factor(paste(X3[,FacSel[1]],X3[,FacSel[2]], sep="-"))
    }

    if (svalue(rb, index=TRUE) ==1) SortByGroups=TRUE
    else SortByGroups=FALSE

    if (svalue(Descrip)){
      DESC=BasicDescription(XX,groups, SortByGroups=SortByGroups, Intervals=svalue(Interval))
      txt=capture.output(DESC)
      GeneralResults(title="Descriptive Exploration", text=txt, Toolkit=Toolkit)}

    if (svalue(StemandLeaf)){
      StPlots=StemPlots(XX,groups, SortByGroups=SortByGroups)
      txt2=capture.output(StPlots)
      GeneralResults(title="Descriptive Exploration", text=txt2, Toolkit=Toolkit)}

    if (svalue(Boxplot)){
      if (svalue(rb3, index=TRUE)==1)
        GroupsTogether=TRUE
      else
        GroupsTogether=FALSE
      print(svalue(Nrows))
      BoxPlotPanel(XX, groups=groups, nrows=svalue(Nrows), panel=svalue(Panel), notch=svalue(Notched), GroupsTogether=GroupsTogether)
    }

    if (svalue(Errplot)){
      if (svalue(rb3, index=TRUE)==1)
        GroupsTogether=TRUE
      else
        GroupsTogether=FALSE
      ErrorBarPlotPanel(XX, groups=groups, nrows=svalue(Nrows), panel=svalue(Panel), GroupsTogether=GroupsTogether )
    }

  }


  HandleFactors <- function(...){
    Selected = svalue(CS1, index=TRUE)
    if (length(Selected)>2){
      svalue(CS1, index=TRUE)=Selected[1:2]
      gmessage("No more than two factor can be selected at a time")
    }
  }



  TitWin="Scattergrams"


  window <- gwindow(TitWin, width = 600, height = 600)
  notebook <- gnotebook(container = window, expand=TRUE)


  # ----------------- Data -------------------------------------
  DataFrame <- ggroup(container = notebook, label = gettext("Numeric Data"), horizontal = FALSE)
  Classes=sapply(X,class)
  Numeric=which(Classes=="numeric")
  X2=X[,Numeric]
  Table <- gdf(X2, container = DataFrame, expand = TRUE)


  # ---------------- Controls the active elements for the construction of the biplot --------------

  DepCols <- function(...){
    Selected=rep(FALSE, dim(X2)[2])
    Selected[svalue(CS2, index=TRUE)]=TRUE
    Selected[svalue(CS, index=TRUE)]=FALSE
    svalue(CS2)=Selected
  }

  IndepCols <- function(...){
    Selected=rep(FALSE, dim(X2)[2])
    Selected[svalue(CS, index=TRUE)]=TRUE
    Selected[svalue(CS2, index=TRUE)]=FALSE
    svalue(CS)=Selected
  }


  VarsFrame <- ggroup(container = notebook, label = gettext("Variables"), horizontal = TRUE)

  # ----------------- Variables selected for the analysis -------------------------------------
  DepFrame = gframe(text = "Dependent", markup = FALSE, horizontal = FALSE, container = VarsFrame)
  CS <- gcheckboxgroup(colnames(X2), container = DepFrame, use.table = TRUE, handler = DepCols)
  svalue(CS) = rep(FALSE, dim(X2)[2])
  size(CS) <- c(220, 400)

  # ---------------- Controls the supplementary elements for the construction of the biplot --------------
  IndepFrame = gframe(text = "Independent", markup = FALSE, horizontal = FALSE, container = VarsFrame)
  CS2 <- gcheckboxgroup(colnames(X2), container = IndepFrame, use.table = TRUE, handler = IndepCols)
  svalue(CS2) = rep(FALSE, dim(X2)[2])
  size(CS2) <- c(220, 400)

  # ----------------- Descriptive Exploration -------------------------------------
  AnalysisDef <- ggroup(container = notebook, label = gettext("Scatter Plots"), horizontal = FALSE)
  StatFrame = gframe(text = "Statistics", markup = FALSE, horizontal = FALSE, container = AnalysisDef)
  gp1 <- ggroup(container=StatFrame)
  Correlations <- gcheckbox("Correlations   ", container = gp1)
  svalue(Correlations) <- TRUE
  Regressions <- gcheckbox("Simple Regressions   ", container = gp1)
  svalue(Regressions) <- TRUE
  sorts <- c("Sort by Dependents", "Sort by Independents")
  gp <- ggroup(container=StatFrame)
  glabel("Organize output:",container=gp, anchor=c(0,1))
  rb <- gradio(sorts, container=gp)
  svalue(rb, index = TRUE) <- 2


  DescPlotFrame = gframe(text = "Plots Panels", markup = FALSE, horizontal = FALSE, container = AnalysisDef)
  Scatter <- gcheckbox("Simple Scattergram", container = DescPlotFrame)
  svalue(Scatter) <- FALSE
  ScatterLin <- gcheckbox("Scattergram with Linear Fit", container = DescPlotFrame)
  svalue(ScatterLin) <- FALSE
  ScatterLoess <- gcheckbox("Scattergram with LOWESS (LOESS)", container = DescPlotFrame)
  svalue(ScatterLoess) <- FALSE
  ScatterKernel <- gcheckbox("Scattergram with kernell Smoothing", container = DescPlotFrame)
  svalue(ScatterKernel) <- FALSE
  ScatterSplines <- gcheckbox("Scattergram with Cubic Splines", container = DescPlotFrame)
  svalue(ScatterSplines) <- FALSE

  LoessPlotFrame = gframe(text = "LOESS Parameters", markup = FALSE, horizontal =FALSE , container = AnalysisDef)
  T1 = ggroup(container = LoessPlotFrame , horizontal =TRUE)
  glabel(text="Degree of Smoothing (smoothness parameter) ", container = T1)
  Span <- gspinbutton( from = 0, to = 1, by = 0.05, value = 0.5, container = T1)
  svalue(Span) <- 0.5
  T2 = ggroup(container = LoessPlotFrame , horizontal =TRUE)
  glabel(text="Degree of Local Polinomial ", container = T2)
  Degree <- gspinbutton( from = 0, to = 10, by = 1, value = 1, container = T2)
  svalue(Degree) <- 1
  T3 = ggroup(container = LoessPlotFrame , horizontal =TRUE)
  glabel(text="Family ", container = T3)
  families = c("Gaussian", "Symmetric")
  Family <- gcombobox(families, editable = FALSE, container = T3)

  KernelFrame = gframe(text = "Kernell Smoothing Parameters (KernSmooth package)", markup = FALSE, horizontal =FALSE , container = AnalysisDef)
  T1k = ggroup(container = KernelFrame , horizontal =TRUE)
  glabel(text="Band Width (smoothness parameter) ", container = T1k)
  BandWidth <- gspinbutton( from = 0, to = 1, by = 0.05, value = 0.5, container = T1k)
  svalue(BandWidth) <- 0.5
  T2k = ggroup(container = KernelFrame , horizontal =TRUE)
  glabel(text="Degree of Local Polinomial ", container = T2k)
  DegreeK <- gspinbutton( from = 0, to = 10, by = 1, value = 1, container = T2k)
  svalue(DegreeK) <- 1
  T3k = ggroup(container = KernelFrame , horizontal =TRUE)
  glabel(text="Kernel  ", container = T3k)
  kernels = c("normal", "box", "epanech", "biweight", "triweight")
  Kelnel <- gcombobox(kernels, editable = FALSE, container = T3k)

  RunFrame <- ggroup(container = notebook, label = gettext("Run Analysis"), horizontal = FALSE)
  PanelFrame = gframe(text = "Number of rows in panels", markup = FALSE, horizontal = TRUE, container = RunFrame)
  Nrows <- gspinbutton(from = 1, to = 10, by = 1, value = 3, container = RunFrame)
  svalue(Nrows) <- 4

  RunButt <- gbutton("Run", container = RunFrame, handler = DoExplore)
  font(RunButt) <- list(family="times",size=18, weight="bold",style="italic", color="red")
  size(RunButt) <- c(100, 50)

  svalue(notebook) <- 2
}
