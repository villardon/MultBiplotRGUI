UnivariateGUI <- function(X, Tab= 4, Toolkit = "tcltk", ...) {
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

  DoCompare <- function(...){

    require(car)

    Results=list()
    ResultsNames=character()
    k=0

    XX= X2[,svalue(CS, index=TRUE)]
    FacSel=svalue(CS1, index=TRUE)
    if (length(svalue(CS1, index=TRUE))==1)
    { groups=X3[,svalue(CS1, index=TRUE)]}
    else {
      if (length(svalue(CS1, index=TRUE))==0)
      {groups=as.factor(rep(1, dim(XX)[1]))
      levels(groups)<- "Complete Sample"}
      else
        groups=as.factor(paste(X3[,FacSel[1]],X3[,FacSel[1]], sep="-"))
    }

    if (svalue(NormTest)){
    txt=capture.output(cat("\n###### NORMALITY TESTS  #######\n\n"))
    txt2=capture.output(NormalityTests(XX, groups, plot=svalue(NormPlot)))
    txt=c(txt, txt2)
    k=k+1
    Results[[k]]=txt
    ResultsNames[k]="Normality"
    }

    if (svalue(BartTest) | svalue(LevTest)){
    txt=capture.output(cat("\n###### EQUALITY OF VARIANCES  #######\n"))
    if (svalue(LevTest)){
    txt=c(txt, capture.output(cat("\nLevene's Test\n")))
    txt2=capture.output(Levene.Tests(XX, groups))
    txt=c(txt, txt2)}

    if (svalue(BartTest)){
    txt=c(txt, capture.output(cat("\nBartlett's Test\n")))
    txt2=capture.output(Bartlett.Tests(XX, groups))
    txt=c(txt, txt2)}
    k=k+1
    Results[[k]]=txt
    ResultsNames[k]="Variances Equality"
    }

    if (svalue(ParamTest)){
      txt=capture.output(cat("\n\n**************** PARAMETRIC TESTS **************** \n"))
      txt2=ANOVA.Tests(XX, groups, posthoc=svalue(PHT), alternative="two.sided")
      txt=c(txt, txt2)
      k=k+1
      Results[[k]]=txt
      ResultsNames[k]="Parametric Comparisons"
      }

    if (svalue(NonParamTest)){
      txt=capture.output(cat("\n\n**************** NON-PARAMETRIC TESTS **************** \n"))
      txt2=Kruskal.Wallis.Tests(XX, groups, posthoc=svalue(PHT2))
      txt=c(txt, txt2)
      k=k+1
      Results[[k]]=txt
      ResultsNames[k]="Non-Parametric Comparisons"}

      names(Results)=ResultsNames
      GeneralResults(title="Compare Groups", text=Results, Toolkit=Toolkit)

      if (svalue(Errplot2)){
        ErrorBarPlotPanel(XX, groups=groups, nrows=svalue(Nrows), panel=T, GroupsTogether=TRUE, p.adjust.method=svalue(adjpgraph), UseANOVA=TRUE)
      }
  }

  HandleFactors <- function(...){
    Selected = svalue(CS1, index=TRUE)
    if (length(Selected)>2){
      svalue(CS1, index=TRUE)=Selected[1:2]
      gmessage("No more than two factor can be selected at a time")
    }
  }

  if (Tab==4)
    TitWin="Descriptive Exploration"
  else
    TitWin="Means Comparison"

  window <- gwindow(TitWin, width = 600, height = 600)
  notebook <- gnotebook(container = window, expand=TRUE)


  # ----------------- Data -------------------------------------
  DataFrame <- ggroup(container = notebook, label = gettext("Numeric Data"), horizontal = FALSE)
  Classes=sapply(X,class)
  Numeric=which(Classes=="numeric")
  X2=X[,Numeric]
  Table <- gdf(X2, container = DataFrame, expand = TRUE)

  # ----------------- External Data -------------------------------------
  NoNumeric=which(Classes!="numeric")
  if (length(NoNumeric)>0){
    SupDataFrame <- ggroup(container = notebook, label = gettext("Categorical Data"), horizontal = FALSE)
    X3=as.data.frame(X[,NoNumeric])
    colnames(X3) <-colnames(X)[NoNumeric]
    Table <- gdf(X3, container = SupDataFrame, expand = TRUE)
  }

  # ----------------- Variables selected for the analysis -------------------------------------
  ActiveFrame <- ggroup(container = notebook, label = gettext("Variables"), horizontal = TRUE)
  TS = gframe(text="Numeric Variables", markup = FALSE, horizontal = FALSE, container = ActiveFrame)

  CS <- gcheckboxgroup(colnames(X2), container = TS, use.table = FALSE)
  if (Toolkit == "RGtk2") size(CS) <- c(220, 400)
  svalue(CS) = rep(TRUE, dim(X2)[2])

  if (length(NoNumeric)>0){
    TS1 = gframe(text="Categorical Variables", markup = FALSE, horizontal = FALSE, container = ActiveFrame)
    CS1 <- gcheckboxgroup(colnames(X3), container = TS1, use.table = FALSE, handler=HandleFactors)
    if (Toolkit == "RGtk2") size(CS1) <- c(220, 180)
    Sel=rep(FALSE, dim(X3)[2])
    Sel[1]=TRUE
    svalue(CS1) = Sel
  }

  # ----------------- Descriptive Exploration -------------------------------------
  AnalysisDef <- ggroup(container = notebook, label = gettext("Explore"), horizontal = FALSE)
  StatFrame = gframe(text = "Statistics", markup = FALSE, horizontal = FALSE, container = AnalysisDef)
  gp1 <- ggroup(container=StatFrame)
  Descrip <- gcheckbox("Descriptives   ", container = gp1)
  svalue(Descrip) <- TRUE
  Interval <- gcheckbox("Confidence intervals", container = gp1)
  svalue(Interval) <- TRUE
  sorts <- c("Sort by Groups", "Sort by Variables")
  gp <- ggroup(container=StatFrame)
  glabel("Organize output:",container=gp, anchor=c(0,1))
  rb <- gradio(sorts, container=gp)
  svalue(rb, index = TRUE) <- 2

  PlotFrame = gframe(text = "Plots", markup = FALSE, horizontal = FALSE, container = AnalysisDef)
  DescPlotFrame = gframe(text = "Descriptive", markup = FALSE, horizontal = TRUE, container = PlotFrame)
  StemandLeaf <- gcheckbox("Stem-and-leaf", container = DescPlotFrame)
  svalue(StemandLeaf) <- FALSE
  Histog <- gcheckbox("Histogram", container = DescPlotFrame)
  svalue(Histog) <- FALSE

  BoxPlotFrame = gframe(text = "Box Plot", markup = FALSE, horizontal = FALSE, container = PlotFrame)
  Boxplot <- gcheckbox("Draw Box PLot    ", container = BoxPlotFrame)
  svalue(Boxplot) <- TRUE
  Notched <- gcheckbox(" Use Notched Box-Plots", container = BoxPlotFrame)
  svalue(Notched) <- FALSE

  ErrorPlotFrame = gframe(text = "Error Bar (Confidence Interval)", markup = FALSE, horizontal = FALSE, container = PlotFrame)
  Errplot <- gcheckbox("Draw Error Bar Plot", container = ErrorPlotFrame)
  svalue(Errplot) <- TRUE

  OrganizeFrame = gframe(text = "Organize Oupput", markup = FALSE, horizontal = FALSE, container = PlotFrame)
  vars <- c("Groups Togeteher", "Variables Togeteher")

  rb3 <- gradio(vars, container=OrganizeFrame)
  Panel <- gcheckbox("Panel (or Separate Plots)", container = OrganizeFrame)
  svalue(Panel) <- TRUE

  PanelFrame = gframe(text = "Number of rows in the panel", markup = FALSE, horizontal = TRUE, container = PlotFrame)
  Nrows <- gspinbutton(from = 1, to = 10, by = 1, value = 3, container = PanelFrame)
  svalue(Nrows) <- 3

  RunButt <- gbutton("Run", container = AnalysisDef, handler = DoExplore)
  font(RunButt) <- list(family="times",size=18, weight="bold",style="italic", color="red")
  size(RunButt) <- c(100, 50)

  # ----------------- Means Comparison -------------------------------------
  CompareDef <- ggroup(container = notebook, label = gettext("Compare"), horizontal = FALSE)
  NormalityFrame = gframe(text = "Normality Tests", markup = FALSE, horizontal = FALSE, container = CompareDef)
  NormTest <- gcheckbox("Shapiro-Wilk", container = NormalityFrame)
  NormPlot <- gcheckbox("Normality plots", container = NormalityFrame)
  svalue(NormTest) <- TRUE

  VarianceFrame = gframe(text = "Variance Equality Tests", markup = FALSE, horizontal = FALSE, container = CompareDef)
  BartTest <- gcheckbox("Bartlett", container = VarianceFrame)
  svalue(BartTest) <- TRUE
  LevTest <- gcheckbox("Levene", container = VarianceFrame)
  svalue(LevTest) <- TRUE

  MeansFrame = gframe(text = "Compare Means (Medians)", markup = FALSE, horizontal = FALSE, container = CompareDef)
  ParamTest <- gcheckbox("Parametric ( t test or ANOVA)", container = MeansFrame)
  svalue(ParamTest) <- TRUE
  NonParamTest <- gcheckbox("Non Parametric (Mann-Whitney or Kruskal-Wallis)", container = MeansFrame)
  svalue(NonParamTest) <- FALSE

  PostFrame = gframe(text = "Post-Hoc tests", markup = FALSE, horizontal = TRUE, container = CompareDef)
  TPH =  gframe(text = "Parametric", markup = FALSE, horizontal = FALSE, container = PostFrame)
  tests=c("none", "tukey", "sidak", "bonferroni",  "holm", "hochberg", "hommel", "BH", "BY", "fdr")
  PHT <- gcheckboxgroup(tests, container = TPH, use.table = FALSE)
  procs=rep(FALSE, 10)
  procs[c(2, 4)]=TRUE
  svalue(PHT) = procs
  # if (Toolkit == "RGtk2") size(PHT) <- c(220, 150)

  TPH1 =  gframe(text = "Non-Parametric (Dunn Test)", markup = FALSE, horizontal = FALSE, container = PostFrame)
  tests2=c("none", "bonferroni", "sidak", "holm", "hs", "hochberg", "bh", "by")
  PHT2 <- gcheckboxgroup(tests2, container = TPH1, use.table = FALSE)
  procs=rep(FALSE, 8)
  procs[c(2, 3)]=TRUE
  svalue(PHT2) = procs
  #if (Toolkit == "RGtk2") size(PHT2) <- c(220, 150)


  Error2PlotFrame = gframe(text = "Error Bar (Confidence Intervals with ANOVA info)", markup = FALSE, horizontal = FALSE, container = CompareDef)
  Errplot2 <- gcheckbox("Draw Error Bar Plot", container = Error2PlotFrame)
  svalue(Errplot2) <- TRUE
  adj=c("None", "Sidak", "Bonferroni")
  adjpgraph=gcombobox(adj, editable = FALSE, container = Error2PlotFrame)


  RunButtC <- gbutton("Run", container = CompareDef, handler = DoCompare)
  font(RunButtC) <- list(family="times",size=18, weight="bold",style="italic", color="green")
  size(RunButtC) <- c(100, 50)

  svalue(notebook) <- Tab
}
