PlotBiplotGUI <- function(bip, HasClusters= FALSE, GraphWindow=FALSE, ppi=12, Toolkit = "tcltk", ...) {
  options(guiToolkit = Toolkit)

  plotBip <- function(...) {
    # No encuentra los primeros objetos que pongo aqui
    vva = svalue(vval)
    SQI = svalue(SizeQI)
    SQV = svalue(SizeQV)
    CQI = svalue(ColorQI)
    CQV = svalue(ColorQV)
    A1 = svalue(ax1, index = TRUE)
    A2 = svalue(ax2, index = TRUE)
    n = dim(bip$Non_Scaled_Data)[1]
    p = dim(bip$Non_Scaled_Data)[2]
    WhatRows = matrix(0, n, 1)
    WhatRows[svalue(RS, index = TRUE)] = 1
    WhatCols = matrix(0, p, 1)
    WhatCols[svalue(CS, index = TRUE)] = 1
    DP = svalue(CP, index = TRUE)
    RP = svalue(RP, index = TRUE)
    PlCl = svalue(PlotClus)
    TypeClus = svalue(ClTy, index = TRUE)
    ClustConf = svalue(ClConf)
    ClustCenters = svalue(PlotClusC)
    UseClusterColors = svalue(UsClusC)
    margin = svalue(marg)
    MinQuIn = svalue(MinQI)
    MinQuVa = svalue(MinQV)
    PlotRows = svalue(PlotR)
    PlotCols = svalue(PlotC)
    LabelRows = svalue(LabelR)
    LabelCols = svalue(LabelC)
    SizeR = svalue(LabelSizeR)
    SizeC = svalue(LabelSizeC)
    ShowBox = svalue(ShowBx)
    mode = svalue(bmode, index = TRUE)
    TYSC = svalue(vtype)
    AbbreviateLabels=svalue(AbbrevLabs)
    ggbiplot=svalue(Useggplot)

    if ((class(bip)=="ContinuousBiplot") & (!ggbiplot)){

      plot(bip, A1 = A1, A2 = A2, margin = margin, PlotInd = PlotRows, PlotVars = PlotCols, LabelInd = LabelRows,
           LabelVars = LabelCols, CexInd = SizeR, CexVar = SizeC, ShowBox = ShowBox, MinQualityInds = MinQuIn,
           MinQualityVars = MinQuVa, mode = mode, TypeScale = TYSC, ValuesScale = vva, SizeQualInd = SQI,
           SizeQualVars = SQV, ColorQualInd = CQI, ColorQualVars = CQV, WhatInds = WhatRows, WhatVars = WhatCols,
           dp = DP, PredPoints = RP, PlotClus = PlCl, TypeClus = TypeClus, ClustConf = ClustConf,
           ClustCenters = ClustCenters, UseClusterColors = UseClusterColors, AbbreviateLabels=AbbreviateLabels)
    }

    if (class(bip)=="Canonical.Biplot"){
      PlotCircle = svalue(PlotCirc)
      TypeCircle = svalue(circletype, index = TRUE)
      voronoi = svalue(ShowVoronoi)
      plot.Canonical.Biplot(bip, A1 = A1, A2 = A2, margin = margin, PlotInd = PlotRows, PlotVars = PlotCols, LabelInd = LabelRows,
           LabelVars = LabelCols, CexInd = SizeR, CexVar = SizeC, ShowBox = ShowBox,
           mode = mode, TypeScale = TYSC, ValuesScale = vva,
           WhatInds = WhatRows, WhatVars = WhatCols,
           dpg = DP, PredPoints = RP, PlotCircle=PlotCircle, TypeCircle=TypeCircle, voronoi=voronoi,
           PlotClus = PlCl, TypeClus = TypeClus, ClustConf = ClustConf,
           ClustCenters = ClustCenters, UseClusterColors = UseClusterColors, AbbreviateLabels=AbbreviateLabels)
    }


    if ((class(bip)=="ContinuousBiplot") & (ggbiplot)) {
      IndLabelAbbrev=svalue(AbbrevRows)
      VarLabelAbbrev=svalue(AbbrevCols)
      IndTextRepel=svalue(RepelRows)
      VarTextRepel=svalue(RepelCols)
      gg=ggplot.ContinuousBiplot(bip, A1 = A1, A2 = A2, margin = margin, PlotInd = PlotRows, PlotVars = PlotCols, LabelInd = LabelRows,
           LabelVars = LabelCols, CexInd = SizeR, CexVar = SizeC, ShowBox = ShowBox, MinQualityInds = MinQuIn,
           MinQualityVars = MinQuVa, mode = mode, TypeScale = TYSC, ValuesScale = vva, SizeQualInd = SQI,
           SizeQualVars = SQV, ColorQualInd = CQI, ColorQualVars = CQV, WhatInds = WhatRows, WhatVars = WhatCols,
           dp = DP, PredPoints = RP, PlotClus = PlCl, TypeClus = TypeClus, ClustConf = ClustConf,
           ClustCenters = ClustCenters, UseClusterColors = UseClusterColors, varname.adjust = 1.5,
           VarLabelAbbrev=VarLabelAbbrev, IndLabelAbbrev=IndLabelAbbrev, IndTextRepel=IndTextRepel, VarTextRepel=VarTextRepel ,
           ShowLegend=TRUE, AddQual2LabVar=FALSE, AddQual2LabInd=FALSE)
      print(gg)}

     if (svalue(Plot3dBiplot)){
       plot3d.ContinuousBiplot(bip, A1 = A1, A2 = A2, margin = margin, PlotInd = PlotRows, PlotVars = PlotCols, LabelInd = LabelRows,
            LabelVars = LabelCols, CexInd = SizeR, CexVar = SizeC, ShowBox = ShowBox, MinQualityInds = MinQuIn,
            MinQualityVars = MinQuVa, mode = mode, TypeScale = TYSC, ValuesScale = vva, SizeQualInd = SQI,
            SizeQualVars = SQV, ColorQualInd = CQI, ColorQualVars = CQV, WhatInds = WhatRows, WhatVars = WhatCols,
            dp = DP, PredPoints = RP, PlotClus = PlCl, TypeClus = TypeClus, ClustConf = ClustConf,
            ClustCenters = ClustCenters, UseClusterColors = UseClusterColors)
     }

  }

  # Función para añadir los clusters nuevos
  AddClus <- function(...) {
    ClusterType = svalue(TypeCl, index = TRUE)
    NGroups = svalue(NumClus)
    if (svalue(Original, index = TRUE)==1)
      Orig=FALSE
    else Orig=TRUE

    if (ClusterType ==1){
      method= svalue(MethodCl)
      bip = AddCluster2Biplot(bip, ClusterType = ClusterType, NGroups = NGroups, method = method, Original=Orig)
    }

    if (ClusterType ==2){
      algorithm= svalue(MethodCk)
      bip = AddCluster2Biplot(bip, ClusterType = ClusterType, NGroups = NGroups, algorithm = algorithm, Original=Orig)
    }

    if (ClusterType ==3){
      algorithm= svalue(MethodCk)
      bip = AddCluster2Biplot(bip, ClusterType = ClusterType, NGroups = NGroups, Original=Orig)
    }

    if (ClusterType ==4){
      Groups=bip$Factors[,svalue(FacSel, index=TRUE)]
      bip = AddCluster2Biplot(bip, ClusterType = ClusterType, Groups = Groups)}
    dispose(window)
    PlotBiplotGUI(bip, HasClusters= TRUE, GraphWindow=GraphWindow, Toolkit = Toolkit)
  }


  n = dim(bip$Non_Scaled_Data)[1]
  p = dim(bip$Non_Scaled_Data)[2]


  window <- gwindow("Biplot Controler", width = 600, height = 600)
  notebook <- gnotebook(container = window, Expand=TRUE)


  # -------------- Original data --------------------------------------------------------------
  DataFrame <- ggroup(container = notebook, label = gettext("Data"), horizontal = FALSE)
  Table <- gdf(bip$Non_Scaled_Data, container = DataFrame, expand = TRUE)

  # ------------- Information about the biplot ----------------------------------------------------------
  InfoFrame <- ggroup(container = notebook, label = gettext("Biplot"), horizontal = FALSE)
  InCall = gframe(text = "Call", markup = FALSE, horizontal = TRUE, container = InfoFrame)
  gedit(as.character(bip$call), container = InCall, col = "Blue")
  InTran = gframe(text = "Transformation of the data", markup = FALSE, horizontal = TRUE, container = InfoFrame)
  gedit(bip$Initial_Transformation, container = InTran)

  if (!is.null(bip$alpha)){
  InType = gframe(text = "Type of coordinates:", markup = FALSE, horizontal = TRUE, container = InfoFrame)
  if (bip$alpha == 2)
    tipo = "Principal Normalization (HJ-Biplot) (Baricentric Scaling)"
  if (bip$alpha == 1)
    tipo = "Row Principal Normalization (RMP-Biplot)"
  if (bip$alpha == 0)
    tipo = "Column Principal Normalization (CMP-Biplot)"
  if (bip$alpha == 0.5)
    tipo = "Symmetrical Normalization (SQRT - Biplot)"
  if ((bip$alpha > 0) & (bip$alpha < 1) & (bip$alpha != 0.5))
    tipo = paste("Custom Normalization (Biplot con \alpha = ", gamma, ")")
  gedit(tipo, container = InType, width = 40)}

  InIner = gframe(text = "Eigenvalues and Explained Variance (Inertia):", markup = FALSE, horizontal = TRUE,
                  container = InfoFrame)
  pp = cbind(1:length(bip$EigenValues), bip$EigenValues, bip$Inertia, bip$CumInertia)
  colnames(pp) = c("Axis", "Eigenvalue", "Explained Variance", "Cummulative")
  TabVarExp <- gtable(pp, container = InIner, expand = TRUE)
  size(TabVarExp) <- c(400, 110)

  InResults = gframe(text = "Summary of Results", markup = FALSE, horizontal = TRUE,
                     container = InfoFrame)
  obj <- gtext("", container=InResults)
  size(obj)<- c(490, 250)
  txt=capture.output(summary(bip))

  insert(obj,txt, font.attr=c(family="monospace", sizes="small"))
  # SaveResButt <- gbutton("Save Results", container = InResults, handler = function(h,...){
  #   setfilename <- paste(gfile(text="Results.txt", type="save"), ".txt", sep="")
  #   capture.output(summary(bip), file=setfilename)
  # })

  ShowResButt <- gbutton("Show Results", container = InResults, handler = function(h,...){
    GeneralResults(text=txt, Toolkit = Toolkit)
    capture.output(summary(bip), file=setfilename)
  })

  # ------------------  Group for the Contributions -----------------------------------------------------
  CotribFrame <- ggroup(container = notebook, label = gettext("Contributions"), horizontal = TRUE)
  RowContribFrame = gframe(text = "Rows", markup = FALSE, horizontal = FALSE,
                     container = CotribFrame)

  ContRow <- gbutton("Show Row Qualities", container = RowContribFrame, handler = function(...) {
      A1=svalue(ax1, index = TRUE)
      A2=svalue(ax2,  index = TRUE)
      matcon=cbind(bip$RowContributions[,c(A1,A2)], bip$RowContributions[,A1]+bip$RowContributions[,A2] )
      colnames(matcon)[3]="Sum"
      GeneralTable(Title="Row Qualities", Data = matcon)
    })
  TableC=cbind(as.data.frame(rownames(bip$RowContributions)), bip$RowContributions)
  names(TableC)=c("Rows", colnames(bip$RowContributions))
  TabRowContr <- gtable(TableC, container = RowContribFrame, expand = TRUE)
  if (Toolkit == "RGtk2") size(TabRowContr) <- c(250, 400)

  ColContribFrame = gframe(text = "Coluumns", markup = FALSE, horizontal = FALSE,
                           container = CotribFrame)
  ContCol <- gbutton("Show Column Qualities", container = ColContribFrame, handler = function(...) {
      A1=svalue(ax1, index = TRUE)
      A2=svalue(ax2, index = TRUE)
      matcon=cbind(bip$ColContributions[,c(A1,A2)], bip$ColContributions[,A1]+bip$ColContributions[,A2] )
      colnames(matcon)[3]="Sum"
      GeneralTable(Title="Column Qualities", Data = matcon)
    })

  TableC=cbind(as.data.frame(rownames(bip$ColContributions)), bip$ColContributions)
  names(TableC)=c("Columns", colnames(bip$ColContributions))
  TabColContr <- gtable(TableC, container = ColContribFrame, expand = TRUE)
  if (Toolkit == "RGtk2") size(TabColContr) <- c(250, 400)


  # -------------------- Group for main characteristics of the plot -------------------------------------
  Disp <- ggroup(container = notebook, label = gettext("Display"), horizontal = FALSE)

  AxisFrame = gframe(text = "Dimensions to Plot", markup = FALSE, horizontal = TRUE, container = Disp)
  gp00 = ggroup(container = AxisFrame)
  lb=glabel("Dimension in Axis X:", container = gp00)
  # font(lb) <- list(family="times",size=12, weight="bold",style="italic", clor="red")

  flavors = 1:bip$dim
  ax1 <- gcombobox(flavors, editable = FALSE, container = gp00, handler = plotBip)
  svalue(ax1, index = TRUE) <- 1

  gp01 = ggroup(container = AxisFrame)
  glabel("Dimension in Axis Y:", container = gp01)
  ax2 <- gcombobox(flavors, editable = FALSE, container = gp01, handler = plotBip)
  svalue(ax2, index = TRUE) <- 2


  PlotFrame = gframe(text = "What to Plot", markup = FALSE, horizontal = TRUE, container = Disp)
  PlotR <- gcheckbox("Individuals   ", container = PlotFrame, handler = plotBip)
  svalue(PlotR) <- TRUE
  PlotC <- gcheckbox("Variables   ", container = PlotFrame, handler = plotBip)
  svalue(PlotC) <- TRUE


  LabelFrame = gframe(text = "Label", markup = FALSE, horizontal = TRUE, container = Disp)
  LabelR <- gcheckbox("Individuals    ", container = LabelFrame, handler = plotBip)
  svalue(LabelR) <- TRUE
  LabelC <- gcheckbox("Variables   ", container = LabelFrame, handler = plotBip)
  svalue(LabelC) <- TRUE
  AbbrevLabs <- gcheckbox("Abbreviate Labels   ", container = LabelFrame, handler = plotBip)
  svalue(AbbrevLabs) <- FALSE

  LabelSizeFrame = gframe(text = "Size", markup = FALSE, horizontal = TRUE, container = Disp)
  glabel("Individuals: ", container = LabelSizeFrame)
  LabelSizeR <- gspinbutton(from = 0.1, to = 2, by = 0.1, value = 1, container = LabelSizeFrame, handler = plotBip)
  svalue(LabelSizeR)<-1
  glabel("   Variables: ", container = LabelSizeFrame)
  LabelSizeC <- gspinbutton(from = 0.1, to = 2, by = 0.1, value = 1, container = LabelSizeFrame, handler = plotBip)
  svalue(LabelSizeC)<-1

  MarginFrame = gframe(text = "Margin", markup = FALSE, horizontal = TRUE, container = Disp)
  marg <- gspinbutton(from = 0, to = 0.3, by = 0.05, value = 0, container = MarginFrame, handler = plotBip)
  svalue(marg)<-0
  ShowAx <- gcheckbox("Show Axis   ", container = MarginFrame, handler = plotBip)
  svalue(ShowAx) <- FALSE
  ShowBx <- gcheckbox("Show Box   ", container = MarginFrame, handler = plotBip)
  svalue(ShowBx) <- FALSE

  ModeFrame = gframe(text = "Variables Mode on the Biplot", markup = FALSE, horizontal = FALSE, container = Disp)
  modes = c("Points", "Arrows", "Both ends", "Half line", "Half Line & Arrow", "Calibrated Axis")
  gp = ggroup(container = ModeFrame)
  glabel("Variables Mode:", container = gp)
  bmode <- gcombobox(modes, editable = FALSE, container = gp, handler = plotBip)
  svalue(bmode, index = TRUE) <- 2

  gp2 = ggroup(container = ModeFrame)
  types = c("Complete", "Standard Deviation", "Box Plot")
  glabel("Type of Scale:", container = gp2)
  vtype <- gcombobox(types, editable = FALSE, container = gp2, handler = plotBip)
  svalue(vtype, index = TRUE) <- 1

  gp3 = ggroup(container = ModeFrame)
  vals = c("Original", "Transformed")
  glabel("Values on the Scale:", container = gp3)
  (vval <- gcombobox(vals, editable = FALSE, container = gp3, handler = plotBip))
  svalue(vval, index = TRUE) <- 1


  if (class(bip)=="Canonical.Biplot"){
    svalue(PlotR) <- FALSE
    CanonFrame = gframe(text = "Canonical", markup = FALSE, horizontal = FALSE, container = Disp)
    PlotCirc <- gcheckbox("Plot Conficence regions for means", container = CanonFrame, handler = plotBip)
    svalue(PlotCirc)<-TRUE
    gp4 = ggroup(container = CanonFrame)
    typescirc = c("Univariate", "Bonferroni", "Hotteling", "Chi-Squared")
    glabel("Type of Circle:", container = gp4)
    circletype <- gcombobox(typescirc, editable = FALSE, container = gp4, handler = plotBip)
    svalue(circletype, index = TRUE) <- 3
    ShowVoronoi <- gcheckbox("Show Voronoi Diagram (Classification Regions", container = CanonFrame, handler = plotBip)
  }


  # ------------------ LIST OF INDIVIDUALS AND VARIABLES PLOTTED ON THE PLOT --------------------------
  PlottedFrame <- ggroup(container = notebook, label = gettext("Plotted"), horizontal = TRUE)
  RowsPlottedFrame = gframe(text = "Rows", markup = FALSE, horizontal = FALSE, container = PlottedFrame)
  SAR <- gbutton("Select All", container = RowsPlottedFrame, handler = function(...) {
    svalue(RS) = rep(TRUE, n)
    plotBip()
  })
  RAR <- gbutton("Remove All", container = RowsPlottedFrame, handler = function(...) {
    svalue(RS) = rep(FALSE, n)
    plotBip()
  })
  RS <- gcheckboxgroup(rownames(bip$Non_Scaled_Data), container = RowsPlottedFrame, handler = plotBip, use.table = TRUE)
  svalue(RS) = rep(TRUE, n)

  ColsPlottedFrame = gframe(text = "Columns", markup = FALSE, horizontal = FALSE, container = PlottedFrame)
  SAC <- gbutton("Select All", container = ColsPlottedFrame, handler = function(...) {
    svalue(CS) = rep(TRUE, n)
    plotBip()
  })
  RAC <- gbutton("Remove All", container = ColsPlottedFrame, handler = function(...) {
    svalue(CS) = rep(FALSE, n)
    plotBip()
  })
  CS <- gcheckboxgroup(colnames(bip$Non_Scaled_Data), container = ColsPlottedFrame, handler = plotBip, use.table = TRUE)
  svalue(CS) = rep(TRUE, p)

  # ------------------ LIST OF INDIVIDUALS AND VARIABLES PROJECTED ON THE PLOT --------------------------
  ProjFrame <- ggroup(container = notebook, label = gettext("Projected"), horizontal = TRUE)
  RowsProjFrame = gframe(text = "Rows", markup = FALSE, horizontal = FALSE, container = ProjFrame)
  RP <- gcheckboxgroup(rownames(bip$Non_Scaled_Data), container = RowsProjFrame, handler = plotBip, use.table = TRUE)
  svalue(RP) = rep(FALSE, n)
  ColsProjFrame = gframe(text = "Columns", markup = FALSE, horizontal = FALSE, container = ProjFrame)
  CP <- gcheckboxgroup(colnames(bip$Non_Scaled_Data), container = ColsProjFrame, handler = plotBip, use.table = TRUE)
  svalue(CP) = rep(FALSE, p)

  # ------------- MANAGES HOW THE QUALITIES ARE REPRESENTED ON THE PLOT -------------
  Qualities <- ggroup(container = notebook, label = gettext("Qualities"), horizontal = TRUE)
  QualIndFrame = gframe(text = "Individuals", markup = FALSE, horizontal = FALSE, container = Qualities)
  glabel("Minimum quality to plot :    ", container = QualIndFrame)
  MinQI <- gspinbutton(from = 0, to = 1, by = 0.1, value = 0, container = QualIndFrame, handler = plotBip)
  svalue(MinQI)<-0
  SizeQI <- gcheckbox("Sizes by quality", container = QualIndFrame, handler = plotBip)
  svalue(SizeQI) <- FALSE
  ColorQI <- gcheckbox("Colors by quality", container = QualIndFrame, handler = plotBip)
  svalue(ColorQI) <- FALSE

  QualVarFrame = gframe(text = "Variables", markup = FALSE, horizontal = FALSE, container = Qualities)
  glabel("Minimum quality to plot :    ", container=QualVarFrame)
  MinQV <- gspinbutton(from = 0, to = 1, by = 0.1, value = 0, container = QualVarFrame, handler = plotBip)
  svalue(MinQV)<-0
  SizeQV <- gcheckbox("Sizes by quality", container = QualVarFrame, handler = plotBip)
  svalue(SizeQV) <- FALSE
  ColorQV <- gcheckbox("Colors by quality", container = QualVarFrame, handler = plotBip)
  svalue(ColorQV) <- FALSE

  # -------------------   PLOTTING CLUSTERS ON THE BIPOT -----------------------------------------------------
  Clusters <- ggroup(container = notebook, label = gettext("Clusters"), horizontal = FALSE)

  PlotCFrame = gframe(text = "Plot Clusters", markup = FALSE, horizontal = FALSE, container = Clusters)
  PlotClus <- gcheckbox("Display Clusters", container = PlotCFrame, handler = plotBip)
  if (HasClusters)
    svalue(PlotClus) <- TRUE
  else
    svalue(PlotClus) <- FALSE

  PlotClusC <- gcheckbox("Plot Cluster Centers", container = PlotCFrame, handler = plotBip)
  svalue(PlotClusC) <- FALSE
  UsClusC <- gcheckbox("Use Cluster Colors", container = PlotCFrame, handler = plotBip)
  svalue(UsClusC) <- FALSE


  TypeCFrame = gframe(text = "Type of Clusters Plot", markup = FALSE, horizontal = FALSE, container = Clusters)
  types = c("Convex Hull", "Ellipse", "Star")
  ClTy <- gcombobox(types, editable = FALSE, container = TypeCFrame, handler = plotBip)


  ConfCFrame = gframe(text = "Confidence", markup = FALSE, horizontal = FALSE, container = Clusters)
  ClConf <- gspinbutton(from = 0, to = 1, by = 0.05, value = 1, container = ConfCFrame, handler = plotBip)
  svalue(ClConf) <- 1

  # -------------------   ADDING CLUSTERS TO THE BIPOT -----------------------------------------------------
  NewCluster = gframe(text = "Add New Clusters", markup = FALSE, horizontal = FALSE, container = Clusters)
  NCL1 = ggroup(container = NewCluster)
  types = c("Hierarchical", "K-Means", "Gaussian Mixture", "User Provided")
  glabel("Types of Cluster", container = NCL1)
  TypeCl <- gcombobox(types, editable = FALSE, container = NCL1)
  svalue(TypeCl, index = TRUE) <- 1
  NCL2 = ggroup(container = NewCluster)
  glabel("Number of Groups:", container = NCL2)
  NumClus <- gspinbutton(from = 2, to = 15, by = 1, value = 3, container = NCL2)
  NCL3 = ggroup(container = NewCluster)
  glabel("Method for Hierarchical Clustering", container = NCL3)
  clt = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
  MethodCl <- gcombobox(clt, editable = FALSE, container = NCL3)
  svalue(MethodCl, index = TRUE) <- 2

  NCL4 = ggroup(container = NewCluster)
  glabel("Method for K-Means Clustering", container = NCL4)
  algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
  MethodCk <- gcombobox(algorithm, editable = FALSE, container = NCL4)
  svalue(MethodCk, index = TRUE) <- 2

  NCL5 = ggroup(container = NewCluster)
  glabel("Cluster with :", container = NCL5)
  cldat=c("Biplot Coordinates", "Original Data")
  Original <- gradio(cldat, container=NCL5)


  if (!is.null(bip$Factors)){
    FactorsFrame = ggroup(container = NewCluster)
    glabel("Factors for Clustering", container = FactorsFrame)
    FacSel <- gcheckboxgroup(colnames(bip$Factors), container = FactorsFrame, use.table = TRUE)
    # size(FacSel) <- c(400, 100)
    Selection <- rep(FALSE, length(bip$Factors))
    Selection[1] <- TRUE
    svalue(FacSel) = Selection
  }

  ClusButt <- gbutton("Add Clusters", container = NewCluster, handler = AddClus)
  font(ClusButt) <- list(size=18, weight="bold",style="italic", color="red")

  if (GraphWindow){
    GraphTab = ggroup(container = notebook, label = gettext("Plot"), horizontal = FALSE)
    Grafico<-ggraphics(ps=ppi,container=GraphTab)
    addHandlerClicked ( Grafico , handler = function ( h , ... ) {
      cat ( sprintf ( "You clicked %.2f x %.2f\n" , h$x , h$y ) )
    } )
  }

  OtherPlots <- ggroup(container = notebook, label = gettext("Other Plots"), horizontal = FALSE)
  ggplotFrame = gframe(text = "ggplot2", markup = FALSE, horizontal = FALSE, container = OtherPlots)
  Useggplot <- gcheckbox("Use ggplot2", container = ggplotFrame, handler = plotBip)
  svalue(Useggplot) <- FALSE
  RepelRows <- gcheckbox("Repel overlapping row labels", container = ggplotFrame, handler = plotBip)
  svalue(RepelRows) <- TRUE
  RepelCols <- gcheckbox("Repel overlapping column labels", container = ggplotFrame, handler = plotBip)
  svalue(RepelCols) <- TRUE
  AbbrevRows <- gcheckbox("Abbreviate row labels", container = ggplotFrame, handler = plotBip)
  svalue(AbbrevRows) <- FALSE
  AbbrevCols <- gcheckbox("Abbreviate column labels", container = ggplotFrame, handler = plotBip)
  svalue(AbbrevCols) <- FALSE

  ThreeDPlotFrame = gframe(text = "3D Plot", markup = FALSE, horizontal = FALSE, container = OtherPlots)
  Plot3dBiplot <- gcheckbox("Plot 3D graph", container = ThreeDPlotFrame, handler = plotBip)
  svalue(Plot3dBiplot) <- FALSE

  if (HasClusters){
    svalue(ClTy, index = TRUE)<-1
    svalue(notebook) <- 8}
  else
    svalue(notebook) <- 2

  plotBip(bip)
}
