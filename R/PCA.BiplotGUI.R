PCA.BiplotGUI <- function(X, HJ=FALSE, Toolkit = "tcltk", ...) {
  options(guiToolkit = Toolkit)

  DoBiplot <- function(...) {

    XX= X2[svalue(RS, index=TRUE),svalue(CS, index=TRUE)]

    if (sum(svalue(RS1, index=TRUE))>0)
      sr=as.matrix(X2[svalue(RS1, index=TRUE),svalue(CS, index=TRUE)])
    else
      sr=NULL

    if (sum(svalue(CS2, index=TRUE))>0)
      cs = as.matrix(X2[svalue(RS, index=TRUE),svalue(CS2, index=TRUE)])
    else
      cs=NULL

    if (!HJ)
    alpha=svalue(Alpha)



    dims= svalue(Dimens)
    print(dims)
    sc=svalue(prevtsc, index=TRUE)

    Classes=sapply(X,class)
    Factors=which(Classes=="factor")
    if (length(Factors)>0){
      X4=as.data.frame(X[svalue(RS, index=TRUE), Factors])
    }

    if (sc==11){
      Classes=sapply(X,class)
      Factors=which(Classes=="factor")
      X4=as.data.frame(X[svalue(RS, index=TRUE), Factors])
      grouping=X4[,svalue(FacSel, index=TRUE)]
    }
    else
      grouping=NULL

    if (HJ)
      bip = HJ.Biplot(XX, dimension = dims, Scaling = sc, sup.rows = sr, sup.cols = sr, grouping=grouping)
    else
      bip = PCA.Biplot(XX, alpha = alpha, dimension = dims, Scaling = sc, sup.rows = sr, sup.cols = cs, grouping=grouping)


    # Selection of External Variables
    if (length(Factors)>0)
    bip$Factors=X4

    if (svalue(GraphWin, index=TRUE)==1)
      GrWin= FALSE
    else
      GrWin= TRUE

    PlotBiplotGUI(bip, GraphWindow=GrWin, Toolkit = Toolkit)
  }

  if (HJ)
    wintit="HJ Biplot"
  else
    wintit="Classical Biplot (PCA)"


  window <- gwindow(wintit, width = 600, height = 600)
  notebook <- gnotebook(container = window, expand=TRUE)


  # ----------------- Data -------------------------------------
  DataFrame <- ggroup(container = notebook, label = gettext("Data"), horizontal = FALSE)
  Classes=sapply(X,class)
  Numeric=which(Classes=="numeric")
  X2=X[,Numeric]
  Table <- gdf(X2, container = DataFrame, expand = TRUE)

  # ----------------- External Data -------------------------------------
  NoNumeric=which(Classes!="numeric")
  if (length(NoNumeric)>0){
    SupDataFrame <- ggroup(container = notebook, label = gettext("External"), horizontal = FALSE)
    X3=as.data.frame(X[,NoNumeric])
    colnames(X3) <-colnames(X)[NoNumeric]
    Table <- gdf(X3, container = SupDataFrame, expand = TRUE)
  }


  # ----------------- Data Transformation ----------------------------
  Disp <- ggroup(container = notebook, label = gettext("Transformation"), horizontal = FALSE)
  PrevFrame = gframe(text = "Previous Transformation", markup = FALSE, horizontal = TRUE, container = Disp)
  types = c("Log", "Logit", "None")
  prevtr <- gcombobox(types, editable = FALSE, container = PrevFrame)
  svalue(prevtr, index = TRUE) <- 3

  Classes=sapply(X,class)
  Factors=which(Classes=="factor")

  SelFac <-function(...){
    if (svalue(prevtsc, index=TRUE)==11){
      if (length(Factors)==0){
        gmessage("You need at least one factor to use Within groups standardization ")
        svalue(prevtsc, index = TRUE) <- 5
        stop("You need at least one factor to use Within groups standardization ")

      }
    }
  }


  SacalingFrame = gframe(text = "Previous Scaling", markup = FALSE, horizontal = TRUE, container = Disp)
  ContinuousDataTransform = c("Raw Data", "Substract the global mean", "Double centering", "Column centering",
                              "Standardize columns", "Row centering", "Standardize rows", "Divide by the column means and center", "Normalized residuals from independence",
                              "Divide by the range", "Within groups standardization", "Ranks")
  prevtsc <- gcombobox(ContinuousDataTransform, editable = FALSE, container = SacalingFrame, handler=SelFac)
  svalue(prevtsc, index = TRUE) <- 5


  if (length(Factors)>0){
    X4=as.data.frame(X[, Factors])
    colnames(X4) <- colnames(X)[Factors]
    FactorsFrame = gframe(text = "Factors", markup = FALSE, horizontal = TRUE, container = Disp)
    FacSel <- gcheckboxgroup(colnames(X4), container = FactorsFrame, use.table = TRUE)
    #size(FacSel) <- c(400, 100)
    Selection <- rep(FALSE, length(Factors))
    Selection[1] <- TRUE
    svalue(FacSel) = Selection
  }


  # -----------------  Biplot definition ----------------------------
  BiplotDef <- ggroup(container = notebook, label = gettext("Biplot Definition"), horizontal = FALSE)
  EstimFrame = gframe(text = "Estimation Method", markup = FALSE, horizontal = TRUE, container = BiplotDef)
  estypes = c("Singular Value Decomposition", "Alternated Regressions", "Robust Alternated Regressions", "Eigenvectors of the Covariance Matrix")
  estim <- gcombobox(estypes, editable = FALSE, container = EstimFrame)
  svalue(estim, index = TRUE) <- 1

  if (!HJ){
  TypeBiplotFrame = gframe(text = "Biplot Type", markup = FALSE, horizontal = TRUE, container = BiplotDef)
  TypeBi = glayout(container = TypeBiplotFrame)
  #size(TypeBi) <- c(400, 50)
  TypeBi[1, 1] <- "GH - CMP "
  TypeBi[1, 2] <- (Alpha <- gslider(from = 0.0, to = 1, by = 0.05, value = 1, container = TypeBi))
  #size(Alpha) <- c(200, 30)
  TypeBi[1, 3] <- "JK - RMP"}

  DimensionFrame = gframe(text = "Dimension of the solution", markup = FALSE, horizontal = TRUE, container = BiplotDef)
  Dimens <- gspinbutton(from = 2, to = 50, by = 1, value = 3, container = DimensionFrame)
  svalue(Dimens) <- 3




  # ---------------- Controls the active elements for the construction of the biplot --------------
  ActiveRows <- function(...){
    Selected=rep(FALSE, dim(X)[1])
    Selected[svalue(RS1, index=TRUE)]=TRUE
    Selected[svalue(RS, index=TRUE)]=FALSE
    svalue(RS1)=Selected
  }

  ActiveCols <- function(...){
    Selected=rep(FALSE, dim(X2)[2])
    Selected[svalue(CS2, index=TRUE)]=TRUE
    Selected[svalue(CS, index=TRUE)]=FALSE
    svalue(CS2)=Selected
  }


  SupRows <- function(...){
    Selected=rep(FALSE, dim(X)[1])
    Selected[svalue(RS, index=TRUE)]=TRUE
    Selected[svalue(RS1, index=TRUE)]=FALSE
    svalue(RS)=Selected
  }

  SupCols <- function(...){
    Selected=rep(FALSE, dim(X2)[2])
    Selected[svalue(CS, index=TRUE)]=TRUE
    Selected[svalue(CS2, index=TRUE)]=FALSE
    svalue(CS)=Selected
  }

  ActiveFrame <- ggroup(container = notebook, label = gettext("Active"), horizontal = TRUE)
  TS = glayout(container = ActiveFrame)
  TS[1, 1] <- "Rows"
  TS[2, 1] <- (RS <- gcheckboxgroup(rownames(X), container = TS, use.table = TRUE, handler = ActiveRows))
  #size(RS) <- c(220, 400)
  svalue(RS) = rep(TRUE, dim(X)[1])
  TS[1, 2] <- "Columns"
  TS[2, 2] <- (CS <- gcheckboxgroup(colnames(X2), container = TS, use.table = TRUE, handler = ActiveCols))
  #size(CS) <- c(220, 400)
  svalue(CS) = rep(TRUE, dim(X2)[2])

  # ---------------- Controls the supplementary elements for the construction of the biplot --------------
  SuppFrame <- ggroup(container = notebook, label = gettext("Supplementary"), horizontal = TRUE)
  TS1 = glayout(container = SuppFrame)
  TS1[1, 1] <- "Rows"
  TS1[3, 1] <- (RS1 <- gcheckboxgroup(rownames(X), container = TS1, use.table = TRUE, handler = SupRows))
  #size(RS1) <- c(220, 300)
  svalue(RS1) = rep(FALSE, dim(X)[1])
  TS1[1, 2] <- "Columns"
  TS1[2, 2] <- "Internal"
  TS1[3, 2] <- (CS2 <- gcheckboxgroup(colnames(X2), container = TS1, use.table = TRUE, handler = SupCols))
  #size(CS2) <- c(220, 300)
  svalue(CS2) = rep(FALSE, dim(X2)[2])

  if (length(NoNumeric)>0){
    TS1[4, 2] <- "External"
    TS1[5, 2] <- (CS1 <- gcheckboxgroup(colnames(X3), container = TS1, use.table = TRUE))
    #size(CS1) <- c(220, 180)
    svalue(CS1) = rep(FALSE, dim(X3)[2])}

  # ----------------------- Runs the biplot -----------------------------------------------
  RunFrame <- ggroup(container = notebook, label = gettext("Run"), horizontal = FALSE)
  GraphFr = gframe(text = "Graph", markup = FALSE, horizontal = TRUE, container = RunFrame)
  grdat=c("Floating", "Embeded")
  GraphWin <- gradio(grdat, container=GraphFr)
  RunFr = gframe(text = "Run", markup = FALSE, horizontal = TRUE, container = RunFrame)
  RunButt <- gbutton("Run Biplot", container = RunFr, handler = DoBiplot)
  font(RunButt) <- list(family="times",size=18, weight="bold",style="italic", color="red")
  size(RunButt) <- c(490, 50)
  svalue(notebook) <- 1
}

