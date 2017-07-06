# Plots a biplot for continuous data
ggplot.ContinuousBiplot <- function(x, A1 = 1, A2 = 2, ShowAxis = TRUE, margin = 0,
                                    PlotVars = TRUE, PlotInd = TRUE, WhatInds = NULL,
                                    WhatVars = NULL, LabelVars = TRUE, LabelInd = TRUE, LabelVar=TRUE,
                                    IndLabels = NULL, VarLabels = NULL, mode = "a", CexInd
                                    = NULL, CexVar = NULL, ColorInd = "blue", ColorVar ="black",
                                    LabelPos = 1, SmartLabels = FALSE,
                                    MinQualityInds = 0, MinQualityVars = 0, dp = 0,
                                    PredPoints = 0, PlotAxis = FALSE, TypeScale =
                                      "Complete", ValuesScale = "Original", SizeQualInd =
                                      FALSE, SizeQualVars = FALSE, ColorQualInd = FALSE,
                                    ColorQualVars = FALSE, PchInd = NULL, PchVar = NULL,
                                    PlotClus = FALSE, TypeClus = "ch", ClustConf = 1,
                                    ClustCenters = FALSE,  UseClusterColors = TRUE, CexClustCenters=1,
                                    PlotSupVars = FALSE, ShowBox=FALSE, nticks=5, NonSelectedGray=FALSE,
                                    PlotUnitCircle=FALSE, PlotContribFA=TRUE, AddArrow=FALSE, varname.adjust = 1.5,
                                    VarLabelAbbrev=FALSE, IndLabelAbbrev=FALSE, IndTextRepel=TRUE, VarTextRepel=FALSE ,
                                    ShowLegend=TRUE, AddQual2LabVar=FALSE, AddQual2LabInd=FALSE, ...){
  require(ggplot2)
  require(ggrepel)
  modes=c("p", "a", "b", "h", "ah", "s")
  if (is.numeric(mode))
    mode = modes[mode]
  TypeScales=c("Complete", "StdDev", "BoxPlot")
  if (is.numeric(TypeScale))
    TypeScale = TypeScales[TypeScale]
  ValuesScales=c("Original", "Transformed")
  if (is.numeric(ValuesScale))
    ValuesScale = ValuesScales[ValuesScale]

  # Setting the Clusters (if any)
  Clusters=x$Clusters
  levels(Clusters) <- x$ClusterNames

  # Obtaining coordinates and qualities for the representation
  A = as.data.frame(x$RowCoordinates[, c(A1, A2)])
  B = as.data.frame(x$ColCoordinates[, c(A1, A2)])
  names(A) <- c("xvar", "yvar")
  names(B) <- names(A)
  n = dim(A)[1]
  p = dim(B)[1]

  if ((ShowBox) | (margin == 0)) margin = 0.05
  if ((margin < 0) | (margin > 0.3)) margin = 0

  xmin = min(A[,1], B[,1])
  xmax = max(A[, 1], B[,1])
  ymin = min(A[, 2], B[,2])
  ymax = max(A[, 2], B[,2])
  xrang=abs(xmax-xmin)
  yrang=abs(ymax-ymin)

  xmin = xmin - (xmax - xmin) * margin
  xmax = xmax + (xmax - xmin) * margin
  ymin = ymin - (ymax - ymin) * margin
  ymax=  ymax + (ymax - ymin) * margin

  if (is.null(IndLabels)) IndLabels=rownames(A)
  if (IndLabelAbbrev) IndLabels= abbreviate(IndLabels)

  if (is.null(VarLabels)) VarLabels=rownames(B)
  if (VarLabelAbbrev) VarLabels= abbreviate(VarLabels)

  # Angles to Draw the vectors
  angle <- with(B, (180/pi) * atan(yvar/xvar))
  hjust = with(B, (1 - varname.adjust * sign(xvar))/2)
  vjust = with(B, (1 - varname.adjust * sign(yvar))/2)

  # Determining what rows to plot
  if (is.null(WhatInds))
    WhatInds = matrix(1, n, 1)
  else
    if (!CheckBinaryVector(WhatInds)) {
      AllRows = matrix(0, n, 1)
      AllRows[WhatInds]=1
      WhatInds=AllRows
    }
  WhatInds=as.logical(WhatInds)

  # Determining what columns to plot
  if (is.null(WhatVars))
    WhatVars = matrix(1, p, 1)
  else
    if (!CheckBinaryVector(WhatVars)){
      AllCols = matrix(0, p, 1)
      AllCols[WhatVars]=1
      WhatVars=AllCols
    }
  WhatVars=as.logical(WhatVars)

  # Labels for the axis
  u.axis.labs <- paste("PC", c(A1,A2), sep = "")
  u.axis.labs <- paste(u.axis.labs, sprintf("(%0.1f%% explained var.)", bip$Inertia[c(A1,A2)]))

  #quality of representation
  Column_Qualities = x$ColContributions[, A1] + x$ColContributions[, A2]
  Row_Qualities = x$RowContributions[, A1] + x$RowContributions[, A2]

  if (AddQual2LabVar) VarLabels=paste(VarLabels," (", round(Column_Qualities), "%)", sep="")
  if (AddQual2LabInd) IndLabels=paste(IndLabels," (", round(Row_Qualities), "%)", sep="")

  WhatInds = WhatInds & (Row_Qualities >MinQualityInds*100)
  WhatVars = WhatVars & (Column_Qualities>MinQualityVars*100)

  # Determining sizes and colors of the points
  if (is.null(CexInd))
    CexInd = rep(0.8, n)
  else if (length(CexInd == 1))
    CexInd = rep(CexInd, n)

  if (is.null(CexVar))
    CexVar = rep(0.8, p)
  else if (length(CexVar == 1))
    CexVar = rep(CexVar, p)

  if (is.null(PchInd))
    PchInd = rep(1,n)
  else if (length(PchInd == 1))
    PchInd = rep(PchInd, p)

  if (is.null(PchVar))
    PchVar = rep(16, p)
  else if (length(PchVar == 1))
    PchVar = rep(PchVar, p)

  if (is.null(ColorInd))
    if (is.null(x$ColorInd))
      ColorInd = rep("blue",n)
  else ColorInd = x$ColorInd
  if (length(ColorInd)==1) ColorInd = rep(ColorInd,n)

  if (is.null(ColorVar))
    ColorVar = rep("black", p)
  if (length(ColorVar)==1) ColorVar = rep(ColorVar,p)

  if (SizeQualInd)
    CexInd = cscale(qlrrows, rescale_pal())
  if (SizeQualVars)
    CexVar = cscale(qlrcols, rescale_pal())

  if (SizeQualInd)
    CexInd = cscale(qlrrows, rescale_pal())
  if (SizeQualVars)
    CexVar = cscale(qlrcols, rescale_pal())

  ColorInd[which(WhatInds==FALSE)]="white"
  ColorVar[which(WhatVars==FALSE)]="white"

  if (NonSelectedGray){
    ColorInd[which(WhatInds==FALSE)]="gray80"
    ColorVar[which(WhatVars==FALSE)]="gray80"
    WhatInds=rep(TRUE, n)
    WhatVars=rep(TRUE, p)
  }

  if (PlotClus){
    ColorInd = x$ClusterColors[x$Clusters]
    ColorQualInd=FALSE
  }

  g <- ggplot(data = A, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + coord_equal()
  g = g + geom_point(color = ColorInd, alpha=as.numeric(WhatInds)) + ggtitle(x$Title) + theme(plot.title = element_text( size=18, face="bold"))

  if (ColorQualInd)
    g = g + geom_point(data=A, aes(color = Row_Qualities)) + scale_colour_gradient(low = "white", high="blue")

  if  (PlotUnitCircle){
    g = g + ggcircle(r=1)
  }

  if (LabelInd){
    if (ColorQualInd)
      g <- g + geom_text_repel(data = A, aes(label = IndLabels, color = Row_Qualities))
    else
      g <- g + geom_text_repel(data = A, aes(label = IndLabels), color = ColorInd, alpha=as.numeric(WhatInds))
  }

  if (ShowAxis==FALSE) {
    g <- g + theme(axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank(),
                   plot.background=element_blank(),
                   panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank(),plot.background=element_blank())
  }

  if (ShowLegend==FALSE) {
    g <- g + theme(legend.position="none")
  }

  g= g + xlim(c(xmin,xmax)) +ylim(c(ymin,ymax))


  if (ShowBox){
    g= g + geom_rect(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, size=0.5, color='black',  alpha=0)}


  if (PlotClus) {
    ng = length(levels(bip$Clusters))
    levellab = levels(bip$Clusters)

    Sizes = zeros(c(ng, 1))
    Means = zeros(c(ng, 2))
    X = list()
    for (i in 1:ng) {
      X[[i]] = as.matrix(A[which(x$Clusters == levellab[i]),1:2 ])
      Sizes[i] = length(which(x$Clusters == levellab[i]))
      if (is.matrix(X[[i]]))
        Means[i, ] = apply(X[[i]], 2, mean)
      else
        Means[i, ] = X[[i]]
    }

    colnames(Means) <- c("xvar", "yvar")
    rownames(Means) <- levellab
    Means=as.data.frame(Means)

    if (ClustCenters){
      Means$groupname=x$ClusterNames
      g= g + geom_point(data=Means, aes(x = xvar, y = yvar, solid=TRUE), color=x$ClusterColors, size=4, shape=1:3)
      g= g + geom_text_repel(data=Means, aes(label=groupname,  x = xvar, y = yvar), color=x$ClusterColors, size=6)
    }

    g=g+geom_point(data=A, shape=x$Clusters)

    if (TypeClus == "el"){
      for (i in 1:ng)
        g= g + ggplotConcEllipse(X[[i]], confidence=ClustConf, color=x$ClusterColors[i])
    }


    if (TypeClus == "ch"){
      for (i in 1:ng){
        fr=Fraction(X[[i]], confidence=ClustConf)
        g= g + ggplotConvHull(fr$fraction, color=x$ClusterColors[i], alpha=0.9)
      }
    }

    if (TypeClus == "st"){
      ST=cbind(A,Means[x$Clusters,])
      names(ST)=c("xvar", "yvar", "xo", "yo")
      SegColors = x$ClusterColors[x$Clusters]
      g= g + geom_segment(data = ST, aes(x = xo, y = yo, xend=xvar, yend=yvar), color = SegColors, size = 0.5, alpha = 1)
    }

  }

  if (PlotVars) {
    if (mode=="ah") linetype=3
    else linetype=1

    C=ggSegBiplotBiplot(B, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, mode=mode)

    if (mode=="p"){
      if (ColorQualVars)
        g <- g + geom_point(data = B, aes(xend = xvar, yend = yvar, color=Column_Qualities)) + scale_colour_gradient(low = "white", high="black")
      else
        g <- g + geom_point(data = B, aes(xend = xvar, yend = yvar), color=ColorVar)
    }
    else{
      if (ColorQualVars)
        g = g + geom_segment(data = C, aes(x = xo, y = yo, xend=xvar, yend=yvar, color = ColorVar), size = 0.2, alpha = 1, linetype=linetype)
      else
        g = g + geom_segment(data = C, aes(x = xo, y = yo, xend=xvar, yend=yvar), color = ColorVar, size = 0.2, alpha = 1, linetype=linetype)

      if ((mode=="a") | (mode=="ah") | (mode=="b")){
        if (ColorQualVars)
          g <- g + geom_segment(data = B, aes(x = 0, y = 0, xend = xvar, yend = yvar, color=Column_Qualities), arrow = arrow(length = unit(1/2,"picas"))) + scale_colour_gradient(low = "white", high="black")
        else
          g <- g + geom_segment(data = B, aes(x = 0, y = 0, xend = xvar, yend = yvar), color=ColorVar, arrow = arrow(length = unit(1/2,"picas")))
      }
    }


    if (mode=="s"){
      Scales = GetBiplotScales(x, nticks=nticks,  TypeScale = TypeScale, ValuesScale = ValuesScale)
      Calibration = ggCalibratedBiplot(C,B,Scales=Scales, Colors=ColorVar, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,  TypeScale = TypeScale)
      for (i in 1:p)
        g = g + ggPlotMarksVarBiplot(Calibration[[i]])

      for (i in 1:p)
        g = g + ggTextMarksVarBiplot(Calibration[[i]], size=2.5, color="black", angle = angle[i])

    }

    if (LabelVar){
      if (VarTextRepel){
        if (ColorQualVars)
          g <- g + geom_text_repel(data = C, aes(label = VarLabels, x = xvar, y = yvar, angle = angle, color=Column_Qualities)) + scale_colour_gradient(low = "white", high="black")
        else
          g <- g + geom_text_repel(data = C, aes(label = VarLabels, x = xvar, y = yvar, angle = angle), color = ColorVar)}
      else{
        if (ColorQualVars)
          g <- g + geom_text(data = C, aes(label = VarLabels, x = xvar, y = yvar, angle = angle, hjust = hjust, color=Column_Qualities)) + scale_colour_gradient(low = "white", high="black")
        else
          g <- g + geom_text(data = C, aes(label = VarLabels, x = xvar, y = yvar, angle = angle, hjust = hjust), color = ColorVar)}
    }

    for (idp in dp)
      if ((idp > 0) & (idp < (p + 1)))
        g = g + ggPlotVarProjBiplot(d=B[idp, ], A=as.matrix(A) , size=0.2, color=ColorVar[idp])
  }

  return(g)
  stop()


  for (idp in PredPoints)
    if ((idp > 0) & (idp < (n + 1)))
      for (j in 1:p){
        g = B[j, ]
        nn = (t(g) %*% g)
        scal <- (A[idp,] %*% g)/nn[1, 1]
        Fpr <- scal %*% t(g)
        nrFpr <- nrow(Fpr)
        dlines(matrix(A[idp,],1,2) , Fpr, color=ColorVar[j])
      }

  if ((x$Type == "FA") & (PlotContribFA)){
    for (i in 1:10){
      Circle(i/10, lty=3)
      text(i/10, 0, labels=i/10, cex=0.5)
      text(0, i/10, labels=i/10, cex=0.5)
      text(-1*i/10, 0, labels=i/10, cex=0.5)
      text(0, -1*i/10, labels=i/10, cex=0.5)
    }
  }

  if (PlotSupVars)
    plot.Supplementary.Variables(x, F1=A1, F2=A2, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, mode=mode, TypeScale=TypeScale)

  # par(op)
}


