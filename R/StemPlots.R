StemPlots <- function(X, groups=NULL, SortByGroups=FALSE, na.rm=FALSE){
  n=dim(X)[1]
  p=dim(X)[2]

  if (is.null(groups)) {
    groups=as.factor(rep(1,n))
    levels(groups)="Complete Sample"}

  if (!is.factor(groups)) stop("The variable defining the groups must be a factor")

  g=length(levels(groups))
  Levels=levels(groups)
  varnames=colnames(X)

  Descriptives=list()
  nameslist=character()
  k=0
  if (SortByGroups==TRUE){
    for (i in 1:g){
      XX = as.matrix(X[which(groups == Levels[i]), ])
      for (j in 1:p){
        k=k+1
        nameslist[[k]]=paste(Levels[i], varnames[j], sep="-")
        Descriptives[[k]]=capture.output(stem(XX[,j]))
      }
    }
    names(Descriptives) = nameslist
  }



  if (SortByGroups==FALSE){
    for (i in 1:p){
      XX = as.numeric(X[,j])
      for (j in 1:g){
        k=k+1
        nameslist[[k]]=paste(varnames[i], Levels[j], sep="-")
      Descriptives[[k]]=capture.output(as.matrix(XX[which(groups == Levels[j]) ]))}
    }
    names(Descriptives) = nameslist
  }
  return(Descriptives)
}

