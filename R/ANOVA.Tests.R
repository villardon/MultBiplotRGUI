# Function to Calculate ANOVAs for each column of a Matrix
ANOVA.Tests <- function(X, groups, posthoc="none", alternative="two.sided", digits=4, UnequalVar=TRUE){
  n=dim(X)[1]
  p=dim(X)[2]

  if (!is.factor(groups)) stop("The variable defining the groups must be a factor")

  g=length(levels(groups))
  Levels=levels(groups)
  varnames=colnames(X)
  txt="\n\n***** ONE-WAY ANALYSIS OF VARIANCE WITH POST-HOC COMPARISOSNS *****"
  txt=c(txt, capture.output(cat("\n\n")))

  Comparisons=combn(levels(groups), 2, paste, collapse=":")

  Summary.ANOVA=matrix(0, p, 4)
  SR=matrix(0, p, 1)
  Summary.posthoc=list()
  for (j in 1:length(posthoc)) {
    Summary.posthoc[[j]]=matrix(0, p , g*(g-1)/2)
    rownames(Summary.posthoc[[j]])=varnames
    colnames(Summary.posthoc[[j]])=Comparisons
  }

  names(Summary.posthoc)=posthoc
  txt=c(txt, capture.output(cat("\n\n************ ASUMING EQUAL VARIANCES ************\n\n")))
  for (i in 1:p){
    txt=c(txt, capture.output(cat("*************************************************************************\n\n")))
    txt=c(txt, capture.output(cat("******* Variable : ", varnames[i], "-------------\n\n")))
    av=aov(X[,i]~groups)
    at=anova(av)
    Summary.ANOVA[i,1] = at$`Mean Sq`[1]
    Summary.ANOVA[i,2] = at$`Mean Sq`[2]
    Summary.ANOVA[i,3] = at$`F value`[1]
    Summary.ANOVA[i,4] = at$`Pr(>F)`[1]
    SR[i]=sqrt(at$`Mean Sq`[2])
    txt=c(txt, capture.output(summary(av)))
    for (j in 1:length(posthoc)){
      if (posthoc[j]=="tukey"){
        tt=TukeyHSD(av)
        txt=c(txt, capture.output(tt))
        Summary.posthoc[[j]][i,]=round(tt$groups[,4], digits=digits)}
      else if ((posthoc[j]=="sidak")){
        ph=pairwise.t.test(X[,i], g=groups, p.adjust.method="none", alternative=alternative)
        ph$p.value=1-(1-ph$p.value)^(1/g)
        ph$p.adjust.method="sidak"
        txt=c(txt, capture.output(ph))
        k=0
        for (l in 1:(g-1))
          for (m in l:(g-1)){
            k=k+1
            Summary.posthoc[[j]][i, k] = round(ph$p.value[m, l], digits=digits)
          }
      }
      else{
        ph=pairwise.t.test(X[,i], g=groups, p.adjust.method=posthoc[j], alternative=alternative)
        txt=c(txt, capture.output(ph))
        k=0
        for (l in 1:(g-1))
          for (m in l:(g-1)){
            k=k+1
            Summary.posthoc[[j]][i, k] = round(ph$p.value[m, l], digits=digits)
          }
      }
    }
    txt=c(txt, capture.output(cat("*************************************************************************\n\n")))
  }
  txt=c(txt, capture.output(cat("\n\n SUMMARY OF RESULTS ****************************************************\n\n")))
  rownames(Summary.ANOVA)<- varnames
  colnames(Summary.ANOVA) <- c("Groups", "Residual", "F value", "Pr(>F)")
  txt=c(txt, capture.output(cat("ANOVA (EQUAL VARIANCES) ****************************************************\n\n")))
  txt=c(txt, capture.output(print(Summary.ANOVA)))
  txt=c(txt, capture.output(cat("\n\n POST-HOC COMPARISONS *************************************************\n\n")))
  txt=c(txt, capture.output(print(Summary.posthoc)))
  txt=c(txt, capture.output(cat("*************************************************************************\n\n")))

  if (UnequalVar){
    txt=c(txt, capture.output(cat("\n\n************ ASUMING UNEQUAL VARIANCES ************\n\n")))
      Summary.ANOVA=matrix(0, p, 4)
      Summary.posthoc=matrix(0, p , g*(g-1)/2)
      rownames(Summary.posthoc)=varnames
      colnames(Summary.posthoc)=Comparisons

    for (i in 1:p){
      txt=c(txt, capture.output(cat("******* Variable : ", varnames[i], "-------------\n\n")))
      av=oneway.test(X[,i]~groups)
      txt=c(txt, capture.output(print(av)))
      Summary.ANOVA[i,1] = av$statistic
      Summary.ANOVA[i,2] = av$parameter[1]
      Summary.ANOVA[i,3] = av$parameter[2]
      Summary.ANOVA[i,4] = av$p.value
      gh=Games_Howell(X[,i],groups)
      txt=c(txt, capture.output(cat("\nGames-Howel test\n")))
      txt=c(txt, capture.output(print(gh)))
      Summary.posthoc[i,]=round(gh[,3], digits=digits)
      txt=c(txt, capture.output(cat("*************************************************************************\n\n")))
    }
      txt=c(txt, capture.output(cat("\n\n SUMMARY OF RESULTS ****************************************************\n\n")))
      rownames(Summary.ANOVA)<- varnames
      colnames(Summary.ANOVA) <- c("F", "num df", "denom df", "Pr(>F)")
      txt=c(txt, capture.output(cat("ANOVA (WELCH) ****************************************************\n\n")))
      txt=c(txt, capture.output(print(Summary.ANOVA)))
      txt=c(txt, capture.output(cat("\n\n POST-HOC COMPARISONS (Games-Howell*************************************************\n\n")))
      txt=c(txt, capture.output(print(Summary.posthoc)))
      txt=c(txt, capture.output(cat("*************************************************************************\n\n")))

  }



  return(txt)
}

