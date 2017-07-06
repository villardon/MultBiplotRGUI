\name{ANOVA.Tests}
\alias{ANOVA.Tests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ANOVA tests for each column of a continuous data Matrix.
}
\description{
ANOVA tests for each column of a continuous data Matrix.
}
\usage{
ANOVA.Tests(X, groups, posthoc = "none", alternative = "two.sided", digits = 4, UnequalVar = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The data matrix. Each column will be analyzed separately.
}
  \item{groups}{
A factor containing the groups to compare
}
  \item{posthoc}{
Post-Hoc Tests for the Anova (used by pairwise t tests). Must be a subset of ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
}
  \item{alternative}{
Alternative hypothesis for the pairwise t tests ("two.sided", "less", "greater"))
}
  \item{digits}{
Number of decimal digits for the results
}
  \item{UnequalVar}{
Logical to indicate if the results for unequal variances should also be included.
}
}
\details{
One way Analysis of Variance for the columns of a data matrix.
}
\value{
A list
}
\references{
Benjamini, Y., and Hochberg, Y. (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B 57, 289–300.

Benjamini, Y., and Yekutieli, D. (2001). The control of the false discovery rate in multiple testing under dependency. Annals of Statistics 29, 1165–1188.

Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics 6, 65–70.

Hommel, G. (1988). A stagewise rejective multiple test procedure based on a modified Bonferroni test. Biometrika 75, 383–386.

Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika 75, 800–803.

Shaffer, J. P. (1995). Multiple hypothesis testing. Annual Review of Psychology 46, 561–576. (An excellent review of the area.)

Sarkar, S. (1998). Some probability inequalities for ordered MTP2 random variables: a proof of Simes conjecture. Annals of Statistics 26, 494–504.

Sarkar, S., and Chang, C. K. (1997). Simes' method for multiple hypothesis testing with positively dependent test statistics. Journal of the American Statistical Association 92, 1601–1608.

Wright, S. P. (1992). Adjusted P-values for simultaneous inference. Biometrics 48, 1005–1013. (Explains the adjusted P-value approach.)

}
\author{
Jose Luis Vicente villardon
}


\seealso{
\code{\link{aov}}, \code{\link{pairwise.t.test}}
}
\examples{
data(iris)
txt=ANOVA.Tests(iris[,1:4], groups=iris[,5])
GeneralResults(title="Analysis of Variance", text=txt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
