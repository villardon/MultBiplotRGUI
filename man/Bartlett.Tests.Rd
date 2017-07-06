\name{Bartlett.Tests}
\alias{Bartlett.Tests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bartlett tests for each column of a continuous data Matrix.
}
\description{
The funtion performs Bartlett tests of equality of variances for each column of a data matrix and returns a test string that can be saved into a file.
}
\usage{
Bartlett.Tests(X, groups = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The data matrix
}
  \item{groups}{
A factor containing the groups to compare.
}
}
\details{
Performs Bartlett's test of the null that the variances in each of the groups (samples) are the same, for each column of the data matrix. Te function is just a way to reorganize the results of \code{bartlett.test}  for several variables
}
\value{
A Matrix containing the results of the tests conveniently organized.
}
\references{
Bartlett, M. S. (1937). Properties of sufficiency and statistical tests. Proceedings of the Royal Society of London Series A 160, 268–282.
}
\author{
Jose Luis vIcente Villardon
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{bartlett.test}}
}
\examples{
Bartlett.Tests(iris[,1:4],iris[,4])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
