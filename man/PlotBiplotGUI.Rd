\name{PlotBiplotGUI}
\alias{PlotBiplotGUI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
User Interface to plot a biplot
}
\description{
User Interface to plot a biplot
}
\usage{
PlotBiplotGUI(bip, HasClusters = FALSE, GraphWindow = FALSE, ppi = 12,
Toolkit = "RGtk2", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bip}{
A biplot object
}
  \item{HasClusters}{
The biplot has clusters?
}
  \item{GraphWindow}{
Is there a window for the graphs.
}
  \item{ppi}{
Points per inch in the graph window
}
  \item{Toolkit}{
RGtk2. Not tried with other toolkits
}
  \item{\dots}{
Any other parameters
}
}
\details{
Graphical user interface to comtrol the parameters of a biplot as colors, sizes, type of representation, etc.
}
\value{
No value returned
}

\author{
Jose Luis Vicente Villardon
}

\examples{
bip=PCA.Biplot(iris[,1:4])
PlotBiplotGUI(bip)
}
