\name{EditorMultBiplotRGUI}
\alias{EditorMultBiplotRGUI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical user interface for the data editor
}
\description{
Graphical user interface for the data editor
}
\usage{
EditorMultBiplotRGUI(X, ppi = 12, Toolkit = "RGtk2", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
The data frame.
}
  \item{ppi}{
Poins per inch for the graph panel
}
  \item{Toolkit}{
RGtk2. Not tried with other toolkits
}
  \item{\dots}{
Aditional arguments
}
}
\details{
Graphical user interface for the data editor and the selection of the analysis.
}
\value{
No value returned
}

\author{
Jose Luis Vicente Villardon
}

\examples{
EditorMultBiplotRGUI(iris)
}
