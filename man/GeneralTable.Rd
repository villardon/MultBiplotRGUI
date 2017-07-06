\name{GeneralTable}
\alias{GeneralTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simple widget to display a table
}
\description{
Simple widget to display a table
}
\usage{
GeneralTable(Title = "General Table", Data = matrix(1, 10, 3), Toolkit = "RGtk2", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Title}{
Title of the window
}
  \item{Data}{
data to display
}
  \item{Toolkit}{
RGtk2. Not tried with other toolkits
}
  \item{\dots}{
Aditional arguments
}
}
\details{
Simple widget to display a table
}
\value{
No value returned
}

\author{
Jose Luis Vicente Villardon
}

\examples{
GeneralTable(Data=iris)
}

