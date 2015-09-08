\name{Laboration3-package}
\alias{Laboration3-package}
\alias{Laboration3}
\docType{package}
\title{
Performs a few simple algorithms.
}
\description{
This package contains two functions, the euclidian() and the dijkstra(). The first performs an euclidean algorithm for two inputs x and y to find the greatest common divisor and the second uses the dijkstra algorithm to calculate the shortest paths between a vertex in a graph and all of the other vertices in it.
}
\details{
\tabular{ll}{
Package: \tab Laboration3\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2015-09-07\cr
License: \tab None \cr
}

}
\author{
Oscar Pettersson
Vuong Tran

Maintainer: Send your complaints to <oscpe436@student.liu.se>
}
\references{
R-packages (Hadley Wickham): \url{http://r-pkgs.had.co.nz/}
}
\keyword{ package }
\examples{
euclidean(75, 100)
# [1] 25
dijkstra(wiki_graph, 3)
# [1]  9 10  0 11 11  2
}