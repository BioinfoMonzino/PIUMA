% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PIUMA_Jaccard.R
\name{jaccardMatrix}
\alias{jaccardMatrix}
\title{Compute the Matrix of Jaccard Indexes}
\usage{
jaccardMatrix(x)
}
\arguments{
\item{x}{A TDAobj object, processed by the  \code{\link{mapperCore}}
function.}
}
\value{
The starting TDAobj object, in which the matrix of Jaccard indexes,
calculated comparing each node of the 'dfMapper' slot, has been added
 (slot: 'jacc')
}
\description{
This function computes the Jaccard index for each pair of
nodes contained in TDAobj, generated by the  \code{\link{mapperCore}}
function. The resulting data.frame can be used to represent data as a
network, for instance, in Cytoscape
}
\details{
The Jaccard index measures the similarity of two nodes A and B.
It ranges from 0 to 1. If A and B share no members, their Jaccard index
would be 0 (= NA). If A and B share all members, their Jaccard index would
be 1. Hence, the higher the index, the more similar the
two nodes. If the Jaccard index between A and B is different from NA, it
means that an edge exists between A and B. The output matrix of Jaccard
indexes can be used as an adjacency matrix. The resulting data.frame can
be used to represent data as a network, for instance, in Cytoscape.
}
\examples{
## use example data:
data(tda_test_data)
jacc_mat <- jaccardMatrix(tda_test_data)

}
\seealso{
\code{\link{makeTDAobj}},
\code{\link{dfToDistance}},
\code{\link{dfToProjection}},
\code{\link{mapperCore}}
}
\author{
Mattia Chiesa, Laura Ballarini, Luca Piacentini
}
