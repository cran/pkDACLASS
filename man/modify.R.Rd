\name{modify.R}
\alias{modify.R}
\title{
Mapping modifications to respective samples
}
\description{
The function modify.R maps modifications of peak intensities to their respective samples.
}
\usage{
modify.R(mat,n)
}
\arguments{
  \item{mat}{
A dataframe with three columns, the first being the m/z values, the second being the corresponding intensities to the m/z values in the first column of the same rows and the third column is the indexing variable indicating the sample from which the peak comes.
}
  \item{n}{
Integer, number of nonzero entries in both the cases and controls.
}
}
\details{
The function modify.R takes as input a dataframe with three columns and outputs a matrix comprising the m/z values and modifed peak intensities.
}
\value{
A dataframe with the first column being the m/z values. The subsequent columns are the corresponding sample intensities i.e. intensities from respective samples.
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

