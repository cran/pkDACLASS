\name{align.R}
\alias{align.R}
\title{
Aligning Peak Intensities based on mass to charge values
}
\description{
Peak intensities are aligned based on the closeness of the mass to charge value to the integer less than or greater than the mass to charge value. 
}
\usage{
align.R(dat)
}
\arguments{
  \item{dat}{
A dataframe of two columns, the first column comprises the mass to charge values, the second column the corresponding intensities.
}
}
\details{
The alignment works as follows. If the mass to charge value is an integer, then the intensity is maintained. If the mass to charge value is a fraction, then the decimal part determines the weight of the corresponding intensity.
}
\value{
  \item{mz }{
aligned mass to charge values.
}
  \item{y }{
corresponding intensities.
}
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line


