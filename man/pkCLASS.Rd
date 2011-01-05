\name{pkCLASS}
\alias{pkCLASS}
\title{
Peak Classification
}
\description{
The function pkCLASS classifies (aligned) peaks based on the randomForest function in the randomForest package. It returns a list of the test performance measures.
}
\usage{
pkCLASS(dat)
}
\arguments{
  \item{dat}{
A data frame for which the first column are the m/z values. The next columns are the corresponding sample intensities.
}
}
\details{
The function pkCLASS returns a list of the performance measures of the test. For the different classification algorithms used, randomForest is the best of all the classifiers.
}
\value{
\item{auc }{
the area under the ROC curve.
}
\item{Accurarcy }{
accuracy of the test.
}
\item{Sensitivity }{
sensitivity of the test.
}
\item{Specificity }{
specificity of the test.
}
\item{Precision }{
precision of the test.
}
\item{Perf }{
performance of the test.
}
}
\author{
Juliet Ndukum, Mourad Atlas, Susmita Datta
}
\keyword{ math }
\keyword{ math }% __ONLY ONE__ keyword per line

