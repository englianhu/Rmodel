\name{optSmoother}
\alias{optSmoother}
\title{optSmoother}
\usage{
optSmoother()
}
\description{
optSmoother() is the function to measure the optimal smoother index for soccer matches.
}
\examples{
xi <- optSmoother(country="England", year=2008, mbase = engmdb)
xi$decay
}
