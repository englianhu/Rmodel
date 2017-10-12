\name{compileOdds}
\alias{compileOdds}
\title{compileOdds}
\usage{
compileOdds()
}
\description{
compileOdds() is the function to compile the team offensive and deffensive index.
}
\examples{
matchdata <- loadMatch("England", 2008)
odds <- compileOdds(eng2008, matchdata)
saveData(eng2008, oddsdata = odds)
}
