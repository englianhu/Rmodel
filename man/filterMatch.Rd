\name{filterMatch}
\alias{filterMatch}
\title{filterMatch}
\usage{
  filterMatch()
}
\description{
  filterMatch() is the function to filter the nuetral ground soccer matches.
}
\examples{
  eng2008f = filterMatch(loadMatch(country = "England", year = 2008))
  eng2009f = filterMatch(loadMatch(country = "England", year = 2009))
  eng2010f = filterMatch(loadMatch(country = "England", year = 2010))
  eng2011f = filterMatch(loadMatch(country = "England", year = 2011))
  eng2012f = filterMatch(loadMatch(country = "England", year = 2012))
  eng2013f = filterMatch(loadMatch(country = "England", year = 2013))
}
