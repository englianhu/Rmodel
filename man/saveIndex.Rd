\name{saveIndex}
\alias{saveIndex}
\title{saveIndex}
\usage{
saveIndex()
}
\description{
saveIndex() is the function to save the team offensive and deffensive index.
}
\examples{
for (i in 370:length(dateID))
{ source <- engmdb[engmdb$Date <= dateID[i],] # dateID[(i-1)] for time series model
  source <- source[!duplicated(data.frame(source$Home,source$Away)),]
  em <- compileIndex(data = source)
  saveIndex(country = "England", year = 2008, indexdata = em)
}; rm(i, dateID, source, em)


}
