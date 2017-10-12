\name{compileIndex}
\alias{compileIndex}
\title{compileIndex}
\usage{
compileIndex()
}
\description{
compileIndex() is the function to compile the team offensive and deffensive index.
}
\examples{
em2008 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2008f)
em2009 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2009f)
em2010 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2010f)
em2011 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2011f)
em2012 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2012f)
em2013 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2013f)
}
