# load soccer matches from folder and filter all the soccer matches
#   which the teams are coming from only certain leagues which from our database
source("./R/loadMatch.R")
source("./R/filterMatch.R")

eng2008f = filterMatch(loadMatch(country = "England", year = 2008))
eng2009f = filterMatch(loadMatch(country = "England", year = 2009))
eng2010f = filterMatch(loadMatch(country = "England", year = 2010))
eng2011f = filterMatch(loadMatch(country = "England", year = 2011))
eng2012f = filterMatch(loadMatch(country = "England", year = 2012))
eng2013f = filterMatch(loadMatch(country = "England", year = 2013))
rm(loadMatch, filterMatch)

# merge the 2 seasons' soccer matches
engmb1 <- eng2008f
engmb2 <- eng2009f

teamID1 <- sort(unique(c(as.character(engmb1$Home), as.character(engmb1$Away))))
teamID2 <- sort(unique(c(as.character(engmb2$Home), as.character(engmb2$Away))))
teamID <- sort(c(teamID1, teamID2)); teamID <- teamID[duplicated(teamID)]
rm(teamID1, teamID2)
engmdb <- merge(engmb1, engmb2, all = TRUE)
engmdb$matchID <- rownames(engmdb)
engmdb <- subset(engmdb, Home %in% teamID & Away %in% teamID)
engmdb <- engmdb[engmdb$Neutral == 0,]; rm(engmb1, engmb2, teamID)

engmdb$Date <- as.POSIXct(engmdb$Date)
engmdb <- engmdb[order(engmdb$Date, decreasing = T),]
rownames(engmdb) <- NULL

# ----------------------------------------------------
source("./R/compileIndex.R")
source("./R/saveIndex.R")
# 225:297,298:369,370:length(dateID)
# calculate FT score static index

dateID <- as.POSIXct(levels(factor(as.Date(engmdb$Date))))
# dateID <- levels(factor(as.Date(engmdb$Date)))

for (i in 370:length(dateID))
{ source <- engmdb[engmdb$Date <= dateID[i],] # dateID[(i-1)] for time series model
  source <- source[!duplicated(data.frame(source$Home,source$Away)),]
  em <- compileIndex(data = source)
  saveIndex(country = "England", year = 2008, indexdata = em)
}; rm(i, dateID, source, em)

# ----------------------------------------------------
# load loadIndex function
source("./R/loadIndex.R")

# REPEAT the below steps where the codes from above
# load soccer matches from folder and filter all the soccer matches
#   which the teams are coming from only certain leagues which from our database
# merge the 2 seasons' soccer matches
#   engmdb is merged data of eng2009f and eng2009f where the codes from above
idx <- loadIndex(country="England",year=2008,mbase=engmdb)

source("./R/optSmoother.R")
xi <- optSmoother(country="England", year=2008, mbase = engmdb)
xi$decay
#Start        End        decay
#1 2009-05-24 2010-05-19 -0.007437105

# ----------------------------------------------------
source("./R/compileIndex.R")
source("./R/saveIndex.R")
# 225:297,298:369,370:length(dateID)
# calculate FT score static index

dateID <- as.POSIXct(levels(factor(as.Date(engmdb$Date))))
# dateID <- levels(factor(as.Date(engmdb$Date)))

# calculate FT score time series index
for (i in 370:length(dateID))
{ source <- engmdb[engmdb$Date <= dateID[i-1],]
  source <- source[!duplicated(data.frame(source$Home,source$Away)),]
  em <- compileIndex(data = source, xi = abs(xi$decay$decay), fordate = dateID[i])
  saveIndex(country = "England", year = 2008, indexdata = em)
}; rm(i, dateID, source, em)





