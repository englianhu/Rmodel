# simulate to scrape English soccer matches
source("C:/Users/xun/Desktop/downloadMatch.R")
URL = "http://app.en.gooooal.com/soccer/statistic/standing.do?lid=4"

# Chinese version url as below...
# URL2 = "http://app.gooooal.com/competition.do?lid=4&sid=2013&pid=6&lang=tr"

# data of Conference in 2008 term as Confenrece National
#  different with the rest of later season. There are some extra coding
#  inside the downloadMatch function which can be deleted if there is no
# such human error beyond the following seasons
eng2008 = downloadMatch(URL, year = 2008)
eng2009 = downloadMatch(URL, year = 2009)
eng2010 = downloadMatch(URL, year = 2010)
eng2011 = downloadMatch(URL, year = 2011)
eng2012 = downloadMatch(URL, year = 2012)
eng2013 = downloadMatch(URL, year = 2013)
rm(downloadMatch)

#=========================================================
# Need to add teamID into downloadMatch function....
tab2 = eng2008$Table
lg = as.list(as.character(eng2008$leagueID$Name))
names(lg)=as.character(eng2008$leagueID$Name)
team = list()
team = lapply(seq(tab2),function(i) {
  if(names(tab2)[[i]]%in%lg) {
    team[[i]] = sort(unique(c(as.character(tab2[[i]]$Home),
                              as.character(tab2[[i]]$Away))))}})
names(team) = names(tab2)
team = lapply(seq(team),function(i) data.frame(Name=names(team)[[i]],Team=team[[i]]))
names(team) = names(tab2)

eng2008 = list(Table=eng2008$Table, URL=eng2008$URL, teamID=team, leagueID=eng2008$leagueID,
               lnk=eng2008$lnk,Rounds=eng2008$Rounds)
rm(tab2,team,lg)


#---------------------------------------------------------
# Simulate before saveMatch
engdb2008 = Reduce(function(x, y)
   merge(x, y, all = T, incomparables = NA), eng2008$Table, accumulate = F)
engdb2009 = Reduce(function(x, y)
  merge(x, y, all = T, incomparables = NA), eng2009$Table, accumulate = F)
engdb2010 = Reduce(function(x, y)
  merge(x, y, all = T, incomparables = NA), eng2010$Table, accumulate = F)
engdb2011 = Reduce(function(x, y)
  merge(x, y, all = T, incomparables = NA), eng2011$Table, accumulate = F)
engdb2012 = Reduce(function(x, y)
  merge(x, y, all = T, incomparables = NA), eng2012$Table, accumulate = F)
engdb2013 = Reduce(function(x, y)
  merge(x, y, all = T, incomparables = NA), eng2013$Table, accumulate = F)

subset(eng2008db,is.na(FTHG)|is.na(FTAG))
subset(eng2009db,is.na(FTHG)|is.na(FTAG))
subset(eng2010db,is.na(FTHG)|is.na(FTAG))
subset(eng2011db,is.na(FTHG)|is.na(FTAG))
subset(eng2012db,is.na(FTHG)|is.na(FTAG))
subset(eng2013db,is.na(FTHG)|is.na(FTAG))

#=========================================================
# save data into database, the folder and empty accdb file
#   createed prior to run this function.
# dir.create() seems unable create sub-folder, need to modify in future
# E:/Database/England/Premier League/2008.accdb
load("C:/Users/xun/Desktop/eng20082013.RData")
source("C:/Users/xun/Desktop/saveMatch.R")
saveMatch(dat = eng2008, country = "England", year = 2008)
saveMatch(dat = eng2009, country = "England", year = 2009)
# eng2010 matches have postponed matches with NA value
saveMatch(dat = eng2010, country = "England", year = 2010)
saveMatch(dat = eng2011, country = "England", year = 2011)
saveMatch(dat = eng2012, country = "England", year = 2012)
saveMatch(dat = eng2013, country = "England", year = 2013)

#=========================================================
# load soccer matches from folder
source("C:/Users/xun/Desktop/loadMatch.R")
eng2008 = loadMatch(country = "England", year = 2008)
eng2009 = loadMatch(country = "England", year = 2009)
# eng2010 matches have postponed matches with NA value
eng2010 = loadMatch(country = "England", year = 2010)
eng2011 = loadMatch(country = "England", year = 2011)
eng2012 = loadMatch(country = "England", year = 2012)
eng2013 = loadMatch(country = "England", year = 2013)
rm(loadMatch)

# filter the soccer matches which the teams are coming from leagues 
source("C:/Users/xun/Desktop/filterMatch.R")
eng2008f = filterMatch(dat = eng2008)
eng2009f = filterMatch(dat = eng2009)
# eng2010 matches have postponed matches with NA value
eng2010f = filterMatch(dat = eng2010)
eng2011f = filterMatch(dat = eng2011)
eng2012f = filterMatch(dat = eng2012)
eng2013f = filterMatch(dat = eng2013)
rm(filterMatch)

# load soccer matches from folder and filter the soccer matches
#   which the teams are coming from leagues 
source("C:/Users/xun/Desktop/loadMatch.R")
source("C:/Users/xun/Desktop/filterMatch.R")
eng2008f = filterMatch(loadMatch(country = "England", year = 2008))
eng2009f = filterMatch(loadMatch(country = "England", year = 2009))
eng2010f = filterMatch(loadMatch(country = "England", year = 2010))
eng2011f = filterMatch(loadMatch(country = "England", year = 2011))
eng2012f = filterMatch(loadMatch(country = "England", year = 2012))
eng2013f = filterMatch(loadMatch(country = "England", year = 2013))
rm(loadMatch, filterMatch)

# calculate the static team index
source("RModel.R")
em2008 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2008f)
em2009 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2009f)
em2010 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2010f)
em2011 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2011f)
em2012 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2012f)
em2013 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2013f)

# load saveIndex function
source("C:/Users/xun/Desktop/saveIndex.R")
saveIndex(country = "England", year = 2008, indexdata = em2008)
saveIndex(country = "England", year = 2009, indexdata = em2009)
saveIndex(country = "England", year = 2010, indexdata = em2010)
saveIndex(country = "England", year = 2011, indexdata = em2011)
saveIndex(country = "England", year = 2012, indexdata = em2012)
saveIndex(country = "England", year = 2013, indexdata = em2013)

# load loadIndex function
source("C:/Users/xun/Desktop/loadIndex.R")
idx2008=loadIndex(country="England",year=2008,mbase=eng2008f)



#=========================================================
# load DixonColes function from folder
source("C:/Users/ryusukekenji/Desktop/DixonColes.R")





#=========================================================
# save data into database, dir.create()
require(RODBC)
dir.create("E:/Database")
dir.create("E:/Database/England")
# create multi folders, below codes unable work, need to review
dir.path = paste("E:/Databse/England/",names(eng2012),sep="")
for(i in seq(names(eng2012))) dir.create(dir.path[i])

con <- odbcConnectAccess2007(paste("E:/Database/England/", lname,"/", lyear, ".accdb", sep = ""))
res <- na.omit(eng2012) # remove the NA value, which is matches not yet kick-off
res <- data.frame(matchID = 1:nrow(res),res)
sqlSave(con, dat=res, tablename='matchdata', addPK='matchID', rownames=F, fast=T,
        varTypes=c(KO='Date'))
odbcCloseAll()

