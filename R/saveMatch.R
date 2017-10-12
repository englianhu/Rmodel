saveMatch = function(dat, country, year) {
  # dat is the name of result from function downloadMatch()
  #   country is the name of folder, year is the name of mdb file.
  # example:
  #   year = 2013
  #   eng2013 = downloadMatch(URL, 2013)
  #   dat = eng2013; country = "England"
  #
  # create 2013.mdb in below path
  # example:
  #   paste("./data/",country,"/",names(dat$Table)[i],"/", year, ".mdb", sep = "")
  #   [1] "./data/England/Premier League/2013.mdb"
  #   
  #   saveMatch(dat=eng2008, country="England", year=2008)
  
  require(RODBC, quietly = T); options(warn = -1)
  lapply(seq(dat$Table), function(i) { res = na.omit(dat$Table)[[i]]
                                       tm = dat$teamID[[i]]
                                       res = data.frame(matchID = 1:nrow(dat$Table[[i]]), dat$Table[[i]])
                                       con = odbcConnectAccess2007(paste("./data/",country,"/",
                                             names(dat$Table)[i],"/", year, ".mdb", sep = ""))
                                       sqlSave(con, dat = res, tablename = 'matches', addPK = 'matchID',
                                               rownames = F, fast = T, varTypes = c(Date = 'Date'))
                                       sqlSave(con, dat = tm, tablename = 'team', addPK = 'teamID',
                                               rownames = F, fast = T)
                                       options(warn = 0)
                                       odbcClose(con) })
  con = odbcConnectAccess2007(paste("./data/",country,"/", year, ".mdb", sep = ""))
  sqlSave(con, dat = dat$leagueID, tablename = 'leagueID', addPK = 'matchID',
          rownames = F, fast = T);odbcClose(con) }
