saveMatch = function(dat, country, year) {
  # dat is the name of result from function downloadMatch()
  #   country is the name of folder, year is the name of accdb file.
  # example:
  #   year = 2013
  #   eng2013 = downloadMatch(URL, 2013)
  #   dat = eng2013; country = "England"
  #
  # create 2013.accdb in below path
  # example:
  #   paste("E:/Database/",country,"/",names(dat$Table)[i],"/", year, ".accdb", sep = "")
  #   [1] "E:/Database/England/Premier League/2013.accdb"
  #   
  #   saveMatch(dat=eng2008, country="England", year=2008)
  
  require(RODBC, quietly = T); options(warn = -1)
  lapply(seq(dat$Table), function(i) { res = na.omit(dat$Table)[[i]]
                                       tm = dat$teamID[[i]]
                                       res = data.frame(matchID = 1:nrow(dat$Table[[i]]), dat$Table[[i]])
                                       con = odbcConnectAccess2007(paste("E:/Database/",country,"/",
                                             names(dat$Table)[i],"/", year, ".accdb", sep = ""))
                                       sqlSave(con, dat = res, tablename = 'matches', addPK = 'matchID',
                                               rownames = F, fast = T, varTypes = c(Date = 'Date'))
                                       sqlSave(con, dat = tm, tablename = 'team', addPK = 'teamID',
                                               rownames = F, fast = T)
                                       options(warn = 0)
                                       odbcClose(con) })
  con = odbcConnectAccess2007(paste("E:/Database/",country,"/", year, ".accdb", sep = ""))
  sqlSave(con, dat = dat$leagueID, tablename = 'leagueID', addPK = 'matchID',
          rownames = F, fast = T);odbcClose(con) }
