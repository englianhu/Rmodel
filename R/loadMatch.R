loadMatch = function(country, year) {
  # dat is the name of result from function downloadMatch()
  #   country is the name of folder, year is the name of mdb file.
  # example:
  #   year = 2013
  #   eng2013 = downloadMatch(URL, 2013)
  #   dat = eng2013
  
  require(RODBC, quietly = T)
  options(warn = -1)
  
  league = dir(paste("./data/",country,sep=""))[regexpr("mdb$",dir(paste("./data/",country,sep="")))<0]
  country = ifelse(is.character(country), country, "Please key in country name in character")
  
  leagueID = sqlFetch(odbcConnectAccess2007(paste("./data/",country,"/",year,".mdb",sep="")),sqtable="leagueID")
  odbcCloseAll()
  
  if(length(year) == 1){
    con = lapply(league, function(x) { 
      odbcConnectAccess2007(paste("./data/", country, "/",
                                  x, "/", year, ".mdb", sep = "")) })
    
    conM = lapply(seq(as.list(league)), function(i) {
      if(any(regexpr("matches",sqlTables(con[[i]])$TABLE_NAME) > 0)) {
        sqlFetch(con[[i]], sqtable = 'matches') }})
    names(conM) = league

    conT = lapply(seq(as.list(league)), function(i) {
      if(any(regexpr("team",sqlTables(con[[i]])$TABLE_NAME) > 0)) {
        sqlFetch(con[[i]], sqtable = 'team') }})
    names(conT) = league
    odbcCloseAll(); rm(con)
    
    # delete entire league/cup if one of season has no matches
    for(i in rev(seq(league))) {
      if(is.null(conM[[i]])) conM[[i]] = NULL
    }; rm(i)
    
    # insert league name and season into data frame
    for(i in seq(conM)) {
      conM[[i]] = data.frame(Season = year, League = names(conM)[i], conM[[i]][-1])
    }; rm(i,j)
    
    # merge all leagues/cups in that country to be one data frame
    tab = Reduce(function(x, y)
      merge(x, y, all = T, incomparables = NA), conM, accumulate = F)
    
    tab = data.frame(matchID = 1:nrow(tab),tab[order(tab$Date),])
    rownames(tab) = NULL; rm(conM)
    
  } else {
    con = lapply(league, function(x) lapply(seq(year),
                                            function(j) odbcConnectAccess2007(paste("./data/", country, "/",
                                                                                    x, "/", year[j], ".mdb", sep = ""))))
    
    conM = lapply(as.list(seq(league)), function(i) lapply(as.list(seq(year)),
                                                           function(j) if(any(regexpr("matches", sqlTables(con[[i]][[j]])$TABLE_NAME) > 0)) {
                                                             sqlFetch(con[[i]][[j]], sqtable = 'matches') }))
    names(conM) = league
    for(i in seq(league)){ names(conM[[i]]) = year }
    odbcCloseAll(); rm(con)
    
    # delete entire league/cup if one of season has no matches
    for(i in rev(seq(league))) for(j in rev(seq(year))) {
      if(is.null(conM[[i]][[j]])) conM[[i]] = NULL
    }; rm(i)
    
    ylist = rep(list(as.list(year)),length(names(conM)))
    ylist = lapply(seq(ylist),function(i)lapply(seq(ylist[[i]]),function(j) {
      rep(ylist[[i]][[j]],nrow(conM[[i]][[j]])) }))
    llist = lapply(names(conM),function(x)rep(list(x),each=2))
    llist = lapply(seq(llist),function(i)lapply(seq(llist[[i]]),function(j) {
      rep(llist[[i]][[j]],nrow(conM[[i]][[j]])) }))
    
    # insert league name and season into data frame
    conM = lapply(seq(conM),function(i) lapply(seq(year),function(j){
      data.frame(Season = ylist[[i]][[j]],League = llist[[i]][[j]],
                 conM[[i]][[j]][-1]) }))
    
    # merge different year data under a league/cup. Which
    #       means merge nested lists to be one-level list
    tab = lapply(conM, function(z) Reduce(function(x, y)
      merge(x, y, all = T, incomparables = NA), z, accumulate = F))
    
    # merge all leagues/cups in that country to be one data frame
    tab = Reduce(function(x, y)
      merge(x, y, all = T, incomparables = NA), tab, accumulate = F)
    
    tab = data.frame(matchID = 1:nrow(tab),tab[order(tab$Date),])
    rownames(tab) = NULL; rm(conM) }
  
  res = list(Table = tab, leagueID = leagueID, teamID = conT)
  options(warn = 0)
  return(res) }