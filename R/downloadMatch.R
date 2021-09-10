stutils::globalVariables(c("MatchID_7M", "MatchID_NG", "Round", "KODate",
                           "Home", "Away", "FTHG", "FTAG", "HTHG", "HTAG"))
#' @title Read a Rmodel query
#'
#' @description download soccer matches from Gooooal.com.
#'
#' @details This function download the soccer matches from Gooooal.com.
#'
#' @seealso \url{https://www.github.com/englianhu/Rmodel} for more details.
#' @examples
#' # this will stop
#' \dontrun{ downloadMatch(URL, year) }
#' #returns data as a data frame
#' #setwd(system.file("data", package = "Rmodel"))
#' #getwd()
#' \dontrun{ downloadMatch(URL, year) }
#' \dontrun{
#' URL = "http://app.en.gooooal.com/soccer/statistic/standing.do?lid=4"
#' eng2012 = downloadMatch(URL, year = 2012) }
#' \dontrun{
#' URL = "http://app.en.gooooal.com/soccer/statistic/standing.do?lid=4"
#' eng2012 = downloadMatch(URL, year = 2013) }
#'
#' @param filename Relative filepath to the current working directory. This must be (*.zip) or it will throw an error.
#' @import RCurl
#' @import XML
#' @import stringr
#' @return A list of data frame.
#' @family Rmodel functions
#' @export
#'
downloadMatch = function(URL, year) {
  # simulate to scrape English soccer matches
  # URL = "http://app.en.gooooal.com/soccer/statistic/standing.do?lid=4"
  # eng2012 = downloadMatch(URL, year = 2012)
  # eng2013 = downloadMatch(URL, year = 2013)
  require(RCurl, quietly = T)
  require(XML, quietly = T)
  require(stringr, quietly = T)
  options(warn = -1)

  # season id, and input url address
  sid = year
  ori.url = paste(URL,"&sid=", sid, sep = "")

  # -----------------------------------------------------------
  listLinks = function (URL, pattern = "", relative = FALSE) {
    doc = htmlParse(URL)
    if(is.null(grep("/$", URL)))
      URL = dirname(URL)
      nodes = getNodeSet(doc, "//a[@href]")
      hrefs = sapply(nodes, function(x) xmlGetAttr(x, "href"))
      paste(if(relative)
      ""
    else URL, hrefs[grep(pattern, hrefs)], sep = "")
  }
# -----------------------------------------------------------
  # download the webpages' url from the website
  lnk = listLinks(ori.url, relative = TRUE)
  lnk = paste("http://app.en.gooooal.com/soccer/statistic/", lnk, sep="")
  std.url = lnk[regexpr("standing", lnk) > 0]
  cup.url = std.url[regexpr("Latest Round", getURL(std.url)) > 0]
  league.url = std.url[regexpr("Latest Round", getURL(std.url)) <= 0]
  lid = unique(unlist(str_match_all(league.url,
    "lid=*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]")))
  league.url = paste(strsplit(lnk[regexpr("result_fixture", lnk) > 0], lid[1])[[1]][1], lid,
    strsplit(lnk[regexpr("result_fixture", lnk) > 0], lid[1])[[1]][2], sep = "")
  lcid = unique(unlist(str_match_all(lnk,
    "lid=*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]*[0-9]")))
  rm(std.url)

  # get the leagues and cups from the source website url
  league = str_match_all(getURL(lnk[1]), "[^<>]+[a-zA-Z]+[^</a>]")
  regexp1 = as.numeric(regexpr("lid=", unlist(league)) > 0) # get the league id
  regexp2 = c(0, regexp1[-length(regexp1)]) #get the league name
  leagueID = data.frame(ID = as.numeric(gsub("lid=", "", lcid)),
    Name = unlist(league)[(regexp2 > 0) & (regexpr("\\b[A-Z]", unlist(league)) > 0)])
  i = as.numeric(gsub("lid=", "", lid))
  leagueID$Cat = ifelse(leagueID$ID %in% i, "League", "Cup")
  x = apply(leagueID, 1, function(x) nchar(x['Name']))
  leagueID$Cat = ifelse((substr(leagueID$Name, x - 1, x) == "PO"), "League", leagueID$Cat)
  rm(x, league, regexp1, regexp2)

  # counting and get the rounds of competition and matches' result
  lRound = str_match_all(getURL(league.url), "Round *[0-9]*[0-9]*[0-9]")
  league.url2 = lapply(lRound, function(x) {
    paste(rownames(x), gsub("Round ", "&roundNum=", x), sep = "")
  })
  rnd = unlist(lapply(lRound,function(x) {
    max(as.numeric(gsub("Round ", "", x)))
  }))
  leagueID$Rnd = 0
  leagueID$Rnd[leagueID$ID %in% i] = rnd
  rm(i, rnd)

  # get the matches' and round of cups
  txt = unlist(str_match_all(getURL(cup.url), "[^<>]+[0-9a-zA-Z]+[^</a>]"))
  rg1 = as.numeric(regexpr("option value=",txt) > 0)
  rg2 = c(0, rg1[-length(rg1)])
  cRnd = data.frame(Rid = txt[rg1 == 1], Rname = txt[rg2 == 1])
  cRnd$Rid = as.character(cRnd$Rid)
  cRnd$Rname = as.character(cRnd$Rname)

  # delete duplicated match it might due to system/human error
  for(i in rev(seq(cRnd$Rname))) if(i > 1) {
    if(cRnd$Rname[i]==cRnd$Rname[i-1]) { cRnd = cRnd[-i,] } }

  cRnd = cRnd[(regexpr("[0-9]{4}", cRnd$Rname) < 0),]
  rownames(cRnd) = NULL
  cRnd$lid = ifelse(cRnd$Rname == "Final", 1, 0)
  cRnd$lid[as.numeric(rownames(cRnd[(regexpr("Final",cRnd$Rname) > 0),]))] =
  leagueID$ID[leagueID$Rnd == 0]
  ic = as.numeric(rownames(cRnd)[cRnd$lid > 0])
  cRnd$lid = rep(cRnd$lid[cRnd$lid > 0], c(ic[1], diff(ic)))
  cRnd$Rid = as.numeric(gsub("[^0-9]", "", cRnd$Rid))

  cup.url2 = paste(rep(cup.url, c(ic[1], diff(ic))), "&l=", cRnd$Rid, sep = "")
  splcon = paste(unlist(strsplit(cup.url2, "&sid=")))[seq(1, (length(cup.url2) * 2), 2)]
  cup.url2 = split(cup.url2, splcon)
  names(cup.url2) = NULL
  cRnd = cRnd[order(cRnd$lid),]
  nm1 = unlist(lapply(split(cRnd, cRnd$lid), nrow))
  leagueID = leagueID[order(leagueID$ID),]
  leagueID$Rnd[leagueID$Rnd == 0] = nm1
  rm(txt, splcon, rg1, rg2, ic, nm1)

  # get the 90 minutes result from Chinese version webpage.
  # scoring / event time only available in Chinese version
  #    for future In-play odds modelling use.
  cup.res = lapply(cup.url2,function(x) {
    gsub("http://app.en.gooooal.com/soccer/statistic/standing.do",
         "http://app.gooooal.com/cup2.do",x)
  })
  attr(cup.res, "names") = unlist(lapply(cup.res, function(x) {
    str_match_all(strsplit(x, "&sid=")[[length(x)]][1], "[0-9]*[0-9]$")
  }))
  cup.res = cup.res[as.character(sort(as.numeric(names(cup.res))))]

  # filter and combine the leagues' and cups' round names
  lnk = c(league.url2, cup.url2)
  nme = lapply(lnk, function(x) {str_match_all(
    strsplit(x, "&sid=")[[length(x)]][1], "[0-9]*[0-9]$")})
  names(lnk) = as.list(unlist(nme))
  lnk = lnk[as.character(sort(as.numeric(names(lnk))))]

  nme = lapply(lRound, function(x) {
    str_match_all(strsplit(rownames(x), "&sid=")[[length(x)]][1], "[0-9]*[0-9]$")
  })
  names(lRound) = as.list(unlist(nme))
  lcRnds = c(lRound, split(cRnd$Rname, cRnd$lid))
  lcRnds = lcRnds[as.character(sort(as.numeric(names(lcRnds))))]
  lcRnds = lapply(lcRnds, function(x) {
    rownames(x) = NULL; as.character(x)
  })
  rm(nme, league.url, cup.url, league.url2, cup.url2, lid, lcid, lRound, cRnd)

  # scrape the tables from websites
  tab = lapply(lnk, function(x) {readHTMLTable(htmlParse(getURL(x)))})

  # delete the aggregate table of cup home-away matches
  for(i in rev(seq(tab))) {
    for(j in rev(seq(tab[[i]]))) {
      if(length(tab[[i]][[j]]) > 0) {
        if(names(tab[[i]][[j]])[2] == "Agg.") {
        tab[[i]][j] = NULL
        }
      }
    }
  };rm(i, j)

  for(i in seq(tab)){
    attr(tab[[i]], "names") = lcRnds[[i]]
  }
  tab = lapply(as.list(seq(tab)), function(i) lapply(as.list(seq(tab[[i]])),
    function(j) {
      data.frame(Round = ifelse(
        is.null(tab[[i]][[j]]), list(NULL),
        names(tab[[i]][j])), tab[[i]][[j]][1:5])
    }))

  attr(tab, "names") = as.character(leagueID[order(as.numeric(names(lcRnds))),]$Name)
  for(i in seq(tab)){ attr(tab[[i]],"names") = lcRnds[[i]] }

  # delete the rounds of competition which has no matches
  for(i in rev(seq(tab))) {
    for(j in rev(seq(tab[[i]]))) {
      if(length(tab[[i]][[j]]) == 0) tab[[i]][j] = NULL
    }
  }
  rm(i, j)

  # merge all rounds within a league/cup to be one data frame per list. Which
  #       means merge nested lists to be one-level list
  tab = lapply(tab, function(z) {
    Reduce(function(x, y)
    merge(x, y, all = T, incomparables = NA), z, accumulate = F)
  })

  # get table from Chinese version
  cup.tab = lapply(cup.res, function(x) {
    readHTMLTable(htmlParse(getURL(x)))
  })
  rm(cup.res)

  # delete the aggregate table of cup home-away matches (Chinese version)
  for(i in rev(seq(cup.tab))) {
    for(j in rev(seq(cup.tab[[i]]))) {
      if((length(cup.tab[[i]][[j]]) == 0)|(length(cup.tab[[i]][[j]]) == 5)) {
        cup.tab[[i]][j] = NULL
      }
    }
  }
  rm(i, j)

  # set a temporary match id to avoid disorder (Chinese version)
  for(i in seq(cup.tab)) {
    for(j in seq(cup.tab[[i]])) {
      cup.tab[[i]][[j]] = na.omit(data.frame(
        leagueID = i, roundID = j, cup.tab[[i]][[j]]))
      cup.tab[[i]][[j]] = data.frame(
        cup.tab[[i]][[j]][1:2], matchID = rev(1:nrow(cup.tab[[i]][[j]])),
        cup.tab[[i]][[j]][6])
    }
  }
  rm(i, j)

  cup.tab = lapply(cup.tab, function(z) {
    Reduce(function(x, y)
    merge(x, y, all = T, incomparables = NA), z, accumulate = F)
  })
  attr(cup.tab,"names") = as.character(leagueID[leagueID$ID %in% names(cup.tab),]$Name)

  tab2 = tab
  for(i in seq(tab2)) {
    for(j in seq(cup.tab)) {
      if(names(tab2)[i] == names(cup.tab)[j]) {
        tab2[[i]]$Score = as.character(unlist(cup.tab[[j]][4]))
      }
    }
  }
  rm(i, j, cup.tab)

  # convert integer Unix Time format to normal date time format
  tab2 = lapply(seq(tab2),function(i) {
    y = as.numeric(unlist(str_match_all(tab2[[i]]$KO, "^([^\\(]+)(\\((.+)\\))?")))
    tab2[[i]]$KO = y[seq(0, length(y), 4)] / 1000
    tab2[[i]]$KO = as.POSIXct(tab2[[i]]$KO, tz = "", origin = "1970-01-01 00:00:00")
    tab2[[i]]$FTHG = as.numeric(gsub("-[0-9]", "", tab2[[i]]$Score))
    tab2[[i]]$FTAG = as.numeric(gsub("[0-9]-", "", tab2[[i]]$Score))
    tab2[[i]]$HTHG = as.numeric(gsub("-[0-9]", "", tab2[[i]]$HT))
    tab2[[i]]$HTAG = as.numeric(gsub("[0-9]-", "", tab2[[i]]$HT))
    tab2[[i]]$Neutral = ifelse(tab2[[i]]$Round == "Final", 1, 0)
    tab2[[i]]$Score = tab2[[i]]$HT = NULL
    names(tab2[[i]]) = gsub("KO", "Date", names(tab2[[i]]))
    tab2[[i]] })
  attr(tab2,"names") = names(tab)
  rm(tab)

  # delete entire league/cup which has no matches
  for(i in rev(seq(tab2))) {
    if(!is.data.frame(tab2[[i]])) tab2[[i]] = NULL
  }
  rm(i)

# -------------------------------------------------------------------------------------
  # Data in 2008, "Conference National" replaced by "Conference" to follow other sason's name
  #   can be deleted if there is no such human error beyond the following seasons.
  leagueID$Name = gsub("Conference National$", "Conference", leagueID$Name)
  names(tab2) = gsub("Conference National$", "Conference", names(tab2))
  for(i in seq(tab2)) {
    tab2[[i]]$Home = gsub("Norwich$", "Norwich City", tab2[[i]]$Home)
    tab2[[i]]$Away = gsub("Norwich$", "Norwich City", tab2[[i]]$Away)
    tab2[[i]]$Home = factor(tab2[[i]]$Home)
    tab2[[i]]$Away = factor(tab2[[i]]$Away)
  }

# -------------------------------------------------------------------------------------
# categorize the teams in league/cup
  lg = as.list(as.character(eng2008$leagueID$Name))
  names(lg) = as.character(eng2008$leagueID$Name)
  team = list()
  team = lapply(seq(tab2),function(i) {
    if(names(tab2)[[i]] %in% lg) {
      team[[i]] = sort(unique(c(as.character(tab2[[i]]$Home),
                              as.character(tab2[[i]]$Away))))
      }
    })
  names(team) = names(tab2)
  team = lapply(seq(team), function(i)
    data.frame(Name = names(team)[[i]], Team=team[[i]]))
  names(team) = names(tab2)

  eng2008 = list(Table = eng2008$Table, URL = eng2008$URL, teamID = team,
                 leagueID = eng2008$leagueID, lnk = eng2008$lnk,
                 Rounds = eng2008$Rounds)
  rm(tab2, team, lg)

# -------------------------------------------------------------------------------------

  res = list(Table = tab2, leagueID = leagueID, teamID = team, URL = sid,
             lnk = lnk, Rounds = lcRnds)
  options(warn = 0)
  return(res)
  }

