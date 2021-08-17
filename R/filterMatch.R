filterMatch <- function(dat) {
  # the soccer matches includes a lot of un-wellkown teams in cups,
  #   therefore filter and only take the teams from leagues for further compileIndex(),
  #   caculation purpose

  tble = dat$Table
  lg = sort(as.character(dat$leagueID[(regexpr("PO$",dat$leagueID$Name)<0) &
                                   (regexpr("League",dat$leagueID$Cat)>0),]$Name))
  team = sort(unique(c(as.character(subset(tble, as.character(League) %in% lg)$Home),
                  as.character(subset(tble, as.character(League) %in% lg)$Away))))
  db = subset(tble, (as.character(Home) %in% team) & (as.character(Away) %in% team))
  db$League = factor(db$League); db$Home = factor(db$Home); db$Away = factor(db$Away)
  return(db)
  }

