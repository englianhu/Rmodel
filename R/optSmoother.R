optSmoother <- function(country, year, mbase, dbaseData = "FT", homeavd = TRUE,
                            basic = TRUE, inflated = TRUE) {
  # this function calculate the optimal decay/smoother rate
  # dbase is loading the Access *.mdb file which are teams rating database, 
  # mbase is loading the matchdata which may get from loadData or other else,
  # dbaseData = "FT" or "HT", there are only two options otherwise will stop process.
  # dbaseData = "FT" indicates calculate Full-Time lambda from index, default is "FT".
  # homeavd determine if calculate home advantage or neutral ground. Default is TRUE.
  # basic determine to import if static/dynamic team rating. Default is TRUE.
  # inflated determine if zero inflated or not. 
  # xi <- optSmoother(country="England", year=2008, mbase = engmdb); xi$decay

  options(warn = -1); require(RODBC)
  templist <- list(mbase = substitute(mbase), dbaseData = dbaseData,
                   basic = basic, inflated = inflated)
  tempcall <- as.call(c(expression(optimalSmoother), templist)); rm(templist)
  # --------------------------------------------------------------
  if(basic == TRUE){
    bcHT <- function(x) { paste('bsHT_', substitute(x), sep = '')}
    bcFT <- function(x) { paste('bsFT_', substitute(x), sep = '')}
  } else {
    bcHT <- function(x) { paste('tsHT_', substitute(x), sep = '')}
    bcFT <- function(x) { paste('tsFT_', substitute(x), sep = '')}}
  
  getData <- function(tble, country=country, year=year) {
    con <- odbcConnectAccess2007(paste("./data/",country,"/",
                                       year,".mdb",sep=""))
    x <- structure(sqlFetch(con, tble)[-1], row.names = 
                     as.character(as.Date(sqlFetch(con, tble)$Date)))
    colnames(x) <- gsub('_', ' ', colnames(x)); odbcClose(con); rm(con); x }
  # --------------------------------------------------------------
  diagdraw <- function(x) { z <- 1:length(x); thetap <- prob * theta
                            for(i in 1:length(z)) { dimnames(x[[i]]) <-
                                                      list(seq(0,(nrow(x[[i]])-1),1),seq(0,(nrow(x[[i]])-1),1)) }; rm(i)
                            tplist <- rep(list(diag(thetap,nrow(x[[1]]),nrow(x[[1]]))),length(z)) 
                            y <- lapply(z, function(z) x[[z]] + tplist[[z]])
                            lapply(z, function(z) y[[z]]/sum(y[[z]])) }
  # --------------------------------------------------------------
  if(dbaseData == "FT") {
    Offence <- getData(tble = bcFT(Offence), country=country, year=year)
    Defence <- getData(tble = bcFT(Defence), country=country, year=year) 
    Effects <- getData(tble = bcFT(Effects), country=country, year=year); rm(bcFT)
  } else if(dbaseData == "HT"){
    Offence <- getData(tble = bcHT(Offence), country=country, year=year)
    Defence <- getData(tble = bcHT(Defence), country=country, year=year) 
    Effects <- getData(tble = bcHT(Effects), country=country, year=year); rm(bcHT)
  } else { stop("Only two options 'FT' or 'HT'") }
  teamID <- names(Offence)
  mbase <- subset(mbase, Home %in% teamID & Away %in% teamID)
  mbase$Home <- factor(mbase$Home); mbase$Away <- factor(mbase$Away)
  mbase$Date <- as.Date(mbase$Date)
  matchdata <- data.frame(mbase, 
                          hmat = apply(mbase, 1, function(x) Offence[x['Date'],x['Home']]), 
                          hmdf = apply(mbase, 1, function(x) Defence[x['Date'],x['Home']]),
                          awat = apply(mbase, 1, function(x) Offence[x['Date'],x['Away']]),
                          awdf = apply(mbase, 1, function(x) Defence[x['Date'],x['Away']]),
                          home = apply(mbase, 1, function(x) Effects[x['Date'],'Home']),
                          effects = apply(mbase, 1, function(x) Effects[x['Date'],'Effect']))

  if(inflated == TRUE)
    { matchdata <- data.frame(matchdata, 
                            p = apply(mbase, 1, function(x) Effects[x['Date'],'p']),
                            theta = apply(mbase, 1, function(x) Effects[x['Date'],'theta']))
    prob <- matchdata$p; theta <- matchdata$theta
  } else { prob <- 0; theta <- 0 }

  matchdata <- matchdata[!is.na(matchdata$hmat),]
  
  if(homeavd == FALSE) { lmb1 <- (1 - prob) * matchdata$hmat * matchdata$awdf
  } else { lmb1 <- (1 - prob) * matchdata$hmat * matchdata$awdf * matchdata$home }
  
  lmb2 <- (1 - prob) * matchdata$awat * matchdata$hmdf
  lmb3 <- (1 - prob) * matchdata$effects
  matchdata$hmat <- matchdata$hmdf <- matchdata$awat <- matchdata$awdf <- NULL
  matchdata$home <- matchdata$effects <- NULL
  
  nl <- 1:length(lmb1)
  mxt <- lapply(nl, function(z){outer(0:20, 0:20, function(x, y) 
    bvp(x, y, lambda = c(lmb1[z], lmb2[z], lmb3[z])))})
  wdw <- data.frame(win  = unlist(lapply(nl, function(z) 
    sum(mxt[[z]][row(mxt[[z]]) >  col(mxt[[z]])]))),
    draw = unlist(lapply(nl, function(z) 
      sum(mxt[[z]][row(mxt[[z]]) == col(mxt[[z]])]))),
    lose = unlist(lapply(nl, function(z) 
      sum(mxt[[z]][row(mxt[[z]]) <  col(mxt[[z]])]))))
  wdw$draw <- prob * theta + wdw$draw; wdw <- na.omit(wdw/apply(wdw,1,sum))
  compdat <- data.frame(matchdata,wdw)
  compdat$hmat <- compdat$hmdf <- compdat$awat <- compdat$awdf <- NULL
  compdat$home <- compdat$effects <- compdat$p <- compdat$theta <- NULL
  rm(lmb1, lmb2, lmb3, matchdata, nl, mxt, wdw)
  
  if(dbaseData == "FT") { 
    opt <- function(dat){
      lg.h = as.numeric(dat$FTHG >  dat$FTAG);
      lg.d = as.numeric(dat$FTHG == dat$FTAG);
      lg.a = as.numeric(dat$FTHG <  dat$FTAG)
      sumxi = sum(lg.h*log(dat$win) + lg.d*log(dat$draw) + 
                    lg.a*log(dat$lose))
      # dydf = as.numeric(sum(max(as.Date(dat$Date, 
      #        tz = Sys.timezone)) - as.Date(dat$Date,
      #        tz = Sys.timezone)))
      dydf = as.numeric(sum(max(as.Date(dat$Date)) - as.Date(dat$Date))) 
      xi = sumxi/dydf; xi }
  } else if(dbaseData == "HT") {
    opt <- function(dat) {
      lg.h = as.numeric(dat$HTHG >  dat$HTAG);
      lg.d = as.numeric(dat$HTHG == dat$HTAG);
      lg.a = as.numeric(dat$HTHG <  dat$HTAG)
      sumxi = sum(lg.h*log(dat$win) + lg.d*log(dat$draw) + 
                    lg.a*log(dat$lose))
      dydf = as.numeric(sum(max(as.Date(dat$Date, 
             tz = Sys.timezone)) - as.Date(dat$Date, 
             tz = Sys.timezone)))
      xi = sumxi/dydf; xi }
  } else { stop("Only two options 'FT' or 'HT'") }

  xi <- data.frame(Start = min(compdat$Date), End = max(compdat$Date),
                   decay = opt(compdat))
  
  result <- list(call = tempcall, decay = xi, wdw = compdat, p = prob, theta = theta,
                 offence = Offence, defence = Defence, effects=Effects) 
  options(warn = 0); class(result) <- 'optSmoother'
  result }
