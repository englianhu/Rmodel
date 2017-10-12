saveIndex <- function(country, year, indexdata = NULL, oddsdata = NULL, basic = TRUE, FT = TRUE) {
  # year is the names of the team rating Access *.mdb file we want to naming.
  # indexdata is the calculated em result from compileIndex. basic and FT only workable
  # for save indexdata, they will useless either TRUE or FALSE for save oddsdata.
  # basic determines if we want to save bs_offence or ts_offence... from em$offence...
  # It only workable after choose rating as TRUE.
  # FT = TRUE/FALSE is save FT/HT rating. If basic=TRUE and FT=TRUE, then 'bsFT_'
  # exmp : em <- compileIndex(data = sourcedata); saveIndex("England", 2008, indexdata = em2008)
  # oddsdata is the calculated odds from compileOdds.
  # exmp : matchdata <- loadMatch("England", 2008)
  #        odds <- compileOdds(eng2008, matchdata); saveData(eng2008, oddsdata = odds)
  
  require(RODBC, quietly = T)
  options(warn = -1)
  year <- substitute(year)
  country = ifelse(is.character(country), country, "Please key in country name in character")
  
  con <- odbcConnectAccess2007(paste("./data/",country,"/",
                                 year, ".mdb", sep = ''))
  
  if(!is.null(indexdata)) { indexdata <- indexdata
                            names(indexdata$offence) <- gsub(' ','_',names(indexdata$offence))
                            names(indexdata$defence) <- gsub(' ','_',names(indexdata$defence))
                            
                            bsc <- function(x) { if(basic == TRUE) {
                              if(FT == TRUE) { paste('bsFT_', substitute(x), sep = '') 
                              } else { paste('bsHT_', substitute(x), sep = '') }
                            } else {
                              if(FT == TRUE) { paste('tsFT_', substitute(x), sep = '') 
                              } else { paste('tsHT_', substitute(x), sep = '') } } }
                            Offence <- bsc(Offence); Defence <- bsc(Defence); Effects <- bsc(Effects); rm(bsc)
                            
                            if(length(sqlTables(con)$TABLE_NAME) == 4) {
                              sqlSave(con, dat=indexdata$offence, tablename=Offence, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
                              sqlSave(con, dat=indexdata$defence, tablename=Defence, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
                              sqlSave(con, dat=indexdata$effects, tablename=Effects, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
                            } else {
                              sqlSave(con, dat=indexdata$offence, tablename=Offence, append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
                              sqlSave(con, dat=indexdata$defence, tablename=Defence, append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
                              sqlSave(con, dat=indexdata$effects, tablename=Effects, append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date')) }
                            close(con); rm(con)
                            
  } else { 
    oddsdata <- oddsdata
    if(length(sqlTables(con)$TABLE_NAME) == 4) {
      if(!is.null(oddsdata$FTWDW) == TRUE) {
        sqlSave(con, dat=oddsdata$FTWDW, tablename='FTWDW', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTAH, tablename='FTAH', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTOU, tablename='FTOU', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTCS, tablename='FTCS', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTTG, tablename='FTTG', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTOE, tablename='FTOE', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTTOU, tablename='FTTOU', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))}
      if(!is.null(oddsdata$HTWDW) == TRUE) {
        sqlSave(con, dat=oddsdata$HTWDW, tablename='HTWDW', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTAH, tablename='HTAH', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTOU, tablename='HTOU', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTCS, tablename='HTCS', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTTG, tablename='HTTG', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTOE, tablename='HTOE', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTTOU, tablename='HTTOU', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))}
      if((!is.null(oddsdata$HTWDW) == TRUE) & (!is.null(oddsdata$FTWDW) == TRUE)) {
        sqlSave(con, dat=oddsdata$HTFT, tablename='HTFT', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HWMG, tablename='HWMG', addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))}
    } else {
      if(!is.null(oddsdata$FTWDW) == TRUE) {
        sqlSave(con, dat=oddsdata$FTWDW, tablename='FTWDW', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTAH, tablename='FTAH', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTOU, tablename='FTOU', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTCS, tablename='FTCS', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTTG, tablename='FTTG', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTOE, tablename='FTOE', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$FTTOU, tablename='FTTOU', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))}
      if(!is.null(oddsdata$HTWDW) == TRUE) {
        sqlSave(con, dat=oddsdata$HTWDW, tablename='HTWDW', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTAH, tablename='HTAH', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTOU, tablename='HTOU', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTCS, tablename='HTCS', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTTG, tablename='HTTG', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTOE, tablename='HTOE', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HTTOU, tablename='HTTOU', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))}
      if((!is.null(oddsdata$HTWDW) == TRUE) & (!is.null(oddsdata$FTWDW) == TRUE)) {
        sqlSave(con, dat=oddsdata$HTFT, tablename='HTFT', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date'))
        sqlSave(con, dat=oddsdata$HWMG, tablename='HWMG', append=T, addPK=T, rownames=F, fast=F, varTypes=c(Date='Date')) }
      close(con); rm(con) } } }