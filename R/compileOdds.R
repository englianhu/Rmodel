## Need to review
## Need to review
## Need to review

## -------------------------------------------------------
#' @title Read a Rmodel query
#'
#' @description compile the odds from the index.
#'
#' @details This function read 1 to 2 vectors and then calculate bivariate poisson possibility and returns a vector.
#'
#' @seealso \url{https://www.github.com/englianhu/Rmodel} for calculate the possibility of bivariate poisson model.
#' @examples
#' # this will stop
#' \dontrun{ compileOdds(dbase = england0809, mbase = ab0709, HT = FALSE) }
#‘
#' @param filename Relative filepath to the current working directory. This must be (*.zip) or it will throw an error.
#' @return A list of dataset from the inputted *.RData file.
#' @family Rmodel functions
#' @export
#'
compileOdds <- function(dbase, mbase, HT = TRUE, FT = TRUE, homeavd = TRUE,
                        inflated = TRUE) {
  # dbase is the Access *.mdb file which are teams rating database,
  # mbase is the matchdata which may get from importData or other else,
  # HT and FT = TRUE will calculate both odds, you may choose either or both.
  # homeavd determine if calculate home advantage or neutral ground. Default is TRUE.
  # inflated determine if zero inflated or not.
  # Simulate English Data 2008/09 (england0809.mdb) compile odds from team index
  # odds <- compileOdds(dbase = england0809, mbase = ab0709, HT = FALSE)
  # saveOdds(england0809, odds)
  # Due to R-programming capacity, length of mbase up to 500 only for once,
  # more than 500 need to separate matches into multiple dataframe to calculate.
  # exmp: length(mbase$Date) = 800; mbase1 <- mbase[1:500,]
  #       mbase2 <- mbase[501:800,]

  ### prop.table might useful for review the odds compilation table, examples: correct-scores etc.
  ### prop.table (vector)
  ## prop.table(table(<var_1>))
  ### row-wise proportions (data.frame)
  ## prop.table(table(<var_1>, <var_2>),1)
  ### column-wise proportions
  ## prop.table(table(<var_1>, <var_2>),2)
  ## ??prop.table

  ### margin.table might useful for review the odds compilation table, examples: win-draw-win
  ## ??margin.table

  options(warn = -1)
  require(RODBC)
  templist <- list(dbase = substitute(dbase), mbase = substitute(mbase),
                   HT = HT, FT = FT,
                   homeavd = homeavd, inflated = inflated)
  tempcall <- as.call(c(expression(compileOdds), templist))
  rm(templist)
  dbase <- substitute(dbase)
  datadate <- max(mbase$Date)

  # --------------------------------------------------------------
  tsHT <- function(x) {
    paste('tsHT_', substitute(x), sep = '')
  }
  tsFT <- function(x) {
    paste('tsFT_', substitute(x), sep = '')
  }
  getData <- function(dbdat, tble) {
    db = as.character(dbdat)
    if(!dir.exists('database')) dir.create('database')
    con = odbcConnectAccess(paste("./database/", db, ".mdb", sep = '')) #can use `mplus` open source tool to create empty *.mdb files.
    x = structure(sqlFetch(con, tble)[-1], row.names =
                    as.character(sqlFetch(con, tble)$Date))
    colnames(x) = gsub('_', ' ', colnames(x))
    close(con)
    rm(con)
    x
  }

  # --------------------------------------------------------------
  diagdraw <- function(x) {
    z <- 1:length(x)
    thetap <- prob * theta
    for(i in 1:length(z)) {
      dimnames(x[[i]]) <- list(seq(0, (nrow(x[[i]]) - 1), 1),
                               seq(0,(nrow(x[[i]]) - 1), 1))
    }
    rm(i)

    tplist <- rep(list(diag(thetap, nrow(x[[1]]), nrow(x[[1]]))), length(z))
    y <- lapply(z, function(z)
      x[[z]] + tplist[[z]] * diag(x[[z]]) / sum(diag(x[[z]])))
    res <- lapply(z, function(z) y[[z]] / sum(y[[z]]))
    return(res)
  }

  # --------------------------------------------------------------
  fodds <- function(x, mdata) {
    z <- 1:length(x)
    data.frame(matchdata,
               Win = unlist(
                 lapply(z, function(z)
                   sum(x[[z]][row(x[[z]]) >  col(x[[z]])]))),
               Draw = unlist(lapply(z, function(z)
                 sum(x[[z]][row(x[[z]]) == col(x[[z]])]))),
               Lose = unlist(lapply(z, function(z)
                 sum(x[[z]][row(x[[z]]) <  col(x[[z]])]))))
  }

  # --------------------------------------------------------------
  cscores <- function(x, mdata, matchview = TRUE) {
    z <- 1:length(x)
    if(matchview == TRUE) {
      y <- lapply(z, function(z) x[[z]][1:5, 1:5])
      mxtrnames <- rep(list(c(gsub(' ', '_', paste(
        gsub(' ', '', paste('H', row(y[[1]]) - 1)),
        gsub(' ', '', paste('A', col(y[[1]]) - 1)))),
        'H_UP5', 'A_UP5')), length(z))
      cslist <- lapply(z, function(z)
        data.frame(
          No = z, matrix(c(y[[z]], sum(x[[z]][row(x[[z]]) - col(x[[z]]) >= 5]),
                           sum(x[[z]][col(x[[z]]) - row(x[[z]]) >= 5])),
                         ncol = 27, dimnames = list(NULL, mxtrnames[[z]]))))
      csdf <- Reduce(function(x, y) merge(x, y, all = TRUE),
                     cslist, accumulate = FALSE)
      data.frame(mdata,csdf[-1])
    } else {
      lapply(z, function(z) x[[z]][1:11, 1:11])
    }
  }

  # --------------------------------------------------------------
  handicap <- function(x, mdata, matchview = T) {
    z <- 1:length(x)
    hmhdp1 <- lapply(z, function (z) {
      data.frame(No = seq(1, 15, 4),
                 hmodds = c('HN3.50', 'HN2.50', 'HN1.50', 'HN0.50'),
                 Home = t(data.frame(
                   HN3.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) >= 4]),
                   HN2.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) >= 3]),
                   HN1.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) >= 2]),
                   HN0.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) >= 1]))),
                 awodds = c('AP3.50', 'AP2.50','AP1.50','AP0.50'),
                 Away = t(data.frame(
                   AP3.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) < 4]),
                   AP2.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) < 3]),
                   AP1.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) < 2]),
                   AP0.50 = sum(x[[z]][row(x[[z]]) - col(x[[z]]) < 1]))))
      })

    awhdp1 <- lapply(z, function (z) {
      data.frame(No = seq(17, 29, 4),
                 hmodds = c('HP0.50', 'HP1.50', 'HP2.50', 'HP3.50'),
                 Home = t(data.frame(
                   HP0.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) < 1]),
                   HP1.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) < 2]),
                   HP2.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) < 3]),
                   HP3.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) < 4]))),
                 awodds = c('AN0.50', 'AN1.50', 'AN2.50', 'AN3.50'),
                 Away = t(data.frame(
                   AN0.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) >= 1]),
                   AN1.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) >= 2]),
                   AN2.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) >= 3]),
                   AN3.50 = sum(x[[z]][col(x[[z]]) - row(x[[z]]) >= 4]))))
    })

    hmhdp2 <- lapply(z, function (z) {
      data.frame(No = seq(3, 15, 4),
                 hmodds = c('HN3.00', 'HN2.00', 'HN1.00', 'H0.00'),
                 Home = t(data.frame(
                   HN3.00 = sum(hmhdp1[[z]]['HN3.50', 'Home'],
                                hmhdp1[[z]]['HN2.50', 'Home']) / 2,
                   HN2.00 = sum(hmhdp1[[z]]['HN2.50', 'Home'],
                                hmhdp1[[z]]['HN1.50', 'Home']) / 2,
                   HN1.00 = sum(hmhdp1[[z]]['HN1.50', 'Home'],
                                hmhdp1[[z]]['HN0.50', 'Home']) / 2,
                   H0.00  = sum(hmhdp1[[z]]['HN0.50', 'Home'],
                                awhdp1[[z]]['HP0.50', 'Home']) / 2)),
                 awodds = c('AP3.00', 'AP2.00', 'AP1.00', 'A0.00'),
                 Away = t(data.frame(
                   AP3.00 = sum(hmhdp1[[z]]['HN3.50', 'Away'],
                                hmhdp1[[z]]['HN2.50', 'Away']) / 2,
                   AP2.00 = sum(hmhdp1[[z]]['HN2.50', 'Away'],
                                hmhdp1[[z]]['HN1.50', 'Away']) / 2,
                   AP1.00 = sum(hmhdp1[[z]]['HN1.50', 'Away'],
                                hmhdp1[[z]]['HN0.50', 'Away']) / 2,
                   A0.00  = sum(hmhdp1[[z]]['HN0.50', 'Away'],
                                awhdp1[[z]]['HP0.50', 'Away']) / 2)))
    })

    awhdp2 <- lapply(z, function (z) {
      data.frame(No = seq(15, 29, 4),
                 hmodds = c('H0.00', 'HP1.00', 'HP2.00', 'HP3.00'),
                 Home = t(data.frame(
                   H0.00  = sum(awhdp1[[z]]['HP0.50', 'Home'],
                                hmhdp1[[z]]['HN0.50', 'Home']) / 2,
                   HP1.00 = sum(awhdp1[[z]]['HP1.50', 'Home'],
                                awhdp1[[z]]['HP0.50', 'Home']) / 2,
                   HP2.00 = sum(awhdp1[[z]]['HP2.50', 'Home'],
                                awhdp1[[z]]['HP1.50', 'Home']) / 2,
                   HP3.00 = sum(awhdp1[[z]]['HP3.50', 'Home'],
                                awhdp1[[z]]['HP2.50', 'Home']) / 2)),
                 awodds = c('A0.00', 'AN1.00', 'AN2.00', 'AN3.00'),
                 Away = t(data.frame(
                   A0.00  = sum(awhdp1[[z]]['HP0.50', 'Away'],
                                hmhdp1[[z]]['HN0.50', 'Away']) / 2,
                   AN1.00 = sum(awhdp1[[z]]['HP1.50', 'Away'],
                                awhdp1[[z]]['HP0.50', 'Away']) / 2,
                   AN2.00 = sum(awhdp1[[z]]['HP2.50', 'Away'],
                                awhdp1[[z]]['HP1.50', 'Away']) / 2,
                   AN3.00 = sum(awhdp1[[z]]['HP3.50', 'Away'],
                                awhdp1[[z]]['HP2.50', 'Away']) / 2)))
    })

    hmhdp3 <- lapply(z, function (z) {
      data.frame( No = seq(2, 15, 2),
                  hmodds = c('HN3.25', 'HN2.75', 'HN2.25', 'HN1.75', 'HN1.25', 'HN0.75', 'HN0.25'),
                  Home = t(data.frame(
                    HN3.25 = sum(hmhdp1[[z]]['HN3.50', 'Home'],
                                 hmhdp2[[z]]['HN3.00', 'Home']) / 2,
                    HN2.75 = sum(hmhdp2[[z]]['HN3.00', 'Home'],
                                 hmhdp1[[z]]['HN2.50', 'Home']) / 2,
                    HN2.25 = sum(hmhdp1[[z]]['HN2.50', 'Home'],
                                 hmhdp2[[z]]['HN2.00', 'Home']) / 2,
                    HN1.75 = sum(hmhdp2[[z]]['HN2.00', 'Home'],
                                 hmhdp1[[z]]['HN1.50', 'Home']) / 2,
                    HN1.25 = sum(hmhdp1[[z]]['HN1.50', 'Home'],
                                 hmhdp2[[z]]['HN1.00', 'Home']) / 2,
                    HN0.75 = sum(hmhdp2[[z]]['HN1.00', 'Home'],
                                 hmhdp1[[z]]['HN0.50', 'Home']) / 2,
                    HN0.25 = sum(hmhdp1[[z]]['HN0.50', 'Home'],
                                 hmhdp2[[z]]['H0.00', 'Home']) / 2)),
                  awodds = c('AP3.25', 'AP2.75', 'AP2.25', 'AP1.75', 'AP1.25', 'AP0.75', 'AP0.25'),
                  Away = t(data.frame(
                    AP3.25 = sum(hmhdp1[[z]]['HN3.50', 'Away'],
                                 hmhdp2[[z]]['HN3.00', 'Away']) / 2,
                    AP2.75 = sum(hmhdp2[[z]]['HN3.00', 'Away'],
                                 hmhdp1[[z]]['HN2.50', 'Away']) / 2,
                    AP2.25 = sum(hmhdp1[[z]]['HN2.50', 'Away'],
                                 hmhdp2[[z]]['HN2.00', 'Away']) / 2,
                    AP1.75 = sum(hmhdp2[[z]]['HN2.00', 'Away'],
                                 hmhdp1[[z]]['HN1.50', 'Away']) / 2,
                    AP1.25 = sum(hmhdp1[[z]]['HN1.50', 'Away'],
                                 hmhdp2[[z]]['HN1.00', 'Away']) / 2,
                    AP0.75 = sum(hmhdp2[[z]]['HN1.00', 'Away'],
                                 hmhdp1[[z]]['HN0.50', 'Away']) / 2,
                    AP0.25 = sum(hmhdp1[[z]]['HN0.50', 'Away'],
                                 hmhdp2[[z]]['H0.00', 'Away']) / 2)))
    })

    awhdp3 <- lapply(z, function (z) {
      data.frame(No = seq(16, 28, 2),
                 hmodds = c('HP0.25','HP0.75', 'HP1.25','HP1.75','HP2.25','HP2.75','HP3.25'),
                 Home = t(data.frame(
                   HP0.25 = sum(awhdp1[[z]]['HP0.50','Home'],awhdp2[[z]]['H0.00','Home'])/2,
                   HP0.75 = sum(awhdp2[[z]]['HP1.00','Home'],awhdp1[[z]]['HP0.50','Home'])/2,
                   HP1.25 = sum(awhdp1[[z]]['HP1.50','Home'],awhdp2[[z]]['HP1.00','Home'])/2,
                   HP1.75 = sum(awhdp2[[z]]['HP2.00','Home'],awhdp1[[z]]['HP1.50','Home'])/2,
                   HP2.25 = sum(awhdp1[[z]]['HP2.50','Home'],awhdp2[[z]]['HP2.00','Home'])/2,
                   HP2.75 = sum(awhdp2[[z]]['HP3.00','Home'],awhdp1[[z]]['HP2.50','Home'])/2,
                   HP3.25 = sum(awhdp1[[z]]['HP3.50','Home'],awhdp2[[z]]['HP3.00','Home'])/2)),
                 awodds = c('AN0.25', 'AN0.75','AN1.25','AN1.75','AN2.25','AN2.75','AN3.25'),
                                                                                                 Away = t(data.frame(
                                                                                                   AN0.25 = sum(awhdp1[[z]]['HP0.50','Away'],awhdp2[[z]]['H0.00','Away'])/2,
                                                                                                   AN0.75 = sum(awhdp2[[z]]['HP1.00','Away'],awhdp1[[z]]['HP0.50','Away'])/2,
                                                                                                   AN1.25 = sum(awhdp1[[z]]['HP1.50','Away'],awhdp2[[z]]['HP1.00','Away'])/2,
                                                                                                   AN1.75 = sum(awhdp2[[z]]['HP2.00','Away'],awhdp1[[z]]['HP1.50','Away'])/2,
                                                                                                   AN2.25 = sum(awhdp1[[z]]['HP2.50','Away'],awhdp2[[z]]['HP2.00','Away'])/2,
                                                                                                   AN2.75 = sum(awhdp2[[z]]['HP3.00','Away'],awhdp1[[z]]['HP2.50','Away'])/2,
                                                                                                   AN3.25 = sum(awhdp1[[z]]['HP3.50','Away'],awhdp2[[z]]['HP3.00','Away'])/2)))})

                                                  a1 <- lapply(z, function(z) merge(hmhdp2[[z]],hmhdp3[[z]],all=T))
                                                  a2 <- lapply(z, function(z) merge(awhdp2[[z]],awhdp3[[z]],all=T))
                                                  a3 <- lapply(z, function(z) merge(hmhdp1[[z]],awhdp1[[z]],all=T))
                                                  a4 <- lapply(z, function(z) merge(a1[[z]],a2[[z]],all=T))
                                                  asnhdp <- lapply(z, function(z) merge(a3[[z]],a4[[z]],all=T))
                                                  asnhdp <- lapply(z, function(z) asnhdp[[z]][order(asnhdp[[z]]$No),])
                                                  asnhdp <- lapply(z, function(z) asnhdp[[z]][-1])
                                                  rm(hmhdp1, hmhdp2, hmhdp3, awhdp1, awhdp2, awhdp3, a1, a2, a3, a4)

                                                  if(matchview == T) {
                                                    fnames <- function(x, mdata) { z <- 1:length(x)
                                                                                   y <- lapply(z, function(z) data.frame(t(data.frame(
                                                                                     t(x[[z]][1:2]),t(x[[z]][3:4])))))
                                                                                   rnames <- rep(list(as.character(factor(y[[1]]$hmodds))),length(z))
                                                                                   ahlist <- lapply(z, function(z) { data.frame(No = z,
                                                                                                                                t(structure(y[[z]], row.names=rnames[[z]])[-1])) })
                                                                                   ahdf <- Reduce(function(x, y) merge(x, y, all = T),
                                                                                                  ahlist, accumulate = F); data.frame(mdata,ahdf[-1]) }
                                                    fnames(asnhdp,mdata)
                                                  } else {
                                                    snames <- function(x) { z <- 1:length(x)
                                                                            x <- lapply(z, function(z) { x <- data.frame(
                                                                              Odds = t(t(c(gsub('H', '-', substr(x[[z]][substring(
                                                                                x[[z]]$hmodds, 2) == 'N',]$hmodds, 1, 5)),0 , gsub('H', '',
                                                                                                                                   substr(x[[z]][substring(x[[z]]$hmodds,2) == 'P',]$hmodds, 1, 5)
                                                                                )))),x[[z]][-c(1,3)])}); x }
                                                    snames(asnhdp) } }
  # --------------------------------------------------------------
  goalline <- function(x, mdata, matchview = T) { z <- 1:length(x)
                                                  ou1 <- lapply(z, function (z) { data.frame( No = seq(1,33,4),
                                                                                              ovodds = gsub(' ', '', paste('O',rev(seq(0.5,8.5,1)))),
                                                                                              Over = t(data.frame(
                                                                                                O8.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 10]),
                                                                                                O7.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 9]),
                                                                                                O6.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 8]),
                                                                                                O5.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 7]),
                                                                                                O4.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 6]),
                                                                                                O3.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 5]),
                                                                                                O2.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 4]),
                                                                                                O1.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 3]),
                                                                                                O0.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])> 2]))),
                                                                                              unodds = gsub(' ', '', paste('U',rev(seq(0.5,8.5,1)))),
                                                                                              Under = t(data.frame(
                                                                                                U8.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 10]),
                                                                                                U7.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 9]),
                                                                                                U6.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 8]),
                                                                                                U5.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 7]),
                                                                                                U4.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 6]),
                                                                                                U3.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 5]),
                                                                                                U2.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 4]),
                                                                                                U1.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 3]),
                                                                                                U0.50 = sum(x[[z]][row(x[[z]]) + col(x[[z]])<= 2]))))})

                                                  ou2 <- lapply(z, function (z) { data.frame( No = seq(3,33,4),
                                                                                              ovodds = gsub(' ', '', paste('O',rev(seq(1,8,1)))),
                                                                                              Over = t(data.frame(
                                                                                                O8.00 = sum(ou1[[z]]['O8.50','Over'],ou1[[z]]['O7.50','Over'])/2,
                                                                                                O7.00 = sum(ou1[[z]]['O7.50','Over'],ou1[[z]]['O6.50','Over'])/2,
                                                                                                O6.00 = sum(ou1[[z]]['O6.50','Over'],ou1[[z]]['O5.50','Over'])/2,
                                                                                                O5.00 = sum(ou1[[z]]['O5.50','Over'],ou1[[z]]['O4.50','Over'])/2,
                                                                                                O4.00 = sum(ou1[[z]]['O4.50','Over'],ou1[[z]]['O3.50','Over'])/2,
                                                                                                O3.00 = sum(ou1[[z]]['O3.50','Over'],ou1[[z]]['O2.50','Over'])/2,
                                                                                                O2.00 = sum(ou1[[z]]['O2.50','Over'],ou1[[z]]['O1.50','Over'])/2,
                                                                                                O1.00 = sum(ou1[[z]]['O1.50','Over'],ou1[[z]]['O0.50','Over'])/2)),
                                                                                              unodds = gsub(' ', '', paste('U',rev(seq(1,8,1)))),
                                                                                              Under = t(data.frame(
                                                                                                U8.00 = sum(ou1[[z]]['O8.50','Under'],ou1[[z]]['O7.50','Under'])/2,
                                                                                                U7.00 = sum(ou1[[z]]['O7.50','Under'],ou1[[z]]['O6.50','Under'])/2,
                                                                                                U6.00 = sum(ou1[[z]]['O6.50','Under'],ou1[[z]]['O5.50','Under'])/2,
                                                                                                U5.00 = sum(ou1[[z]]['O5.50','Under'],ou1[[z]]['O4.50','Under'])/2,
                                                                                                U4.00 = sum(ou1[[z]]['O4.50','Under'],ou1[[z]]['O3.50','Under'])/2,
                                                                                                U3.00 = sum(ou1[[z]]['O3.50','Under'],ou1[[z]]['O2.50','Under'])/2,
                                                                                                U2.00 = sum(ou1[[z]]['O2.50','Under'],ou1[[z]]['O1.50','Under'])/2,
                                                                                                U1.00 = sum(ou1[[z]]['O1.50','Under'],ou1[[z]]['O0.50','Under'])/2)))})

                                                  ou3 <- lapply(z, function (z) { data.frame( No = seq(2,33,2),
                                                                                              ovodds = gsub(' ', '', paste('O',rev(seq(0.75,8.5,0.5)))),
                                                                                              Over = t(data.frame(
                                                                                                O8.25 = sum(ou1[[z]]['O8.50','Over'],ou2[[z]]['O8.00','Over'])/2,
                                                                                                O7.75 = sum(ou2[[z]]['O8.00','Over'],ou1[[z]]['O7.50','Over'])/2,
                                                                                                O7.25 = sum(ou1[[z]]['O7.50','Over'],ou2[[z]]['O7.00','Over'])/2,
                                                                                                O6.75 = sum(ou2[[z]]['O7.00','Over'],ou1[[z]]['O6.50','Over'])/2,
                                                                                                O6.25 = sum(ou1[[z]]['O6.50','Over'],ou2[[z]]['O6.00','Over'])/2,
                                                                                                O5.75 = sum(ou2[[z]]['O6.00','Over'],ou1[[z]]['O5.50','Over'])/2,
                                                                                                O5.25 = sum(ou1[[z]]['O5.50','Over'],ou2[[z]]['O5.00','Over'])/2,
                                                                                                O4.75 = sum(ou2[[z]]['O5.00','Over'],ou1[[z]]['O4.50','Over'])/2,
                                                                                                O4.25 = sum(ou1[[z]]['O4.50','Over'],ou2[[z]]['O4.00','Over'])/2,
                                                                                                O3.75 = sum(ou2[[z]]['O4.00','Over'],ou1[[z]]['O3.50','Over'])/2,
                                                                                                O3.25 = sum(ou1[[z]]['O3.50','Over'],ou2[[z]]['O3.00','Over'])/2,
                                                                                                O2.75 = sum(ou2[[z]]['O3.00','Over'],ou1[[z]]['O2.50','Over'])/2,
                                                                                                O2.25 = sum(ou1[[z]]['O2.50','Over'],ou2[[z]]['O2.00','Over'])/2,
                                                                                                O1.75 = sum(ou2[[z]]['O2.00','Over'],ou1[[z]]['O1.50','Over'])/2,
                                                                                                O1.25 = sum(ou1[[z]]['O1.50','Over'],ou2[[z]]['O1.00','Over'])/2,
                                                                                                O0.75 = sum(ou2[[z]]['O1.00','Over'],ou1[[z]]['O0.50','Over'])/2)),
                                                                                              unodds = gsub(' ', '', paste('U',rev(seq(0.75,8.5,0.5)))),
                                                                                              Under = t(data.frame(
                                                                                                U8.25 = sum(ou1[[z]]['O8.50','Under'],ou2[[z]]['O8.00','Under'])/2,
                                                                                                U7.75 = sum(ou2[[z]]['O8.00','Under'],ou1[[z]]['O7.50','Under'])/2,
                                                                                                U7.25 = sum(ou1[[z]]['O7.50','Under'],ou2[[z]]['O7.00','Under'])/2,
                                                                                                U6.75 = sum(ou2[[z]]['O7.00','Under'],ou1[[z]]['O6.50','Under'])/2,
                                                                                                U6.25 = sum(ou1[[z]]['O6.50','Under'],ou2[[z]]['O6.00','Under'])/2,
                                                                                                U5.75 = sum(ou2[[z]]['O6.00','Under'],ou1[[z]]['O5.50','Under'])/2,
                                                                                                U5.25 = sum(ou1[[z]]['O5.50','Under'],ou2[[z]]['O5.00','Under'])/2,
                                                                                                U4.75 = sum(ou2[[z]]['O5.00','Under'],ou1[[z]]['O4.50','Under'])/2,
                                                                                                U4.25 = sum(ou1[[z]]['O4.50','Under'],ou2[[z]]['O4.00','Under'])/2,
                                                                                                U3.75 = sum(ou2[[z]]['O4.00','Under'],ou1[[z]]['O3.50','Under'])/2,
                                                                                                U3.25 = sum(ou1[[z]]['O3.50','Under'],ou2[[z]]['O3.00','Under'])/2,
                                                                                                U2.75 = sum(ou2[[z]]['O3.00','Under'],ou1[[z]]['O2.50','Under'])/2,
                                                                                                U2.25 = sum(ou1[[z]]['O2.50','Under'],ou2[[z]]['O2.00','Under'])/2,
                                                                                                U1.75 = sum(ou2[[z]]['O2.00','Under'],ou1[[z]]['O1.50','Under'])/2,
                                                                                                U1.25 = sum(ou1[[z]]['O1.50','Under'],ou2[[z]]['O1.00','Under'])/2,
                                                                                                U0.75 = sum(ou2[[z]]['O1.00','Under'],ou1[[z]]['O0.50','Under'])/2)))})

                                                  a1 <- lapply(z, function(z) merge(ou2[[z]],ou3[[z]],all=T))
                                                  ouhdp <- lapply(z, function(z) merge(ou1[[z]],a1[[z]],all=T))
                                                  ouhdp <- lapply(z, function(z) ouhdp[[z]][order(ouhdp[[z]]$No),])
                                                  ouhdp <- lapply(z, function(z) ouhdp[[z]][-1])
                                                  rm(ou1, ou2, ou3, a1)

                                                  if(matchview == T) {
                                                    fnames <- function(x, mdata) { z <- 1:length(x)
                                                                                   y <- lapply(z, function(z) data.frame(t(data.frame(
                                                                                     t(x[[z]][1:2]),t(x[[z]][3:4])))))
                                                                                   rnames <- rep(list(as.character(factor(y[[1]]$ovodds))),length(z))
                                                                                   oulist <- lapply(z, function(z) { data.frame(No = z,
                                                                                                                                t(structure(y[[z]], row.names=rnames[[z]])[-1])) })
                                                                                   oudf <- Reduce(function(x, y) merge(x, y, all = T),
                                                                                                  oulist, accumulate = F); data.frame(mdata,oudf[-1]) }
                                                    fnames(ouhdp,mdata)
                                                  } else {
                                                    snames <- function(x) { z <- 1:length(x)
                                                                            x <- lapply(z, function(z) { x <- data.frame(
                                                                              Odds = t(t(c(gsub('O', '', x[[z]]$ovodds)))),x[[z]][-c(1,3)])}); x }
                                                    snames(ouhdp) } }
  # --------------------------------------------------------------
  tgoal <- function(x, mdata) { z <- 1:length(x)
                                tglist <- lapply(z, function (z) { data.frame(No=z,
                                                                              UP7 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) > 8]),
                                                                              T4_6 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) > 5 &
                                                                                                  row(x[[z]]) + col(x[[z]]) < 9]),
                                                                              T2_3 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) > 3 &
                                                                                                  row(x[[z]]) + col(x[[z]]) < 6]),
                                                                              T0_1 = sum(x[[z]][row(x[[z]]) + col(x[[z]]) < 4]))})
                                tgdf <- Reduce(function(x, y) merge(x, y, all = T),
                                               tglist, accumulate = F); data.frame(mdata,tgdf[-1]) }
  # --------------------------------------------------------------
  oddeven <- function(x, mdata) { z <- 1:length(x)
                                  oelist <- lapply(z, function (z) { data.frame(No=z,
                                                                                Odd  = sum(x[[z]][(row(x[[z]]) + col(x[[z]])) %% 2==1]),
                                                                                Even = sum(x[[z]][(row(x[[z]]) + col(x[[z]])) %% 2==0]))})
                                  oedf <- Reduce(function(x, y) merge(x, y, all = T),
                                                 oelist, accumulate = F); data.frame(mdata,oedf[-1]) }
  # --------------------------------------------------------------
  teamgoal <- function(x, mdata, matchview = T) { z <- 1:length(x)
                                                  ou1 <- lapply(z, function (z) { data.frame( No = seq(1,24,4),
                                                                                              ovodds = gsub(' ', '', paste('O',rev(seq(0.5,5.5,1)))),
                                                                                              Over = t(data.frame(
                                                                                                O5.50 = sum(x[[z]][row(x[[z]]) > 7]),
                                                                                                O4.50 = sum(x[[z]][row(x[[z]]) > 6]),
                                                                                                O3.50 = sum(x[[z]][row(x[[z]]) > 5]),
                                                                                                O2.50 = sum(x[[z]][row(x[[z]]) > 4]),
                                                                                                O1.50 = sum(x[[z]][row(x[[z]]) > 3]),
                                                                                                O0.50 = sum(x[[z]][row(x[[z]]) > 2]))),
                                                                                              unodds = gsub(' ', '', paste('U',rev(seq(0.5,5.5,1)))),
                                                                                              Under = t(data.frame(
                                                                                                U5.50 = sum(x[[z]][row(x[[z]]) <= 7]),
                                                                                                U4.50 = sum(x[[z]][row(x[[z]]) <= 6]),
                                                                                                U3.50 = sum(x[[z]][row(x[[z]]) <= 5]),
                                                                                                U2.50 = sum(x[[z]][row(x[[z]]) <= 4]),
                                                                                                U1.50 = sum(x[[z]][row(x[[z]]) <= 3]),
                                                                                                U0.50 = sum(x[[z]][row(x[[z]]) <= 2]))))})

                                                  ou2 <- lapply(z, function (z) { data.frame( No = seq(3,21,4),
                                                                                              ovodds = gsub(' ', '', paste('O',rev(seq(1,5,1)))),
                                                                                              Over = t(data.frame(
                                                                                                O5.00 = sum(ou1[[z]]['O5.50','Over'],ou1[[z]]['O4.50','Over'])/2,
                                                                                                O4.00 = sum(ou1[[z]]['O4.50','Over'],ou1[[z]]['O3.50','Over'])/2,
                                                                                                O3.00 = sum(ou1[[z]]['O3.50','Over'],ou1[[z]]['O2.50','Over'])/2,
                                                                                                O2.00 = sum(ou1[[z]]['O2.50','Over'],ou1[[z]]['O1.50','Over'])/2,
                                                                                                O1.00 = sum(ou1[[z]]['O1.50','Over'],ou1[[z]]['O0.50','Over'])/2)),
                                                                                              unodds = gsub(' ', '', paste('U',rev(seq(1,5,1)))),
                                                                                              Under = t(data.frame(
                                                                                                U5.00 = sum(ou1[[z]]['O5.50','Under'],ou1[[z]]['O4.50','Under'])/2,
                                                                                                U4.00 = sum(ou1[[z]]['O4.50','Under'],ou1[[z]]['O3.50','Under'])/2,
                                                                                                U3.00 = sum(ou1[[z]]['O3.50','Under'],ou1[[z]]['O2.50','Under'])/2,
                                                                                                U2.00 = sum(ou1[[z]]['O2.50','Under'],ou1[[z]]['O1.50','Under'])/2,
                                                                                                U1.00 = sum(ou1[[z]]['O1.50','Under'],ou1[[z]]['O0.50','Under'])/2)))})

                                                  ou3 <- lapply(z, function (z) { data.frame( No = seq(2,21,2),
                                                                                              ovodds = gsub(' ', '', paste('O',rev(seq(0.75,5.5,0.5)))),
                                                                                              Over = t(data.frame(
                                                                                                O5.25 = sum(ou1[[z]]['O5.50','Over'],ou2[[z]]['O5.00','Over'])/2,
                                                                                                O4.75 = sum(ou2[[z]]['O5.00','Over'],ou1[[z]]['O4.50','Over'])/2,
                                                                                                O4.25 = sum(ou1[[z]]['O4.50','Over'],ou2[[z]]['O4.00','Over'])/2,
                                                                                                O3.75 = sum(ou2[[z]]['O4.00','Over'],ou1[[z]]['O3.50','Over'])/2,
                                                                                                O3.25 = sum(ou1[[z]]['O3.50','Over'],ou2[[z]]['O3.00','Over'])/2,
                                                                                                O2.75 = sum(ou2[[z]]['O3.00','Over'],ou1[[z]]['O2.50','Over'])/2,
                                                                                                O2.25 = sum(ou1[[z]]['O2.50','Over'],ou2[[z]]['O2.00','Over'])/2,
                                                                                                O1.75 = sum(ou2[[z]]['O2.00','Over'],ou1[[z]]['O1.50','Over'])/2,
                                                                                                O1.25 = sum(ou1[[z]]['O1.50','Over'],ou2[[z]]['O1.00','Over'])/2,
                                                                                                O0.75 = sum(ou2[[z]]['O1.00','Over'],ou1[[z]]['O0.50','Over'])/2)),
                                                                                              unodds = gsub(' ', '', paste('U',rev(seq(0.75,5.5,0.5)))),
                                                                                              Under = t(data.frame(
                                                                                                U5.25 = sum(ou1[[z]]['O5.50','Under'],ou2[[z]]['O5.00','Under'])/2,
                                                                                                U4.75 = sum(ou2[[z]]['O5.00','Under'],ou1[[z]]['O4.50','Under'])/2,
                                                                                                U4.25 = sum(ou1[[z]]['O4.50','Under'],ou2[[z]]['O4.00','Under'])/2,
                                                                                                U3.75 = sum(ou2[[z]]['O4.00','Under'],ou1[[z]]['O3.50','Under'])/2,
                                                                                                U3.25 = sum(ou1[[z]]['O3.50','Under'],ou2[[z]]['O3.00','Under'])/2,
                                                                                                U2.75 = sum(ou2[[z]]['O3.00','Under'],ou1[[z]]['O2.50','Under'])/2,
                                                                                                U2.25 = sum(ou1[[z]]['O2.50','Under'],ou2[[z]]['O2.00','Under'])/2,
                                                                                                U1.75 = sum(ou2[[z]]['O2.00','Under'],ou1[[z]]['O1.50','Under'])/2,
                                                                                                U1.25 = sum(ou1[[z]]['O1.50','Under'],ou2[[z]]['O1.00','Under'])/2,
                                                                                                U0.75 = sum(ou2[[z]]['O1.00','Under'],ou1[[z]]['O0.50','Under'])/2)))})

                                                  a1 <- lapply(z, function(z) merge(ou2[[z]],ou3[[z]],all=T))
                                                  teamou <- lapply(z, function(z) merge(ou1[[z]],a1[[z]],all=T))
                                                  teamou <- lapply(z, function(z) teamou[[z]][order(teamou[[z]]$No),])
                                                  teamou <- lapply(z, function(z) teamou[[z]][-1])
                                                  rm(ou1, ou2, ou3, a1)

                                                  if(matchview == T) {
                                                    fnames <- function(x, mdata) { z <- 1:length(x)
                                                                                   y <- lapply(z, function(z) data.frame(t(data.frame(
                                                                                     t(x[[z]][1:2]),t(x[[z]][3:4])))))
                                                                                   rnames <- rep(list(as.character(factor(y[[1]]$ovodds))),length(z))
                                                                                   oulist <- lapply(z, function(z) { data.frame(No = z,
                                                                                                                                t(structure(y[[z]], row.names=rnames[[z]])[-1])) })
                                                                                   oudf <- Reduce(function(x, y) merge(x, y, all = T),
                                                                                                  oulist, accumulate = F); data.frame(mdata,oudf[-1]) }
                                                    fnames(teamou,mdata)
                                                  } else {
                                                    snames <- function(x) { z <- 1:length(x)
                                                                            x <- lapply(z, function(z) { x <- data.frame(
                                                                              Odds = t(t(c(gsub('O', '', x[[z]]$ovodds)))),x[[z]][-c(1,3)])}); x }
                                                    snames(teamou) } }
  # --------------------------------------------------------------
  htft <- function(ht, ft, mdata) {data.frame(mdata,
                                              HH = ht$Win  * ft$Win, HD = ht$Win  * ft$Draw, HA = ht$Win  * ft$Lose,
                                              DH = ht$Draw * ft$Win, DD = ht$Draw * ft$Draw, DA = ht$Draw * ft$Lose,
                                              AH = ht$Lose * ft$Win, AD = ht$Lose * ft$Draw, AA = ht$Lose * ft$Lose) }
  # --------------------------------------------------------------
  halfmostgoal <- function(htmxt, ftmxt, mdata) { z <- 1:length(htmxt)
                                                  dfmxt <- lapply(z, function(z) { ftmxt[[z]] - htmxt[[z]] })
                                                  handicap(x = dfmxt, mdata = mdata) }
  # --------------------------------------------------------------
  if(FT == TRUE) {
    Offence <- getData(dbase, tsFT(Offence)); Defence <- getData(dbase, tsFT(Defence))
    Effects <- getData(dbase, tsFT(Effects)); rm(tsFT)

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
      matchdata <- matchdata[!is.na(matchdata$hmat),]
      prob <- matchdata$p; theta <- matchdata$theta
    } else { prob <- 0; theta <- 0 }

    matchdata <- matchdata[!is.na(matchdata$hmat),]
    if(homeavd == FALSE) { lmb1 = (1 - prob) * matchdata$hmat * matchdata$awdf
    } else { lmb1 = (1 - prob) * matchdata$hmat * matchdata$awdf * matchdata$home }
    lmb2 <- (1 - prob) * matchdata$awat * matchdata$hmdf
    lmb3 <- (1 - prob) * matchdata$effects
    lmb1 <- lmb1[!is.na(lmb1)]; lmb3 <- lmb3[!is.na(lmb3)]; lmb3 <- lmb3[!is.na(lmb3)]
    matchdata$hmat <- matchdata$hmdf <- matchdata$awat <- matchdata$awdf <- NULL
    matchdata$home <- matchdata$effects <- matchdata$p <- matchdata$theta <- NULL

    nl <- 1:length(lmb1)
    mxt <- lapply(nl, function(z){outer(0:20, 0:20, function(x, y)
      bvp(x, y, lambda = c(lmb1[z], lmb2[z], lmb3[z])))})
    rm(lmb1, lmb2, lmb3, nl)

    FTmxt <- diagdraw(mxt); rm(mxt)
    FTWDW <- fodds(FTmxt,matchdata)
    FTCS <- cscores(FTmxt, matchdata)
    FTAH <- handicap(FTmxt, matchdata)
    FTOU <- goalline(FTmxt, matchdata)
    FTTG <- tgoal(FTmxt, matchdata)
    FTOE <- oddeven(FTmxt, matchdata)
    FTTOU <- teamgoal(FTmxt, matchdata) }

  if(HT == TRUE) {
    Offence <- getData(dbase, tsHT(Offence)); Defence <- getData(dbase, tsHT(Defence))
    Effects <- getData(dbase, tsHT(Effects)); rm(tsHT)

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
      matchdata <- matchdata[!is.na(matchdata$hmat),]
      prob <- matchdata$p; theta <- matchdata$theta
    } else { prob <- 0; theta <- 0 }

    matchdata <- matchdata[!is.na(matchdata$hmat),]
    if(homeavd == FALSE) { lmb1 = (1 - prob) * matchdata$hmat * matchdata$awdf
    } else { lmb1 = (1 - prob) * matchdata$hmat * matchdata$awdf * matchdata$home }
    lmb2 <- (1 - prob) * matchdata$awat * matchdata$hmdf
    lmb3 <- (1 - prob) * matchdata$effects
    lmb1 <- lmb1[!is.na(lmb1)]; lmb3 <- lmb3[!is.na(lmb3)]; lmb3 <- lmb3[!is.na(lmb3)]
    matchdata$hmat <- matchdata$hmdf <- matchdata$awat <- matchdata$awdf <- NULL
    matchdata$home <- matchdata$effects <- matchdata$p <- matchdata$theta <- NULL

    nl <- 1:length(lmb1)
    mxt <- lapply(nl, function(z){outer(0:20, 0:20, function(x, y)
      bvp(x, y, lambda = c(lmb1[z], lmb2[z], lmb3[z])))})
    rm(lmb1, lmb2, lmb3, nl)

    HTmxt <- diagdraw(mxt); rm(mxt)
    HTWDW <- fodds(HTmxt,matchdata)
    HTCS <- cscores(HTmxt, matchdata)
    HTAH <- handicap(HTmxt, matchdata)
    HTOU <- goalline(HTmxt, matchdata)
    HTTG <- tgoal(HTmxt, matchdata)
    HTOE <- oddeven(HTmxt, matchdata)
    HTTOU <- teamgoal(HTmxt, matchdata) }

  testObject <- function(object) { exists(as.character(substitute(object))) }
  if(testObject(FTWDW) == FALSE) {
    FTWDW <- NULL
    FTCS <- NULL
    FTAH <- NULL
    FTOU <- NULL
    FTTG <- NULL
    FTOE <- NULL
    FTTOU <- NULL }

  if(testObject(HTWDW) == FALSE) {
    HTWDW <- NULL
    HTCS <- NULL
    HTAH <- NULL
    HTOU <- NULL
    HTTG <- NULL
    HTOE <- NULL
    HTTOU <- NULL }

  if((!is.null(HTWDW) == TRUE) & (!is.null(FTWDW) == TRUE)) {
    HTFT <- htft(HTWDW, FTWDW, matchdata)
    HWMG <- halfmostgoal(HTmxt, FTmxt, matchdata)
    rm(HTmxt, FTmxt)
  } else {
    HTFT <- NULL; HWMG <- NULL }

  rm(lmb1, lmb2, lmb3, nl, matchdata)
  rm(Offence, Defence, Effects, theta, prob, getData)

  result <- list(call = tempcall, homeavd = homeavd, HTFT = HTFT, HWMG = HWMG,
                 FTWDW = FTWDW, FTCS = FTCS, FTAH = FTAH, FTOU = FTOU, FTTG = FTTG, FTOE = FTOE, FTTOU = FTTOU,
                 HTWDW = HTWDW, HTCS = HTCS, HTAH = HTAH, HTOU = HTOU, HTTG = HTTG, HTOE = HTOE, HTTOU = HTTOU)
  options(warn = 0); class(result) <- 'compileOdds'
  result }
