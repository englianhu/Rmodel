## -------------------------------------------------------
#' @title Read a Rmodel query
#'
#' @description compile the odds from the index.
#'
#' @details This function load dataset.
#'
#' @seealso \url{https://www.github.com/englianhu/Rmodel} for calculate the possibility of bivariate poisson model.
#' @examples
#' # this will stop
#' \dontrun{
#' mdata <- loadData(Japan, 2010, 4)$matches
#' idxdata <- loadIndex(dbase = japan2010, mbase = mdata)
#' }
#‘
#' @param filename Relative filepath to the current working directory. This must be (*.zip) or it will throw an error.
#' @return A list of dataset from the inputted *.RData file.
#' @family Rmodel functions
#' @export
#'
loadIndex <- function(country, year, mbase, dbaseData = "FT", homeavd = TRUE,
                      basic = TRUE, matrix = FALSE, inflated = TRUE) {
  # country and year is the Access *.mdb file which are teams rating database,
  # mbase is the matchdata which may get from importData or other else,
  # dbaseData = "FT" or "HT", there are only two options otherwise will stop process.
  # dbaseData = "FT" indicates calculate Full-Time lambda from index, default is "FT".
  # homeavd determine if calculate home advantage or neutral ground. Default is TRUE.
  # basic = TRUE equals to basic model, while basic = FALSE equals to time-series decay model
  # inflated determine if zero inflated or not. matrix determine if we want to
  # calculate the matrix table of every single match.
  # exmp1: mdata <- loadData(Japan, 2010, 4)$matches
  #        idxdata <- loadIndex(dbase = japan2010, mbase = mdata)
  # Due to R-programming capacity, length of mbase up to 500 only for once,
  # more than 500 need to separate matches into multiple dataframe to calculate.
  # exmp2: length(mbase$Date) = 800; mbase1 <- mbase[1:500,]
  #        mbase2 <- mbase[501:800,]
  options(warn = -1)
  require(RODBC, quietly = TRUE)
  templist <- list(mbase = substitute(mbase), dbaseData = dbaseData,
                   homeavd = homeavd, basic = basic, inflated = inflated)
  tempcall <- as.call(c(expression(loadIndex), templist))
  rm(templist)

  if (is.null(country)|is.null(year))
    stop("enter country name and year to load index")
  if (is.null(mbase))
    stop("enter soccer matches data")

  # --------------------------------------------------------------
  if(basic == TRUE){
    bcHT <- function(x) {
      paste('bsHT_', substitute(x), sep = '')
    }
    bcFT <- function(x) {
      paste('bsFT_', substitute(x), sep = '')
    }
  } else {
    bcHT <- function(x) {
      paste('tsHT_', substitute(x), sep = '')
    }
    bcFT <- function(x) {
      paste('tsFT_', substitute(x), sep = '')
    }
  }

  getData <- function(tble, country = country, year = year) {
    con <- odbcConnectAccess2007(paste("./data/", country, "/",
                year, ".mdb", sep = ""))
    x <- structure(sqlFetch(con, tble)[-1],
                   row.names = as.character(as.Date(sqlFetch(con, tble)$Date)))
    colnames(x) <- gsub('_', ' ', colnames(x))
    odbcClose(con)
    rm(con)
    x
  }

  # --------------------------------------------------------------
  diagdraw <- function(x) {
    z <- 1:length(x)
    thetap <- prob * theta

    for(i in 1:length(z)) {
      dimnames(x[[i]]) <- list(seq(0, (nrow(x[[i]]) - 1), 1),
                               seq(0, (nrow(x[[i]]) - 1), 1))
    }
    rm(i)
    tplist <- rep(list(diag(thetap, nrow(x[[1]]), nrow(x[[1]]))), length(z))
    y <- lapply(z, function(z) x[[z]] + tplist[[z]])
    lapply(z, function(z) y[[z]] / sum(y[[z]]))
  }

  # --------------------------------------------------------------
  if(dbaseData == "FT") {
    Offence <- getData(tble = bcFT(Offence), country = country, year = year)
    Defence <- getData(tble = bcFT(Defence), country = country, year = year)
    Effects <- getData(tble = bcFT(Effects), country = country, year = year)
    rm(bcFT)

  } else if(dbaseData == "HT") {
    Offence <- getData(tble = bcHT(Offence), country = country, year=year)
    Defence <- getData(tble = bcHT(Defence), country = country, year=year)
    Effects <- getData(tble = bcHT(Effects), country = country, year=year)
    rm(bcHT)

  } else {
    stop("Only two options 'FT' or 'HT'")
  }
  teamID <- names(Offence)
  mbase  <- subset(mbase, Home %in% teamID & Away %in% teamID)
  mbase$Home <- factor(mbase$Home)
  mbase$Away <- factor(mbase$Away)
  mbase$Date <- as.Date(mbase$Date)
  matchdata  <- data.frame(
    mbase,
    hmat = apply(mbase, 1, function(x) Offence[x['Date'],x['Home']]),
    hmdf = apply(mbase, 1, function(x) Defence[x['Date'],x['Home']]),
    awat = apply(mbase, 1, function(x) Offence[x['Date'],x['Away']]),
    awdf = apply(mbase, 1, function(x) Defence[x['Date'],x['Away']]),
    home = apply(mbase, 1, function(x) Effects[x['Date'],'Home']),
    effects = apply(mbase, 1, function(x) Effects[x['Date'],'Effect']))

  if(inflated == TRUE) {
    prob  <- apply(mbase, 1, function(x) Effects[x['Date'],'p'])
    theta <- apply(mbase, 1, function(x) Effects[x['Date'],'theta'])

  } else {
    prob  <- 0
    theta <- 0
  }

  if(homeavd == FALSE) {
    lambda1 <- (1 - prob) * matchdata$hmat * matchdata$awdf

  } else {
    lambda1 <- (1 - prob) * matchdata$hmat * matchdata$awdf * matchdata$home
  }

  lambda2 <- (1 - prob) * matchdata$awat * matchdata$hmdf
  lambda3 <- (1 - prob) * matchdata$effects
  matchdata$hmat <- matchdata$hmdf <- matchdata$awat <- matchdata$awdf <- NULL
  matchdata$home <- matchdata$effects <- NULL

  if(matrix == TRUE) {
    nl  <- 1:length(lambda1)
    mxt <- lapply(nl, function(z){
      outer(0:20, 0:20, function(x, y)
      bvp(x, y, lambda = c(lambda1[z], lambda2[z], lambda3[z])))
    })

    MXTtable <- diagdraw(mxt)
    rm(mxt, nl)

  } else {
    MXTtable <- "No"
  }
  finaldata <- data.frame(matchdata, lambda1, lambda2, lambda3, prob, theta)
  rm(lambda1, lambda2, lambda3, teamID, Offence, Defence, Effects, getData)

  result <- list(call = tempcall, homeavd = homeavd, basic = basic,
                 dbaseData = dbaseData, data = finaldata, matrix = MXTtable)
  options(warn = 0)
  class(result) <- 'loadIndex'
  return(result)
  }

