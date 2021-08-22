#' @title Read a Rmodel query
#'
#' @description read vectors to calculate diagonal zero inflated bivariate poisson possibility and returns a list
#'
#' @details This function read 1 to 2 vectors and then calculate bivariate poisson possibility and returns a vector.
#'
#' @seealso \url{https://www.github.com/englianhu/Rmodel} for calculate the possibility of bivariate poisson model.
#' @examples
#' # this will stop
#' \dontrun{
#' compileIndex(FTHG ~ 1, FTAG ~ 1, ~c(Home, Away) + c(Away, Home),
#' data = sample, xi = 0.0065, fordate = '2010-08-12') }
#' #returns data as vector
#' #setwd(system.file("data", package = "Rmodel"))
#' #getwd()
#' \dontrun{
#' compileIndex(FTHG ~ 1, FTAG ~ 1, ~c(Home, Away) + c(Away, Home),
#' data = sample, xi = 0.0065, fordate = '2010-08-12') }
#' \dontrun{
#' em2008 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2008f) }
#' \dontrun{
#' em2009 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2009f) }
#' \dontrun{
#' em2010 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2010f) }
#' \dontrun{
#' em2011 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2011f) }
#' \dontrun{
#' em2012 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2012f) }
#' \dontrun{
#' em2013 = compileIndex(FTHG~1, FTAG~1, ~c(Home, Away) + c(Away, Home), data = eng2013f) }
#‘
#' @param filename Relative filepath to the current working directory. This must be (*.zip) or it will throw an error.
#' @return A list of dataset from the inputted *.RData file.
#' @family Rmodel functions
#' @export
#'
compileIndex <- function (
  l1 = FTHG ~ 1, l2 = FTAG ~ 1, l1l2 = ~c(Home, Away) + c(Away, Home),
  l3 = ~1, data, maxit = 300, xi = NULL, fordate = NULL,
  inflated = TRUE) {
  # l1, l2, l3 are weighted linear function of home, away, independence, l1l2 is
  # defined the home and away into counting, data is sample data for measurement,
  # xi is the decay rate, it can be either negative/positive while system will
  # auto convert to negative value from 0 to 1, bigger value cause weighted more
  # on latest results. fordate is the date to forecast.
  #
  # exmp1(basic): em <- compileIndex(
  #                        FTHG ~ 1, FTAG ~ 1, ~c(Home, Away) + c(Away, Home),
  #                        data = sample)
  #
  # exmp2(time series): em <- compileIndex(
  #                              FTHG ~ 1, FTAG ~ 1, ~c(Home, Away) + c(Away, Home),
  #                              data = sample, xi = 0.0065, fordate = '2010-08-12')
  #
  # > names(eng1314)
  # [1] "MatchID_7M" "MatchID_NG" "Round" "KODate" "Home" "Away"
  # [7] "FTHG"       "FTAG"       "HTHG"  "HTAG"

  options(warn = -1)
  templist <- list(
    l1 = l1, l2 = l2, l1l2 = l1l2, l3 = l3, data = substitute(data),
    maxit = maxit, xi = xi, fordate = fordate, inflated = inflated)
  tempcall <- as.call(c(expression(compileIndex), templist))
  rm(templist)

  # data$Date <- as.POSIXct(data$Date)
  data <- data[order(data$Date, decreasing = TRUE),]
  attr(data$Home,'contrasts') <- stats::contrasts(stats::C(factor(data$Home), sum))
  attr(data$Away,'contrasts') <- stats::contrasts(stats::C(factor(data$Away), sum))
  attr(data$Home,'levels') <- levels(factor(data$Home))
  attr(data$Away,'levels') <- levels(factor(data$Away))
  data$FTHG <- as.numeric(data$FTHG)
  data$FTAG <- as.numeric(data$FTAG)
  data$Date <- as.Date(data$Date)

  intercepted <- FALSE
  pres <- 1e-08
  dist <- 1
  jmax <- 1
  formula1.terms <- 'internal.data1$noncommon'
  namex <- as.character(l1[2])
  namey <- as.character(l2[2])
  x <- data[, names(data) == namex]
  y <- data[, names(data) == namey]
  n <- length(x)

  internal.data1 <- data.frame(y1y2 = c(x, y))
  internal.data2 <- data.frame(y3 = rep(0, n))
  p <- length(as.data.frame(data))

  data1 <- rbind(data, data)
  names(data1) <- names(data)
  data1 <- data1[, names(data1) != namex]
  data1 <- data1[, names(data1) != namey]

  if (as.character(l1[3]) == '.') {
    l1 <- formula(paste(
      as.character(l1[2]), paste(names(data1),'', collapse = '+', sep = ''),
      sep = '~'))
  }
  if (as.character(l2[3]) == '.') {
    l2 <- formula(paste(
      as.character(l2[2]), paste(names(data1),'', collapse = '+', sep = ''),
      sep = '~'))
  }
  if (as.character(l3[2]) == '.') {
    l3 <- formula(paste('',
      paste(names(data1),'', collapse = '+', sep = ''), sep = '~'))
  }

  formula2 <- formula(paste('internal.data2$y3~', as.character(l3[2]), sep = ''))
  internal.data1$noncommon <- as.factor(c(1:n * 0, 1:n * 0 + 1))
  contrasts(internal.data1$noncommon) <- stats::contr.treatment(2, base = 1)
  internal.data1$indct1 <- c(1:n * 0 + 1, 1:n * 0)
  internal.data1$indct2 <- c(1:n * 0, 1:n * 0 + 1)

  data2 <- data1[1:n, ]
  names(data2) <- names(data1)
  if (!is.null(l1l2)) {
    formula1.terms <- paste(formula1.terms, as.character(l1l2[2]), sep = '+')
  }

  templ1 <- labels(terms(l1))
  if (length(templ1) > 0) {
    for (k1 in 1:length(templ1)) {
      if (!is.null(l1l2)) {
        checkvar1 <- sum(labels(terms(l1l2)) == templ1[k1]) == 1
      } else {
        checkvar1 <- FALSE
      }
      checkvar2   <- sum(labels(terms(l2)) == templ1[k1]) == 1

      if (checkvar1 & checkvar2) {
        formula1.terms <- paste(formula1.terms,
                                paste('internal.data1$noncommon*',
                                      templ1[k1], sep = ''), sep = '+')
      } else {
        formula1.terms <- paste(formula1.terms,
                                paste('+I(internal.data1$indct1*',
                                      templ1[k1], sep = ''), sep = '')
        formula1.terms <- paste(formula1.terms, ')', sep = '')
      }
    }
  }

  templ2 <- labels(terms(l2))
  if (length(templ2) > 0) {
    for (k1 in 1:length(templ2)) {
      if (!is.null(l1l2)) {
        checkvar1  <- (sum(labels(terms(l1l2)) == templ2[k1]) +
                        sum(labels(terms(l1)) == templ2[k1])) != 2
      } else {
        checkvar1  <- TRUE
      }
      if (checkvar1) {
        formula1.terms <- paste(formula1.terms,
                                paste('+I(internal.data1$indct2*',
                                      templ2[k1], sep = ''), sep = '')
        formula1.terms <- paste(formula1.terms, ')', sep = '')
      }
    }
  rm(templ1, templ2, checkvar1, checkvar2)
  }

  formula1 <- formula(paste('internal.data1$y1y2~', formula1.terms, sep = ''))
  tmpform1 <- as.character(formula1[3])
  newformula <- formula1

  while (regexpr('c\\(', tmpform1) != -1) {
    temppos1  <- regexpr('c\\(', tmpform1)[1]
    tempfor   <- substring(tmpform1, first = temppos1 + 2)
    temppos2  <- regexpr('\\)', tempfor)[1]
    tempvar   <- substring(tempfor, first = 1, last = temppos2 - 1)
    temppos3  <- regexpr(', ', tempvar)[1]
    tempname1 <- substring(tempfor, first = 1, last = temppos3 - 1)
    tempname2 <- substring(tempfor, first = temppos3 + 2, last = temppos2 - 1)
    tempname2 <- sub('\\)', '', tempname2)
    tempvar1  <- data[, names(data) == tempname1]
    tempvar2  <- data[, names(data) == tempname2]
    data1$newvar1 <- c(tempvar1, tempvar2)

    if (is.factor(tempvar1) & is.factor(tempvar2)) {
      data1$newvar1 <- as.factor(data1$newvar1)
      if (all(levels(tempvar1) == levels(tempvar2))) {
        attributes(data1$newvar1) <- attributes(tempvar1)
      }
    }
    tempvar    <- sub(', ', '..', tempvar)
    names(data1)[names(data1) == 'newvar1'] <- tempvar
    newformula <- sub('c\\(', '', tmpform1)
    newformula <- sub('\\)', '', newformula)
    newformula <- sub(', ', '..', newformula)
    tmpform1   <- newformula
    formula1   <- formula(paste('internal.data1$y1y2~', newformula,
                                sep = ''))
  }
  rm(temppos1, temppos2, temppos3)
  rm(tmpform1, tempfor)
  rm(tempvar, tempvar1, tempvar2)
  rm(tempname1, tempname2)

  if(!is.null(fordate)) {
    if(!is.null(xi)) {
      if(as.Date(fordate) >= max(data$Date)) {

        #if(as.POSIXct(fordate) >= max(data$Date))
        weightdata2 <- exp(-abs(xi) * as.numeric(
          difftime(fordate, data$Date, units = 'days')))
        weightdata1 <- rep(weightdata2, 2)

      } else {
        stop('forecast date is less than sample dates')
      }
    } else {
      weightdata2 <- rep(1, length(x))
      weightdata1 <- rep(weightdata2, 2)
    }

  } else {
    if(!is.null(xi)) {

      fordate     <- max(data$Date)
      weightdata2 <- exp(-abs(xi) * as.numeric(
        difftime(fordate, data$Date, units = 'days')))
      weightdata1 <- rep(weightdata2, 2)

    } else {
      fordate     <- max(data$Date)
      weightdata2 <- rep(1, length(x))
      weightdata1 <- rep(weightdata2, 2)
    }
  }

  s       <- rep(0, n)
  like    <- 1:n * 0
  zero    <- (x == 0) | (y == 0)
  lambda3 <- rep(max(0.1, cov(x, y, use = 'complete.obs')), n)

  if(inflated == TRUE) {
    prob    <- 0.2
    vi      <- 1:n * 0
    v1      <- 1 - c(vi, vi)
    internal.data1$v1 <- 1 - c(vi, vi)
    dilabel <- paste('Inflation dist: Discrete with J=', jmax)
    theta   <- 1:jmax * 0 + 1/(jmax + 1)

    di.f <- function(x, theta) {
      JMAX  <- length(theta)

      if (x > JMAX) {
        res <- 0
      } else if (x == 0) {
        res <- 1 - sum(theta)
      } else {
        res <- theta[x]
      }
      return(res)
    }

    lambda <- glm(formula1, family = poisson, data = data1,
                  weights = (internal.data1$v1 * weightdata1),
                  maxit = 100)$fitted

  } else {
    dilabel  <- 'No Inflation'
    prob     <- 0
    theta    <- 0
    meandiag <- 0
    lambda   <- glm(formula1, family = poisson, data = data1,
                    weights = weightdata1)$fitted
  }

  lambda1  <- lambda[1:n]
  lambda2  <- lambda[(n + 1):(2 * n)]
  difllike <- 100
  loglike0 <- 1000
  i        <- 0
  ii       <- 0

  #   ------------------------------------------------------------
  splitbeta <- function (bvec) {
    p3    <- length(bvec)
    indx1 <- grep('\\(l1\\):', names(bvec))
    indx2 <- grep('\\(l2\\):', names(bvec))
    indx3 <- grep('\\(l2-l1\\):', names(bvec))
    tempnames <- sub('\\(l2-l1)\\:', 'k', names(bvec))
    tempnames <- sub('\\(l2)\\:', 'k', tempnames)
    tempnames <- sub('\\(l1)\\:', 'k', tempnames)
    indx4 <- tempnames %in% names(bvec)
    beta1 <- c(bvec[indx4], bvec[indx1])
    beta2 <- c(bvec[indx4], bvec[indx3], bvec[indx2])
    indexbeta2 <- c(rep(0, sum(indx4)), rep(1, length(indx3)),
                    rep(2, length(indx2)))
    names(beta1) <- sub('\\(l1\\):', '', names(beta1))
    names(beta2) <- sub('\\(l2\\):', '', names(beta2))
    names(beta2) <- sub('\\(l2-l1\\):', '', names(beta2))
    beta1        <- beta1[order(names(beta1))]
    indexbeta2   <- indexbeta2[order(names(beta2))]
    beta2        <- beta2[order(names(beta2))]
    ii <- 1:length(beta2)
    ii <- ii[indexbeta2 == 0]
    for (i in ii) {
      beta2[i] <- sum(beta2[names(beta2)[i] == names(beta2)])
    }
    beta2 <- beta2[indexbeta2 %in% c(0, 2)]
    btemp <- list(beta1 = beta1, beta2 = beta2)
    return(btemp)
  }

  #   ------------------------------------------------------------
  newnamesbeta <- function (bvec) {
    names(bvec) <- sub('\\)', '', names(bvec))
    names(bvec) <- sub('\\(Intercept', '(Intercept)', names(bvec))
    names(bvec)[pmatch('internal.data1$noncommon2', names(bvec))] <- '(l2-l1):(Intercept)'
    names(bvec) <- sub('internal.data1\\$noncommon2:','(l2-l1):', names(bvec))
    names(bvec) <- sub('internal.data1\\$noncommon0:','(l1):', names(bvec))
    names(bvec) <- sub('internal.data1\\$noncommon1:','(l2):', names(bvec))
    names(bvec) <- sub(':internal.data1\\$noncommon2','(l2-l1):', names(bvec))
    names(bvec) <- sub(':internal.data1\\$noncommon0','(l1):', names(bvec))
    names(bvec) <- sub(':internal.data1\\$noncommon1','(l2):', names(bvec))
    names(bvec) <- sub('I\\(internal.data1\\$indct1 \\* ','(l1):', names(bvec))
    names(bvec) <- sub('I\\(internal.data1\\$indct2 \\* ','(l2):', names(bvec))
    return(names(bvec))
  }

  #   ------------------------------------------------------------
  loglike <- rep(0, maxit)
  while ((difllike > pres) && (i <= maxit)) {
    i <- i + 1
    if(inflated == TRUE) {
      for (j in 1:n) {
        if (zero[j]) {
          s[j] <- 0
          if (x[j] == y[j]) {
            density.di <- di.f(0, theta)
            like[j] <- log((1 - prob) * exp(
              -lambda1[j] - lambda2[j] - lambda3[j]) +
                prob * density.di)
            vi[j] <- prob * density.di * exp(-like[j])

          } else {
            like[j] <- log(1 - prob) - lambda3[j] +
              log(dpois(x[j], lambda1[j])) +
              log(dpois(y[j], lambda2[j]))
            vi[j] <- 0
          }

        } else {
          lbp1 <- bvp(x[j] - 1, y[j] - 1,
                      lambda = c(lambda1[j], lambda2[j], lambda3[j]),
                      log = TRUE)
          lbp2 <- bvp(x[j], y[j], lambda = c(lambda1[j], lambda2[j],
                                             lambda3[j]), log = TRUE)
          s[j] <- exp(log(lambda3[j]) + lbp1 - lbp2)

          if (x[j] == y[j]) {
            density.di <- di.f(x[j], theta)
            like[j] <- log((1 - prob) * exp(lbp2) + prob * density.di)
            vi[j] <- prob * density.di * exp(-like[j])

          } else {
            vi[j] <- 0
            like[j] <- log(1 - prob) + lbp2
          }
        }
      }

      x1 <- x - s
      x2 <- y - s
      loglike[i] <- sum(like)
      difllike   <- abs((loglike0 - loglike[i])/loglike0)
      loglike0   <- loglike[i]
      prob <- sum(vi)/n

      for (ii in 1:jmax) {
        temp <- as.numeric((x == ii) & (y == ii))
        theta[ii] <- sum(temp * vi)/sum(vi)
      }

      internal.data2$v1 <- 1 - vi
      internal.data2$v1[(internal.data2$v1 < 0) &
                          (internal.data2$v1 > -1e-10)] <- 0
      internal.data2$y3 <- s

      m0 <- glm(formula2, family = poisson, data = data2, weights =
                  (internal.data2$v1 * weightdata2), maxit = 100)

      beta3 <- m0$coef
      lambda3 <- m0$fitted
      internal.data1$v1 <- 1 - c(vi, vi)
      internal.data1$v1[(internal.data1$v1 < 0) &
                          (internal.data1$v1 > -1e-10)] <- 0
      x1[(x1 < 0) & (x1 > -1e-10)] <- 0
      x2[(x2 < 0) & (x2 > -1e-10)] <- 0
      internal.data1$y1y2 <- c(x1, x2)

      m <- glm(formula1, family = poisson, data = data1,
               weights = (internal.data1$v1 * weightdata1),
               maxit = 100)

    } else {
      for (j in 1:n) {
        if (zero[j]) {
          s[j] <- 0
          like[j] <- log(dpois(x[j], lambda1[j])) +
            log(dpois(y[j], lambda2[j])) - lambda3[j]

        } else {
          lbp1 <- bvp(x[j] - 1, y[j] - 1, lambda =
                        c(lambda1[j], lambda2[j], lambda3[j]), log = TRUE)
          lbp2 <- bvp(x[j], y[j], lambda =
                        c(lambda1[j], lambda2[j], lambda3[j]), log = TRUE)
          s[j] <- exp(log(lambda3[j]) + lbp1 - lbp2)
          like[j] <- lbp2
        }
      }

      x1 <- x - s
      x2 <- y - s
      x1[(x1 < 0) & (x1 > -1e-08)] <- 0
      x2[(x2 < 0) & (x2 > -1e-08)] <- 0
      loglike[i] <- sum(like)
      difllike <- abs((loglike0 - loglike[i])/loglike0)
      loglike0 <- loglike[i]
      internal.data2$y3 <- s

      m0 <- glm(formula2, family = poisson, data = data2,
                weights = weightdata2)

      beta3   <- m0$coef
      lambda3 <- m0$fitted
      internal.data1$y1y2 <- c(x1, x2)

      m <- glm(formula1, family = poisson, data = data1,
               weights = weightdata1)
    }

    p3   <- length(m$coef)
    beta <- m$coef
    names(beta) <- newnamesbeta(beta)
    lambda  <- fitted(m)
    lambda1 <- lambda[1:n]
    lambda2 <- lambda[(n + 1):(2 * n)]
  }

  x.mean <- x
  x.mean[x == 0] <- 1e-12
  y.mean <- y
  y.mean[y == 0] <- 1e-12
  betaparameters <- splitbeta(beta)
  betaparameters$beta2[1] <- beta[1] + beta[2]
  beta1 <- betaparameters$beta1
  beta2 <- betaparameters$beta2

  teamn <- length(levels(data$Home))
  Offence <- c(beta1[(teamn + 1):(teamn * 2 - 1)],
               missHome = (-sum(beta1[(teamn + 1):(teamn * 2 - 1)])))
  Defence <- c(beta2[2:teamn], missAway = (-sum(beta2[2:teamn])))
  Effects <- data.frame(Date = fordate,
                        Home = exp((beta1[1] - beta2[1])),
                        Effect = exp(beta3), p = prob, theta,
                        row.names = NULL)

  # --------------------------------------------------------------
  snames <- function(x) {
    y <- paste(c(substring(names(x)[1:(teamn - 1)], 11), teamn))
    names(x) <- ifelse(as.numeric(y) < 10, paste('0', sep = '', y), y)
    x <- x[sort(names(x))]
    z <- data.frame(Date = fordate, t(exp(x)))
    names(z)[-1] <- levels(data$Home)
    return(z)
  }

  # --------------------------------------------------------------
  Offence    <- snames(Offence)
  Defence <- snames(Defence)
  beta.frame <- data.frame(
    Offence = t(Offence), Defence = t(Defence),
    row.names = c('Date', levels(data$Home)))

  if (inflated == TRUE) {
    fittedval1 <- (1 - prob) * (m$fitted[1:n] + lambda3)
    fittedval2 <- (1 - prob) * (m$fitted[(n + 1):(2 * n)] + lambda3)
    meandiag   <- sum(theta[1:jmax] * 1:jmax)
    fittedval1[x == y] <- prob * meandiag + fittedval1[x == y]
    fittedval2[x == y] <- prob * meandiag + fittedval2[x == y]

  } else {
    fittedval1 <- (m$fitted[1:n] + lambda3)
    fittedval2 <- (m$fitted[(n + 1):(2 * n)] + lambda3) }

  result <- list(
    coefficients = unlist(beta.frame), rating = beta.frame,
    fitted.values = data.frame(x = fittedval1, y = fittedval2),
    residuals = data.frame(x = x - fittedval1, y = y - fittedval2),
    offence = Offence, defence = Defence, effects = Effects,
    lambda1 = m$fitted[1:n], lambda2 = m$fitted[(n + 1):(2 * n)],
    lambda3 = lambda3, diagonal.dist = dilabel,
    loglikelihood = loglike[1:i], call = tempcall)

  options(warn = 0)
  class(result) <- c('compileIndex')
  return(result)
}

