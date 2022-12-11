# Rmodel

---

<!-- badges: start -->
![GitHub R package version](https://img.shields.io/github/r-package/v/englianhu/rmodel?color=%238E1F07&style=plastic)
[![R](https://github.com/scibrokes/Rmodel/actions/workflows/r.yml/badge.svg)](https://github.com/scibrokes/Rmodel/actions/workflows/r.yml)
[![R-CMD-check](https://github.com/scibrokes/Rmodel/workflows/R-CMD-check/badge.svg)](https://github.com/scibrokes/Rmodel/actions)

Travis: 
[![Travis build status](https://travis-ci.com/scibrokes/Rmodel.svg?branch=master)](https://travis-ci.com/scibrokes/Rmodel)
[![Travis build status](https://travis-ci.com/englianhu/Rmodel.svg?branch=master)](https://travis-ci.com/englianhu/Rmodel)
<!-- badges: end -->


> A place for publishing new versions of (some) stan-dev R packages before they reach CRAN and for stan-dev R packages and versions where releasing on CRAN is not a (current) goal. As of 2021-03-16 this is most relevant for rstan, where the CRAN version is unfortunately several releases behind and pushing a new version to CRAN has been difficult.

Citation : [Repository for distributing (some) stan-dev R packages](https://github.com/englianhu/r-packages)

<br>

## 1. Soccer Scores Modelling

  Initially I collected soccer odds data of 40 bookmakers (in Excel format) and compare the efficiency and accuracy. I tried to submit the spreadsheets to [Ladbrokes](https://www.ladbrokescoralplc.com/) in order to get the trading department when I worked in [Scicom (MSC) Bhd](http://www.scicom-intl.com/).

- Learn the knowledge in sportsbook from tip-to-toe as you can know via [®γσ, ξηg Lian Hu](https://englianhu.wordpress.com/).
- Collect the livescore and also 1x2, Asian Handicap, Over Under odds price data of 29 sportsbookmakers manually from 500WAN, BET007 and NowGoal website and filter the odds price data from 2006 to 2011.
- Apply Poisson model in R to test the return of the investment. This research job is the most completed, success and the first research which write the whole odds compilation EM model and data management by refer to thousands of research papers in sportsbook odds modelling after resigned from Caspo Inc.

`Rmodel` which compile the index of soccer teams and predict the soccer scores. Odds modelling for 1x2, Asian Handicap, Over Under, Correct Score, Half-Time Full-Time etc.

```{r}
if(!require('devtools')) install.packages('devtools')
devtools::install_github('englianhu/Rmodel')
```

## 2. Betting Strategy

  I have simulate a betting model on 13 bookmakers across 2 soccer seasons in English 2011/12 and 2012/13. Kindly refer to below techincal research.

  - [Application of Kelly model in English Soccer session 2011/12](http://rpubs.com/englianhu/kelly_eng1112)
  - [Application of Kelly model in English Soccer session 2012/13](http://rpubs.com/englianhu/kelly_eng1213)

  Kindly refer to [Application of Kelly Criterion model in Sportsbook Investment](https://github.com/scibrokes/kelly-criterion) for further information.

## 3. Vignettes

Kindly refer to below for further information.

- [Odds Modelling and Testing Inefficiency of Sports Bookmakers](https://github.com/scibrokes/odds-modelling-and-testing-inefficiency-of-sports-bookmakers)
- [Bookdown contest submission : Odds Modelling and Testing Inefficiency of Sports Bookmakers](http://rpubs.com/englianhu/rmodel-vignettes1)

<br>

---

<br>

[<img src='文艺坊图库/Scibrokes.png' height='14'/> Sςιβrοκεrs Trαdιηg®](http://www.scibrokes.com)<br>
<span style='color:RoyalBlue'>**[<img src='文艺坊图库/Scibrokes.png' height='14'/> 世博量化®](http://www.scibrokes.com)企业知识产权及版权所有，盗版必究。**</span>
