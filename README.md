# Rmodel
<!-- badges: start -->
![GitHub R package version](https://img.shields.io/github/r-package/v/englianhu/rmodel?color=%238E1F07&style=plastic)
[![R](https://github.com/scibrokes/Rmodel/actions/workflows/r.yml/badge.svg)](https://github.com/scibrokes/Rmodel/actions/workflows/r.yml)
[![R-CMD-check](https://github.com/scibrokes/Rmodel/workflows/R-CMD-check/badge.svg)](https://github.com/scibrokes/Rmodel/actions)
Travis: [![Travis Build Status](https://travis-ci.com/englianhu/wk4package.svg?branch=main)](https://travis-ci.com/englianhu/wk4package)
<!-- badges: end -->

## 1. Soccer Scores Modelling

  Initially I collected soccer odds data of 40 bookmakers (in Excel format) and compare the efficiency and accuracy. I tried to submit the spreadsheets to [Ladbrokes](https://www.ladbrokescoralplc.com/) in order to get the trading department when I worked in [Scicom (MSC) Bhd](http://www.scicom-intl.com/).

- Learn the knowledge in sportsbook from tip-to-toe as you can know via [®γσ, ξηg Lian Hu](https://englianhu.wordpress.com/).
- Collect the livescore and also 1x2, Asian Handicap, Over Under odds price data of 29 sportsbookmakers manually from 500WAN, BET007 and NowGoal website and filter the odds price data from 2006 to 2011.
- Apply Poisson model in R to test the return of the investment. This research job is the most completed, success and the first research which write the whole odds compilation EM model and data management by refer to thousands of research papers in sportsbook odds modelling after resigned from Caspo Inc.

`Rmodel` which compile the index of soccer teams and predict the soccer scores. Odds modelling for 1x2, Asian Handicap, Over Under, Correct Score, Half-Time Full-Time etc.

```
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

---

**Powered by - Copyright® Intellectual Property Rights of <img src='figure/scb-logo3rs.jpg' width='16'> [Scibrokes®](http://www.scibrokes.com)個人の経営企業**
