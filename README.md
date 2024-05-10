# 鄀尤物

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

## 1. 足球入球计数|机数编程造物

  Initially I collected soccer odds data of 40 bookmakers (in Excel format) and compare the efficiency and accuracy. I tried to submit the spreadsheets to [Ladbrokes](https://www.ladbrokescoralplc.com/) in order to get the trading department when I worked in [Scicom (MSC) Bhd](http://www.scicom-intl.com/).

- Learn the knowledge in sportsbook from tip-to-toe as you can know via [®γσ, ξηg Lian Hu](https://englianhu.wordpress.com/).
- Collect the livescore and also 1x2, Asian Handicap, Over Under odds price data of 29 sportsbookmakers manually from 500WAN, BET007 and NowGoal website and filter the odds price data from 2006 to 2011.
- Apply Poisson model in R to test the return of the investment. This research job is the most completed, success and the first research which write the whole odds compilation EM model and data management by refer to thousands of research papers in sportsbook odds modelling after resigned from Caspo Inc.

`Rmodel` which compile the index of soccer teams and predict the soccer scores. Odds modelling for 1x2, Asian Handicap, Over Under, Correct Score, Half-Time Full-Time etc.

```{r}
## 倘若程序包尚未安装，自动下载、安装并读取。
if(!require('Rmodel')) {
  if(!require('devtools')) install.packages('devtools')
  devtools::install_github('englianhu/Rmodel')
  library('devtools')
}
```

## 二、投注策略|投资战略

在此愚生从[7M](http://www.7msport.com)和[NowGoal.com](http://www.nowgoal.com)赔率资讯网上自动採撷赔率数据，欲知更多详情请查阅[「猫城」WebDriver-DynamicWebpage-Scrapping](https://github.com/scibrokes/webdriver-dynamicwebpage-scrapping)并且使用凯利标准计数|机数尤物，从模拟与回测天下诸侯霸主主要十三家博彩庄两个赛季中可以获利超过三成。

- [「猫城」在足彩投注策略|投资战略中，采用凯利标准计数|机数尤物（英）](https://github.com/scibrokes/kelly-criterion)
- [在英超二零二一/二零二二年赛季中，使用凯利标准计数|机数尤物（英）](http://rpubs.com/englianhu/kelly_eng1112)
- [在英超二零二二/二零二三年赛季中，使用凯利标准计数|机数尤物（英）](http://rpubs.com/englianhu/kelly_eng1213)

欲知更多详情，请查阅[「猫城」投注策略|投资战略和计数|机数造物鉴别（英）](https://github.com/scibrokes/betting-strategy-and-model-validation)。

## 3. 相关资源与课外参考文献

欲知更多详情，请查阅：

- [「猫城」赔率计数|机数造物（打造尤物）建模与试探体育彩券商的昏庸、无能、腐败与破绽（英）](https://github.com/scibrokes/odds-modelling-and-testing-inefficiency-of-sports-bookmakers)
- [「鄀客栈」Bookdown竞赛参赛作品：赔率计数|机数造物（打造尤物）建模与试探体育彩券商的昏庸、无能、腐败与破绽（英）](http://rpubs.com/englianhu/rmodel-vignettes1)

<br><br>

---

[<img src='诸子百家考工记/世博量化.png' height='14'/> Sςιβrοκεrs Trαdιηg®](http://www.scibrokes.com)<br>
<span style='color:RoyalBlue'>**[<img src='诸子百家考工记/世博量化.png' height='14'/> 世博量化®](http://www.scibrokes.com)企业知识产权®及版权®所有，盗版必究。**</span>
