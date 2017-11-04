# Time Series Analysis of Bitcoin
## MSDS 6372 - Section 403
## Project 2

[Data Science @ Southern Methodist University](https://datascience.smu.edu/)

![Source: bitcoin.com](/images/readme1.png)

# Table of Contents
* [Team Members](#team-members)
* [Introduction](#introduction)
* [Problem Statement](#problem-statement)
* [Constraints and Limitations](#constraints)
* [Data set and variable descriptions](#descriptions)

# <a name="team-members"></a>Team Members
* [Jostein Barry-Straume](https://github.com/josteinstraume)
* [Laura Ludwig](https://github.com/laurajludwig)
* [David Tran](https://github.com/zdtranz)

# <a name="introduction"></a>Introduction
> Cryptocurrency is a digit currency and acts as a medium for exchanges/transactions. Cryptocurrencies are decentralized, which means it is not processed by any banking system and goes straight to the consumers. The transactions are posted on an online ledger for transparency. Users’ identities are protected through an encryption key, which is a feature that Bitcoin has.
> Bitcoin is one of the popular choices of cryptocurrency. Since its introduction into the market in 2009, it has drastically increased and decreased in value. The analysis below will offer insights on the characteristics of the cryptocurrency and its projected value and trend.

# <a name="problem-statement"></a>Problem Statement
> Develop a time series model based on an observed set of explanatory variables that can be utilized to predict future price of Bitcoin.

# <a name="constraints"></a>Constraints and Limitations
> Bitcoin was created in 2009, and the available data in the dataset begins in April 2013. We are constrained by not seeing all of the history of this currency within the dataset.
> The data is sourced from Kaggle, which is ultimately sourced from another site that tracks Bitcoin and other cryptocurrencies. There are limitations on the amount of metadata available from this source, particularly around how the market-level breakdown is sourced into one cohesive price in the Historical data.
> There are some potentially confounding variables inherent in an analysis of Bitcoin. The market valuation is consistently changing on a daily basis with the mining of coins, and the nature of the market is highly dependent on supply and demand. There is also one owner who has 5% of the market share, whose actions may contribute to the behavior of the market prices.
> There is some data missing, particularly in the Volume variable. The subsequent analysis does not rely on Volume due to lack of colinearity with this variable, but this may have been due to missing data.

# <a name="descriptions"></a>Data Set Description
> Add data set description here

| Variable | Variable Type | Summary |
| :------: | :-----------: | :-----: |
| Date | DateTime | Date for summary info|
| Open | Numeric | Opening market price for Bitcoin |
| High | Numeric | Daily high price for Bitcoin |
| Low | Numeric | Daily low price for Bitcoin |
| Close | Numeric | Closing market price for Bitcoin |
| Volume | Numeric | Total amount of Bitcoin available|
| Market Cap | Numeric | Market Capitalization ([valuation](https://news.bitcoin.com/bitcoins-market-cap-not-think/) of the overall currency market)|
| Time | DateTime| Conversion of original Date variable for analysis use|