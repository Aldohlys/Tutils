% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prices.R
\name{getLastPriceDate}
\alias{getLastPriceDate}
\title{getLastPriceDate}
\usage{
getLastPriceDate(ticker)
}
\arguments{
\item{ticker}{ticker name, as known by IBKR - can be one name or a vector of names}
}
\value{
a list of dates for each ticker. It calls \code{getSymFromDate} to get the dates.
}
\description{
This function takes one ticker (or a vector of tickers) as input and returns the last available
date with an available price - see also \code{getLastAdjustedPrice}.
}
\examples{
getLastPriceDate("SPY")
}
