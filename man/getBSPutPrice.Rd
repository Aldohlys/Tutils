% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{getBSPutPrice}
\alias{getBSPutPrice}
\title{getBSPutPrice}
\usage{
getBSPutPrice(
  S,
  K,
  r = as.numeric(config::get("interest_rate")),
  DTE,
  sig,
  div = 0
)
}
\arguments{
\item{S}{Spot price or current underlying price}

\item{K}{Strike price}

\item{r}{Interest rate - default value is defined in config.yml file}

\item{DTE}{Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))}

\item{sig}{annualized volatility}

\item{div}{annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.}
}
\description{
This function supplies an option price based upon Black-Scholes-Merton model
}
\details{
It performs the computation as per textbooks using derivmkts package:

d1 <- (log(S/K) + (r + sig^2/2) x T) / (sig x sqrt(T))

d2 <- d1 - sig*sqrt(T)

value <-  (K x exp(-rT) x pnorm(-d2) - S x pnorm(-d1))

This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
\itemize{
\item DTE=0 and S=K -> option price = 0
\item DTE < 0 -> option price = 0
\item S=K=0 -> option price = 0
}
}
\examples{
getBSPutPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
getBSPutPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
}
\keyword{Black-Scholes}
\keyword{trading}
