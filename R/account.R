#### Account related utilities
#'   readAccount
#'
#' This function reads the Account.csv file and formats it with right Date and HMS format
#' It then filters data so that it matches \code{accountnr} number
#'@param accountnr is the account number (IBKR)
#'@returns a tibble with the following fields: \code{ account	date	heure
#' NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
#' FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
#'  CashFlow}
#'@export
readAccount = function(accountnr) {
  file=paste0(config::get("DirNewTrading"),"Account",".csv")
  account_data = suppressMessages(readr::read_delim(file=file,delim=";",
                                             locale=readr::locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))

  ### account	date	heure
  ### NetLiquidation	EquityWithLoanValue	FullAvailableFunds	FullInitMarginReq	FullMaintMarginReq
  ### FullExcessLiquidity	OptionMarketValue	StockMarketValue	UnrealizedPnL	RealizedPnL	TotalCashBalance
  ### Starts on Oct 4th, 2022 for IBKR, on June 1st for Gonet

  ### Convert from European date format to internal R date format
  account_data$date=as.Date(account_data$date,format="%d.%m.%Y")
  account_data$heure=hms::parse_hms(account_data$heure)
  filter(account_data,account==accountnr)
}



############# PORTFOLIO specific functions #############
#' readPortfolio
#'
#'
#' This function reads the _account_nr.csv_ file and performs a bunch of data wrangling before returning a tibble with all data.
#'
#'
#' Data wrangling:
#' 1. formats it with right Date and HMS format
#' 2. converts \code{position} to \code{integer}
#' 3. removes all CASH positions
#' 4. rename IBKR columns to more friendly names
#'
#' It then filters data so that it matches \code{accountnr} number
#'
#'@param accountnr is the account number (IBKR)
#'@returns a tibble with the following columns:
#' \code{date; heure; type; symbol; expiration; strike; Put/Call; pos; mktPrice; optPrice}
#' \code{mktValue; avgCost; uPnL; IV; pvDividend; delta; gamma; vega; theta; uPrice; multiplier; currency}
#'@export
#'@examples
#'readPortfolio("D100000")
#'
readPortfolio = function(accountnr) {
  message("readPortfolio")
  file=paste0(config::get("DirNewTrading"),accountnr,".csv")
  #### Test if requested portfolio is present (e.g. Live.csv does not exist)
  if (file.exists(file)) {
    portf= suppressMessages(readr::read_delim(file=file,delim=";",
                                       locale=readr::locale(date_names="en",decimal_mark=".",grouping_mark="",encoding="UTF-8")))
    ### Convert from European date format to internal R date format
    portf$date=lubridate::dmy(portf$date)
    portf$heure=hms::parse_hms(portf$heure)
    ### Convert position into an integer (this is not a float)
    portf$position=as.integer(portf$position)
    ## Remove all CASH positions that are virtual
    portf = dplyr::filter(portf, secType!="CASH")

    ## For nicer output on screen -makes column names shorter

    ### Case where there are options in the portfolio
    if ("lastTradeDateOrContractMonth" %in% colnames(portf)) portf = dplyr::rename(portf, expiration=lastTradeDateOrContractMonth)
    if ("right" %in% colnames(portf)) portf = dplyr::rename(portf, `Put/Call`=right)
    if ("undPrice" %in% colnames(portf)) portf = dplyr::rename(portf, uPrice=undPrice)
    if ("impliedVol" %in% colnames(portf)) portf = dplyr::rename(portf,IV=impliedVol)

    portf = dplyr::rename(portf, type=secType,pos=position,
                      mktPrice=marketPrice, mktValue=marketValue,
                      avgCost=averageCost,uPnL=unrealizedPnL)
    return(portf)
  }
  else {
    message("File doesn't exist, please check")
    display_error_message("File doesn't exist, please check")
    return(dplyr::tibble())
  }
}

###############  TWR function
##############################
#'   twr
#'
#' This function Time Weighted Return computes TWR for
#' a given list of dates with corresponding end of day net liquidation values plus cashflows (inflows/outflows).
#'
#'  It is assumed that portfolio value begin of day (n) = portfolio value end of day (n-1)
#'  Also it is assumed that portfolio is valued every calendar day (incl. non business days)
#'  twr will be computed for all dates and then filtered so that it fit with input dates.
#'
#'@param dates list of dates when data are provided - there should be only one data point per date
#'@param e_nlv End of day net liquidation values: D1, D2, ... Dn
#'@param cashflows Begin of day cash flow inflows: D1, D2,... Dn
#'@returns a numerical vector of TWR values
#'@export
twr <- function(dates, e_nlv, cashflows) {
  ### dates are dates when data are provided - there should be only one data point per dates
  if (!all(!duplicated(dates))) {
    display_error_message("twr:All dates must be different!")
    return(NA)
  }
  else {
    #####  e_nlv: End of day net liquidation values: D1, D2, ... Dn
    ##### cash_flows: Begin of day cash flow inflows: D1, D2,... Dn
    ### The merge takes care of missing days -ensures that all time periods are equal -i.e=1 day

    ###

    e_nlv_regular <- merge(xts::xts(e_nlv, order.by = dates),
                           seq(from = min(dates), to = max(dates), by = "day"))
    cash_flows <- merge(xts::xts(cashflows, order.by = dates),
                        seq(from = min(dates), to = max(dates), by = "day"))

    e_nlv <- as.numeric(zoo::na.approx(e_nlv_regular))
    cash_flows[is.na(cash_flows)]=0
    cash_flows=as.numeric(cash_flows)

    n <- length(e_nlv)
    ##print(paste0("n:",n," nb elts:",length(dates)))

    ### If missing cash flows then means that equals to 0
    if (missing(cash_flows)) cash_flows=rep(0,n)

    ### Error management
    if (length(cash_flows) != n)  {
      print(paste0("NLV:",length(e_nlv)))
      print(paste0("Cash flows",length(cash_flows)))
      display_error_message("twr:Cash flows number of elements different from Porfolio values!!!!")
      return(NA)
    }
    else {
      ### Cash flows are beginning of the day, cash_flows
      ## Special case for first day return computation
      rn=numeric()
      twr=numeric()
      rn[1]= 0
      twr[1]= 1

      ### If only one portfolio value (end of day, cash flow)
      if (n==1) return(twr-1)

      for (i in 2:n) {
        rn[i]= e_nlv[i]/(e_nlv[i-1]+cash_flows[i])
        twr[i]=twr[i-1]*rn[i]
      }

      ### After computation, extract only twr values corresponding to dates
      ##print(paste0("twr min:",min(dates)," max:",max(dates)))

      twr=xts::xts(twr,order.by=seq(from=min(dates),to=max(dates),by="day"))[dates]

      ### returns twr as a numerical vector
      ### Substract 1 to all ratios so to get returns
      return(as.numeric(twr)-1)
    }
  }
}

