
###library(DescTools) ### for pgcd
###library(quantmod) ## to retrieve data from Yahoo - getSymbols
###library(stringr) ### for str_to_upper function
##library(RQuantLib) ## for isBusinessDay function
### library(lubridate) ### for today() function

#' getAllTrades function
#'
#' This function work only for IBKR accounts not for Gonet account
#' This function is used by other Tutils functions - NOT to be exported
#' No argument - takes its source from config::get()
#' @importFrom readr read_delim locale
getAllTrades = function() {
  suppressMessages(read_delim(file=config::get("Trades"),
                                     delim=";",locale=locale(date_names="en",decimal_mark=".",
                                                             grouping_mark="",encoding="UTF-8")))
}

#' getTradeNr
#'
#' This function retrieves one or a vector of trade numbers, all pertaining to open/adjusted trades
#' including instruments listed in \code{v_instrument} argument.
#'
#' It works by matching all instruments in Trades.csv file argument,
#' If no instruments are retrieved then NA is returned and an error message is displayed.
#'
#'@param v_instrument String on IBKR format, or vector of strings. Such as "SPY 15DEC23 400 P"
#'@param account_type one of the string values ("Live", "Simu", or NA), specifies with which account type trades are to be retrieved.
#'Default value is NA, which means no filtering done on account_type
#'@return Integer or a vector of integers
#'@export
getTradeNr = function(v_instrument,account_type=NA) {
  if(length(v_instrument)==0) {
    display_error_message("No instrument to be searched!")
    return(NA)
  }

  if (!is.na(account_type) && !(account_type %in% c("Live","Simu")))
    stop("Trades.csv understands only Live/Simu types of account or must be equal to NA")
  if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  ### Only opened trades can be retrieved
  trades = getAllTrades()
  trades = dplyr::filter(trades, Statut=="Ouvert" | Statut=="Ajust\u00e9")
  if (!is.na(account_type)) trades = dplyr::filter(trades, Account == account_type)
  trades = dplyr::select(trades, Instrument,TradeNr)

  ### Retrieve trade_nr in trades.csv corresponding to instruments of dt
  ### Suppress join message by suppresswMessages
  trade_nr=suppressMessages(dplyr::pull(
    dplyr::group_by(dplyr::left_join(as.data.frame(list(Instrument=v_instrument)),trades),
                    Instrument),TradeNr))
  ### If left join returns NA -> trade is not present - not yet recorded in Trades.csv
  if (all(is.na(trade_nr))) {
    display_error_message("Trades not opened/adjusted in Trades.csv file!")
    print(v_instrument)
    return(NA)
  }

  ### Retrieve the common Trade Nr - there may be several trade_nr
  trade_nr=unique(trade_nr)

  ### If at least one then retrieve corresponding trade nr
  # ### And get the original trade date of the trade nr
  # if (length(trade_nr) >1) {
  #   display_error_message("There is more than one trade in Instrument argument! Display oldest trade nr")
  #   return(NA)
  # }

  ### Converted to integer (vector of integer if necessary)
  return(as.integer(trade_nr))
}

#' getOpenDate
#'
#' This function retrieves one or a vector of dates, all pertaining to open/adjusted trades
#'
#' It works by matching all trade number in Trades.csv file argument against trade dates
#' Then the oldest date per trade is returned.
#'
#'@param trade_nr an integer or a vector of integers
#'@return a data frame giving for each trade number an expiration date \code{expdate} and the original trade date \code{orig_date}
#'@export
getOpenDate = function(trade_nr) {

  if (!is.numeric(trade_nr)) {
    display_error_message("trade_nr must be a numeric")
    return(NA)
  }

  trades=getAllTrades()
  trades = dplyr::group_by(dplyr::right_join(trades, data.frame(TradeNr=trade_nr), by="TradeNr"), TradeNr)

  if (nrow(trades)==0) {
    display_error_message("Trade do not exist !")
    return(NA)
  }
  if (all(trades$Statut %in% "Ferm\u00e9")) {
    display_error_message("All these trades are closed in Trades.csv file!")
    return(NA)
  }

  ### Remove closed trades
  trades = dplyr::filter(trades, Statut != "Ferm\u00e9")

  ## If there are several dates for the trade nr (adjusted case), take the oldest one (min)
  ### same thing for expiration date
  dplyr::summarize(trades,orig_date=min(lubridate::dmy(TradeDate)),expdate=min(lubridate::dmy(Exp.Date)))
}

#' getRnR
#'
#' This function retrieves a data frame listing reward and risk for a given list of trade numbers
#'
#' It works by matching all trade number in Trades.csv file argument against the argument given.
#' Then it groups by trade number and compute for each trade numnber the total of reward and risks
#'
#'@param trade_nr an integer or a vector of integers
#'@return a dataframe with column trade number, reward and risk.
#'Each row is associated with a trade number.
#'Reward column is the sum of rewards (non-NA) for all given instruments that belong to the trade number
#'Risk column is the sum of risks (non-NA) for all given instruments that belong to the trade number
#'@export
getRnR = function(trade_nr) {
  message("getRnR - Reward and Risk")

  if (!is.numeric(trade_nr)) {
    display_error_message("trade_nr must be a numeric")
    return(NA)
  }

  ##if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  trades = getAllTrades()
  trades = dplyr::group_by(dplyr::right_join(trades, data.frame(TradeNr=trade_nr), by="TradeNr"), TradeNr)

  if (nrow(trades)==0) {
    display_error_message("Trade does not exist!")
    return(NA)
  }

  if (all(trades$Statut %in% "Ferm\u00e9")) {
    display_error_message("All these trades are closed in Trades.csv file!")
    return(NA)
  }

  ### Remove closed trades - Extract only open/adjusted trades
  trades = dplyr::filter(trades, Statut != "Ferm\u00e9")

  ### Retrieve instruments in trades.csv corresponding to instruments of dt
  ### If there are several records for the same instrument, take the oldest one (min)
  RnR=  dplyr::summarize(trades, reward= sum(as.double(Reward),na.rm=T),risk= sum(as.double(Risk),na.rm=T))
  #print(RnR)
  return(RnR)
}
