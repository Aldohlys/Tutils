
###library(DescTools) ### for pgcd
###library(quantmod) ## to retrieve data from Yahoo - getSymbols
###library(stringr) ### for str_to_upper function
##library(RQuantLib) ## for isBusinessDay function
### library(lubridate) ### for today() function

### This function work only for IBKR accounts not for Gonet account
### This function is used by other Tutils functions - NOT to be exported
getAllTrades = function() {
  suppressMessages(readr::read_delim(file=config::get("Trades"),
                                     delim=";",locale=readr::locale(date_names="en",decimal_mark=".",
                                                             grouping_mark="",encoding="UTF-8")))
}


#'@export
getTradeNr = function(v_instrument) {
  if(length(v_instrument)==0) return(NA)
  if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades = getAllTrades()
  trades = dplyr::filter(trades, Statut=="Ouvert" | Statut=="Ajust\u00e9")
  trades = dplyr::select(trades, Instrument,TradeNr)

  ### Retrieve dates in trades.csv corresponding to instruments of dt
  ### If there are several dates for the same instrument, take the oldest one (min)
  ### Suppress join by message
  # trade_nr=suppressMessages(dplyr::left_join(as.data.frame(list(Instrument=v_instrument)),trades)  %>%
  #                             dplyr::group_by(Instrument) %>%
  #                             dplyr::pull(TradeNr))
  trade_nr=suppressMessages(dplyr::pull(
    dplyr::group_by(dplyr::left_join(as.data.frame(list(Instrument=v_instrument)),trades),
                    Instrument),TradeNr))
  ### If all instrument are NA -> trade is not present - not yet recorded in Trades.csv
  if (length(trade_nr)==0) {
    display_error_message("Trade not present in Trades.csv file!")
    print(v_instrument)
    return(NA)
  }

  ### Retrieve the common Trade Nr
  trade_nr=unique(trade_nr)

  ### If at least one then retrieve corresponding trade nr
  ### And get the original trade date of the trade nr
  if (length(trade_nr) >1) {
    display_error_message("There is more than one trade in Instrument argument! Display oldest trade nr")
    return(NA)
  }

  message("getTradeNr: ",trade_nr)
  return(as.integer(trade_nr))
}

#'@export
getOpenDate = function(trade_nr) {

  stopifnot("trade_nr must be a numeric" = is.numeric(trade_nr))
  trades=getAllTrades()

  trades = dplyr::filter(trades, TradeNr==trade_nr)
  if (nrow(trades)==0) {
    display_error_message("Trade does not exist!")
    return(NA)
  }
  if (unique(trades$Statut)=="Ferm\u00e9") {
    display_error_message("Trade is closed in Trades.csv file!")
    return(NA)
  }

  trades = dplyr::select(trades, TradeDate)
  trade_dates=lubridate::dmy(trades$TradeDate)

  ### If there are several dates for the trade nr (adjusted case), take the oldest one (min)
  orig_trade_date=suppressWarnings(min(trade_dates,na.rm=T))

  message("getOpenDate: ",trade_nr," orig_date: ",format(orig_trade_date,"%d-%m-%Y"))
  return(orig_trade_date)
}

### Returns a number - sum of rewards (non-NA) for all given instruments that are Open/adjusted
#'@export
getRnR = function(trade_nr) {
  message("getRnR - Reward and Risk")
  stopifnot("trade_nr must be a numeric" = is.numeric(trade_nr))

  ##if (is.unsorted(v_instrument)) stop("Instrument must be sorted - prog. error")
  ### Read Trades.csv file and extract open/adjusted trades, to select all instruments present in dt argument
  trades = getAllTrades()

  trades = dplyr::filter(trades, TradeNr==trade_nr)
  if (nrow(trades)==0) {
    display_error_message("Trade does not exist!")
    return(NA)
  }
  if (unique(trades$Statut)=="Ferm\u00e9") {
    display_error_message("Trade is closed in Trades.csv file!")
    return(NA)
  }

  ### Retrieve instruments in trades.csv corresponding to instruments of dt
  ### If there are several records for the same instrument, take the oldest one (min)
  RnR=  dplyr::summarize(trades, reward= sum(as.double(Reward),na.rm=T),risk= sum(as.double(Risk),na.rm=T))
  #print(RnR)
  return(RnR)
}
