
###################  Retrieve prices functions #######################

##### Yahoo-based symbol lookup
### getSym is based upon quantmod getSymbols function

#### auto.assign=TRUE is necessary if multiple symbols at the same time
#'@export
getSymFromDate = function(sym, date) {
  lookup_yahoo = c("ESTX50"="^STOXX50E","MC"="MC.PA","OR"="OR.PA","TTE"="TTE.PA","AI"="AI.PA",
                   "SPX"="^SPX","XSP"="^XSP","NESN"="NESN.SW","HOLN"="HOLN.SW","SLHN"="SLHN.SW")
  sym=dplyr::if_else(sym %in% names(lookup_yahoo), lookup_yahoo[sym], sym)
  lapply(sym, function(x) { quantmod::getSymbols(x,from=date,auto.assign = F,warnings=FALSE)})
}

#'@export
getSym = function(sym){
  getSymFromDate(sym,lubridate::ymd(config::get("CurrentTradesInitialDate")))
}

# getAdjReturns = function(sym) {
#   returns=data.frame(date=as.Date(index(sym)),
#                      return=c('NA',round(diff(log(coredata(coredata(sym[,6])))),3)))
#   colnames(price)=c("date","return")
#   return(returns)
# }

#'@export
getPrice= function(sym_list) {
  ### Takes only the first element of the sym list
  ### This is for compatibility with getSymFromDate
  sym=sym_list[[1]]
  price=data.frame(date=as.Date(zoo::index(sym)), value=zoo::coredata(sym[,6]))
  colnames(price)=c("date","value")
  return(price)
}


### Tries first on Yahoo (close price) - this works only for previous days, not for today
### THen on IBKR and if not available returns NA
### report_date can be a closed day -> then takes nearest day in the list
### prices_list goes back only to CurrentTradesInitialDate for IBKR so begin of 2023 - not suited for Gonet account
#'@export
getsymPrice = function(sym,currency,report_date){

  ### First case - requested date is an holiday
  ### Get last close price in this case
  if ((!RQuantLib::isBusinessDay("UnitedStates",report_date)) | report_date < lubridate::today()) {
    prices_list=getPrice(getSymFromDate(sym,lubridate::ymd("2023-01-03")))
    report_date = findNearestNumberOrDate(prices_list$date, report_date)
    price = dplyr::filter(prices_list,date==report_date)
    return(price$value)
  }

  #### If report_date is today and is not an holiday
  ####  then tries to retrieve current market price and finally asks the user
  ####
  ### SMART works fine in all cases except for SPX and XSP cases
  exchange = switch(sym, ESTX50= "EUREX", SPX =, XSP = "CBOE", "SMART")
  sec=switch(sym, SPX=,XSP=,ESTX50="IND","STK")
  return(stock_price(sec,sym,currency,exchange,reqType=4))

}

### Takes last open date if current date provided does not work
#'@export
getLastAdjustedPrice = function(ticker) {
  dplyr::if_else( (is.null(ticker) | ticker %in% c("","All","STOCK")),
                  NA,
                  {
                    ## Case date is Tuesday morning and US market not yet opened + Monday and Friday were off -> Get THur data
                    ### This returns all last data available
                    ticker=getSymFromDate(ticker,lubridate::today()-5)
                    ### Last column is "ticker.Adjusted"
                    sapply(ticker, function(x) round(x[[nrow(x),6]],2))
                  }
  )
}


#'@export
getLastPriceDate = function(ticker) {
  dplyr::if_else( (is.null(ticker) | ticker %in% c("","All","STOCK")),
                  NA,
                  {ticker=getSymFromDate(ticker,lubridate::today()-5)
                  ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
                  sapply(ticker,function(x) format(zoo::index(x[nrow(x)]),"%d.%m.%Y"))
                  })
}

### Retrieve data from Yahoo Finance - no need to launch IBKR TWS
### Get last price and last change (J/J-1)
#'@export
getLastTickerData = function(ticker) {
  if (is.null(ticker) |
      ticker %in% c("","All","STOCK")) return(list(last=NA,change=NA))
  tryCatch({
    ticks=getSymFromDate(ticker,lubridate::today()-5)[[1]] ## Case Tuesday morning and US market not yet opened + Monday and Friday were off -> Get Wed and THur data
    ##names(ticks)[length(names(ticks))]="Adjusted" ### Last column is "ticker.Adjusted" -> to be renamed as Adjusted
    last_data=ticks[[nrow(ticks),6]]
    p_last_data=ticks[[nrow(ticks)-1,6]]
    return(list(
      last=round(last_data,2),
      change=scales::label_percent(accuracy=0.01)(last_data/p_last_data-1)
    ))
  }, error = function(e) {
    print(paste("Error:", e))
    return(list(last="Non disponible",change=NA))
  })
}

### The following will not work in a package:
### lastSPY will namely be called at package build and be associated with SPY value at that time
### lastSPY value will never change thereafter
####lastSPY=getLastTickerData("SPY")  ### Mkt value

#'@export
getVal=function(sym) {
  cat("No value for ",sym,"\n Enter new price: ")
  if (interactive()) {
    ## display_error_message(paste0("No value for",sym," You need to enter a new price"))
    # showModal(modalDialog(
    #   tags$h2('No value for Please enter your personal information'),
    #   numericInput('val', 'Value'),
    #   footer=tagList(
    #     actionButton('submit', 'Submit'),
    #     modalButton('cancel')
    #   )
    # ))
    val=readline(prompt="(interactive) ")
  }
  else val= readLines(con="stdin", n=1)[[1]]
  as.double(val)
}

#### Used by Gonet.R script and RAnalysis
#'@export
stock_price = function(sec="STK",sym,currency,exchange="SMART",reqType=4) {
  #### Default value for security type is Stock
  #### Default value for exchange is SMART
  message("stock_price")
  ### Special case for CSBGU0 stock I won in Gonet portfolio
  if (sym == "CSBGU0") reqType=3
  line=reticulate::py$getStockValue(sec=sec,sym=sym,currency=currency,exchange=exchange,reqType=reqType)

  #### readline works only in interactive mode,
  #### readLines works only in non-interactive mode
  ### Case where no IBKR connection exists (NULL) or no value returned
  ### isTRUE let is.na test works also if val=NULL
  ### length(val) is TRUE when val=numeric(0)
  if (is.null(line)) {
    val=getVal(sym)
    #### Write data to CSV file as data input by end user or Yahoo
    line=tibble::tibble(datetime=format(lubridate::now(),"%e %b %Y %Hh%M"),sym=sym)
    line$price=val
    utils::write.table(line,paste0(config::get("DirNewTrading"),"prices.csv"),sep=";",
                       row.names = FALSE,quote=F,col.names = FALSE,append=TRUE)
  }
  val=line[["price"]]
  print(paste0("DateTime:",line[["datetime"]], " Value: ",val))
  return(val)
}
