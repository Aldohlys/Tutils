
########### Utilities for Line module
#################################################


### Stand-alone: needs global.R !!!! But has to be removed otherwise as implies loops
# source("global.R")
# library(shiny)
# library(dplyr)
# library(reticulate)
# py_run_file("C:/Users/aldoh/Documents/RApplication/Tutils/inst/python/getContractValue.py")
# library(lubridate)
# library(derivmkts)

NBBO = function(sym,right,strike,expiration,currency,exchange,tradingClass) {
  if (tradingClass == "Stock") {
    display_error_message("A valid Trading Class must be provided!")
    return(NA)
  }
  if (right=="Put") right="P"
  if (right=="Call") right="C"
  expiration=format(expiration,"%Y%m%d")
  strike=as.numeric(strike)
  message("NBBO Option value Sym:",sym," Type:",right," Strike:",strike," Expiration:",expiration,
              " Currency:",currency," Exchange:",exchange," tradingClass:",tradingClass)
  val=reticulate::py$getOptValue(sym=sym,strike=strike,expiration=expiration,
                     right=right,currency=currency,exchange=exchange,tradingClass=tradingClass)
  if (is.null(val)) val=-1
  else message("NBBO value:",val)
  return(val)
}

#'  build_line function
#'
#'This utility transforms a set of arguments into a dataframe. Any missing argument gets a default value,
#'
#'@param type Put Call or Stock
#'@param sym ticker name Empty default value
#'@param strike strike value - 0 is default
#'@param expdate expiration date Empty default value
#'@param DTE number of days to expiration -NA is default
#'@param startPrice initial price - 0 is default
#'@param endPrice final price - 0 is default
#'@param p_u_price projected underlying price - 0 is default
#'@param startPriceIV implied vol for initial price - 0 is default
#'@param endPriceIV implied vol for final price - 0 is default
#'@param pos position  - 0 is default
#'@param mul multiplier - NA is default
#'@param delta delta value  - NA is default
#'@param deltanotional delta notional value (delta dollars)  - NA is default
#'@returns a data frame with the following arguments: instrument,type,strike,mul,pos,startPrice,startPriceIV,
#'endPrice,endPriceIV, change= (endPrice-startPrice)/startPrice, startValue=mul*pos*startPrice,
#'endValue=mul*pos*endPrice, unrealizedPnL=mul*pos*(endPrice-startPrice), DTE, delta, deltanotional.
#'NB: If mul is NA then startValue, endValue, unPnL are all equal to NA.
#'@examples build_line()
#'@examples build_line(type="Stock",startPrice=100,endPrice=110,mul=1,pos=5,delta=5,deltanotional = 5*110)
#'@export
build_line = function(type="", sym="", strike=0, expdate="", DTE=NA, startPrice=0, endPrice=0, p_u_price=0,
                      startPriceIV=0, endPriceIV=0, pos=0, mul=NA, delta=NA, deltanotional=NA) {
  # print(paste("build_line input type=",type, "sym=",sym, "strike=",strike, "expdate=",expdate, "DTE=",DTE,
  #             "startPrice=",startPrice,"endPrice=",endPrice, "p_u_price=",p_u_price,
  #             "startPriceIV=",startPriceIV, "endPriceIV=",endPriceIV, "pos=",pos, "mul=",mul, "delta=",delta,
  #             "deltanotional=",deltanotional))

  default_line=data.frame(instrument="", type="", strike=0, mul=NA, pos=0, startPrice=0, startPriceIV=0,
                          endPrice=0, endPriceIV=0, change=0,
                          startValue=NA, endValue=NA, unrealizedPnL=NA, DTE=NA, delta=NA, deltanotional=NA)

  stopifnot(is.numeric(c(strike,pos,p_u_price,mul,startPrice,startPriceIV,endPrice,endPriceIV,
                         DTE, delta)) & is.character(c(type, sym, expdate)))
  # mul=if_else(mul==0,{display_error_message("Multiplier must be different from 0!")
  #                     0},mul)
  line=dplyr::if_else (type=="", default_line,
                data.frame(
                  instrument= dplyr::case_match (type,
                                          "Stock" ~ paste(sym,startPrice),
                                          c("Put","Call") ~  paste(type,sym,strike,expdate)),
                  type=type,
                  strike=strike,
                  mul=mul,
                  pos=pos,
                  startPrice=startPrice,
                  startPriceIV= startPriceIV,
                  endPrice=endPrice,
                  endPriceIV= endPriceIV,
                  change= (endPrice-startPrice)/startPrice,
                  startValue=mul*pos*startPrice, ##equals NA if multiplier=NA
                  endValue=mul*pos*endPrice, ##equals NA if multiplier=NA
                  unrealizedPnL=mul*pos*(endPrice-startPrice), ##equals NA if multiplier=NA
                  DTE= DTE,
                  delta=delta,
                  deltanotional= deltanotional ##equals NA if multiplier or delta=NA
                ))

  # print(paste("build_line output ",
  #             "instrument=",line$instrument, "type=",line$type,  "strike=",line$strike,
  #             "mul=",line$mul,"pos=",line$pos,
  #               "startPrice=",line$startPrice,"startPriceIV=",line$startPriceIV,
  #             "endPrice=",line$endPrice,  "endPriceIV=",line$endPriceIV,
  #             "change=",line$change, "startValue=",line$startValue,"endValue=",line$endValue,
  #             "unrealizedPnL=",line$unrealizedPnL,
  #             "DTE=",line$DTE,"delta=",line$delta,"deltanotional=",line$deltanotional))
  return(line)
}

#################################################
########################  Line Module


######################

#'   Ligne module UI
#'
#'This module provides the ability to enter parameters to an option
#'And then to output corresponding option price by using either a computation model (BS), looking at IBKR interface or manually.
#'It also allows to give the option price given a number of days and an end unerlying price (BS)
#'@param id this is used by caller to identify line and have the link with server piece
#'@export
ligneUI = function(id) {
  ns=shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("Type"),label="Type:",choices=c("Select","Stock","Put","Call"),
                selected="Select"),
    shiny::numericInput(ns("Qty"),label="Quantity:",value=0),
    shiny::uiOutput(ns("Controls1")),
    shiny::uiOutput(ns("Controls2"))
  )
}

#'   Journal ligne server
#'
#'This module provides the ability to write log entries into a journal and to retrieve them
#'@param id this is used by caller to identify line and have the link with server piece
#'@param sym this is to filter only log entries corresponding to \code{sym}
#'@param mul multiplier - usually 100, but also 10 (ESTX50)
#'@param s_datetime start date time- currently now
#'@param i_rate value for interest rate (options BS computation)
#'@param u_price value for underlying spot price
#'@param div dividend yield used by BS option computation
#'@param p_days number of days till projected date
#'@param p_u_price projected underlying price
#'@param currency string - "USD", "EUR" or "CHF"
#'@param exchange string usually "SMART", may be "CBOE" or "EUREX"
#'@param tradingClass string necessary to get option price from IBKR
#'@param exp_dates a vector of dates
#'@param strikes a vector of strikes
#'@export
ligneServer = function(id, sym, mul, s_datetime, i_rate, u_price, div,
                       p_days, p_u_price,currency,exchange,tradingClass,
                       exp_dates,strikes
                       ) {
  shiny::moduleServer(id, function(input,output,session) {
    ############  Action to be defined #########
    ns=shiny::NS(id)
    #### Add strike, Exp. date and pricing methods to User Interface for calls and puts
    shiny::observeEvent(input$Type,{
      if ((shiny::req(input$Type) == "Call") | (input$Type == "Put")) {
        output$Controls1=shiny::renderUI({
          shiny::tagList(
            shiny::selectInput(ns("ExpDate"),label="Exp. date",choices=exp_dates(),selected=exp_dates()[4]),
            {if (length(strikes())>1) shiny::selectInput(ns("Strike"),label="Strike:",choices=strikes(),selected=strikes()[1])
             else shiny::numericInput(ns("Strike"),label="Strike:",value=0)
              },
            shiny::selectInput(ns("Pricing"),label="Pricing: ",choices=c("Select","Black-Scholes","NBBO","Manual"),selected="Select")
            )
        })
      }
      ##### {} is necessary otherwise gets an obscure error:
      ##### Error in installExprFunction: argument "expr" is missing, with no default....
      else {
        output$Controls1 = shiny::renderUI({})
        output$Controls2 = shiny::renderUI({})
      }
    })

    ##### Add field to enter price for Manual pricing method
    shiny::observeEvent(input$Pricing, {
      message("observe pricing")
      if (shiny::req(input$Pricing) == "Manual") {
          output$Controls2=shiny::renderUI(shiny::tagList(
            shiny::numericInput(ns("startPrice"),label=" ",value=""),
            shiny::numericInput(ns("p_vol"),label="End Implied Vol (%):",value="")))
        }
        else if (input$Pricing == "Black-Scholes") {
          output$Controls2=shiny::renderUI(shiny::tagList(shiny::numericInput(ns("startIV"),label="Start Implied Vol (%): ",value=""),
                                                          shiny::numericInput(ns("p_vol"),label="End Implied Vol (%):",value="")))
        }
        else if (input$Pricing == "NBBO") {
          output$Controls2=shiny::renderUI(shiny::tagList(shiny::numericInput(ns("p_vol"),label="End Implied Vol (%):",value="")))

        }
        else output$Controls2=shiny::renderUI({})
    })

    shiny::observeEvent(input$ExpDate,{
      message("observe ExpDate: ",sym(),"  ",currency(),"  ",exchange(),"  ",tradingClass(),"  ",input$ExpDate)
      print(strikes())
      if (length(strikes())!=1) {
        expdate=format(as.Date(input$ExpDate,"%d %b %Y"),"%Y%m%d")
        expdate_strikes=reticulate::py$getStrikesfromExpDate(sym=sym(),currency=currency(),exchange=exchange(),
                                         tradingClass=tradingClass(),
                                         expdate=expdate,
                                         strikes=strikes())
        print(expdate_strikes)
        shiny::updateSelectInput(session=session,inputId="Strike",choices= expdate_strikes,
                                             selected=findNearestNumberOrDate(expdate_strikes,u_price()))
      }
    })

    #### As initial value copies strt vol into end vol
    shiny::observeEvent(startPriceIV(),{
      shiny::updateNumericInput(session=session,inputId="p_vol",value=input$startIV)
    },once=TRUE)

    ### Compute line elements:
    ### By default every field set to 0 or empty
    ### Stock: take start price and end price given as inputs to server function
    ### Call/Put: compute start price according to pricing method (BS, NBBO, Manual) - end price is always BS
    ###           compute vol for start price - vol for BS is already known

     expDate=shiny::reactive({
       message("expdate: ",input$ExpDate)
       shiny::req(input$ExpDate)
       as.Date(input$ExpDate,"%d %b %Y")
     })

     strike=shiny::reactive({
       shiny::req(input$Strike)
       message("strike: ",input$Strike)
       as.numeric(input$Strike)
     })

     startPrice= shiny::reactive({
       message("startPrice: ",input$Pricing)
       shiny::req(input$Pricing,input$Strike)
       ### Other arguments like Type, ExpDate, s_datetime, vol and i_rate have default values or tested before (Type)
       switch(input$Pricing,
              "Manual"= shiny::req(input$startPrice),
              "Black-Scholes"= {
                getBSOptPrice(type=input$Type, S=u_price(),K=strike(),r=i_rate(),
                              DTE=getDTE(s_datetime(),expDate()),
                              sig=startPriceIV(),div=div())
              },
              "NBBO"= {
                nbbo= NBBO(sym=sym(),right=input$Type, strike=strike(),expiration=expDate(),
                           currency=currency(),exchange=exchange(),tradingClass=tradingClass())
                   if (is.na(nbbo)) 0
                   else if (nbbo != -1) nbbo
                        else {
                          display_error_message("Can't get an option price from IBKR!")
                          0
                   }
              },
              0
       )
     })

     sDTE =shiny:: reactive({
       shiny::req(expDate)
       sdte=getDTE(s_datetime(),expDate())
       message("sDTE: ",sdte)
       sdte
     })

     eDTE = shiny::reactive({
       shiny::req(expDate)
       edte=getDTE(s_datetime()+lubridate::ddays(as.numeric(p_days())),expDate())
       message("eDTE: ",edte)
       edte
     })

     startPriceIV = shiny::reactive({
       shiny::req(input$Type,input$Strike,input$Pricing)
       if (input$Pricing== "Black-Scholes") {
         shiny::req(input$startIV)
         startiv=as.numeric(input$startIV)/100
        } else
         startiv=getImpliedVolOpt(type=input$Type,S=u_price(),K=strike(),r=i_rate(),
                        DTE=sDTE(),price=startPrice(),div=div())
       message("startPriceIV: ",startiv)
       startiv
     })

     endPrice = shiny::reactive({
       shiny::req(input$Strike)
       endp=0
       ### Test for arguments that have no default value (only empty string)
       if ((p_u_price() != "") & (p_days() != "")) {
         endp=getBSOptPrice(type=input$Type,S=p_u_price(),K=strike(),r=i_rate(),
                       DTE=eDTE(),sig=endPriceIV(), div=div())
       }
       print(paste("endPrice:",endp))
       endp
     })

     endPriceIV = shiny::reactive({
       shiny::req(input$p_vol)
       print(paste("endPriceIV:",input$p_vol/100))
       input$p_vol/100
     })

     delta = shiny::reactive({
       delta=NA
       if (input$Type != "Stock") {
         shiny::req(input$Strike)
         if ((p_u_price() != "") & (p_days() != "")) {
           print(paste("Delta BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
                       "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
           delta=getBSOptDelta(type=input$Type,S=p_u_price(),K=strike(),
                               DTE=eDTE(),sig=endPriceIV(),r=i_rate(),div=div())
         }
       }
       print(paste("Delta:",delta))
       delta
     })


    return(shiny::reactive({
      stopifnot(is.numeric(c(mul(),i_rate(),u_price(),p_days(),p_u_price())))

      ### build_line 14 arguments: type="", sym="", strike=0, expdate="",
      ####                         DTE=NA, startPrice=0, endPrice=0, u_price=0, p_u_price=0,
      ####                         startPriceIV=0, endPriceIV=0, pos=0, mul=NA, delta=NA
      ### No required argument

      switch(input$Type,
             "Stock" = build_line(type="Stock", sym=shiny::req(sym()), startPrice=u_price(), endPrice=p_u_price(),
                                  pos=shiny::req(input$Qty), mul=1, delta=input$Qty,deltanotional=input$Qty*p_u_price()),
             "Put"=,
             "Call"= {
                        delta=delta()*input$Qty*mul()
                        build_line(type=input$Type, sym=shiny::req(sym()), strike=shiny::req(strike()),expdate=format(shiny::req(expDate()),"%d-%b-%Y"),
                        DTE=eDTE(), startPrice=startPrice(), endPrice=endPrice(), p_u_price=p_u_price(),
                        startPriceIV=startPriceIV(), endPriceIV=endPriceIV(),
                        pos=shiny::req(input$Qty), mul=mul(), delta=delta,deltanotional=delta*p_u_price())
                      },
             build_line()) ## Default value
    }))
  })
}

##print("Ligne module loaded!")


ligneDemoApp <- function() {
  ##### List of  default expiration dates values
  weekly_dates=c(lubridate::ymd("2023-12-15"),lubridate::ymd("2023-12-22"),lubridate::ymd("2023-12-29"),
                 lubridate::ymd("2024-01-05"),lubridate::ymd("2024-01-12"),lubridate::ymd("2024-01-19"),lubridate::ymd("2024-01-26"),
                 lubridate::ymd("2024-02-02"),lubridate::ymd("2024-02-09"),lubridate::ymd("2024-02-16"),lubridate::ymd("2024-02-23")
  )


  monthly_dates=c(lubridate::ymd("2024-03-15"),lubridate::ymd("2024-04-19"),lubridate::ymd("2024-05-17"),lubridate::ymd("2024-06-21"),lubridate::ymd("2024-07-19"),
                  lubridate::ymd("2024-08-16"),lubridate::ymd("2024-09-20"),lubridate::ymd("2024-10-18"),lubridate::ymd("2024-11-15"),lubridate::ymd("2024-12-20"),
                  lubridate::ymd("2025-01-17"), lubridate::ymd("2025-03-21"), lubridate::ymd("2025-06-20"), lubridate::ymd("2025-09-19"), lubridate::ymd("2025-12-19"),
                  lubridate::ymd("2026-01-16"))
  dates_list=c(weekly_dates, monthly_dates)

  ui = shiny::fluidPage(
    shiny::h1("AI Demo"),
    shiny::h3(shiny::textOutput("title")),
    ligneUI("Ligne"),
    shiny::tableOutput("table"),
    shiny::numericInput("u_price","Current underlying price:",value=159.4),
    shiny::numericInput("p_u_price","Projected price:",value=160.5),
    shiny::numericInput("div","Dividend yield (%):",value=0),
    shiny::numericInput("p_days","Number of days for projected price:",value=10))

  server = function(input, output, session) {
    ligne=ligneServer(id="Ligne",sym=shiny::reactive("AI"),mul=shiny::reactive(100),
                      s_datetime=shiny::reactive(lubridate::now()),i_rate=shiny::reactive(0.05),
                      u_price=shiny::reactive(input$u_price),
                      div=shiny::reactive(input$div/100),
                      p_days=shiny::reactive(input$p_days),
                      p_u_price=shiny::reactive(input$p_u_price),
                      currency=shiny::reactive("EUR"),exchange=shiny::reactive("EUREX"),
                      tradingClass=shiny::reactive("AIR"),
                      exp_dates=shiny::reactive(format(dates_list,"%d %b %Y")),
                      strikes=shiny::reactive(160)
                      #strikes=reactive(seq(150,170,2))
                      )

    output$table=shiny::renderTable(ligne())
    output$title=shiny::renderText({paste0("AI current price=",input$u_price,", projected price=",input$p_u_price," in ",
                                    input$p_days," days, div yield=",input$div,"%")})

  }
  shiny::shinyApp(ui, server)
}


##ligneDemoApp()


