test_that("readPortfolio returns some data and columns are all authorized names", {
  expect_true({
    portf=readPortfolio(paste0(config::get("DirNewTrading"),"U1804173.csv"))
       all(colnames(portf) %in% c("date", "heure", "symbol", "type", "expiration", "strike", "pos", "mktPrice", "optPrice",
              "mktValue", "avgCost", "uPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
              "uPrice", "multiplier", "currency")) &&
      nrow(portf) >=1
  })
})

test_that("readPortfolio returns some data and columns are all authorized names", {
  expect_true({
    portf=readPortfolio(paste0(config::get("DirNewTrading"),"Gonet.csv"))
    all(colnames(portf) %in% c("date", "heure", "symbol", "type", "expiration", "strike", "pos", "mktPrice", "optPrice",
                               "mktValue", "avgCost", "uPnL", "IV", "pvDividend", "delta", "gamma", "vega", "theta",
                               "uPrice", "multiplier", "currency")) &&
      nrow(portf) >=1
  })
})

test_that("readAccount Simu  returns some data and columns look good", {
  expect_true({
    acc=readAccount("DU5221795")
    identical(colnames(acc),  c("account", "date","heure", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashFlow")) &&
      nrow(acc) >1
  })
})


test_that("readAccount Gonet  returns some data and columns look good", {
  expect_true({
    acc=readAccount("Gonet")
    identical(colnames(acc),  c("account", "date","heure", "NetLiquidation",	"EquityWithLoanValue",	"FullAvailableFunds",
                                "FullInitMarginReq",	"FullMaintMarginReq", "FullExcessLiquidity",
                                "OptionMarketValue",	"StockMarketValue",	"UnrealizedPnL",
                                "RealizedPnL",	"TotalCashBalance", "CashFlow")) &&
      nrow(acc) >1
  })
})
