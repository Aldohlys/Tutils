getTestTrades = function() {suppressMessages(read_delim(file=config::get("TestTrades"),
                                        delim=";",
                                        locale=locale(date_names="en",decimal_mark=".",
                                                      grouping_mark="",encoding="UTF-8")))}


test_that("It is possible to retrieve a trade number for a single instrument", {
    with_mocked_bindings(
      getAllTrades = getTestTrades, {
        trade_nr=getTradeNr("TLT 16FEB24 103 C",account_type="Live")
        expect_true(trade_nr == 368)
      })
})

test_that("It is possible to retrieve a trade number for a single instrument without specifying account type", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("TLT 16FEB24 103 C")
      expect_true(trade_nr == 368)
    })
})


test_that("It is possible to retrieve a trade number for a single instrument with account type=NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("TLT 16FEB24 103 C",NA)
      expect_true(trade_nr == 368)
    })
})

test_that("It is possible to retrieve a trade number for a trade with multiple instruments", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("VALE 19JAN24 13 P",account_type="Live")
      expect_true(trade_nr == 367)
    })
})


test_that("It is possible to retrieve a trade number for a trade with multiple instruments, using multiple inputs", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("VALE 19JAN24 13 P","VALE 15MAR24 14 P")), account_type="Live")
      expect_true(trade_nr == 367)
    })
})

test_that("It is possible to retrieve trade numbers for 2 trades with multiple instruments, using one input per trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("FNV 19JAN24 95 P","VALE 15MAR24 14 P")), account_type="Live")
      expect_true(all(trade_nr == c(364,367)))
    })
})

test_that("It is possible to retrieve a trade number for 2 trades with multiple instruments, using multiple inputs per trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("FNV 19JAN24 95 P","VALE 15MAR24 14 P","VALE 19JAN24 13 P")),
                          account_type="Live")
      expect_true(all(trade_nr == c(364,367)))
    })
})

test_that("It is possible to retrieve a trade number for 3 trades with multiple instruments, using multiple inputs per trade", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("FNV 19JAN24 95 P","VALE 15MAR24 14 P","VALE 19JAN24 13 P","TLT 16FEB24 103 C")),
                          account_type="Live")
      expect_true(all(trade_nr == c(364,368,367)))
    })
})

test_that("It is possible to retrieve a trade number providing also data for closed trades", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("VALE 19JAN24 13 P","STOCK")), account_type="Live")
      expect_true(all(trade_nr == 367,na.rm=TRUE))
    })
})

test_that("If no trade or closed trade then returns NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("VALE 19JAN24 13 P","STOCK")), account_type="Live")
      expect_true(all(trade_nr == 367,na.rm=TRUE))
    })
})

test_that("If no trade at all then returns NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SPY 19JAN24 500 P","STOCK")), account_type="Live")
      expect_true(is.na(trade_nr))
    })
})


test_that("If no trade at all then returns NA without account type specified", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SPY 19JAN24 500 P","STOCK")))
      expect_true(is.na(trade_nr))
    })
})


test_that("If no trade at all then returns NA with account type equals to NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr(sort(c("SPY 19JAN24 500 P","STOCK")),NA)
      expect_true(is.na(trade_nr))
    })
})

test_that("Test a trade from Simu account with account type equals to NA", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("MCL MAY24",NA)
      expect_true(trade_nr == 347)
    })
})


test_that("Test a trade from Simu account NA with account type equals to Simu", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("MCL MAY24","Simu")
      expect_true(trade_nr == 347)
    })
})


test_that("Test a trade from Simu account NA with account type equals to Live", {
  with_mocked_bindings(
    getAllTrades = getTestTrades, {
      trade_nr=getTradeNr("MCL MAY24","Live")
      expect_true(is.na(trade_nr))
    })
})
