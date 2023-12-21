test_that("display_error_message works outside Shiny", {
  skip("display_message") ### This is a useless test and clutters the console with one message
  expect_equal(display_message("This is a test message"), NULL)
})

test_that("display_error_message works outside Shiny", {
  skip("display_error_message") ### This is a useless test and clutters the console with one message
  expect_equal(display_error_message("This is a test error message"), NULL)
})

test_that("pgcd works with one integer", {
  expect_true(pgcd(3)==3)
})

test_that("pgcd works with 2 integers", {
  expect_true(pgcd(c(5,3))==1)
})


test_that("build instrument name works with an option", {
  expect_equal(buildInstrumentName("SPY",as.Date("2023-08-15"),400.0,"Put"),"SPY 15AUG23 400 P")
})

test_that("build instrument name works with something that is not an option", {
  expect_equal(buildInstrumentName("SPY",type="STK"),"SPY")
})

test_that("build Instrument name with AMGN 275 Call Jan'24", {
  expect_equal(buildInstrumentName("AMGN",expdate=as.Date("2024-01-19"),275,"C"),
               "AMGN 19JAN24 275 C")
})

