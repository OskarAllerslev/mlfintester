

# we create a connection
IBrokers::twsDisconnect(conn)



conn <- mlfintester::conn(1,
                  4001,
                  verbose = FALSE)


IBrokers::reqManagedAccts(conn)
account_info <- IBrokers::reqAccountUpdates(conn)

stk <- IBrokers::twsEquity(
  "AAPL",
  "SMART",
  "ISLAND"
)

data <- mlfintester::historical_data(
  conn = conn,
  equity = stk,
  barSize = "1 hour",
  duration = "1 Y"
)



equity_list <- list(
  GOOG  = IBrokers::twsEquity("GOOG"),
  GOOGL = IBrokers::twsEquity("GOOGL")
)



data <- mlfintester::historical_data_multi(conn,
                                   equity_list,
                                   barSize = "1 hour",
                                   duration = "1 W")


# eksempel ----------------------------------------------------------------


library(mlr3verse)
library(mlr3resampling)
library(mlr3temporal)
library(dplyr)


dt <- data %>%
  dplyr::arrange(index) %>%
  dplyr::mutate(
    ret_1 = TTR::ROC(AAPL.Close, n = 1, type = "discrete"),
    ret_2 = TTR::ROC(AAPL.Close, n = 2, type = "discrete"),
    sma20 = TTR::SMA(AAPL.Close, n = 20),
    target = lead(ret_1)              # næste periods afkast = målvariabel
  ) %>%
  tidyr::drop_na()


task <- TaskRegr$new(
  id      = "one_stock",
  backend = dt,
  target  = "target"
)

outer <- rsmp("forecast_cv",
              folds        = 5,
              window_size  = 20,
              horizon      = 5,
              fixed_window = TRUE)

outer$instantiate(task)   # nu virker det


inner <- rsmp("cv", folds = 3)







