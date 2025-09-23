library(dplyr)
IBrokers::twsDisconnect(conn)



# 4001 er live 4002 er paper
conn <- mlfintester::conn(1,
                  4001,
                  verbose = FALSE)



build_open_tbl <- function(dat, tickers) {
  out <- NULL
  for (i in seq_along(tickers)) {
    tk <- tickers[i]
    open_col <- paste0(tk, ".Open")

    ti <- tibble::as_tibble(dat[[tk]])

    # første ticker: behold index + open; resten kun open
    if (i == 1) {
      ti <- ti %>% dplyr::select(index, !!sym(open_col))
    } else {
      ti <- ti %>% dplyr::select(!!sym(open_col))
    }

    # omdøb til Open_i
    ti <- ti %>% dplyr::rename(!!paste0("Open_", i) := !!sym(open_col))

    out <- if (is.null(out)) ti else dplyr::bind_cols(out, ti)
  }
  out
}
equity_list <- list(
  # GOOG  = IBrokers::twsEquity("GOOG"),
  # GOOGL = IBrokers::twsEquity("GOOGL"),
  # PEP = IBrokers::twsEquity("PEP"),
  # KO = IBrokers::twsEquity("KO")
  XOM = IBrokers::twsEquity("XOM"),
  CVX = IBrokers::twsEquity("CVX")
)

# ---- Lister med tickers ----
equity_list_sp500_3 <- list(
  SPY  = IBrokers::twsEquity("SPY"),
  IVV  = IBrokers::twsEquity("IVV"),
  VOO  = IBrokers::twsEquity("VOO")
)

equity_list_sp500_4 <- list(
  SPY  = IBrokers::twsEquity("SPY"),
  IVV  = IBrokers::twsEquity("IVV"),
  VOO  = IBrokers::twsEquity("VOO"),
  SPLG = IBrokers::twsEquity("SPLG")
)

equity_list_gold_3 <- list(
  GLD = IBrokers::twsEquity("GLD"),
  IAU = IBrokers::twsEquity("IAU"),
  SGOL = IBrokers::twsEquity("SGOL")
)

equity_list_google_2 <- list(
  GOOG  = IBrokers::twsEquity("GOOG"),
  GOOGL = IBrokers::twsEquity("GOOGL")
)


dat <- mlfintester::historical_data_multi(conn,
                                   equity_list_sp500_3,
                                   barSize = "1 day",
                                   duration = "10 Y")



p1 <- tibble::as_tibble(dat$XOM) %>%
  dplyr::rename(Open_1 = XOM.Open) %>%
  dplyr::select(index, Open_1)

p2 <- tibble::as_tibble(dat$CVX) %>%
  dplyr::rename(Open_2= CVX.Open) %>%
  dplyr::select( Open_2)

p1 <- tibble::as_tibble(dat$GOOG) %>%
  dplyr::rename(Open_1 = GOOG.Open) %>%
  dplyr::select(index, Open_1)

p2 <- tibble::as_tibble(dat$GOOGL) %>%
  dplyr::rename(Open_2= GOOGL.Open) %>%
  dplyr::select( Open_2)

p3 <- tibble::as_tibble(dat$PEP) %>%
  dplyr::rename(Open_3= PEP.Open) %>%
  dplyr::select( Open_3)


p4 <- tibble::as_tibble(dat$KO) %>%
  dplyr::rename(Open_4= KO.Open) %>%
  dplyr::select( Open_4)

p12 <- tibble::as_tibble(cbind(p1,p2, p3, p4))
p12 <- tibble::as_tibble(cbind(p1,p2))



# 2 aktier (GOOG/GOOGL)
p12_google <- build_open_tbl(dat, c("GOOG","GOOGL"))

# 3 S&P500-trackere
p_sp500_3 <- build_open_tbl(dat, c("SPY","IVV","VOO"))

# 4 S&P500-trackere
p_sp500_4 <- build_open_tbl(dat, c("SPY","IVV","VOO","SPLG"))

# 3 guld-ETFer
p_gold_3 <- build_open_tbl(dat, c("GLD","IAU","SGOL"))


res <- mlfintester::backtest_coint(
  data = p_sp500_3,
  z_in = 2.3,
  const_bps = 0
)




ggplot2::ggplot(res, ggplot2::aes(t, equity)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Equity curve", x = NULL, y = "Equity") +
  ggplot2::theme_minimal()

res <- dplyr::mutate(res, peak = cummax(equity), drawdown = equity - peak)

ggplot2::ggplot(res, ggplot2::aes(t, drawdown)) +
  ggplot2::geom_area() +
  ggplot2::labs(title = "Drawdown", x = NULL, y = "Drawdown") +
  ggplot2::theme_minimal()










