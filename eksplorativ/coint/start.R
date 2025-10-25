library(dplyr)
IBrokers::twsDisconnect(conn)



# 4001 er live 4002 er paper
conn <- mlfintester::conn(6,
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

equity_list_gold_3 <- list(
  # GLD = IBrokers::twsEquity("GLD"),
  # IAU = IBrokers::twsEquity("IAU"),
  SGOL = IBrokers::twsEquity("SGOL")
)

equity_list_gold_miners_usd <- list(
  NEM = IBrokers::twsEquity("NEM"),
  AEM = IBrokers::twsEquity("AEM"),
  KGC = IBrokers::twsEquity("KGC"),
  GFI = IBrokers::twsEquity("GFI")
)

equity_list_dk_bluechips <- list(
  # NOVO_B   = IBrokers::twsEquity("NOV"),
  # MAERSK_B = IBrokers::twsEquity("MAERSK.B"),
  # DSV      = IBrokers::twsEquity("DSV",      "SMART"),
  VWS      = IBrokers::twsEquity("VWS"),
  # ORSTED   = IBrokers::twsEquity("ORSTED"),
  CARL_B   = IBrokers::twsEquity("CARL")
)



dat <- mlfintester::historical_data_multi(conn,
                                   equity_list_gold_3,
                                   barSize = "1 day",
                                   duration = "1 Y")


p_dat <- build_open_tbl(
  dat,
  c("NEM", "AEM", "KGC", "GFI")
)



# 1 aktier ()
p12_stk <- build_open_tbl(dat, c("CVX","XOM"))
# 2 aktier (GOOG/GOOGL)
p12_google <- build_open_tbl(dat, c("GOOG","GOOGL"))

# 3 S&P500-trackere
p_sp500_3 <- build_open_tbl(dat, c("SPY","IVV","VOO"))

# 4 S&P500-trackere
p_sp500_4 <- build_open_tbl(dat, c("SPY","IVV","VOO","SPLG"))

# 3 guld-ETFer
p_gold_3 <- build_open_tbl(dat, c("GLD","IAU","SGOL"))


res <- backtest_coint(
  data = p_dat,
  z_in = 2.2,
  z_out = 0.5,
  notional = 200000,
  fee_flat = ,
  rank_level = "10pct",
  min_abs_weight = 0.1
)




ggplot2::ggplot(res, ggplot2::aes(row_in_data, equity)) +
  ggplot2::geom_rect(
    data = subset(
      dplyr::summarise(
        dplyr::group_by(
          dplyr::mutate(res, z_on = coint==1L,
                        grp = cumsum(z_on != dplyr::lag(z_on, default = first(z_on)))),
          grp),
        xmin = dplyr::first(row_in_data), xmax = dplyr::last(row_in_data), z_on = dplyr::first(z_on)),
      z_on),
    ggplot2::aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
    inherit.aes=FALSE, fill="red", alpha=0.08) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(y = fee_cum), color="red") +
  ggplot2::labs(title="Equity curve (gross) og fees separat", x=NULL, y="equity") +
  ggplot2::theme_minimal()



















# hyperparams -------------------------------------------------------------


library(doParallel)

grid <- seq(1, 2.5, by = 0.2)
resultat <- numeric(length(grid))
cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(cores)
on.exit(parallel::stopCluster(cl), add = TRUE)
doParallel::registerDoParallel(cl)


parallel::clusterExport(cl, varlist = "p12_stk", envir = environment())

resultat <- foreach(
  i = grid,
  .combine = c,
  .packages = c("mlfintester", "magrittr", "dplyr")
) %dopar% {
  res <- mlfintester::backtest_coint_v2(p12_stk, z_in = i, const_bps = 0)
  # hurtigere og uden dplyr-afhængighed:
  res$equity[length(res$equity)]  # eller: tail(res$equity, 1L)
}

names(resultat) <- sprintf("%.1f", grid)








