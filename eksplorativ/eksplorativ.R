

# we create a connection
IBrokers::twsDisconnect(conn)



conn <- mlfintester::conn(1,
                  4002,
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
  barSize = "1 day",
  duration = "10 Y"
)



equity_list <- list(
  # GOOG  = IBrokers::twsEquity("GOOG"),
  # GOOGL = IBrokers::twsEquity("GOOGL"),
  PEP = IBrokers::twsEquity("PEP"),
  KO = IBrokers::twsEquity("KO")
)



dat <- mlfintester::historical_data_multi(conn,
                                   equity_list,
                                   barSize = "1 day",
                                   duration = "15 Y")


data <- dat

for (nm in names(data)) {
  data[[nm]] <- data[[nm]] %>%
    dplyr::arrange(index) %>%
    dplyr::mutate(
      log_return = base::log(
        .data[[paste0(nm,".Close")]] /
          dplyr::lag(.data[[paste0(nm,".Close")]])),
      VOL20 = TTR::runSD(log_return, n = 14),
      TTR::ATR(
        cbind(
          High  = .data[[ paste0(nm, ".High") ]],
          Low   = .data[[ paste0(nm, ".Low")  ]],
          Close = .data[[ paste0(nm, ".Close")]]
        ),
        n = 14
      )[, "atr"]
         ) %>%
    na.omit()
}

pair_list <- utils::combn(names(data), 2, simplify = FALSE)
calc_pair_kf <- function(pair) {

  sym1 <- pair[1]
  sym2 <- pair[2]
  df1 <- data[[sym1]]
  df2 <- data[[sym2]]

  p1 <- df1[[paste0(sym1, ".Close")]]
  p2 <- df2[[paste0(sym2, ".Close")]]
  idx <- df1[["index"]]
  mod <- dlm::dlmModReg(
    X = p2,
    dV = 1e-4,
    dW = c(1e-5, 1e-5)
  )
  out <- dlm::dlmFilter(p1, mod)
  st <- dlm::dropFirst(out$m)
  alpha <- st[, 1]
  beta <- st[, 2]
  spread <- p1 - alpha - beta * p2
  w1 <- rep(1, length(beta))
  w2 <- -beta
  tibble::tibble(
    sym1    = sym1,
    sym2    = sym2,
    p1 = p1,
    p2 = p2,
    index   = idx,       # første state er initial og droppes
    alpha   = alpha,
    beta    = beta,
    spread  = spread,
    w1      = w1,
    w2      = w2
  )

}


pair_results <- purrr::map_dfr(pair_list, calc_pair_kf)



window    <- 20
entry_z   <- 2.5
exit_z    <- 0.5

signals <- pair_results %>%
  dplyr::arrange(sym1, sym2, index) %>%
  dplyr::group_by(sym1, sym2) %>%
  dplyr::mutate(
    ma_spread = TTR::runMean(spread, n = window),
    sd_spread = TTR::runSD  (spread, n = window),
    zscore    = (spread - ma_spread) / sd_spread,
    raw_sig   = dplyr::case_when(
      zscore >  entry_z ~ -1,
      zscore < -entry_z ~ +1,
      abs(zscore) < exit_z ~  0,
      TRUE               ~ NA_real_
    )
  ) %>%
  tidyr::fill(raw_sig, .direction = "down") %>%
  dplyr::mutate(signal = tidyr::replace_na(raw_sig, 0)) %>%
  dplyr::ungroup()


signals <- signals %>%
  dplyr::group_by(sym1, sym2) %>%
  dplyr::mutate(pos = dplyr::lag(signal, default = 0)) %>%  # position = gårsdagens signal
  dplyr::ungroup()


signals_ret <- signals %>%
  dplyr::group_by(sym1, sym2) %>%
  dplyr::mutate(
    d_spread  = spread - dplyr::lag(spread),
    strat_ret = pos * d_spread,                # PnL ≈ Δ-spread
    strat_ret = tidyr::replace_na(strat_ret, 0),
    cum_ret   = cumsum(strat_ret)
  ) %>%
  dplyr::ungroup()



signals_dd <- signals_ret %>%                    # <- din equity-tibble
  dplyr::group_by(sym1, sym2) %>%                # (kun PEP / KO i dit setup)
  dplyr::mutate(
    run_max   = base::cummax(cum_ret),           # højeste lgn. equity til dato
    dd_log    = cum_ret - run_max,               # ≤ 0 (log-afkast)
    dd_pct    = base::exp(cum_ret) /
                base::exp(run_max) - 1           # drawdown i %
  ) %>%
  dplyr::ungroup()

dd_stats <- signals_dd %>%                       # én række, fordi ét par
  dplyr::summarise(
    max_dd_log = base::min(dd_log),
    max_dd_pct = base::min(dd_pct)
  )


p3 <- ggplot2::ggplot(signals_dd,
        ggplot2::aes(index, dd_pct)) +
  ggplot2::geom_area(fill = "firebrick") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    y     = "Drawdown (%)",
    x     = "Tid",
    title = "Strategi-drawdown (PEP‒KO)"
  ) +
  ggplot2::theme_minimal()







p1 <- ggplot2::ggplot(signals_ret, ggplot2::aes(x = index, y = zscore)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = c(-entry_z, entry_z), linetype = "dashed") +
  ggplot2::geom_hline(yintercept = c(-exit_z,  exit_z),  linetype = "dotted") +
  ggplot2::geom_point(
    data = dplyr::filter(signals_ret, signal ==  1),
    ggplot2::aes(x = index, y = zscore),
    color = "blue", size = 1.5
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(signals_ret, signal == -1),
    ggplot2::aes(x = index, y = zscore),
    color = "red",  size = 1.5
  ) +
  ggplot2::labs(
    y = "Z-score", x = NULL,
    title = "Z-score & signalpunkter"
  ) +
  ggplot2::theme_minimal()


p2 <- ggplot2::ggplot(signals_ret, ggplot2::aes(x = index, y = cum_ret)) +
  ggplot2::geom_line() +
  ggplot2::labs(
    y     = "Cumuleret log-return",
    x     = "Tid",
    title = "Strategi-equity curve"
  ) +
  ggplot2::theme_minimal()

patchwork::wrap_plots(
  p1,                      # z-score & signalpunkter (fra før)
  p2,                      # equity-kurven (fra før)
  p3,                      # nyt draw-down-panel
  ncol    = 1,
  heights = c(2, 1, 1)
)
















































































