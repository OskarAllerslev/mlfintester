
conn <- mlfintester::conn(1,
                  4002,
                  verbose = FALSE)
dat <- mlfintester::historical_data_multi(conn,
                                   equity_list,
                                   barSize = "1 day",
                                   duration = "10 Y")
vix3 <- mlfintester::historical_data(conn = conn,
                                    equity =
                                      vix_contract <- IBrokers::twsIndex(
                                        symbol   = "VIX3M",
                                        exch     = "CBOE",
                                        currency = "USD",),
                                        barSize = "1 day",
                                        duration = "10 Y")

vix <- mlfintester::historical_data(conn = conn,
                                    equity =
                                      vix_contract <- IBrokers::twsIndex(
                                        symbol   = "VIX",
                                        exch     = "CBOE",
                                        currency = "USD",),
                                        barSize = "1 day",
                                        duration = "10 Y")
library(dplyr)
V <- vix %>%
  dplyr::rename_with(~ gsub("VIX.", "", .x))
V3 <- vix3 %>%
  dplyr::rename_with(~ gsub("VIX3M.", "", .x))

spdat <- dat$SP500 %>%
  dplyr::rename_with(~ base::gsub("^SPY\\.", "", .x)) %>%
  dplyr::select(index, Close) %>%
  dplyr::left_join(V  %>% dplyr::transmute(index, VIX  = Close),  by = "index") %>%
  dplyr::left_join(V3 %>% dplyr::transmute(index, VIX3M = Close), by = "index") %>%
  dplyr::arrange(index) %>%
  dplyr::mutate(
    log_ret   = base::log(Close / dplyr::lag(Close)),
    rv20      = base::sqrt(252) * zoo::rollapply(log_ret, width = 20, FUN = sd, fill = NA, align = "right"),
    vix_slope = VIX3M / VIX
  )

split_idx <- base::floor(base::nrow(spdat) * 0.5)

init_dat <- spdat %>%
  dplyr::slice(1:split_idx)

HMM <- mlfintester::HMM_1(data = init_dat)


HMM$predict(what = "tpm",
             t = c(split_idx, split_idx +1 ),
             n_post =0 )

fb <- HMM$forward_backward()
log_alpha <- t(fb$logforward)
log_alpha[!base::is.finite(log_alpha)] <- -1e300

row_max <- matrixStats::rowMaxs(log_alpha)
tmp     <- base::exp(base::sweep(log_alpha, 1, row_max, "-"))
alpha   <- tmp / base::rowSums(tmp)

init_dat <- init_dat %>%
  dplyr::mutate(
    s1 = alpha[, 1],
    s2 = alpha[, 2],
    s3 = alpha[, 3]
  )
# %>%
#   dplyr::mutate(
#     signal_today = dplyr::if_else(s1 > 0.9 | s2 > 0.9 , 1, 0),
#     signal_act = dplyr::lag(signal_today),
#     strat_ret =  signal_act * log_ret
#   ) %>%
#   tidyr::replace_na(list(strat_ret = 0, log_ret = 0)) %>%
#   dplyr::mutate(
#     cum_ret = cumsum(strat_ret),
#     cum_bh = cumsum(log_ret)
#   )















