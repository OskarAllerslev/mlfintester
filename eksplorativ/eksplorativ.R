

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
  RF = IBrokers::twsEquity("BIL"),
  SP500 = IBrokers::twsEquity("SPY")
)



dat <- mlfintester::historical_data_multi(conn,
                                   equity_list,
                                   barSize = "1 day",
                                   duration = "5 Y")
vix3 <- mlfintester::historical_data(conn = conn,
                                    equity =
                                      vix_contract <- IBrokers::twsIndex(
                                        symbol   = "VIX3M",
                                        exch     = "CBOE",
                                        currency = "USD",),
                                        barSize = "1 day",
                                        duration = "5 Y")

vix <- mlfintester::historical_data(conn = conn,
                                    equity =
                                      vix_contract <- IBrokers::twsIndex(
                                        symbol   = "VIX",
                                        exch     = "CBOE",
                                        currency = "USD",),
                                        barSize = "1 day",
                                        duration = "5 Y")
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
  ) %>%
  tidyr::replace_na(list(log_ret = 0, rv20 = 0))


# lib
library(ggplot2)
library(dplyr)


# hidden markov model
f <- ~ vix_slope + rv20
hid1 <- hmmTMB::MarkovChain$new(data = spdat,
                                n_states = 3,
                                formula = f)

dists <- list(log_ret = "norm")

par0_2s <- list(log_ret = list(mean = c(0,0.001,-0.2),
                               sd = c(0.3,0.6,0.9)))

obs1 <- hmmTMB::Observation$new(data = spdat,
                                dists = dists,
                                par = par0_2s)

hmm1 <- hmmTMB::HMM$new(obs = obs1, hid = hid1)
# model fitting

hmm1$fit(silent = TRUE)


data_plot <- spdat

data_plot$state <- base::factor(base::paste0("State: ", hmm1$viterbi()))
ggplot2::ggplot(data_plot, ggplot2::aes(index, Close, col = state)) +
  ggplot2::geom_point()

data_plot$pr_s2 <- hmm1$state_probs()[, 2]
ggplot2::ggplot(data_plot, ggplot2::aes(index, Close, col = pr_s2)) +
  ggplot2::geom_point()

hmm1$plot_dist("log_ret")


pr <- hmm1$pseudores()

qqnorm(pr$log_ret)
abline(0,1)
acf(na.omit(pr$log_ret))



hmm1$predict(what = "tpm",
             t = 1258,
             n_post =0 )
























