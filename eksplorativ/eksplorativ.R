

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
library(mlr3pipelines)
library(mlr3tuning)
library(slider)
library(tidyr)

data.table::setDTthreads(0)


dt <- data %>%
  arrange(index) %>%
  mutate(
    ret_1  = TTR::ROC(AAPL.Close, n = 1, type = "discrete"),
    ret_2  = TTR::ROC(AAPL.Close, n = 2, type = "discrete"),
    sma20  = TTR::SMA(AAPL.Close, n = 20),
    log_ret = log(AAPL.Close/lag(AAPL.Close))
  ) %>%
  mutate(
    roll_sd30 = slide_dbl(log_ret, ~ sd(.x, na.rm = TRUE),
                          .before = 29, .complete = TRUE),
    mom1      = ret_1,                                        # H1
    mr_flag   = if_else(abs(ret_1) > 2*roll_sd30, 1, 0),      # H2
    sma_diff  = (AAPL.Close/sma20) - 1                        # H3
  ) %>%
  drop_na() %>%
  mutate(
    target = as.factor(if_else(lead(ret_1) > 0, 1, 0))        # NÆSTE dags ret = label!
  ) %>%
  drop_na(target)


### mlr3 task


library(mlr3verse)     # mlr3, mlr3tuning, mlr3pipelines, paradox ...
library(mlr3forecast)
library(paradox)

# Lav en kopi du kan altid vende tilbage til
dt_ml <- dt

task <- TaskClassif$new(
  id       = "AAPL_cls",
  backend  = dt_ml,
  target   = "target",
  positive = "1"
)

# Brug index som tids-akse, men IKKE som feature
task$col_roles$order   <- "index"
task$col_roles$feature <- setdiff(task$col_roles$feature, "index")


### pipeline


pl <- po("datefeatures", keep_date_var = FALSE) %>>%
  po("scale")


### learners + tuning


# --- XGBoost + tuning ---------------------------------------------------
lrn_xgb <- lrn("classif.xgboost",
               predict_type = "prob",
               objective    = "binary:logistic",
               eval_metric  = "logloss",
               booster      = "gbtree")

ps_xgb <- ps(
  nrounds            = p_int(100, 600),
  eta                = p_dbl(0.01, 0.4),
  max_depth          = p_int(3, 8),
  subsample          = p_dbl(0.5, 1),
  colsample_bytree   = p_dbl(0.5, 1),
  min_child_weight   = p_int(1, 10),
  gamma              = p_dbl(0, 5)
)

at_xgb <- AutoTuner$new(
  learner      = lrn_xgb,
  resampling   = rsmp("cv", folds = 8),     # inner-CV
  measure      = msr("classif.bacc"),
  tuner        = tnr("grid_search"),        # brug evt. "random_search" hvis du vil brede dig mere
  search_space = ps_xgb,
  terminator   = trm("evals", n_evals = 50) # ~50 grid-punkter
)

lrn_glm <- lrn("classif.log_reg", predict_type = "prob")

glrn_xgb <- GraphLearner$new(pl %>>% at_xgb, id = "xgboost_tuned")
glrn_glm <- GraphLearner$new(pl %>>% lrn_glm, id = "glm_baseline")

learners <- list(glrn_xgb, glrn_glm)


### cv

outer <- rsmp("forecast_cv",
              folds        = 5,
              window_size  = 252 * 2,   # ca 2 år
              horizon      = 20,        # ca 1 måned
              fixed_window = TRUE)
outer$instantiate(task)

### benchmark

design <- benchmark_grid(
  tasks       = task,
  learners    = learners,
  resamplings = outer
)

bmr <- benchmark(design, store_models = TRUE)


### evaluation
agg_bacc <- bmr$aggregate(msr("classif.bacc"))
print(agg_bacc)

autoplot(bmr, type = "boxplot")      # error-rate / bacc diff pr fold
autoplot(bmr, type = "roc")
autoplot(bmr, type = "prc")

### oos-pred

get_oof_probs <- function(bmr_obj, learner_id, positive = "1") {
  btmp <- bmr_obj$clone(deep = TRUE)
  btmp$filter(learner_ids = learner_id)
  # her har vi kun 1 task i designet, så resample_result(1) er sikkert:
  rr   <- btmp$resample_result(1)
  pred <- rr$prediction()
  tibble(
    row_id = pred$row_ids,
    prob_1 = pred$prob[, positive]
  )
}

oof_list <- lapply(learners, function(lrn) {
  get_oof_probs(bmr, lrn$id, positive = task$positive)
})
names(oof_list) <- sapply(learners, `[[`, "id")

library(purrr)
oof_long <- imap_dfr(oof_list, ~ mutate(.x, learner = .y))

### join på data oprindeligt



dt_pnl <- dt_ml %>%
  mutate(row_id = row_number(),
         ret_next = lead(ret_1)) %>%
  select(row_id, index, ret_1, ret_next, everything())

# merge OOS probs
eval_dt <- oof_long %>%
  left_join(dt_pnl, by = "row_id") %>%
  arrange(learner, index)



### strategi

fee    <- 0.0002   # 2 bp
thresh <- 0.51

eval_dt <- eval_dt %>%
  group_by(learner) %>%
  mutate(
    signal   = if_else(prob_1 > thresh, 1L, 0L),      # long/flat
    ret_next = replace_na(ret_next, 0),
    trade_ret= signal * ret_next,
    cost     = fee * abs(signal - lag(signal, default = 0)),
    strat_ret= trade_ret - cost,
    eq_curve = cumsum(replace_na(strat_ret, 0))
  ) %>%
  ungroup()


### performance

library(PerformanceAnalytics)
perf <- eval_dt %>%
  group_by(learner) %>%
  summarise(
    trades     = sum(signal == 1),
    hit_rate   = mean(ret_next > 0 & signal == 1, na.rm = TRUE),
    mean_ret   = mean(strat_ret, na.rm = TRUE),
    sd_ret     = sd(strat_ret,   na.rm = TRUE),
    sharpe_ann = (mean_ret / sd_ret) * sqrt(252),
    max_dd     = {
      pnl_xts <- xts::xts(strat_ret, order.by = index)
      PerformanceAnalytics::maxDrawdown(pnl_xts)[[1]]
    },
    .groups = "drop"
  )
print(perf)

### equity
library(ggplot2)
ggplot(eval_dt, aes(x = index, y = eq_curve, colour = learner)) +
  geom_line() +
  labs(title = "OOS equity curves", x = NULL, y = "Cum. return") +
  theme_minimal()



dt_gap <- dt %>%
  mutate(
    gap_pct = (AAPL.Open / lag(AAPL.Close) - 1),
    trade   = if_else(gap_pct < -0.02, 1, 0),
    profit  = trade * (AAPL.Close / AAPL.Open - 1) - fee*trade
  ) %>%
  na.omit()


































































































