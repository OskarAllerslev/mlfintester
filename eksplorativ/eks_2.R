library(rvest)
library(tidyquant)


# we create a connection
IBrokers::twsDisconnect(conn)



conn <- mlfintester::conn(1,
                  4002,
                  verbose = FALSE)


IBrokers::reqManagedAccts(conn)
account_info <- IBrokers::reqAccountUpdates(conn)






index <- tidyquant::tq_index("SP600")
dat <- tidyquant::tq_get(index, from = "2015-08-05", to = "2025-08-05")

data.table::fwrite(dat, "sp500_M.csv")


data <- utils::read.csv(file = "sp500_M.csv")
data <- tibble::as.tibble(data) %>%
  dplyr::mutate(
    date = as.Date(date)
  ) %>%
  dplyr::arrange(symbol, date)



data_ret <- data %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    type       = "arithmetic",
    col_rename = "ret"
  ) %>%
  dplyr::ungroup()




x <- 100
hold_months <- 6
lookback <- 12
tc <- 0

data_m <- data_ret %>%
  dplyr::arrange(symbol, date) %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(ret = monthly.returns) %>%
  dplyr::ungroup() %>%
  dplyr::select(symbol, date, ret)



mom6_tbl <- data_m %>%
  dplyr::group_by(symbol) %>%
  dplyr::mutate(mom6 = slider::slide_dbl(ret, ~prod(1 + .x) - 1, .before = lookback - 1, .complete = TRUE)) %>%
  dplyr::ungroup()

all_months <- mom6_tbl %>%
  dplyr::distinct(date) %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(idx = dplyr::row_number())


rebal_dates <- all_months %>%
  dplyr::filter((idx - first(idx)) %% hold_months == 0) %>%
  dplyr::pull(date)

port_members <- mom6_tbl %>%
  dplyr::filter(date %in% rebal_dates) %>%
  dplyr::group_by(date) %>%
  dplyr::filter(!is.na(mom6)) %>%
  dplyr::mutate(rnk = base::rank(mom6, ties.method = "first")) %>%
  dplyr::arrange(date, dplyr::desc(mom6)) %>%
  dplyr::mutate(side = dplyr::case_when(
    dplyr::row_number() <= x ~ "LONG",
    dplyr::row_number() > dplyr::n() - x ~ "SHORT",
    TRUE ~ NA_character_
  )) %>%
  dplyr::filter(!is.na(side)) %>%
  # vægte equal-weight pr. side, re-vægtes automatisk på tværs af tilgængelige aktier
  dplyr::group_by(date, side) %>%
  dplyr::mutate(w = 1 / dplyr::n()) %>%
  dplyr::ungroup()



hold_map <- tibble::tibble(rebald = rebal_dates) %>%
  dplyr::mutate(hold_seq = purrr::map(rebald, ~{
    idx0 <- all_months$idx[all_months$date == .x]
    idx_hold <- (idx0 + 1):(idx0 + hold_months)
    all_months$date[pmax(1, pmin(idx_hold, base::max(all_months$idx)))]
  })) %>%
  tidyr::unnest(hold_seq) %>%
  dplyr::rename(rebal_date = rebald, hold_date = hold_seq)


positions <- port_members %>%
  dplyr::rename(rebal_date = date) %>%
  dplyr::inner_join(hold_map, by = "rebal_date") %>%
  # vægt pr. side beholdes; vi justerer senere hvis nogle mangler retur i en måned
  dplyr::select(rebal_date, hold_date, symbol, side, w)

port_monthly <- positions %>%
  dplyr::left_join(data_m, by = c("symbol" = "symbol", "hold_date" = "date")) %>%
  dplyr::group_by(rebal_date, hold_date, side) %>%
  # re-weight blandt dem der faktisk har ret (ikke-NA)
  dplyr::mutate(w_eff = ifelse(!is.na(ret), w / sum(w[!is.na(ret)]), 0)) %>%
  dplyr::summarise(side_ret = sum(w_eff * ret, na.rm = TRUE), .groups = "drop") %>%
  # pivot til LONG og SHORT kolonner
  tidyr::pivot_wider(names_from = side, values_from = side_ret, values_fill = 0) %>%
  # transaktionsomkostning lægges (kun ved første hold-måned efter rebal)
  dplyr::group_by(rebal_date) %>%
  dplyr::mutate(step  = dplyr::row_number(),
                LONG  = LONG  - ifelse(step == 1, tc, 0),
                SHORT = SHORT + ifelse(step == 1, tc, 0)) %>%  # sæt tc = 0 i start
  dplyr::ungroup() %>%
  dplyr::mutate(ls_ret = LONG - SHORT)


performance <- port_monthly %>%
  dplyr::arrange(hold_date) %>%
  dplyr::mutate(
    ls_equity    = base::cumprod(1 + ls_ret),
    long_equity  = base::cumprod(1 + LONG),
    short_equity = base::cumprod(1 + (-SHORT))  # short-benet vendt så "op" = tjener på fald
  )

summ <- performance %>%
  dplyr::summarise(
    start            = base::min(hold_date),
    end              = base::max(hold_date),
    n_months         = dplyr::n(),
    CAGR_ls          = (dplyr::last(ls_equity))^(12 / n_months) - 1,
    mean_m           = base::mean(ls_ret, na.rm = TRUE),
    sd_m             = stats::sd(ls_ret, na.rm = TRUE),
    Sharpe_m_0rf     = mean_m / sd_m,
    Sharpe_ann_0rf   = base::sqrt(12) * Sharpe_m_0rf,
    mdd = {
      dd <- performance$ls_equity / base::cummax(performance$ls_equity) - 1
      base::min(dd, na.rm = TRUE)
    }
  )


plt_df <- performance %>%
  dplyr::select(hold_date, ls_equity, long_equity, short_equity) %>%
  tidyr::pivot_longer(
    cols = c(ls_equity, long_equity, short_equity),
    names_to = "series",
    values_to = "equity"
  ) %>%
  tidyr::drop_na(equity)

ggplot2::ggplot(plt_df, ggplot2::aes(x = hold_date, y = equity, color = series)) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::labs(
    title = "Portefølje Equity (Long, Short og Long–Short)",
    x = "Dato", y = "Kumuleret værdi (start=1)",
    color = "Serie"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")



