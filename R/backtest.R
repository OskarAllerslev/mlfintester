
#' Backtest_coint
#'
#' backtester
#'
#' @param data data
#' @param window_in window_in
#' @param lag_max lag_max
#' @param z_in z_in
#' @param z_out z_out
#' @param lookback_z lookback
#' @param const_bps cost
#' @param notional notioanl
#'
#' @return tibble
#' @export
#'
backtest_coint <- function(
    data,
    window_in = 250,
    lag_max = 5,
    z_in = 2,
    z_out = 0.5,
    lookback_z = 60,
    const_bps = 5,
    notional = 1000
){

  logret_tbl <- tibble::as_tibble(data) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~log(.) - log(dplyr::lag(.)), .names = "logret_{.col}")
    ) %>%
    dplyr::select( dplyr::starts_with("logret_")) %>%
    tidyr::drop_na()

  lp_mat <- tibble::as_tibble(data) %>%
    dplyr::select(where(is.numeric)) %>%
    dplyr::mutate(dplyr::across(everything(), ~ log(as.double(.)))) %>%
    as.matrix()

  nc <- ncol(logret_tbl)
  n <- nrow(logret_tbl)

  in_trade <- F

  ret_mat <- as.matrix(logret_tbl)
  m <- ncol(ret_mat)

  z_vec  <- rep(NA_real_, n)
  pnl    <- numeric(n)
  equity <- numeric(n)
  w_prev <- rep(0, m)
  side   <- 0L

  make_weights <- function(v, side){
    w_raw <- side * as.numeric(v)
    pos <- pmax(w_raw, 0); neg <- pmax(-w_raw, 0)
    if (sum(pos) == 0 || sum(neg) == 0) return(rep(0, length(w_raw)))
    0.5 * pos / sum(pos) - 0.5 * neg / sum(neg)   # dollar-neutral, L1=1
  }


  # koden under skal du hjælpe med
  for (t in seq.int(window_in + 1L, n - 1L)) {

    idx_start <- t - window_in + 1L
    # vælg data udsnit
    win_tbl <- logret_tbl %>%
      dplyr::slice(idx_start:t)


    # find lag
    k <- as.integer(
      vars::VARselect(
        win_tbl,
        lag.max = lag_max,
        type = "const"
      )$selection["AIC(n)"]
    )
    k <- max(2L, k)
    win_lp <- lp_mat[idx_start:t, , drop = F]

    # johansen test
    jo <- urca::ca.jo(
      win_lp,
      K = k,
      type = "trace",
      ecdet = "const",
      spec = "transitory"
    )
    r <- sum(jo@teststat > jo@cval[, "10pct"])

    if (r == 0) {
      next
    }

    # hent hedge ratio
    v <- jo@V[, 1]
    v <-  v[names(v) != "constant"]
    anchor_idx <- 1L
    if (!base::isTRUE(all.equal(v[anchor_idx], 1))) v <- v / v[anchor_idx]

    # dynamisk spread for window
    if (t - lookback_z + 1 < 1) next
    row_from <- t - lookback_z + 1L
    row_to <- t

    s_hist <- as.numeric(lp_mat[row_from:row_to, ] %*% v)
    mu <- mean(s_hist, na.rm = T)
    sd_ <- stats::sd(s_hist, na.rm = T)
    if (!is.finite(sd_)) next

    s_t <- sum(lp_mat[t, ] * v)
    z_t <- (s_t - mu) / sd_
    z_vec[t] <- z_t

    if (!in_trade && is.finite(z_t) && abs(z_t) > z_in) {
      side   <- if (z_t < 0) +1L else -1L
      w_tgt  <- make_weights(v, side)
      in_trade <- TRUE
    } else if (in_trade && is.finite(z_t) && abs(z_t) < z_out) {
      w_tgt  <- rep(0, m)
      in_trade <- FALSE
      side <- 0L
    } else {
      w_tgt <- w_prev
    }

    turn <- sum(abs(w_tgt - w_prev))
    cost <- (const_bps / 1e4) * notional * turn

    r_next <- ret_mat[t + 1L, ]
    pnl[t + 1L] <- notional * sum(w_tgt * r_next) - cost
    w_prev <- w_tgt

  }

  equity <- cumsum(replace(pnl, is.na(pnl), 0))
  tibble::tibble(
    t = seq_len(n),
    z = z_vec,
    pnl = pnl,
    equity = equity
  )
}
