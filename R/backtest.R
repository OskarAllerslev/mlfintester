
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
  window_in = 250, lag_max = 5,
  z_in = 2, z_out = 0.5, lookback_z = 60,
  notional = 100000,
  fee_flat = 0,                    # fast fee pr. handel (entry/exit), uafhængig af notional/ben
  rank_level = c("10pct","5pct","1pct"),
  min_abs_weight = 0
){
  rank_level <- match.arg(rank_level)

  X <- tibble::as_tibble(data) |> dplyr::select(where(is.numeric))
  stopifnot(ncol(X) >= 2L)
  LP <- as.matrix(dplyr::mutate(X, dplyr::across(everything(), ~log(as.double(.)))))
  N  <- nrow(LP); m <- ncol(LP)
  RET <- LP[2:N, , drop=FALSE] - LP[1:(N-1), , drop=FALSE]
  n_  <- nrow(RET)

  make_weights <- function(beta, side){
    w_raw <- side * as.numeric(beta)
    pos <- pmax(w_raw, 0); neg <- pmax(-w_raw, 0)
    if (sum(pos)==0 || sum(neg)==0) return(rep(0, length(w_raw)))
    w <- 0.5 * pos / sum(pos) - 0.5 * neg / sum(neg)
    w[abs(w) < min_abs_weight] <- 0
    w
  }

  # output
  z_vec   <- rep(NA_real_, n_)
  pnl_g   <- numeric(n_)
  coint   <- integer(n_); coint[] <- 0L
  entry   <- integer(n_)
  exit    <- integer(n_)
  fee_cum <- numeric(n_)
  K_used  <- integer(n_)
  rank_u  <- integer(n_)

  # state
  w_prev   <- rep(0, m)
  in_trade <- FALSE
  fee_acc  <- 0

  for (t in seq.int(window_in, n_-1L)) {
    i0 <- t - window_in + 1L
    W  <- LP[(i0 + 1L):(t + 1L), , drop=FALSE]

    k <- tryCatch({
      sel <- vars::VARselect(as.data.frame(W), lag.max = lag_max, type="const")$selection
      kval <- suppressWarnings(as.integer(sel["AIC(n)"]))
      if (is.na(kval)) 2L else max(2L, kval)
    }, error=function(e) 2L)
    K_used[t] <- k

    jo <- tryCatch(urca::ca.jo(W, K=k, type="trace", ecdet="const", spec="transitory"), error=function(e) NULL)
    if (is.null(jo)) { pnl_g[t+1L] <- notional * sum(w_prev * RET[t+1L,]); fee_cum[t+1L] <- fee_acc; next }

    rnk <- sum(jo@teststat > jo@cval[, rank_level])
    rank_u[t] <- rnk
    coint[t]  <- as.integer(rnk > 0L)
    if (rnk == 0L) {
      # ingen ny handel, men bær eksisterende position én bar og luk på signal hvis du vil
      pnl_g[t+1L] <- notional * sum(w_prev * RET[t+1L,])
      fee_cum[t+1L] <- fee_acc
      next
    }

    beta <- jo@V[,1]
    if (!is.null(names(beta)) && any(names(beta)=="constant")) beta <- beta[names(beta)!="constant"]
    if (!isTRUE(all.equal(beta[1], 1))) beta <- beta / beta[1]

    if ((t - lookback_z + 1L) < 1L) { pnl_g[t+1L] <- notional * sum(w_prev * RET[t+1L,]); fee_cum[t+1L] <- fee_acc; next }
    s_hist <- as.numeric(LP[(t - lookback_z + 2L):(t + 1L), , drop=FALSE] %*% beta)
    sd_ <- stats::sd(s_hist); if (!is.finite(sd_) || sd_==0) { pnl_g[t+1L] <- notional * sum(w_prev * RET[t+1L,]); fee_cum[t+1L] <- fee_acc; next }
    mu_ <- mean(s_hist)

    s_t <- sum(LP[t + 1L, ] * beta)
    z_t <- (s_t - mu_)/sd_
    z_vec[t] <- z_t

    # beslut vægte for NÆSTE bar
    open_sig  <- (!in_trade) && is.finite(z_t) && abs(z_t) > z_in
    close_sig <- in_trade && is.finite(z_t) && abs(z_t) < z_out

    w_next <- w_prev
    if (open_sig) {
      side <- if (z_t < 0) +1L else -1L
      w_next <- make_weights(beta, side)
      fee_acc <- fee_acc + fee_flat
      entry[t] <- 1L
      in_trade <- TRUE
    } else if (close_sig) {
      w_next <- rep(0, m)
      fee_acc <- fee_acc + fee_flat
      exit[t] <- 1L
      in_trade <- FALSE
    }

    # PnL for interval (t -> t+1) med AKTUEL position w_prev
    pnl_g[t + 1L] <- notional * sum(w_prev * RET[t + 1L, ])

    # opdater til næste bar
    w_prev <- w_next
    fee_cum[t + 1L] <- fee_acc
  }

  tibble::tibble(
    row_in_data = 2:N,
    coint = coint,
    rank  = rank_u,
    K     = K_used,
    z     = z_vec,
    entry = entry,
    exit  = exit,
    fee_cum = fee_cum,           # akkumuleret fast fee (separat)
    pnl    = pnl_g,              # gross PnL pr. bar
    equity = cumsum(replace(pnl_g, !is.finite(pnl_g), 0))  # gross equity uden fees
  )
}










#' backtest coint v2
#'
#' bare rettelser
#'
#' @param data data
#' @param window_in a
#' @param lag_max b
#' @param z_in c
#' @param z_out d
#' @param lookback_z e
#' @param const_bps f
#' @param notional g
#'
#' @return data
#' @export
#'
backtest_coint_v2 <- function(
    data,
    window_in  = 250,
    lag_max    = 5,
    z_in       = 2,
    z_out      = 0.5,
    lookback_z = 60,
    const_bps  = 0,
    notional   = 100
){
  stopifnot(lookback_z <= window_in)

  price_tbl <- tibble::as_tibble(data) |>
    dplyr::select(where(is.numeric))

  # log-afkast (m rækker), dropper 1. række
  logret_tbl <- price_tbl |>
    dplyr::mutate(dplyr::across(
      everything(), ~ log(.) - log(dplyr::lag(.)), .names = "logret_{.col}"
    )) |>
    dplyr::select(dplyr::starts_with("logret_")) |>
    tidyr::drop_na()

  # log-priser alignet til logret_tbl (starter ved 2. observation)
  lp_mat <- price_tbl |>
    dplyr::slice(-1) |>
    dplyr::mutate(dplyr::across(everything(), ~ log(as.double(.)))) |>
    as.matrix()

  n <- nrow(logret_tbl)
  m <- ncol(logret_tbl)

  ret_mat <- as.matrix(logret_tbl)
  z_vec   <- rep(NA_real_, n)
  pnl     <- numeric(n)
  w_prev  <- rep(0, m)
  in_trade <- FALSE
  side     <- 0L
  z_prev   <- NA_real_

  make_weights <- function(v, side){
    v <- side * as.numeric(v)
    pos <- pmax(v, 0); neg <- pmax(-v, 0)
    if (sum(pos) == 0 || sum(neg) == 0) return(rep(0, length(v)))
    0.5 * pos / sum(pos) - 0.5 * neg / sum(neg)  # dollar-neutral L1=1
  }

  for (t in seq.int(window_in + 1L, n - 1L)) {
    idx_start <- t - window_in + 1L
    win_tbl <- logret_tbl |> dplyr::slice(idx_start:t)
    k <- as.integer(vars::VARselect(win_tbl, lag.max = lag_max, type = "const")$selection["AIC(n)"])
    k <- max(2L, k)

    win_lp <- lp_mat[idx_start:t, , drop = FALSE]
    jo <- urca::ca.jo(win_lp, K = k, type = "trace", ecdet = "const", spec = "transitory")
    r  <- sum(jo@teststat > jo@cval[, "10pct"])

    # hvis cointegration forsvinder: luk positionen pænt
    if (r == 0) {
      if (in_trade) {
        turn <- sum(abs(0 - w_prev))
        pnl[t + 1L] <- pnl[t + 1L] - (const_bps/1e4) * notional * turn
        w_prev  <- rep(0, m)
        in_trade <- FALSE
        side     <- 0L
      }
      z_prev <- NA_real_
      next
    }

    # cointegrationsvektor via cajorls (inkl. intercept -> fjernes)
    beta_mat <- try(urca::cajorls(jo, r = 1)$beta, silent = TRUE)
    if (inherits(beta_mat, "try-error")) next
    v <- as.numeric(beta_mat[rownames(beta_mat) != "constant", 1, drop = TRUE])

    # robust forankring
    anchor_idx <- which.max(abs(v))
    if (!is.finite(v[anchor_idx]) || abs(v[anchor_idx]) < 1e-8) next
    v <- v / v[anchor_idx]

    if (t - lookback_z + 1L < 1L) next
    row_from <- t - lookback_z + 1L; row_to <- t

    s_hist <- as.numeric(lp_mat[row_from:row_to, , drop = FALSE] %*% v)
    mu  <- mean(s_hist); sd_ <- stats::sd(s_hist)
    if (!is.finite(sd_) || sd_ == 0) next

    s_t <- sum(lp_mat[t, ] * v)
    z_t <- (s_t - mu) / sd_
    z_vec[t] <- z_t

    enter_now <- !in_trade && is.finite(z_t) &&
      (!is.finite(z_prev) || (abs(z_prev) <= z_in && abs(z_t) > z_in))
    exit_now  <-  in_trade && is.finite(z_t) && abs(z_t) < z_out

    if (enter_now) {
      side  <- if (z_t < 0) +1L else -1L
      w_tgt <- make_weights(v, side)
    } else if (exit_now) {
      w_tgt <- rep(0, m)
      side  <- 0L
    } else {
      w_tgt <- w_prev
    }

    turn <- sum(abs(w_tgt - w_prev))
    cost <- if (turn > 0) (const_bps/1e4) * notional * turn else 0

    r_next <- ret_mat[t + 1L, ]
    pnl[t + 1L] <- pnl[t + 1L] + notional * sum(w_tgt * r_next) - cost

    in_trade <- any(w_tgt != 0)
    w_prev   <- w_tgt
    z_prev   <- z_t
  }

  equity <- cumsum(replace(pnl, is.na(pnl), 0))
  tibble::tibble(t = seq_len(n), z = z_vec, pnl = pnl, equity = equity)
}
