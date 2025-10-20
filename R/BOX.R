#' Long BOX (europæisk, cash-settled indexoptions) – tibble kalkulator
#'
#' Returnerer én tibble-række med nøgletal inkl. profit efter fees.
#'
#' @param K1,K2 Strikes (K2 > K1)
#' @param C1_bid,C1_ask,C2_bid,C2_ask,P1_bid,P1_ask,P2_bid,P2_ask Bid/Ask for ben:
#'   C1 = Call(K1), C2 = Call(K2), P1 = Put(K1), P2 = Put(K2)
#' @param expiry Udløbsdato "YYYY-MM-DD" (samme for alle ben)
#' @param today Dato for beregning (default: Sys.Date())
#' @param r_annual Risk-free årlig rente til PV-check (fx 0.04)
#' @param fees Samlede fees pr. box (alle 4 ben, i samme valuta)
#' @param multiplier Kontrakt-multiplikator (XSP/SPX=100, OESX=10)
#' @param tick Mindste pris-tick i point (typisk 0.01)
#' @param limit Din NET limit-debet (valgfri; NA hvis ingen)
#' @param target_apy Ønsket årlig afkast-rate (fx 0.08), NA hvis ingen
#' @param digits Afrunding af tal i output (default 4)
#'
#' @return tibble::tibble med nøgletal (én række)
box_calc <- function(
  K1, K2,
  C1_bid, C1_ask,
  C2_bid, C2_ask,
  P1_bid, P1_ask,
  P2_bid, P2_ask,
  expiry,
  today = Sys.Date(),
  r_annual = 0.04,
  fees = 0,
  multiplier = 100,
  tick = 0.01,
  limit = NA_real_,
  target_apy = NA_real_,
  digits = 4
){
  stopifnot(K2 > K1)
  mid  <- function(b,a) (b + a)/2

  # Tidsparametre
  T_days  <- as.numeric(as.Date(expiry) - as.Date(today))
  T_years <- T_days / 365
  if (T_years < 0) warning("Expiry er i fortiden ift. 'today'.")

  dK <- K2 - K1

  # Mid/worst/best net-debit for LONG BOX: D = (C1 - C2) + (P2 - P1)
  C1_m <- mid(C1_bid, C1_ask); C2_m <- mid(C2_bid, C2_ask)
  P1_m <- mid(P1_bid, P1_ask); P2_m <- mid(P2_bid, P2_ask)

  D_mid   <- (C1_m - C2_m) + (P2_m - P1_m)
  D_worst <- (C1_ask - C2_bid) + (P2_ask - P1_bid)   # køber betaler asks, sælger får bids
  D_best  <- (C1_bid - C2_ask) + (P2_bid - P1_ask)   # teoretisk bedst

  # Teoretisk PV af payoff
  PV <- dK * exp(-r_annual * max(T_years, 0))

  # Break-even efter fees og (valgfri) target-apy
  D_be   <- dK - fees / multiplier   # max debit for 0 kr netto
  D_tgt  <- if (!is.na(target_apy) && T_years > 0) {
              dK/(1 + target_apy*T_years) - fees/multiplier
            } else NA_real_

  # Foreslået limit: må ikke overstige nogen "cap" (PV, break-even, target) eller mid
  cap_base <- min(
    PV,
    D_be,
    if (is.na(D_tgt)) Inf else D_tgt
  )
  suggest_limit <- min(cap_base, D_mid) - tick
  suggest_limit <- max(0, suggest_limit) # ikke negativ

  payoff_cash <- dK * multiplier

  profit_fun <- function(D) if (is.na(D)) NA_real_ else (dK - D)*multiplier - fees
  cost_fun   <- function(D) if (is.na(D)) NA_real_ else (D*multiplier + fees)
  apy_fun    <- function(D) {
    if (is.na(D) || T_years <= 0) return(NA_real_)
    gain <- profit_fun(D); cost <- cost_fun(D)
    if (is.na(gain) || is.na(cost) || cost <= 0) return(NA_real_)
    (gain / cost) / T_years
  }

  # Ved bruger-limit (hvis angivet)
  profit_limit <- profit_fun(limit)
  cost_limit   <- cost_fun(limit)
  apy_limit    <- apy_fun(limit)

  # Ved foreslået limit
  profit_sug <- profit_fun(suggest_limit)
  cost_sug   <- cost_fun(suggest_limit)
  apy_sug    <- apy_fun(suggest_limit)

  # Nogle nyttige “edges” (i point og ticks) ift. mid
  edge_vs_be_point   <- D_be  - D_mid
  edge_vs_pv_point   <- PV    - D_mid
  edge_vs_tgt_point  <- if (is.na(D_tgt)) NA_real_ else (D_tgt - D_mid)
  ticks_to_fill_from_limit <- if (is.na(limit)) NA_real_ else (D_mid - limit) / tick
  ticks_to_fill_from_sug   <- (D_mid - suggest_limit) / tick

  out <- tibble::tibble(
    width_dK          = round(dK, digits),
    days_to_expiry    = T_days,
    time_years        = round(T_years, digits),
    pv_deltaK         = round(PV, digits),

    debit_mid         = round(D_mid, digits),
    debit_worst       = round(D_worst, digits),
    debit_best        = round(D_best, digits),

    max_debit_break_even = round(D_be, digits),
    max_debit_target_apy = round(D_tgt, digits),

    edge_vs_break_even  = round(edge_vs_be_point, digits),
    edge_vs_pv          = round(edge_vs_pv_point, digits),
    edge_vs_target      = round(edge_vs_tgt_point, digits),

    suggest_limit       = round(suggest_limit, digits),
    ticks_below_mid_suggest = round(ticks_to_fill_from_sug, 2),

    limit_input         = if (is.na(limit)) NA_real_ else round(limit, digits),
    ticks_below_mid_input = if (is.na(limit)) NA_real_ else round(ticks_to_fill_from_limit, 2),

    payoff_cash         = round(payoff_cash, 2),

    # Profit efter fees, cost og APY ved hhv. input-limit og suggested limit
    cost_at_input_limit     = if (is.na(cost_limit)) NA_real_ else round(cost_limit, 2),
    profit_net_at_input     = if (is.na(profit_limit)) NA_real_ else round(profit_limit, 2),
    apy_at_input            = if (is.na(apy_limit)) NA_real_ else round(apy_limit, 4),

    cost_at_suggest_limit   = round(cost_sug, 2),
    profit_net_at_suggest   = round(profit_sug, 2),
    apy_at_suggest          = round(apy_sug, 4)
  )

  out
}
