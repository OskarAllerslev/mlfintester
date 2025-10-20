
#' best_box_spread
#'
#' @param op_chain chain
#' @param contract_multiplier 100 som standard
#' @param min_qty 1 std
#' @param today dato
#' @param slippage_pad slippage
#'
#' @return tibble
#' @export
#'
best_box_spread <- function(op_chain,
                            contract_multiplier = 100,
                            min_qty = 1,
                            today = Sys.Date(),
                            slippage_pad = 0) {
  # helper: check at alle nødvendige kolonner er der
  need_cols <- c("Expiry","Strike",
                 "Bid_Put","Ask_Put","Bid_Call","Ask_Call",
                 "BidSize_Put","BidSize_Call","AskSize_Put","AskSize_Call")
  miss <- setdiff(need_cols, names(op_chain))
  if (length(miss)) stop("Mangler kolonner i op_chain: ", paste(miss, collapse=", "))

  dat <- op_chain %>%
    mutate(Expiry = as.Date(Expiry)) %>%
    group_by(Expiry) %>%
    arrange(Strike, .by_group = TRUE) %>%
    # lav alle K1<K2 kombinationer pr. udløb
    summarise(
      combos = list({
        df <- cur_data()
        idx <- which(rep(TRUE, nrow(df)))
        pairs <- which(outer(idx, idx, "<"), arr.ind = TRUE)
        tibble(
          K1 = df$Strike[pairs[,1]],
          K2 = df$Strike[pairs[,2]],
          # priser/sizes for de to strikes:
          BidC1 = df$Bid_Call[pairs[,1]],
          AskC1 = df$Ask_Call[pairs[,1]],
          BidP1 = df$Bid_Put[pairs[,1]],
          AskP1 = df$Ask_Put[pairs[,1]],
          BidC2 = df$Bid_Call[pairs[,2]],
          AskC2 = df$Ask_Call[pairs[,2]],
          BidP2 = df$Bid_Put[pairs[,2]],
          AskP2 = df$Ask_Put[pairs[,2]],
          BidSizeC1 = df$BidSize_Call[pairs[,1]],
          AskSizeC1 = df$AskSize_Call[pairs[,1]],
          BidSizeP1 = df$BidSize_Put[pairs[,1]],
          AskSizeP1 = df$AskSize_Put[pairs[,1]],
          BidSizeC2 = df$BidSize_Call[pairs[,2]],
          AskSizeC2 = df$AskSize_Call[pairs[,2]],
          BidSizeP2 = df$BidSize_Put[pairs[,2]],
          AskSizeP2 = df$AskSize_Put[pairs[,2]]
        )
      }),
      .groups = "drop"
    ) %>%
    unnest(combos) %>%
    mutate(
      width = K2 - K1,
      days = as.numeric(Expiry - as.Date(today)),
      t_years = pmax(days, 1) / 365 # undgå 0 divider
    ) %>%
    # filtrér væk manglende priser
    filter(is.finite(width), width > 0)

  # --- LONG BOX (lån ud: betal nu, modtag width ved udløb) ---
  # Ben: +C(K1) (AskC1), -C(K2) (BidC2), -P(K1) (BidP1), +P(K2) (AskP2)
  # Netto debit (konservativt): buys på ask, sells på bid
  long_box <- dat %>%
    mutate(
      # likviditet: buy -> AskSize >= min_qty, sell -> BidSize >= min_qty
      fillable_long = (AskSizeC1 >= min_qty) & (BidSizeC2 >= min_qty) &
                      (BidSizeP1 >= min_qty) & (AskSizeP2 >= min_qty) &
                      is.finite(AskC1) & is.finite(BidC2) &
                      is.finite(BidP1) & is.finite(AskP2),

      debit_px = (AskC1 + AskP2 - BidC2 - BidP1) + slippage_pad,   # pr. 1 aktie
      payoff = width,                                              # pr. 1 aktie
      profit = payoff - debit_px,                                  # pr. 1 aktie
      profit_cash = profit * contract_multiplier,                  # pr. kontrakt
      roi = profit / debit_px,                                     # enkel afkast
      roi_annual = (payoff / debit_px)^(1 / t_years) - 1,          # annualiseret
      box_type = "LONG"
    )

  # --- SHORT BOX (lån ind: modtag nu, betal width ved udløb) ---
  # Ben: -C(K1) (BidC1), +C(K2) (AskC2), +P(K1) (AskP1), -P(K2) (BidP2)
  # Netto credit: sells på bid, buys på ask
  short_box <- dat %>%
    mutate(
      fillable_short = (BidSizeC1 >= min_qty) & (AskSizeC2 >= min_qty) &
                       (AskSizeP1 >= min_qty) & (BidSizeP2 >= min_qty) &
                       is.finite(BidC1) & is.finite(AskC2) &
                       is.finite(AskP1) & is.finite(BidP2),

      credit_px = (BidC1 + BidP2 - AskC2 - AskP1) - slippage_pad,  # pr. 1 aktie
      payoff = width,                                              # skyld ved udløb
      profit = credit_px - payoff,                                 # pr. 1 aktie
      profit_cash = profit * contract_multiplier,
      # for short box giver det mening at se "lånerente":
      # du modtager credit_px nu og "betaler" width ved udløb
      # Effektiv rente ~ (credit / width) annualiseret, men vi rangerer efter profit
      roi = profit / payoff,
      # Approks. annualisering for "lånerente": width/credit er "tilbagebetaling"
      roi_annual = (credit_px / payoff)^(1 / t_years) - 1,
      box_type = "SHORT"
    )

  # saml og filtrér kun legitime/udfyldelige
  all_boxes <- bind_rows(
    long_box %>% mutate(fillable = fillable_long,
                        entry_cost_px = debit_px,
                        entry_cash = debit_px * contract_multiplier) %>%
      select(Expiry, K1, K2, width, days, t_years, box_type,
             fillable, entry_cost_px, entry_cash, profit, profit_cash, roi, roi_annual),
    short_box %>% mutate(fillable = fillable_short,
                         entry_cost_px = -credit_px,                  # credit (negativ "cost")
                         entry_cash = -credit_px * contract_multiplier) %>%
      select(Expiry, K1, K2, width, days, t_years, box_type,
             fillable, entry_cost_px, entry_cash, profit, profit_cash, roi, roi_annual)
  ) %>%
    filter(fillable, is.finite(profit), is.finite(roi_annual)) %>%
    arrange(desc(profit_cash))

  if (nrow(all_boxes) == 0) {
    return(list(
      best = NULL,
      all = tibble(),
      note = "Ingen fillbare box-spreads med nuværende likviditet/priser."
    ))
  }

  # vælg den med højest annualiseret afkast og positiv profit
  best <- all_boxes %>%
    filter(profit_cash > 0) %>%
    arrange(desc(roi_annual), desc(profit_cash)) %>%
    slice_head(n = 1)

  if (nrow(best) == 0) {
    # hvis ingen med positiv profit, returnér bedste (mindst dårlig) til info
    best <- all_boxes %>% slice_max(profit_cash, n = 1, with_ties = FALSE)
  }

  # konstruér de fire ben som en "ordreplan"
  legs <- function(x) {
    type <- x$box_type
    K1 <- x$K1; K2 <- x$K2
    if (type == "LONG") {
      tibble::tribble(
        ~Action, ~Type, ~Strike, ~PriceSide, ~Reason,
        "BUY",  "CALL", K1, "ASK_Call", "Long box: +C(K1) på ask",
        "SELL", "CALL", K2, "BID_Call", "Long box: -C(K2) på bid",
        "SELL", "PUT",  K1, "BID_Put",  "Long box: -P(K1) på bid",
        "BUY",  "PUT",  K2, "ASK_Put",  "Long box: +P(K2) på ask"
      )
    } else {
      tibble::tribble(
        ~Action, ~Type, ~Strike, ~PriceSide, ~Reason,
        "SELL", "CALL", K1, "BID_Call", "Short box: -C(K1) på bid",
        "BUY",  "CALL", K2, "ASK_Call", "Short box: +C(K2) på ask",
        "BUY",  "PUT",  K1, "ASK_Put",  "Short box: +P(K1) på ask",
        "SELL", "PUT",  K2, "BID_Put",  "Short box: -P(K2) på bid"
      )
    }
  }

  list(
    best = best,
    legs = legs(best),
    all = all_boxes %>% arrange(desc(roi_annual), desc(profit_cash)),
    note = "Priser antager konservativ udførsel (buy=ask, sell=bid). Justér `slippage_pad`, `min_qty` efter behov."
  )
}
