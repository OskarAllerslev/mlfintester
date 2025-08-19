library(dplyr)

raw_text <- base::paste(clipr::read_clip(),
                        collapse = "\n")

m <- stringr::str_match_all(raw_text, "\\b([A-Z0-9]{2,}):([a-z]{3,5})\\b")[[1]]
colnames(m) <- c("full", "ticker", "exch")

matches <- tibble::as_tibble(m) |>
  dplyr::transmute(ticker, exch) |>
  dplyr::distinct()

suffix_map <- c(
  xlon = ".L",
  xetr = ".DE",
  xams = ".AS",
  xmil = ".MI",
  xswx = ".SW",
  xpar = ".PA"
)
symbols <- matches %>%
  dplyr::mutate(yahoo_suffix = suffix_map[exch],
                yahoo = ifelse(!is.na(yahoo_suffix),
                               base::paste0(ticker, yahoo_suffix),
                               NA_character_))
unknown_exchanges <- symbols %>%
  dplyr::filter(is.na(yahoo_suffix))
if (nrow(unknown_exchanges) > 0) {
  messages("Ukendte exchanges (tjek manuelt):")
  print(unknown_exchanges)
}

symbols_ok <- symbols %>%
  dplyr::filter(!is.na(yahoo)) %>%
  dplyr::distinct(yahoo, .keep_all = T)


from_date <- base::as.Date("2023-01-01")

pull_one <- function(sym)
{
  base::tryCatch(
    tidyquant::tq_get(sym, from = from_date),
    error = function(e) {
      message("Fejl for: ", sym, " -> ", conditionMessage(e))
      tibble::tibble()
    }
  )
}

prices_list <- purrr::map(purrr::set_names(symbols_ok$yahoo), pull_one)

prices_list1 <- purrr::keep(prices_list, ~inherits(.x, "data.frame") && nrow(.x) > 0)


prices <- dplyr::bind_rows(prices_list1)

readr::write_csv(prices, "etf_prices_all.csv")

prices_adj <- prices %>% dplyr::select(symbol,
                                       date,
                                       adjusted) %>%
  dplyr::arrange(symbol, date)
readr::write_csv(prices_adj, "etf_prices_adjusted.csv")

rets_m <- prices %>%
  dplyr::group_by(symbol) %>%
  tidyquant::tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "monthly",
    type = "log",
    col_rename = "ret"
  )

rets_m_wide <- tidyr::pivot_wider(
  rets_m,
  names_from = symbol,
  values_from = ret
) %>%
  dplyr::arrange(date)


sym_cols <- setdiff(names(rets_m_wide), "date")

rets_m_wide <- rets_m_wide %>%
  dplyr::select(date, dplyr::all_of(sort(sym_cols))) %>%
  dplyr::select(where(~ !all(is.na(.x))))

rets_dups <- rets_m %>%
  dplyr::count(symbol, date) %>%
  dplyr::filter(n > 1)


# matrix til EF port

R <- rets_m_wide %>%
  dplyr::select(-date) %>%
  as.matrix()




if (sum(colSums(!is.na(R)) >= 36) == 0) {
  message("Ingen kolonner med >=36 mdr. data. Sænker kravet til >=24.")
  keep <- colSums(!is.na(R)) >= 24
  R <- R[, keep, drop = FALSE]
} else {
  keep <- colSums(!is.na(R)) >= 36
  R <- R[, keep, drop = FALSE]
}
stopifnot(ncol(R) > 0)


library(Matrix)
mu_m  <- colMeans(R, na.rm = TRUE)                              # månedlig (log) mean
Sigma <- stats::cov(R, use = "pairwise.complete.obs")           # månedlig kovarians
Sigma_psd <- as.matrix(Matrix::nearPD(Sigma, corr = FALSE)$mat) # PSD-projektion
Sigma_pd  <- Sigma_psd + diag(1e-8, ncol(Sigma_psd))            # lille ridge til QP


mu_ann <- exp(12 * mu_m) - 1
Sigma_ann <- 12 * Sigma_pd

library(quadprog)





efficient_target_noshort <- function(Sigma, mu, target_m_log, ridge = 1e-8) {
  n <- length(mu)
  Dmat <- Sigma + diag(ridge, n)       # stabilitet
  dvec <- rep(0, n)
  Amat <- cbind(rep(1, n), mu, diag(n))  # t(Amat) %*% w >= bvec
  bvec <- c(1, target_m_log, rep(0, n))
  meq  <- 1                             # kun første constraint som lighed: sum w = 1
  sol <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq),
    error = function(e) NULL
  )
  if (is.null(sol)) return(NULL)
  w <- pmax(sol$solution, 0)
  w / sum(w)
}

# ----- 2) Greedy med kardinalitet og mål-afkast
cardinality_target_greedy <- function(Sigma_full, mu_full, K = 10, target_ann = 0.10, ridge = 1e-8) {
  stopifnot(ncol(Sigma_full) == length(mu_full))
  p    <- ncol(Sigma_full)
  syms <- colnames(Sigma_full)

  # Konverter årligt simpelt mål -> månedligt log-mål
  target_m_log <- log(1 + target_ann) / 12

  # Grov feasibility check (uden kovarians): max muligt mu er max(mu_full)
  if (target_m_log > max(mu_full, na.rm = TRUE) + 1e-12) {
    stop(sprintf("Målafkast er urealistisk højt for universet: max månedlig log-mean = %.6f (≈ %.2f%% årligt).",
                 max(mu_full, na.rm = TRUE),
                 100*(exp(12*max(mu_full, na.rm = TRUE))-1)))
  }

  # Startvalg: blandt enkeltaktiver der alene opfylder målet, vælg lavest varians; ellers vælg højeste mu som seed
  singles_ok   <- which(mu_full >= target_m_log)
  if (length(singles_ok) > 0) {
    sel <- singles_ok[ which.min(diag(Sigma_full)[singles_ok]) ]
  } else {
    sel <- which.max(mu_full)  # kan ikke opfylde målet alene; vi bygger videre
  }

  best_w    <- NULL
  best_var  <- Inf
  best_sel  <- sel
  feasible  <- FALSE

  while (length(sel) <= K) {
    cand <- setdiff(seq_len(p), sel)
    if (length(cand) == 0) break

    res <- lapply(cand, function(j) {
      S   <- c(sel, j)
      Sg  <- Sigma_full[S, S, drop = FALSE]
      mug <- mu_full[S]
      wS  <- efficient_target_noshort(Sg, mug, target_m_log, ridge = ridge)
      if (is.null(wS)) {
        return(list(j = j, feasible = FALSE, var = Inf, w = NULL, S = S))
      }
      varS <- as.numeric(t(wS) %*% Sg %*% wS)
      list(j = j, feasible = TRUE, var = varS, w = wS, S = S)
    })

    # Er der nogen feasible?
    feas_idx <- which(vapply(res, `[[`, logical(1), "feasible"))
    if (length(feas_idx) > 0) {
      # vælg den der giver lavest varians
      best_idx <- feas_idx[ which.min(vapply(res[feas_idx], `[[`, numeric(1), "var")) ]
      pick     <- res[[best_idx]]
      sel      <- pick$S
      best_w   <- pick$w
      best_var <- pick$var
      best_sel <- sel
      feasible <- TRUE
      if (length(sel) >= K) break
    } else {
      # ingen kandidat gør det feasible endnu: tilføj den kandidat med højest mu for at nærme os målet
      j_max <- cand[ which.max(mu_full[cand]) ]
      sel   <- c(sel, j_max)
      # fortsæt loopet uden at opdatere best_* (stadig infeasible)
      if (length(sel) >= K) break
    }
  }

  # Hvis stadig ikke feasible ved |sel| = K, så giv fejl med bedste realistiske info
  if (!feasible) {
    # prøv alligevel med den valgte mængde, find vægte der minimerer var uden mål (GMV) og rapportér opnåeligt afkast
    Sg  <- Sigma_full[sel, sel, drop = FALSE]
    mug <- mu_full[sel]
    # GMV for reference
    gmv_noshort <- function(Sigma_mat) {
      n <- ncol(Sigma_mat)
      Dmat <- Sigma_mat + diag(1e-8, n)
      dvec <- rep(0, n)
      Amat <- cbind(rep(1, n), diag(n))
      bvec <- c(1, rep(0, n))
      meq  <- 1
      sol  <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq)
      w    <- pmax(sol$solution, 0); w / sum(w)
    }
    w_try <- gmv_noshort(Sg)
    mu_try <- as.numeric(sum(w_try * mug))
    stop(sprintf("Målafkast kunne ikke opnås med K=%d. Opnåeligt (månedligt log) ≈ %.6f (≈ %.2f%% årligt).",
                 K, mu_try, 100*(exp(12*mu_try)-1)))
  }

  # Returnér løsning på best_sel
  Sg  <- Sigma_full[best_sel, best_sel, drop = FALSE]
  mug <- mu_full[best_sel]
  wS  <- efficient_target_noshort(Sg, mug, target_m_log, ridge = ridge)  # burde være feasible
  var <- as.numeric(t(wS) %*% Sg %*% wS)

  w_full <- rep(0, p); w_full[best_sel] <- wS
  names(w_full) <- syms

  list(
    selected   = syms[best_sel],
    w          = w_full,
    var_m      = var,                                   # månedlig varians
    sd_ann     = sqrt(12*var),                          # årlig std ca.
    mu_m       = sum(wS * mug),                         # månedlig log-mean
    mu_ann     = exp(12 * sum(wS * mug)) - 1,           # årlig simpel mean (ca.)
    target_ann = target_ann
  )
}




K          <- 4      # kardinalitet
target_ann <- 0.10    # 10% årligt simpelt afkast

sol_tgt <- cardinality_target_greedy(
  Sigma_full = Sigma_pd,
  mu_full    = mu_m,
  K          = K,
  target_ann = target_ann
)

# Se resultater
sol <- tibble(symbol = names(sol_tgt$w), w = as.numeric(sol_tgt$w)) %>%
  filter(w > 0) %>%
  arrange(desc(w)) %>%
  print(n = 100)

sol_stats <- sol %>%
  dplyr::mutate(
  mu_m = sol_tgt$mu_m,
  var_m = sol_tgt$var_m,
  mu_ann = sol_tgt$mu_ann,
  var_ann = sol_tgt$var_m
)

cat(sprintf("\nOpnået (årligt): μ ≈ %.2f%%, σ ≈ %.2f%%  (mål: %.2f%%)\n",
            100*sol_tgt$mu_ann, 100*sol_tgt$sd_ann, 100*sol_tgt$target_ann))





readr::write_csv(
  sol_stats,
  "weights_gmv_cardinality.csv"
)





