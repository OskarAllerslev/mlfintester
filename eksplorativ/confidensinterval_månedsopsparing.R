
sol <- readr::read_csv("weights_gmv_cardinality.csv")

library(tidyverse)


simulate_walk_contrib_tibble <- function(mu_m, var_m,
                                         months = 180, B = 1000,
                                         initial = 30000, contrib = 5000,
                                         timing = c("start","end"),
                                         seed = 1) {
  set.seed(seed)
  timing  <- match.arg(timing)
  sigma_m <- sqrt(var_m)

  # 1) Simulér månedlige log-afkast ~ N(mu_m, sigma_m^2)
  Rlog <- matrix(
    rnorm(months * B, mean = mu_m, sd = sigma_m),
    nrow = months, ncol = B
  )
  # 2) Konverter til simple afkast
  Rsimple <- exp(Rlog) - 1  # months x B

  # 3) Byg konto-saldo pr. path med indbetalinger
  W <- rep(initial, B)                # saldo ved t=0 for alle paths
  Wmat <- matrix(NA_real_, months, B) # gem saldo pr. måned

  for (m in 1:months) {
    if (timing == "start") {
      W <- (W + contrib) * (1 + Rsimple[m, ])  # indbetal før månedens afkast
    } else {
      W <- W * (1 + Rsimple[m, ]) + contrib    # indbetal efter månedens afkast
    }
    Wmat[m, ] <- W
  }

  colnames(Wmat) <- paste0("path_", seq_len(B))

  # 4) Tibble (long) klar til ggplot
  as_tibble(Wmat) |>
    mutate(month = seq_len(months)) |>
    pivot_longer(-month, names_to = "path", values_from = "balance")
}

# --- Brug dine parametre ---
# enten:
mu_m  <- sol$mu_m
var_m <- sol$var_m

traj <- simulate_walk_contrib_tibble(mu_m, var_m,
                                     months = 180, B = 100,
                                     initial = 30000, contrib = 5000,
                                     timing = "start",  # eller "end"
                                     seed = 1)


# År-markører ved hvert 5. år (60 mdr)
months <- 180
year_breaks <- seq(60, months, by = 60)

# Plot alle paths + lodrette linjer
ggplot(traj, aes(month, value, group = path)) +
  geom_line(alpha = 0.06) +
  geom_vline(xintercept = year_breaks, linetype = "dashed", alpha = 0.4) +
  labs(
    title = "Monte Carlo — konto-saldo med månedlige indbetalinger",
    subtitle = sprintf("Start: 30.000, +5.000/md, timing=start | μ=%.5f, σ=%.5f, paths=%d",
                       mu_m, sqrt(var_m), n_distinct(traj$path)),
    x = "Måned", y = "Saldo"
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark=".", decimal.mark=",")) +
  theme_minimal()

# Kvantilbånd + år-markører
qdf <- traj |>
  group_by(month) |>
  summarise(p05 = quantile(value, 0.05),
            p50 = quantile(value, 0.50),
            p95 = quantile(value, 0.95),
            .groups = "drop")

p <- ggplot() +
  geom_line(data = traj, aes(month, value, group = path), alpha = 0.2) +
  geom_ribbon(data = qdf, aes(month, ymin = p05, ymax = p95), alpha = 0.3) +
  geom_line(data = qdf, aes(month, y = p50), size = 2) +
  geom_vline(xintercept = year_breaks, linetype = "dashed", alpha = 0.9) +
  labs(title = "Monte Carlo — kvantilbånd for konto-saldo",
       x = "Måned", y = "Saldo") +
  scale_y_continuous(labels = scales::label_number(big.mark=".", decimal.mark=",")) +
  theme_minimal()









