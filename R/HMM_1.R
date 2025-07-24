#' HMM_1
#'
#' Function for fitting a specific model
#'
#' @param data data for fit
#'
#' @export
HMM_1 <- function(data)
{
  f <- ~ vix_slope + rv20
  hid1 <- hmmTMB::MarkovChain$new(data = data,
                                  n_states = 3,
                                  formula = f)
  dists <- list(log_ret = "norm")
  par0_2s <- list(log_ret = list(mean = c(0,0.001,-0.2),
                                 sd = c(0.3,0.6,0.9)))
  obs1 <- hmmTMB::Observation$new(data = data,
                                  dists = dists,
                                  par = par0_2s)
  hmm1 <- hmmTMB::HMM$new(obs = obs1, hid = hid1)
  # model fitting
  hmm1$fit(silent = TRUE)
  return(hmm1)
}
