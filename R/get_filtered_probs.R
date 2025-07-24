#' Filtered probs
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @export
get_filtered_probs <- function(HMM_object)
{
  fb <- HMM_object$forward_backward()
  log_alpha <- t(fb$logforward)
  log_alpha[!base::is.finite(log_alpha)] <- -1e300

  row_max <- matrixStats::rowMaxs(log_alpha)
  tmp     <- base::exp(base::sweep(log_alpha, 1, row_max, "-"))
  alpha   <- tmp / base::rowSums(tmp)
  return(alpha)
}
