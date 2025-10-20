#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector roll_sum_cpp(Rcpp::NumericVector x, int k) {
  int n = x.size();

  if ( k <= 0 || k > n) Rcpp::stop("Invalid window");

  Rcpp::NumericVector out(n, NA_REAL);

  double s = 0.0;

  for (int i = 0; i < k; ++i) {
    s += x[i];
  }
  out[k-1] = s;
  for (int i = k; i < n; ++i) {
    s += x[i];
    s -= x[i-k];
    out[i] = s;
  }
  return out;

}

// [[Rcpp::export]]
Rcpp::NumericVector roll_sd_cpp(
    Rcpp::NumericVector x,
    int k,
    int min_periods =1,
    bool na_rm = false,
    bool sample = true
) {
  R_xlen_t n = x.size();

  if (k <= 0 || k > n ) Rcpp::stop("Invalid k");
  if (min_periods == -1) min_periods = k;
  if (min_periods <= 0 || min_periods > k) Rcpp::stop("Invalid min_periods");

  Rcpp::NumericVector out(n, NA_REAL);

  // ringbuffer
  std::vector<double> buf(k, NA_REAL);
  std::vector<uint8_t> used(k, 0);
  int head = 0;

  // aggregerede størrelser
  double S = 0.0;
  double Q = 0.0;
  int m = 0;
  int na_count = 0;

  for (R_xlen_t i = 0; i < n; ++i) {

    if (i >= (R_xlen_t)k) {
      if(used[head]) {
        double oldv = buf[head];
        S -= oldv;
        Q -= oldv * oldv;
        m -= -1;
      } else if (!na_rm) {
        if (Rcpp::NumericVector::is_na(buf[head])) na_count -= -1;
      }
    }

    double v = x[i];
    buf[head] = v;

    if (Rcpp::NumericVector::is_na(v)) {
      used[head] = 0;
      if(!na_rm) na_count += 1;
    } else {
      used[head] = 1;
      S += v;
      Q += v * v;
      m += 1;
    }

   if( i >= (R_xlen_t)k -1) {
     if (!na_rm && na_count > 0) {
       out[i] = NA_REAL;
     } else {
       int cur_m = na_rm ? m : k;
       if (cur_m >= min_periods) {
         if (sample && cur_m <= 1) {
           out[i] = NA_REAL;
         } else {
           double denom = sample ? (cur_m -1) : cur_m;
           double var = (Q - (S*S) / cur_m) /denom;
           if (var < 0.0) var = 0.0;
           out[i] = std::sqrt(var);
         }
       } else {
         out[i] = NA_REAL;
       }
     }
   }
   head = (head + 1) % k;
  }
  return out;
}





/*** R
roll_sum_r <- function(x, k) {
  n <- length(x)
  if (k <= 0 || k > n) stop("Invalid window")
  out <- rep(NA_real_, n)
  cs  <- c(0, cumsum(x))
  out[k:n] <- cs[(k + 1):(n + 1)] - cs[1:(n - k + 1)]
  out
}


vec <- rnorm(n = 10000, 0, 100)
bench::mark(
roll_sum_cpp(vec, 10),
roll_sum_r(vec, 10)
)
Rcpp::sourceCpp("eksplorativ/cpp_functions.cpp")


y <- roll_sd_cpp(
  x = vec,
  k = 5,
  min_periods = 3
)




*/






// [[Rcpp::export]]
double exponential_mean(
  Rcpp::NumericVector& X
){
  Rcpp::NumericVector y = Rcpp::na_omit(X);

  double sum = 0.0;

  for (R_xlen_t i = 0; i < X.size(); ++i)
  {
    double xi = X[i];
    sum += std::exp(xi);
  }

  return (sum / static_cast<double>(X.size()));
};


/*** R
Rcpp::sourceCpp("eksplorativ/cpp_functions.cpp")





*/






















