#' Fetch historical market data for multiple contracts from Interactive Brokers
#'
#' Wrapper around \code{mlfintester::historical_data()} to retrieve historical bars
#' for a list of Tws contract objects, returning a named list of \code{data.table}s.
#'
#' @param conn An object of class \code{twsconn}, as returned by \code{IBrokers::twsConnect()} or a custom wrapper.
#' @param equity_list A named list of Tws contract objects (e.g.\ produced by \code{IBrokers::twsSTK()}, \code{IBrokers::twsFuture()}, etc.).
#'   The names of the list will be used as names in the returned list.
#' @param barSize Character. Size of each bar, e.g. \code{"1 min"}, \code{"5 mins"}, \code{"1 hour"}, \code{"1 day"}.
#' @param duration Character. Look-back period, e.g. \code{"1 D"} (one trading day), \code{"1 W"}, \code{"1 M"}, \code{"1 Y"}.
#' @param ... Additional arguments passed on to \code{mlfintester::historical_data()}, such as
#'   \describe{
#'     \item{\code{whatToShow}}{Character, one of \code{"TRADES"}, \code{"BID"}, \code{"ASK"}, \code{"MIDPOINT"}, etc.}
#'     \item{\code{useRTH}}{Integer, 1 = regular trading hours only, 0 = all hours}
#'     \item{\code{formatDate}}{Integer, 1 = R Date, 2 = Unix timestamp}
#'   }
#'
#' @return A named \code{list} of \code{data.table} objects. Each element corresponds to one contract
#'   in \code{equity_list} and contains the historical bars returned by
#'   \code{mlfintester::historical_data()}.
#'
#' @examples
#' \dontrun{
#' # 1. Connect to IB
#' conn <- IBrokers::twsConnect(clientId = 1, host = "localhost", port = 4002)
#'
#' # 2. Define a list of contracts
#' symbols <- c("AAPL", "MSFT", "GOOG")
#' equities <- setNames(
#'   lapply(symbols, function(sym) {
#'     IBrokers::twsSTK(sym, exch = "SMART", primary = "NASDAQ", currency = "USD")
#'   }),
#'   symbols
#' )
#'
#' # 3. Fetch 1-hour bars for the past month for all symbols
#' data_list <- historical_data_multi(
#'   conn         = conn,
#'   equity_list  = equities,
#'   barSize      = "1 hour",
#'   duration     = "1 M",
#'   whatToShow   = "TRADES",
#'   useRTH       = 1,
#'   formatDate   = 1
#' )
#'
#' # Inspect AAPL data
#' head(data_list$AAPL)
#'
#' # Disconnect when done
#' IBrokers::twsDisconnect(conn)
#' }
#'
#' @seealso \code{\link{historical_data}}, \code{\link[IBrokers]{reqHistoricalData}}, \code{\link[IBrokers]{twsSTK}}
#' @export
historical_data_multi <- function(conn,
                                  equity_list,
                                  barSize,
                                  duration,
                                  ...) {
  res <- vector("list", length(equity_list))
  names(res) <- names(equity_list)

  for (nm in names(equity_list)) {
    res[[nm]] <- mlfintester::historical_data(
      conn      = conn,
      equity    = equity_list[[nm]],
      barSize   = barSize,
      duration  = duration,
      ...
    )
  }

  return(res)
}
