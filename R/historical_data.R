#' Fetch historical market data from Interactive Brokers
#'
#' Wrapper around \code{IBrokers::reqHistoricalData()} to retrieve historical bars as a \code{data.table}.
#' Must be connected to TWS or IB-Gateway when calling this function.
#'
#' @param conn An object of class \code{twsconn}, as returned by \code{IBrokers::twsConnect()} or your own \code{conn()} wrapper.
#' @param equity A Tws contract object (e.g. created by \code{IBrokers::twsSTK()}, \code{IBrokers::twsFuture()}, etc.). Must include a valid \code{exchange} and, for SMART routing, a \code{primary} exchange.
#' @param barSize Character. Size of each bar, e.g. \code{"1 min"}, \code{"5 mins"}, \code{"1 day"}.
#' @param duration Character. Look-back period, e.g. \code{"1 D"} (one trading day), \code{"1 W"}, \code{"1 M"}, \code{"1 Y"}.
#' @param ... Additional arguments passed on to \code{IBrokers::reqHistoricalData()}, such as:
#'   \describe{
#'     \item{\code{endDateTime}}{Character "YYYYMMDD HH:MM:SS", end of the period}
#'     \item{\code{whatToShow}}{Character, one of \code{"TRADES"}, \code{"MIDPOINT"}, \code{"BID"}, \code{"ASK"}, etc.}
#'     \item{\code{useRTH}}{Integer 1 (regular hours) or 0 (all hours)}
#'     \item{\code{formatDate}}{Integer 1 (R Date) or 2 (Unix timestamp)}
#'   }
#'
#' @return A \code{data.table} with one row per bar. Typical columns are:
#'
#' Wrapper around \code{IBrokers::reqHistoricalData()} to retrieve historical bars as a \code{data.table}.
#' Must be connected to TWS or IB-Gateway when calling this function.
#'
#' @param conn An object of class \code{twsconn}, as returned by \code{IBrokers::twsConnect()} or your own \code{conn()} wrapper.
#' @param equity A Tws contract object (e.g. created by \code{IBrokers::twsSTK()}, \code{IBrokers::twsFuture()}, etc.). Must include a valid \code{exchange} and, for SMART routing, a \code{primary} exchange.
#' @param barSize Character. Size of each bar, e.g. \code{"1 min"}, \code{"5 mins"}, \code{"1 day"}.
#' @param duration Character. Look-back period, e.g. \code{"1 D"} (one trading day), \code{"1 W"}, \code{"1 M"}, \code{"1 Y"}.
#' @param ... Additional arguments passed on to \code{IBrokers::reqHistoricalData()}, such as:
#'   \describe{
#'     \item{\code{endDateTime}}{Character "YYYYMMDD HH:MM:SS", end of the period}
#'     \item{\code{whatToShow}}{Character, one of \code{"TRADES"}, \code{"MIDPOINT"}, \code{"BID"}, \code{"ASK"}, etc.}
#'     \item{\code{useRTH}}{Integer 1 (regular hours) or 0 (all hours)}
#'     \item{\code{formatDate}}{Integer 1 (R Date) or 2 (Unix timestamp)}
#'   }
#'
#' @return A \code{data.table} with one row per bar. Typical columns are:
#'   \describe{
#'     \item{\code{date}}{POSIXct or Date, depending on \code{formatDate}}
#'     \item{\code{open}}{Opening price}
#'     \item{\code{high}}{Highest price}
#'     \item{\code{low}}{Lowest price}
#'     \item{\code{close}}{Closing price}
#'     \item{\code{volume}}{Volume traded}
#'     \item{\code{average}}{Average price}
#'     \item{\code{barCount}}{Number of trades in the bar}
#'   }
#'
#' @examples
#' \dontrun{
#' # 1. Connect to IB
#' conn <- IBrokers::twsConnect(clientId = 1, host = "localhost", port = 4002)
#'
#' # 2. Define AAPL contract on SMART/NASDAQ
#' stk <- IBrokers::twsSTK("AAPL", exch = "SMART", primary = "NASDAQ", currency = "USD")
#'
#' # 3. Fetch last day’s 5-min bars
#' dt <- historical_data(
#'   conn       = conn,
#'   equity     = stk,
#'   barSize    = "5 mins",
#'   duration   = "1 D",
#'   whatToShow = "TRADES",
#'   useRTH     = 1,
#'   formatDate = 1
#' )
#' head(dt)
#'
#' # 4. Disconnect
#' IBrokers::twsDisconnect(conn)
#' }
#'
#' @seealso \code{\link[IBrokers]{reqHistoricalData}}, \code{\link[IBrokers]{twsSTK}}
#' @importFrom data.table as.data.table
#' @examples
#' \dontrun{
#' # 1. Connect to IB
#' conn <- IBrokers::twsConnect(clientId = 1, host = "localhost", port = 4002)
#'
#' # 2. Define AAPL contract on SMART/NASDAQ
#' stk <- IBrokers::twsSTK("AAPL", exch = "SMART", primary = "NASDAQ", currency = "USD")
#'
#' # 3. Fetch last day’s 5-min bars
#' dt <- historical_data(
#'   conn       = conn,
#'   equity     = stk,
#'   barSize    = "5 mins",
#'   duration   = "1 D",
#'   whatToShow = "TRADES",
#'   useRTH     = 1,
#'   formatDate = 1
#' )
#' head(dt)
#'
#' # 4. Disconnect
#' IBrokers::twsDisconnect(conn)
#' }
#'
#' @importFrom data.table as.data.table
#' @export
historical_data <- function(conn,
                            equity,
                            barSize,
                            duration,
                            ...
) {
  dat <- data.table::as.data.table(
    IBrokers::reqHistoricalData(
      conn        = conn,
      Contract    = equity,
      barSize     = barSize,
      duration = duration,
      ...
    )
  )
  return(dat)
}
