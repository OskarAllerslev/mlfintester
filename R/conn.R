#' Connect to Interactive Brokers Trader Workstation (TWS)
#'
#' Establishes a connection to Interactive Brokers Trader Workstation (TWS) or IB Gateway
#' using the IBrokers package.
#'
#' @param clientId Numeric. A unique identifier for the client connection. Must be different
#'   for each simultaneous connection.
#' @param host Character. The hostname or IP address of the TWS or IB Gateway
#'   (e.g., `"localhost"` or `"127.0.0.1"`).
#' @param ... Additional arguments passed on to \code{IBrokers::twsConnect},
#'   such as \code{port}, \code{timeout}, etc.
#'
#' @return An object of class \code{twsconn} representing the established connection.
#'
#' @examples
#' \dontrun{
#' # Connect to TWS on localhost, port 7496, using client ID 1
#' conn_obj <- conn(clientId = 1, host = "127.0.0.1", port = 7496)
#' }
#'
#' @export
conn <- function(clientId,
                 port,
                 ...)
{
  IBrokers::twsConnect(
    clientId = clientId,
    port     = port,
    ...
  )
}
