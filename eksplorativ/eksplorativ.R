

# we create a connection

conn <- mlfintester::conn(1,
                  4002,
                  verbose = FALSE)


IBrokers::reqManagedAccts(conn)
account_info <- IBrokers::reqAccountUpdates(conn)

stk <- IBrokers::twsEquity(
  "AAPL",
  "SMART",
  "ISLAND"
)

IBrokers::reqMktData(
  conn = conn,
  stk

)
