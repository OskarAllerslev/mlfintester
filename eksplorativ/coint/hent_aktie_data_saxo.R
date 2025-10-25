

# prelims -----------------------------------------------------------------


library(httr2)
library(jsonlite)
library(dplyr)
`%||%` <- function(a, b) if (is.null(a)) b else a

## husk at req nu 24 h token ellers virker det ikke
token <- "eyJhbGciOiJFUzI1NiIsIng1dCI6IjY3NEM0MjFEMzZEMUE1OUNFNjFBRTIzMjMyOTVFRTAyRTc3MDMzNTkifQ.eyJvYWEiOiI3Nzc3NSIsImlzcyI6Im9hIiwiYWlkIjoiMTA5IiwidWlkIjoiT3R3bmk3dlk1bUV6VXZUTThkUnJvQT09IiwiY2lkIjoiT3R3bmk3dlk1bUV6VXZUTThkUnJvQT09IiwiaXNhIjoiRmFsc2UiLCJ0aWQiOiIyMDAyIiwic2lkIjoiNmI5NTdhNWZmNzNhNDg5NGJiMjcxZjVmNjM1MTA4MzIiLCJkZ2kiOiI4NCIsImV4cCI6IjE3NjE0OTk0ODciLCJvYWwiOiIxRiIsImlpZCI6ImZkMGMwOGQzMTQyOTQ3ZTYyNzg1MDhkZTBkNWQzMjhkIn0.QrrAvD_tHImVsoU6E4bNyGrh9WewbdnvJ6P5y67QzG-sLqgW8d8iFv6Z1LNZuDtsZvlym2ukyMb7ixC3HTNgbA"
base <- "https://gateway.saxobank.com/sim/openapi"


ua <- "oskar-r-openapi/0.1"

acc <- oapi_get("port/v1/accounts/me")
acc_keys <- acc$body$Data %>%
  tibble::as_tibble()

account_key <- acc_keys$AccountKey[1]


# hent uic
get_uic <- function(
    symbol = "NOVO",
    AssetTypes = "CfdOnStock",
    top = 200
){
  r <- oapi_get("ref/v1/instruments", list(
    AssetTypes = AssetTypes,
    IncludeNonTradable = "true",
    Keywords = symbol,
    "$top" = top
  ))
  d <- tibble::as_tibble(r$body$Data)
  if (!nrow(d)) return(NA_integer_)
  hit <- if ("Symbol" %in% names(d)) d %>% dplyr::filter(Symbol == symbol) else d[0,]
  if (!nrow(hit)) hit <- d[1,]
  hit$Identifier[[1]]
}



# hent historisk data
get_historic_data <- function(
    uic = get_uic(),
    horizon_min = 1440,
    days = 252,
    mode = "UpTo",
    time_iso = NULL
){
  q <- list(
    Uic        = uic,                # din UIC
    AssetType  = "CfdOnStock",
    Horizon    = 1440,                 # 1 dag
    Count      = days,                 # maks pr. kald
    Mode       = "UpTo",               # seneste op til Time
    Time       = strftime(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    FieldGroups = "Data"
  )
  r <- oapi_get("chart/v3/charts", q)
  df <- r$body$Data %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      Time = base::as.Date(Time)
    )
}



data <- get_historic_data(
  uic = get_uic()
)

# log transform af data
transform_historic_data_to_log <- function(
    data
){
  data <- data %>%
    dplyr::mutate(
      log_open = log(Open)
    ) %>%
  dplyr::select(log_open, Time) %>%
  return(data)
}

log_data <- transform_historic_data_to_log(
  data = data
)



## eksempel til coint ----

#
# equity_list_dk_bluechips <- list(
#   # NOVO_B   = IBrokers::twsEquity("NOV"),
#   # MAERSK_B = IBrokers::twsEquity("MAERSK.B"),
#   # DSV      = IBrokers::twsEquity("DSV",      "SMART"),
#   VWS      = IBrokers::twsEquity("VWS"),
#   # ORSTED   = IBrokers::twsEquity("ORSTED"),
#   CARL_B   = IBrokers::twsEquity("CARL")
# )


# henter uic
novo_uic <- get_uic(symbol = "NOVOb:xcse")
maersk_uic <- get_uic(symbol = "MAERSKb:xcse")
dsv_uic <- get_uic(symbol = "DSV:xcse")
vws_uic <- get_uic(symbol = "VWS")
carl_uic <- get_uic(symbol = "CARLb")

# henter data
t <- 252 * 5

novo_data <- get_historic_data(uic = novo_uic, days = t)
maersk_data <- get_historic_data(uic = maersk_uic, days = t)
dsv_data <- get_historic_data(uic = dsv_uic, days = t)
vws_data <- get_historic_data(uic = vws_uic, days = t)
carl_data <- get_historic_data(uic = carl_uic, days = t)


# smid data sammen
d1 <- transform_historic_data_to_log(novo_data)   %>% dplyr::rename(Open_1 = log_open)
d2 <- transform_historic_data_to_log(maersk_data) %>% dplyr::rename(Open_2 = log_open)
d3 <- transform_historic_data_to_log(dsv_data)    %>% dplyr::rename(Open_3 = log_open)
d4 <- transform_historic_data_to_log(vws_data)    %>% dplyr::rename(Open_4 = log_open)
d5 <- transform_historic_data_to_log(carl_data)    %>% dplyr::rename(Open_5 = log_open)



# join

p_dat <- Reduce(function(x,y) dplyr::inner_join(x, y, by="Time"), list(d1,d2)) %>%
  dplyr::transmute(
    index = as.POSIXct(Time, tz="UTC"),
    Open_1,
    Open_2
    # Open_3,
    # Open_4,
    # Open_5
  )

# plot af resultater

res <- backtest_coint(
  data = p_dat,
  z_in = 2.2,
  z_out = 0.5,
  notional = 200000,
  fee_flat = ,
  rank_level = "10pct",
  min_abs_weight = 0.1
)




ggplot2::ggplot(res, ggplot2::aes(row_in_data, equity)) +
  ggplot2::geom_rect(
    data = subset(
      dplyr::summarise(
        dplyr::group_by(
          dplyr::mutate(res, z_on = coint==1L,
                        grp = cumsum(z_on != dplyr::lag(z_on, default = first(z_on)))),
          grp),
        xmin = dplyr::first(row_in_data), xmax = dplyr::last(row_in_data), z_on = dplyr::first(z_on)),
      z_on),
    ggplot2::aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
    inherit.aes=FALSE, fill="red", alpha=0.08) +
  ggplot2::geom_line() +
  ggplot2::geom_line(ggplot2::aes(y = fee_cum), color="red") +
  ggplot2::labs(title="Equity curve (gross) og fees separat", x=NULL, y="equity") +
  ggplot2::theme_minimal()





# utils -------------------------------------------------------------------


oapi_get <- function(path, query=list()) {
  req <- request(base) |>
    req_url_path_append(path) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_user_agent(ua)

  if (length(query)) {
    req <- do.call(req_url_query, c(list(req), query))
  }

  resp <- req_perform(req)
  list(
    status = resp_status(resp),
    headers = resp_headers(resp),
    body = resp_body_json(resp, simplifyVector = TRUE)
  )
}








