
library(httr2)
library(jsonlite)
library(dplyr)
`%||%` <- function(a, b) if (is.null(a)) b else a

token <- "eyJhbGciOiJFUzI1NiIsIng1dCI6IjY3NEM0MjFEMzZEMUE1OUNFNjFBRTIzMjMyOTVFRTAyRTc3MDMzNTkifQ.eyJvYWEiOiI3Nzc3NSIsImlzcyI6Im9hIiwiYWlkIjoiMTA5IiwidWlkIjoiT3R3bmk3dlk1bUV6VXZUTThkUnJvQT09IiwiY2lkIjoiT3R3bmk3dlk1bUV6VXZUTThkUnJvQT09IiwiaXNhIjoiRmFsc2UiLCJ0aWQiOiIyMDAyIiwic2lkIjoiMGM2ZWY1ZGVmYTA3NGJhMmJkODMyYjI4MzNiNzZlNTQiLCJkZ2kiOiI4NCIsImV4cCI6IjE3NjEwNTAyNzkiLCJvYWwiOiIxRiIsImlpZCI6ImZkMGMwOGQzMTQyOTQ3ZTYyNzg1MDhkZTBkNWQzMjhkIn0.XeiBbZ6NCjaloNArl-Y7DPudUB8f2JwYpLFjdsw8LCcwRPS3M0xky69yfSmmMZp0Bq2kaz9IRgcZ2AgK7fXm3w"
base <- "https://gateway.saxobank.com/sim/openapi"


ua <- "oskar-r-openapi/0.1"


## get
oapi_put <- function(path) {
  req <- request(base) |>
    req_url_path_append(path) |>
    req_method("PUT") |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_user_agent(ua)
  resp <- req_perform(req)
  resp_status(resp)
}
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
oapi_post_nosimplify <- function(path, body) {
  req <- request(base) |>
    req_url_path_append(path) |>
    req_headers(
      Authorization = paste("Bearer", token),
      "Content-Type" = "application/json"
    ) |>
    req_user_agent(ua) |>
    req_body_json(body, auto_unbox = TRUE)
  resp <- req_perform(req)
  list(
    status  = resp_status(resp),
    headers = resp_headers(resp),
    body    = resp_body_json(resp, simplifyVector = FALSE)  # <- vigtigt
  )
}
oapi_patch <- function(path, body) {
  req <- request(base) |>
    req_url_path_append(path) |>
    req_method("PATCH") |>
    req_headers(
      Authorization = paste("Bearer", token),
      "Content-Type" = "application/json"
    ) |>
    req_user_agent(ua) |>
    req_body_json(body, auto_unbox = TRUE)
  resp <- req_perform(req)
  list(
    status  = resp_status(resp),
    headers = resp_headers(resp),
    body    = tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
  )
}
acc <- oapi_get("port/v1/accounts/me")
acc_keys <- acc$body$Data %>%
  tibble::as_tibble()

account_key <- acc_keys$AccountKey[1]


# find instrument



unwrap_data <- function(body) if (!is.null(body$Data)) body$Data else body

idx_search <- oapi_get("ref/v1/instruments", list(
  AssetTypes = "StockIndex",
  IncludeNonTradable = "true",
  Keywords   = "S&P 500",
  "$top"     = 200
))

idx_tbl <- tibble::as_tibble(unwrap_data(idx_search$body)) %>%
  dplyr::filter(Symbol == "XSP.I")

uic_index <- idx_tbl$Identifier

det_idx <- oapi_get(glue::glue(
  "ref/v1/instruments/details/{uic_index}/StockIndex"))

roots <- tibble::as_tibble(
  det_idx$body$RelatedOptionRootsEnhanced
)
root_id <- roots$OptionRootId

space <- oapi_get(
  glue::glue("ref/v1/instruments/contractoptionspaces/{root_id}")
)


## strikes indeholder alle udløb osv
# indeni er der dataframes af de udløb, lidt ligesom når man ser på en chain
strikes1 <- tibble::as_tibble(space$body$OptionSpace)

## hvis man så vælger en strike
# så ser man jo følgende put calls
specific_strike <- strikes1[[6]][[1]]

## vi skal dog også have bid, ask osv.
uics <- specific_strike %>%
  dplyr::pull(Uic) %>%
  unique()

## funktion til at hente snapshots i batches



ctx_id <- base::paste0("ctx_", as.integer(Sys.time()))
ref_id <- "xsp1"

sub_body <- list(
  Arguments = list(
    AssetType = "StockIndexOption",
    Identifier = root_id,      # <-- OptionRootId, ikke UIC
    AccountKey = account_key,
    Amount = 1,
    Expiries = list(list(Index = 1)), # nærmeste udløb (kan du udvide med flere)
    MaxStrikesPerExpiry = 100
  ),
  ContextId   = ctx_id,
  ReferenceId = ref_id,
  RefreshRate = 2000
)

sub <- oapi_post_nosimplify("trade/v1/optionschain/subscriptions", sub_body)

snap <- sub$body$Snapshot



# ren faktisk optionsdata
chain_prices <- purrr::imap_dfr(snap$Expiries, function(e, .i){
  if (is.null(e$Strikes)) return(tibble())
  purrr::map_dfr(e$Strikes, function(s){
    rows <- list()
    if (!is.null(s$Call)) rows[[length(rows)+1]] <- tibble(
      Expiry = as.Date(e$Expiry), Strike = s$Strike, Side = "Call",
      Uic = s$Call$Uic %||% NA_integer_,
      Bid = s$Call$Bid %||% NA_real_, Ask = s$Call$Ask %||% NA_real_,
      BidSize = s$Call$BidSize %||% NA_real_, AskSize = s$Call$AskSize %||% NA_real_,
      DeltaPct = s$Call$DeltaPct %||% NA_real_
    )
    if (!is.null(s$Put)) rows[[length(rows)+1]] <- tibble(
      Expiry = as.Date(e$Expiry), Strike = s$Strike, Side = "Put",
      Uic = s$Put$Uic %||% NA_integer_,
      Bid = s$Put$Bid %||% NA_real_, Ask = s$Put$Ask %||% NA_real_,
      BidSize = s$Put$BidSize %||% NA_real_, AskSize = s$Put$AskSize %||% NA_real_,
      DeltaPct = s$Put$DeltaPct %||% NA_real_
    )
    dplyr::bind_rows(rows)
  })
}) %>% arrange(Expiry, Strike, desc(Side))


library(purrr)
library(tidyr)
library(lubridate)

op_chain <- chain_prices %>%
  select(Expiry, Strike, Side, Uic, Bid, Ask, BidSize, AskSize, DeltaPct) %>%
  tidyr::pivot_wider(
    names_from = Side,
    values_from = c(Uic, Bid, Ask, BidSize, AskSize, DeltaPct),
    names_glue = "{.value}_{Side}"
  ) %>%
  arrange(Expiry, Strike)


res <- best_box_spread(
  op_chain = op_chain,
  contract_multiplier = 100,
  min_qty = 1
)























