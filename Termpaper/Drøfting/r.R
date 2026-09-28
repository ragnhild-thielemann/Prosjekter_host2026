library(tidyverse)
library(httr2)

url <- "https://allemannsdata.com/wiki/api/v1/kilder/strompris/get_price_range"

test <- request(url) |>
  req_url_query(
    start_date = "2025-01-01",
    end_date = "2025-01-31",
    area = "NO1",
    limit = 100,
    offset = 0
  ) |>
  req_perform() |>
  resp_body_json()

prices <- test$data$prices |>
  bind_rows()

glimpse(prices)


get_period <- function(area, start_date, end_date) {
  
  url <- paste0(
    "https://allemannsdata.com/wiki/api/v1/kilder/strompris/",
    "get_price_range"
  )
  
  offset <- 0
  all_prices <- list()
  
  repeat {
    
    cat(
      "Getting", area,
      as.character(start_date), "to",
      as.character(end_date),
      "offset:", offset, "\n"
    )
    
    result <- request(url) |>
      req_url_query(
        start_date = as.character(start_date),
        end_date = as.character(end_date),
        area = area,
        limit = 100,
        offset = offset
      ) |>
      req_perform() |>
      resp_body_json()
    
    prices <- result$data$prices
    
    if (length(prices) == 0) {
      break
    }
    
    all_prices[[length(all_prices) + 1]] <- bind_rows(prices)
    
    # If fewer than 100 records were returned,
    # we have reached the end
    if (length(prices) < 100) {
      break
    }
    
    offset <- offset + 100
    
    # Small pause to avoid rate limiting
    Sys.sleep(1)
  }
  
  bind_rows(all_prices) |>
    mutate(
      area = area
    )
}


test_data <- get_period(
  "NO1",
  as.Date("2025-01-01"),
  as.Date("2025-01-31")
)

nrow(test_data)
