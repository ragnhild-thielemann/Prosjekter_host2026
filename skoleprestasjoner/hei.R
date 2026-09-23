library(pxweb)

url <- "https://data.ssb.no/api/v0/no/table/07501"

tabell <- pxweb_get(url)

View(tabell$variables)
