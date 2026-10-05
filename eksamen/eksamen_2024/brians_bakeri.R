

library(tidyverse)
library(readxl)
library(lubridate)
sub <- read_excel("eksamen/eksamen_2024/subscribers (1).xlsx")


april_15 <- sub |>
  mutate(i = interval(ymd(Start),ymd(End)))|>
  filter(ymd("2024-04-02") %within% i)|>
  summarise(antall = sum(Number))

sprintf("Han ma produsere %g brod 15 April 2024",april_15$antall)

dager <- seq.Date(ymd("2024-01-01"),ymd("2024-12-31"))

print(dager)
total <- tibble(dager = dager)

brod <- vector("integer",length(dager))

i = 1
for (d in dager){
  
  t <- sub |>
    mutate(i = interval(ymd(Start),ymd(End)))|>
    filter(as.Date(d) %within% i)|>
    summarise(antall = sum(Number))
  brod[i] = t$antall
  i = i + 1
    
}

print(t)
total <- tibble(dager = dager, brod = brod)

p1 <- total |>
  ggplot(aes(x = dager, y = brod)) + 
  geom_line()

p1
