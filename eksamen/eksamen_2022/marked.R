
library(tidyverse)
S <- function(p){
  S = 3
  return(S)
}

D <- function(p){
  
  return (7 - p - log(p))
}

start <- 0.1 ; end <- 5

p <- seq(start,end,0.001)

t <- tibble(p = p, Sup = S(p), Dem = D(p))
t
p1 <- t|>
  ggplot() + 
  geom_line(aes(x = Dem, y = p))+
  geom_line(aes(x = Sup, y = p))

p1


los <- function(p){
  return(D(p)-S(p))
}
e = 0.01
a = start ; b = end ; m = (a + b)/2
while (abs(los(m))>e){
  m = (a + b)/2
  if (los(a)*los(m)>0){
    a = m
  } else {
    b = m
  }
}
print(m)

print(m)


price <- function(S){
  p = 0.001 #prisen må være postiv
  delta = 0.001
  while (D(p)-S>0){
    p = p + delta
  }
  return(p)
}


der <- function(S){
  delta = 0.01
  return((price(S + delta)-price(S))/delta)

}

print(der(2))