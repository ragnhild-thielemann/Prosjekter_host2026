
library(tidyverse)
correckt <- function(x){
  return (x)
  
}
print(pi)
approx <- function(x,N){
  v <- 0
  
  for (a in 1:N){
    ledd = ((-1)^(a+1))*(2/a)*sin(a*x)
    v = v + ledd
    
  }
  return(v)
  
}

approx(1,10000)

x_verdier <- seq(-pi,pi,0.1)

N = 100
dataframe <- tibble(x_verdier,correckt(x_verdier),approx(x_verdier,N))

View(dataframe)