

library(tidyverse)
correckt <- function(x){
  return (x)

}
print(pi)
approx <- function(x,N){
  v <- pi/2
  
  for (a in 1:N){
    ledd = (-1)**a*(1/a)*sin(a*x)
    v = v + ledd
  }
  return(v)
  
}
x_verdier <- seq(0,10,0.001)


dataframe <- tibble(x_verdier,correckt(x_verdier),approx(x_verdier,n))

View(dataframe)

