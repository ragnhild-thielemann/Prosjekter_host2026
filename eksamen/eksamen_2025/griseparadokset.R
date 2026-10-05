
price <- function(stock){
  
  return(100-stock)
}


time = 500

x = 50
c = 1
x_verdier <- vector("integer",time)
p_verdier <- vector("integer",time)
p_verdier[1] <- 50; x_verdier[1] <- 50

p_verdier[2] <- 50; x_verdier[2] <- 50
for (t in 3:time){
 
  p <- price(x_verdier[t-1]) + rbinom(1,1,0.02)*20
  x <- (p_verdier[t-1] + p_verdier[t-2])/2
  print(x)
  
  x_verdier[t] <- x
  p_verdier [t] <- p
 
}

plot(x_verdier)