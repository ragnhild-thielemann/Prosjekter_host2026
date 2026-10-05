

lommebok <- c(100,50,20,10,5,1)

pris <- 49

betaling <- c()


money <- function(pris){
for (mynt in lommebok){
  
  while(pris %/% mynt>0) {
    heltall = pris%/% mynt
    pris = pris %% mynt
    betaling = c(betaling,rep(mynt,heltall))
  }}
  return (length(betaling))
  }


for (p in 1:100){
  en_is = money(p)
  to_is = money(2*p)
  tre_is = money(3*p)
  if (en_is == 4 & to_is == 6 & tre_is == 2){
  print(p)}
  
  
}



