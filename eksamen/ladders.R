
library(readr)
library(tidyverse)
set.seed(67)
ladder <- read_csv("eksamen/4170_2021_ladders.csv")
start <- ladder$start
end <- ladder$end
spille_spillet <- function() {
  plass <- 1
  kast <- 0
  while (plass < 90){
    terning <- sample(seq(1,6,1),size = 1)
    plass <- plass + terning
    kast <- kast + 1
  }
  return(kast)
}
spille_spillet_stiger <- function() {
  plass <- 1
  kast <- 0
  while (plass < 90){
    terning <- sample(seq(1,6,1),size = 1)
    plass <- plass + terning
    if (plass %in% ladder$start){
      ladder_plass = ladder |>
        filter(start == plass) 
      plass = ladder_plass$end
    }
    kast <- kast + 1
  }
  return(kast)
}

spille_spillet_stiger_begge_veier<- function() {
  plass <- 1
  kast <- 0
  while (plass < 90){
    terning <- sample(seq(1,6,1),size = 1)
    plass <- plass + terning
    if (plass %in% ladder$start){
      ladder_plass = ladder |>
        filter(start == plass) 
    
      plass = ladder_plass$end
    } 
    if (plass %in% ladder$end){
      ladder_plass = ladder |>
        filter(end == plass) 
      
      plass = ladder_plass$start
    }
    kast <- kast + 1
  }
  return(kast)
}



# konstant funskjon for å spille spillet
simuleringer <- function(spill,n){
  gjennomsnitt <- vector("numeric",n)
  for (i in 1:n){
    gjennomsnitt [i] = spill()
  }
  return((gjennomsnitt))
  
}

n <- 10000
t <- tibble(orign = simuleringer(spille_spillet,n), envei = simuleringer(spille_spillet_stiger,n), tovei = simuleringer(spille_spillet_stiger_begge_veier,n))


means <- t |>
  summarise(orign = mean(orign), envei = mean(envei), tovei = mean(tovei))

means

p1 <- t |>
  pivot_longer(cols = everything(),
               names_to = "regel",
               values_to = "kast")|>
  ggplot()+
  
  geom_qq(aes(sample = kast, color = regel))
  
p1
View(t)
