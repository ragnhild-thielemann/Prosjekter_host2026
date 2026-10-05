library(readxl)

# 1. Definer banen til zip-filen og filen inni
zip_sti <- "eksamen/eksamen_2024/subscribers (1).zip"

# 2. Pakk ut Excel-filen til en midlertidig mappe
oppakkede_filer <- unzip(zip_sti, exdir = tempdir())

antall = 3
total <- tibble()
kunde <- vector("character", antall)
number <- vector("integer", antall)
start <- vector ("integer", antall)
end <- vector("character", antall)
for (i in 1:antall){
  fil <- as.character(paste0("s",i,".xlsx"))
  fil_sti <- oppakkede_filer[grep(fil, oppakkede_filer)]

  sheet <- read_excel(as.character(fil_sti))
  View(sheet)
  kunde[i] <- sheet[4,2]
  number[i] <- sheet[5,2]
  start[i]<- (sheet[7,2])
  end[i] <- (sheet[7,5])
  
  
  }

a <- parse_date_time (45405)
total <- tibble(kunde = kunde, number = number, start = start, end = end)
View(total)
