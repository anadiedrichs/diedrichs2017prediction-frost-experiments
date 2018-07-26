#' To install frost package run:
# install.packages("devtools")
# library(devtools)
# install_github("anadiedrichs/frost")

#' Get sunset time for each location
#' between 2001-01-01 and 2016-02-10
#' 
desc_estaciones_dacc <- read_csv("desc-estaciones-dacc.csv")

source("../dataset-processing.R")
dataset <- dacc_v2()
dates <- dataset$data$date

prefix <- "https://api.sunrise-sunset.org/json?formatted=0&"

counter <- 0
for(est in desc_estaciones_dacc$var)
{
  print(est)
  latt <- desc_estaciones_dacc[which(desc_estaciones_dacc$var==est),]$Lat
  longg <- desc_estaciones_dacc[which(desc_estaciones_dacc$var==est),]$Long
  ss <- paste("lat=",latt,"&lng=",longg,sep = "")
  print(ss)
  #crear data frame vacio
  for(index.day in 1:length(dates))
  {
    print(dates[index.day])
    counter <- counter +1
    sufix <- paste(ss,"&date=",format(dates[index.day]),sep = "")
    print(sufix)
    
    r <- GET(paste(prefix,sufix,sep=""))
    sunset <- content(r)$results$sunset
    print(sunset)
    
    sunset_t = with_tz(ymd_hms(sunset),"America/Buenos_Aires")
    d_2 = sunset_t - dhours(2)
    print(sunset_t)
    print(round_date(sunset_t,unit="hour"))
    print(d_2)
    if(counter>=10){break;}
    # convertir a time
    # restar dos horas
    
    # crear fila
    # añadir fila a dataset
  }
  
  # obtener temperatura y dew point a esas horas
  #estacion <- strsplit(est,"[.]")[[1]][1]
  # obtener minima de dataset dacc_v2 junin.temp_min
  #mins <- dataset$data[,paste(estacion,".temp_min",sep = "")]
  # obtener máxima de dataset dacc_v2 junin.temp_max
  #maxs <- dataset$data[,paste(estacion,".temp_max",sep = "")]
  #TODO añadir columnas al dataset
  # guardar dataset con nombre de estacion/variable predictora
}
