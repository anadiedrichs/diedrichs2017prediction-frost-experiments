#' To install frost package run:
# install.packages("devtools")
# library(devtools)
# install_github("anadiedrichs/frost")

#' Get sunset time for each location
#' between 2001-01-01 and 2016-02-10
#' 
library(readr)
library(httr)
library(lubridate)
library(frost)

desc_estaciones_dacc <- read_csv("desc-estaciones-dacc.csv")

source("../dataset-processing.R")
dataset <- dacc_v2()
dates <- dataset$data$date

prefix <- "https://api.sunrise-sunset.org/json?formatted=0&"

#counter <- 0
for(est in desc_estaciones_dacc$var)
{
  print(est)
  latt <- desc_estaciones_dacc[which(desc_estaciones_dacc$var==est),]$Lat
  longg <- desc_estaciones_dacc[which(desc_estaciones_dacc$var==est),]$Long
  ss <- paste("lat=",latt,"&lng=",longg,sep = "")
  print(ss)
  #crear data frame vacio
  data.times <- data.frame(tsunset=as.character(),
                           ts2=as.character(),
                           ts2round =as.character(),
                           ts2rounddate=as.character(),
                           Tmin=as.double(),
                           Tmax=as.double())

  for(index.day in 4391:length(dates))
  {
    print(dates[index.day])
    #counter <- counter +1
    sufix <- paste(ss,"&date=",format(dates[index.day]),sep = "")
    print(sufix)
    
    r <- GET(paste(prefix,sufix,sep=""))
    sunset <- content(r)$results$sunset
    print(sunset)
    # convertir a time
    # restar dos horas
    
    sunset_t = with_tz(ymd_hms(sunset),"America/Buenos_Aires")
    d_2 = sunset_t + dhours(2)
    print(sunset_t)
    print(d_2)
    print(round_date(d_2,unit="hour"))
    
    #if(counter>=10){break;}

    # crear fila
    row <- cbind(tsunset=as.character(sunset_t),
                 ts2=as.character(d_2),
                 ts2round =as.character(round_date(d_2,unit="hour")),
                 ts2rounddate=as.character(date(round_date(d_2,unit="hour"))) )
    
    # añadir fila a dataset
    data.times <- rbind(data.times,row)
  }
  # obtener temperatura y dew point a esas horas
  estacion <- strsplit(est,"[.]")[[1]][1]
  # obtener minima de dataset dacc_v2 junin.temp_min
  mins <- dataset$data[,paste(estacion,".temp_min",sep = "")]
  # obtener máxima de dataset dacc_v2 junin.temp_max
  maxs <- dataset$data[,paste(estacion,".temp_max",sep = "")]
  #TODO añadir columnas al dataset
  # añadir status viento y status nubosidad (despejado o no)
  data.times <- cbind(data.times,Tmin=mins,Tmax=maxs)
  # guardar dataset con nombre de estacion/variable predictora
  write.csv(data.times,file=paste(estacion,".csv",sep = ""))
  
}
