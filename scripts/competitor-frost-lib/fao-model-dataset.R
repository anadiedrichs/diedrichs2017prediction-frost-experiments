#' To install frost package run:
# install.packages("devtools")
# library(devtools)
# install_github("anadiedrichs/frost")

# notebook published --> http://rpubs.com/adiedrichs/410193

# variables: colnames o conjunto de variables del dataset

# var: nombre variable a predecir,ejemplo *_tmin
vars.del.sensor <- function(var,variables,dataset_tmin_chaar=FALSE)
{
  v <- variables
  sensor <- unlist(strsplit(var,split=".",fixed = TRUE))[1]
  if(dataset_tmin_chaar==TRUE) sensor <- paste(sensor,".",sep="") # esto es lo diferente, por bug #
  
  vars <- v[grepl( sensor, v, fixed = TRUE)] # extraigo todas las variables relacionadas con sensor
  #vars <- vars[-length(vars)] # quito la última variable min_t
  return(vars)
}
library(caret)
source("../dataset-processing.R")
source("../metrics.R")

dataset <- get.dataset("dacc")

data <- dataset$data

#dividir dataset en entrenamiento y testeo.
### Training set y test dataset
# porcentaje para train set split
porc_train = 0.68
until <- round(nrow(data)*porc_train)
#' consideramos estacion junin
print(dataset$pred[1])
vars <- vars.del.sensor(dataset$pred[1],colnames(data))
training.set = data[1:until-1, vars] # This is training set to learn the parameters
test.set = data[until:nrow(data), vars]

library(frost)
#' Maldonado
#' 

dewpoint <- calcDewPoint(training.set$junin.humedad_med,training.set$junin.temp_med,mode = "B")
dw.test <- calcDewPoint(test.set$junin.humedad_med,test.set$junin.temp_med,mode = "B")

model.mza <- buildMdz(dw=dewpoint, tempMax=training.set$junin.temp_max, tmin=training.set$junin.temp_min)

# espero un arreglo de valores. si da error, deberé usar sapply.
predmza <- predMdz(dw = dw.test, tempMax = test.set$junin.temp_max, model=model.mza)

evaluate(predmza,test.set$junin.temp_min)

plot(predmza,test.set$junin.temp_min)

#' FAO modelo
#' 
model.FAO <- buildFAO(dw=dewpoint,temp = training.set$junin.temp_med,tmin=training.set$junin.temp_min)

# espero un arreglo de valores. si da error, deberé usar sapply.
pred <- predFAO(model=model.FAO,t=test.set$junin.temp_med,dw=dw.test)
# comparar resultados
evaluate(pred,test.set$junin.temp_min)

plot(pred,test.set$junin.temp_min)


#' random forest
#' 
library(readr)
dacc_junin_rf <- read_csv("dacc--junin.temp_min--normal--all--1--rf--Y-vs-Y_pred.csv")
evaluate(dacc_junin_rf$y_pred,dacc_junin_rf$y_real)
plot(dacc_junin_rf$y_pred,dacc_junin_rf$y_real)

dacc_junin_bn <- read_csv("dacc--junin.temp_min--normal--all--1--bnReg--Y-vs-Y_pred.csv")
evaluate(dacc_junin_bn$y_pred,dacc_junin_bn$y_real)
plot(dacc_junin_bn$y_pred,dacc_junin_bn$y_real)

#' ignorar desde aqui abajo
### AHORA dejando solo los dias claros/despejados 
#' load dataset 
#library(readr)
#df <- read_csv("~/phd-repos/datasets/darksky/competitor-frost-lib-iot-journal-fao-maldonado/dataset-FAO-test.csv", 
#                             col_types = cols(time = col_character(), 
#                                              time_dw_temp = col_character()))

#train <- training.set[which(training.set$icon %in% c("clear-day","partly-cloudy-day")),]
#test <-  test.set[which(test.set$icon  %in% c("clear-day","partly-cloudy-day")),]

