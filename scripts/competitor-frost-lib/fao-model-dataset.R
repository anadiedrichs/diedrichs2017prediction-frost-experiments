#' To install frost package run:
# install.packages("devtools")
# library(devtools)
# install_github("anadiedrichs/frost")

# notebook published --> http://rpubs.com/adiedrichs/410193

source("../dataset-processing.R")

dataset <- get.dataset("dacc")

data <- dataset$data

library(caret)
source("../metrics.R")
# porcentaje para train set split
porc_train = 0.68

# var: nombre variable a predecir,ejemplo *_tmin
# variables: colnames o conjunto de variables del dataset
vars.del.sensor <- function(var,variables,dataset_tmin_chaar=FALSE)
{
  v <- variables
  sensor <- unlist(strsplit(var,split=".",fixed = TRUE))[1]
  if(dataset_tmin_chaar==TRUE) sensor <- paste(sensor,".",sep="") # esto es lo diferente, por bug #
  
  vars <- v[grepl( sensor, v, fixed = TRUE)] # extraigo todas las variables relacionadas con sensor
  #vars <- vars[-length(vars)] # quito la última variable min_t
  return(vars)
}

#' load dataset 
#library(readr)
#df <- read_csv("~/phd-repos/datasets/darksky/competitor-frost-lib-iot-journal-fao-maldonado/dataset-FAO-test.csv", 
#                             col_types = cols(time = col_character(), 
#                                              time_dw_temp = col_character()))

#' consideramos estacion junin
est <- "junin"
#dividir dataset en entrenamiento y testeo.
### Training set y test dataset
until <- round(nrow(df)*porc_train)
training.set = df[1:until-1, ] # This is training set to learn the parameters
test.set = df[until:nrow(df), ]


library(frost)

#' FAO modelo
#' 
model.FAO <- buildFAO(dw=training.set$dewPoint,temp = training.set$temperature,tmin=training.set$junin.temp_min)

# espero un arreglo de valores. si da error, deberé usar sapply.
pred <- predFAO(a=model.FAO$a,b=model.FAO$b,c=model.FAO$c,t=test.set$temperature,dw=test.set$dewPoint)
# comparar resultados
evaluate(pred,test.set$junin.temp_min)

plot(pred,test.set$junin.temp_min)
#' Maldonado
#' 
model.mza <- buildMdz(dw=training.set$dewPoint, tempMax=training.set$junin.temp_max, tmin=training.set$junin.temp_min)
# espero un arreglo de valores. si da error, deberé usar sapply.
predmza <- predMdz(dw = test.set$dewPoint, tempMax = test.set$junin.temp_max, K=model.mza$k)

evaluate(predmza,test.set$junin.temp_min)

plot(predmza,test.set$junin.temp_min)
### AHORA dejando solo los dias claros/despejados 

train <- training.set[which(training.set$icon %in% c("clear-day","partly-cloudy-day")),]
test <-  test.set[which(test.set$icon  %in% c("clear-day","partly-cloudy-day")),]
 
model.FAO <- buildFAO(dw=train$dewPoint,temp = train$temperature,tmin=train$junin.temp_min)

# espero un arreglo de valores. si da error, deberé usar sapply.
pred <- predFAO(a=model.FAO$a,b=model.FAO$b,c=model.FAO$c,t=test$temperature,dw=test$dewPoint)
# comparar resultados

evaluate(pred,test$junin.temp_min)

plot(pred,test$junin.temp_min)
#' Maldonado
#' 
model.mza <- buildMdz(dw=train$dewPoint, tempMax=train$junin.temp_max, tmin=train$junin.temp_min)
# espero un arreglo de valores. si da error, deberé usar sapply.
predmza <- predMdz(dw = test$dewPoint, tempMax = test$junin.temp_max, K=model.mza$k)

evaluate(predmza,test$junin.temp_min)

plot(predmza,test$junin.temp_min)