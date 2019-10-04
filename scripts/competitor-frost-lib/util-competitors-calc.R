library(frost)
library(caret)
library(dplyr)
source("../dataset-processing.R")
source("../metrics.R")

# global variables

dataset <- get.dataset("dacc")

data <- dataset$data
#dividir dataset en entrenamiento y testeo.
### Training set y test dataset
# porcentaje para train set split
porc_train = 0.68
until <- round(nrow(data)*porc_train)

# function to get variables related to the station
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

# return table of results for each station
competitors_calculation <- function(station_name)
{
  
  vars <- vars.del.sensor(station_name,colnames(data)) # variables de la estacion #TODO
  
  training.set = data[1:until-1, vars] # This is training set to learn the parameters
  
  test.set = data[until:nrow(data), vars]
  
  station <- unlist(strsplit(station_name,split=".",fixed = TRUE))[1]
  
  nombres <- training.set %>% dplyr::rename_all(~stringr::str_replace_all(., paste(station,".",sep=""), "")) %>% colnames()
  colnames(training.set) <- nombres
  colnames(test.set) <- nombres
  
  dewpoint <- calcDewPoint(training.set$humedad_med,training.set$temp_med,mode = "B")
  dw.test <- calcDewPoint(test.set$humedad_med,test.set$temp_med,mode = "B")
  
  model.mza <- buildMdz(dw=dewpoint, tempMax=training.set$temp_max, tmin=training.set$temp_min)
  
  # espero un arreglo de valores. si da error, deberé usar sapply.
  predmza <- predMdz(dw = dw.test, tempMax = test.set$temp_max, model=model.mza)
  
  ev.mza <- evaluate(predmza,test.set$temp_min)
  
#  plot(predmza,test.set$temp_min)
  
  model.FAO <- buildFAO(dw=dewpoint,temp = training.set$temp_med,tmin=training.set$temp_min)
  
  # espero un arreglo de valores. si da error, deberé usar sapply.
  predfao <- predFAO(model=model.FAO,t=test.set$temp_med,dw=dw.test)
  # comparar resultados
  ev.fao <- evaluate(predfao,test.set$temp_min)
  
  #plot(pred,test.set$junin.temp_min)
  
  return(list(predmza=predmza,evalMza=ev.mza,predfao=predfao,evalFAO=ev.fao))
}


# to test competitors method above
#cc <- competitors_calculation(dataset$pred[1]) # nombre de variable a predecir!! 

colheader <- c("Station","Method","MAE","r2","RMSE","Recall","Spec","F1")

df <- data.frame(Station=character(),
                 Method=character(),
                 MAE=double(),
                 r2=double(),
                 RMSE=double(),
                 Recall=double(),
                 Specificity=double(),
                 F1=double(),
                  stringsAsFactors = FALSE)

#df <- NULL

for(place in dataset$pred)
{
  station <- unlist(strsplit(place,split=".",fixed = TRUE))[1]
  
  cc <- competitors_calculation(place)
  
  #  row <- data.frame(station,"buildMdz",cc$evalMza$MAE,cc$evalMza$r2,cc$evalMza$rmse,cc$evalMza$sens,cc$evalMza$spec,cc$evalMza$cm$byClass["F1"])
  
  row <- data.frame(Station=station,
                    Method="buildMdz",
                    MAE=cc$evalMza$MAE,
                    r2=cc$evalMza$r2,
                    RMSE=cc$evalMza$rmse,
                    Recall=cc$evalMza$sens,
                    Specificity=cc$evalMza$spec,
                    F1=cc$evalMza$cm$byClass["F1"],
                    stringsAsFactors = FALSE)
  df <- rbind(df,row,stringsAsFactors=FALSE)
  
  row <- data.frame(Station=station,
                     Method="FAO",
                     MAE=cc$evalFAO$MAE,
                     r2=cc$evalFAO$r2,
                     RMSE=cc$evalFAO$rmse,
                     Recall=cc$evalFAO$sens,
                     Specificity=cc$evalFAO$spec,
                     F1=cc$evalFAO$cm$byClass["F1"],
                     stringsAsFactors = FALSE)
  
 # row <- data.frame(station,"FAO",cc$evalFAO$MAE,cc$evalFAO$r2,cc$evalFAO$rmse,cc$evalFAO$sens,cc$evalFAO$spec,cc$evalFAO$cm$byClass["F1"])
  df <- rbind(df,row,stringsAsFactors=FALSE)
  
  # random forest
  rf <- read_csv(paste("dacc--",place,"--normal--all--1--rf--Y-vs-Y_pred.csv",sep=""))
  rfeval <- evaluate(rf$y_pred,rf$y_real)
  #row <- data.frame(station,"RF",rfeval$MAE,rfeval$r2,rfeval$rmse,rfeval$sens,rfeval$spec,rfeval$cm$byClass["F1"])
  row <- data.frame(Station=station,
                    Method="RF",
                    MAE=rfeval$MAE,
                    r2=rfeval$r2,
                    RMSE=rfeval$rmse,
                    Recall=rfeval$sens,
                    Specificity=rfeval$spec,
                    F1=rfeval$cm$byClass["F1"],
                    stringsAsFactors = FALSE)
  
  df <- rbind(df,row,stringsAsFactors=FALSE)
  
  # Bayesian networks
  bn <- read_csv(paste("dacc--",place,"--normal--all--1--bnReg--Y-vs-Y_pred.csv",sep=""))
  bneval <- evaluate(bn$y_pred,bn$y_real)
  #row <- data.frame(station,"BN",bneval$MAE,bneval$r2,bneval$rmse,bneval$sens,bneval$spec,bneval$cm$byClass["F1"])
  row <- data.frame(Station=station,
                    Method="BN",
                    MAE=bneval$MAE,
                    r2=bneval$r2,
                    RMSE=bneval$rmse,
                    Recall=bneval$sens,
                    Specificity=bneval$spec,
                    F1=bneval$cm$byClass["F1"],
                    stringsAsFactors = FALSE)
  
  df <- rbind(df,row,stringsAsFactors=FALSE)
  #plot(dacc_junin_bn$y_pred,dacc_junin_bn$y_real)
  
}

colnames(df) <- colheader
                          
# save .csv file of results
write.csv(df,file="competitors-table.csv")