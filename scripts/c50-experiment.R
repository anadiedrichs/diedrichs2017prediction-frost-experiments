library(caret)
library(C50)

RMSE = function(m, o){  sqrt(mean((m - o)^2)) } #tested
rsq <- function(x, y){ summary(lm(y~x))$r.squared } #tested
MAE <- function(m, o){  mean(abs(m - o))} #tested

evaluate <- function(pred, obs) #tested
{
  #"RMSE","r2","Sensitivity","Acc","Precision"
  rmse <- round(RMSE(pred,obs),2)
  r2 <- round(rsq(obs,pred),2)
  breaks <- c(-20,0,50) # caso Helada y no helada
  y <- cut(obs, breaks = breaks)
  y_pred <- cut(pred, breaks = breaks)
  sens <- round(sensitivity(y_pred,y),2)
  spec <- round(specificity(y_pred, y),2)
  p <- round(precision(y_pred,y),2)
  c <- confusionMatrix(y_pred,y)
  acc <- round(c$overall["Accuracy"],2)
  print(c)
  return(list(rmse = rmse, r2 = r2, sens= sens, spec= spec, prec= p, acc= acc))
}

evaluate.discreto <- function(pred, obs) #tested
{
  y <- obs
  y_pred <- pred
  sens <- round(sensitivity(y_pred,y),2)
  spec <- round(specificity(y_pred, y),2)
  p <- round(precision(y_pred,y),2)
  c <- confusionMatrix(y_pred,y)
  acc <- round(c$overall["Accuracy"],2)
  print(c)
  return(list( sens= sens, spec= spec, prec= p, acc= acc))
}
# var: nombre variable a predecir,ejemplo *_tmin
# variables: colnames o conjunto de variables del dataset
vars.del.sensor <- function(var,variables,dataset_tmin_chaar=FALSE)
{
  v <- variables
  sensor <- unlist(strsplit(var,split=".",fixed = TRUE))[1]
  if(dataset_tmin_chaar==TRUE) sensor <- paste(sensor,".",sep="") # esto es lo diferente, por bug #

  vars <- v[grepl( sensor, v, fixed = TRUE)] # extraigo todas las variables relacionadas con sensor
  vars <- vars[-length(vars)] # quito la última variable min_t
  return(vars)
}


source("dataset-processing.R")
dataset <- dacc_v2()
sensores <- dataset$data[-1]
pred_sensores_base <- dataset$pred

# Por cada estacion

# por cada T

# config normal o SMOTE

# probar config solo y config TODOS

#'
#' probar C50 default
#' probar C50 con cross validation
# probar C50 con cross validation y boosting



#' Pequeño experimento estacion junin para T = 1
aux <- desfasar.dataset.T(1,sensores, pred_sensores_base)
pred_sensores <<- aux$vars # pred_sensores variable global
df <- aux$data

df <- df[,1:31]
y <- "junin.temp_min_t"


# porcentaje para train set split
porc_train = 0.68

#' ### Training set y test dataset
df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
until <- round(nrow(df)*porc_train)

df[which(df[,y] <= 0),y] <- "frost" 
df[which(df[,y] != "frost"),y] <- "nofrost" 
df[,y] <- as.factor(df[,y])

training.set = df[1:until-1, ] # This is training set to learn the parameters
test.set = df[until:nrow(df), ]



#' Usando variables vecinas
treeModel1 <- C5.0(x = training.set[,-31], y= training.set$junin.temp_min_t,control = C5.0Control(winnow = TRUE))
pred <- predict.C5.0(treeModel1,test.set)
#pred.num <- as.numeric(levels(pred))[pred]
evaluate.discreto(pred,test.set[,y])

#' Usando sólo variables del sensor mismo
#'
vv <- vars.del.sensor(y,colnames(training.set))
treeModelSolo <- C5.0(x = training.set[,vv], y= as.factor(training.set$junin.temp_min_t))
pred <- predict.C5.0(treeModelSolo,test.set)
#pred.num <- as.numeric(levels(pred))[pred]
evaluate.discreto(pred,test.set[,y])

#' Probamos con boosting
#'
treeModel2 <- C5.0(x = training.set[,-31], y= as.factor(training.set$junin.temp_min_t), trials=50)
pred <- predict.C5.0(treeModel2,test.set)
#pred.num <- as.numeric(levels(pred))[pred]
evaluate.discreto(pred,test.set[,y])

#' Probamos etiqueta binaria en vez de etiquetas multiples
breaks <- c(-20,0,50) # caso Helada y no helada
training.set[,y] <- cut(training.set[,y], breaks = breaks)
test.set[,y] <- cut(test.set[,y], breaks = breaks)

# modelo simple todas las variables
treeModelDisc1 <- C5.0(x = training.set[,-31], y= training.set$junin.temp_min_t)
pred <- predict.C5.0(treeModelDisc1,test.set)
evaluate.discreto(pred,test.set[,y])

# modelo sólo con variables de la estacion
treeModelDisc2 <- C5.0(x = training.set[,vv], y= training.set$junin.temp_min_t)
pred <- predict.C5.0(treeModelDisc2,test.set)
evaluate.discreto(pred,test.set[,y])

#' modelo con SMOTE
#'

#' modelo con cross validation
#'
