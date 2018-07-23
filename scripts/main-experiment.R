
#' reingeniring experiment scripts using CARET library
#' 
library(caret)
porc_train = 0.68
#' TODO  LO SIGUIENTE ES LA IDEA BORRADOR PARA ENTRENAR LOS MODELOS
#' 

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
  c <- confusionMatrix(y_pred,y, mode = "everything")
  acc <- round(c$overall["Accuracy"],2)
  print(c)
  return(list(rmse = rmse, r2 = r2, sens= sens, spec= spec, prec= p, acc= acc))
}

source("../dataset-processing.R")
dataset <- dacc_v2()
sensores <- dataset$data[-1]
pred_sensores_base <- dataset$pred
previous_days <- 1:2 # cambiar luego de 1:4
#aux <- desfasar.dataset.T(1,sensores, pred_sensores_base)
#pred_sensores <<- aux$vars # pred_sensores variable global
#df <- aux$data

library(caret)
set.seed(3456)
# si no arranco de cero no considera la primera fila, por eso el cero
trainIndex <- createDataPartition(0:nrow(sensores), p = porc_train, 
                                  list = FALSE, times = 1) 

training.set <- as.data.frame(sensores[ trainIndex,])
test.set  <- as.data.frame(sensores[-trainIndex,])

breaks <- c(-20,0,50) # caso Helada y no helada

X <- training.set[,-which(colnames(sensores) %in% pred_sensores_base)]
lista <- list()
pred_sensores_base <- pred_sensores_base[1:2]
# por cada una de las estaciones a predecir
print(Sys.time())
init <- Sys.time()
for(varpred in pred_sensores_base)
{
  Y <- as.numeric(training.set[,varpred])
  print(varpred)
  # por cada T
  for(T in previous_days)
  {
    print(T)
    #timeSlicesTrain <- createTimeSlices(1:nrow(training.set),initialWindow = T,horizon = 1,fixedWindow = TRUE)
    my.train.control <- trainControl(method = "cv", number = 5, initialWindow = T, horizon = 1, fixedWindow = TRUE)
    model <- train(x=X,y=Y,method="rf",trControl = my.train.control, metric="RMSE",tuneLength=5,importance=T)
    #plot(model)
    print(model)
    varImp(model)
    # agregar modelo a una lista 
    name <- paste(varpred,"rf",T,sep="-")
    
    lista[[name]] <- model
    #' random forest para clasificacion
    #' 
    y.disc <- cut(Y, breaks = breaks)
    data <- cbind(X,y.disc)
    model.rf.class <- train(x=X,y=y.disc,method="rf",trControl = my.train.control, metric="Accuracy",tuneLength=5,importance=T)
    
    #' logistic regresion 
    #' este método puede requerir formula, data
    
    model.lg <- train(y.disc ~ ., data = data, method="glm", family="binomial",trControl = my.train.control, metric="Accuracy")
    summary(model.lg)
    varImp(model.lg)
  }
  
}
end <- Sys.time()
print(end-init)

resamps <- resamples(lista)
summary(resamps)
bwplot(resamps)
dotplot(resamps)

#' a evaluar resultados en el conjunto de testeo.
mm <- lista[[1]]
pred <- predict(mm,test.set)
real <- test.set$junin.temp_min

evaluate(pred, real)

y <- cut(real, breaks = breaks)
y_pred <- cut(pred, breaks = breaks)
dat <- data.frame(obs = y, pred= y_pred)
classes <- as.character(unique(y_pred))
twoClassSummary(dat, lev = classes)