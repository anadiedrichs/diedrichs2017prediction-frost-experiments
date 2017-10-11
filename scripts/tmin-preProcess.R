#' El objetivo de este documento es probar una red bayesiana para realizar predicción sobre los datos de sensores. 
#' Para ello aprendemos una red bayesiana y ajustamos sus parámetros, considerando los datos de entrada como continuos 
#' (no realizamos discretización), por eso veremos que se samplean la distribución de cada nodo/variable a una gaussiana.

#' El dataset utilizado es de valores diarios de cuatro estaciones meteorologicas del DACC. Las variables diarias son:
#' temperatura (máxima, minima y media), y humedad (maxima, minima y media)
#' 
#' 5/10/2017 PROBAR ESTE CODIGO Y TAMBIEN GENERACION HTML

#' ### Preprocesando el dataset
#' 
#' 
source("bnlearn-utils.R")
set.seed(147)
library(readr)
sensores <- read_csv("~/phd-repos/tmin/bnlearn/data/sensores.csv")

#' Variables del dataset
colnames(sensores)
#' Una vista breve

head(sensores)
#' Cantidad de filas y columnas
#' 
cat("Rows: ",nrow(sensores)," Columns: ",ncol(sensores))

#' como denomino a las variables que quiero predecir


pred_sensores = c("S10.min_t","S11.min_t","S12.min_t","S13.min_t","S14.min_t","S15.min_t","S16.min_t","S18.min_t", "S19.min_t","S1.min_t","S20.min_t","S2.min_t","S3.min_t","S4.min_t","S5.min_t",
                  "S6.min_t","S7.min_t","S8.min_t","S9.min_t")
pred_sensores


#' Columnas a borrar, en este caso la del timestamp o fechas
erase <- colnames(sensores)[1]
erase
sensores <- sensores[,-which(names(sensores) %in% erase)]
colnames(sensores)
#' Procedemos a armar un dataset con las variables, colocando los datos de hace dos días, 
#'  luego hace un día y luego día presente. Por ello, desfazamos el dataset para que 
#'  queden primero las variables en T-2, T-1 y luego en t o tiempo presente.

sensores_T_2 <- sensores[1:(nrow(sensores)-2),] 
sensores_T_1 <- sensores[2:(nrow(sensores)-1),] # no incluyo la primera fila
sensores_t <- sensores[3:nrow(sensores),] 

#' renombro las columnas 
colnames(sensores_T_2) <- paste(colnames(sensores_T_2),"_T_2",sep="")
colnames(sensores_T_1) <- paste(colnames(sensores_T_1),"_T_1",sep="")
colnames(sensores_t) <- paste(colnames(sensores_t),"_t",sep="")

#' del tiempo presente solo me interesa la temperatura mínima, las que quiero predecir
sensores_t <- sensores_t[,pred_sensores]

#' creo dataset de datos de T-2, T-1 y t
df <- cbind.data.frame(sensores_T_2,sensores_T_1,sensores_t)

#' nombres de las variables a usar para entrenar/testear
colnames(df)
#' nro de ejemplos
nrow(df)
#' nro de variables
ncol(df)

#' black list de aristas
#' 
bl <- get_blacklist(pred_sensores)

#' white list de aristas, lista de arcos que si o si tienen que ir en la red Bayesiana. 
#' Consideramos arcos dirigidos desde las variables _T_1 y _T_2 hacia el t de un mismo sensor
#' 

wl <- get_whitelist(pred_sensores,colnames(df),dataset_tmin_chaar = TRUE)

#' ### Training set y test dataset
#' 
#' bl
#' 
df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
until <- round(nrow(df)*.67) 
training.set = df[1:until, ] # This is training set to learn the parameters
test.set = df[until:nrow(df), ]
until

#' Librería para aprendizaje de redes bayesianas
library(bnlearn) 
#' Aprendemos una red bayesiana usando el algoritmo hc y pasando como restricciones las white y black lists
#' 
start_time <- Sys.time()
print(start_time)
res = hc(training.set, whitelist=wl,blacklist = bl) # , cluster = cl) # no funciona esta funcion de cluster
end_time <- Sys.time()
print(end_time)
print(end_time - start_time)

#' guardo modelo para más análisis o corridas posteriores
#' 
save(res, file = paste(file="hc-tminchaar-t2",Sys.time(),".RData",sep=""))
#' Aprendizaje de parametros
#' 
start_time <- Sys.time()
fitted = bn.fit(res, training.set)     # learning of parameters
end_time <- Sys.time()
end_time - start_time

#' Mostramos los parámetros para los nodos que nos interesan predecir
#' 
fitted[pred_sensores]

#' Imprimimos algunos plots de bn.fit de una variable 
#' 
bn.fit.qqplot(fitted[[ncol(df)]])
bn.fit.xyplot(fitted[[ncol(df)]])
bn.fit.histogram(fitted[[ncol(df)]])

#' Markov blanket de las variables de interés para predecir
#' 
for(i in 1:length(pred_sensores))
{
  cat("Markov blanket of ",pred_sensores[i],"\n")
  print(mb(res,pred_sensores[i]))
}

#' Predicciones, evaluación en conjunto de testeo, caso regresión predicción temperaturas
#' 
df_res <- errors_regression(pred_sensores, fitted, test.set, verbose = FALSE)
  
df_res

#' plots de R squared
#' 
#' 
r2_plots_inline(pred_sensores, fitted, test.set)

#' llamar confusionMatrix de caret, pasar primero "a factor of predicted classes, then a factor
#'  of classes to be used as the true results

breaks.binario <- c(-10,0,50) # caso Helada y no helada
my.breaks <- c(-10,-5,0,2,5,10,50)

#' ### Caso binario: helada o no helada
#' 

conf_matrix_binario = conf_matrix(fitted,pred_sensores,test.set, breaks.binario)


#' ### Evaluación de predicción en rangos de temperaturas
#' 

conf_matrix_temp = conf_matrix(fitted,pred_sensores,test.set, my.breaks)
    
#' ## Test de predicción con randomForest
#' 

library(randomForest)
library(miscTools)
library(ggplot2)


for(i in 1:length(pred_sensores))
{
  # determinar la variable predictora
  y_label <- pred_sensores[i]
  df2 <- df
  #' renombro variable predictora por y, para facilitar formula 
  colnames(df2)[which(colnames(df2)==y_label)] <- "y"
  #' quito las otras variables predictoras, ya que solo analizaré la que se encuentre en y_label
  #' 
  df2 <- df2[,-which(names(df2) %in% pred_sensores)]
  colnames(df2)
  
  # train y test set
  until <- round(nrow(df2)*.67)
  training.set = df2[1:until, ] # This is training set to learn the parameters
  test.set = df2[until:nrow(df2), ]
  
  
  model <- randomForest(y ~ ., data = training.set, importance = TRUE )
  pred <- predict(model, test.set)
  #View(cbind(pred,test.set$y))
  mse <- mean((test.set$y - pred)^2)
  r2 <- rSquared(test.set$y, test.set$y - pred)
  
  cat("Variable ",pred_sensores[i]," MSE:",mse," Rsquared: ",r2)
  
  #' Plot R_2, valores predichos vs valores reales
  #' 
  
  p <- ggplot(aes(x=actual, y=pred),
              data=data.frame(actual=test.set$y, pred=pred))
  p2 <- p + geom_point() +
    geom_abline(color="red") +
    ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))
  
  plot(p2)
  
  cat("Confusion matrix helada/no helada",pred_sensores[2],"\n")
  
  y <- cut(test.set[,"y"], breaks = breaks.binario)
  y_pred <- cut(pred, breaks = breaks.binario)
  print(confusionMatrix(y_pred,y))
  
  cat("Confusion matrix ",pred_sensores[i],"\n")    
  y <- cut(test.set[, "y"], breaks = my.breaks)
  y_pred <- cut(pred, breaks = my.breaks)
  print(confusionMatrix(y_pred,y))
  
  
}


#' ## Oversampling: generamos más datos en el training.set mediante SMOTE 
#' Para ello dividimos el dataset entre training y testing. En el training  set aplicamos SMOTE.
#' 
#' genero etiquetas: 1 noche de helada y 0 no helada
#' 
#' Como todos los sensores Sxx.min tienen comportamiento similar, puedo aplicar esta etiqueta a todos.
#' 
#' Es decir, cuando la temperatura está bajo cero, todos la están registrando como bajo cero

Y_class <- as.factor(with(df,ifelse(S20.min_t <= 0,1,0)))
hasta <- round(nrow(df)*.67)
hasta

summary(Y_class[hasta:length(Y_class)])
test.set <- df[hasta:nrow(df),]

#' 
library(unbalanced)

#' datos para entrenar
data_smote <- ubBalance(df[1:hasta,],Y_class[1:hasta],type = "ubSMOTE",percOver = 300, percUnder = 150)

#' para visualizar la distribución de las clases (etiquetas)
#' 
summary(data_smote$Y)
training.set <- data_smote$X
#' Procedemos a realizar las predicciones y evaluar el error.
#' 
##' Aprendemos una red bayesiana usando el algoritmo hc y pasando como restricciones las white y black lists
#' 
start_time <- Sys.time()
print(start_time)
res = hc(training.set, whitelist=wl,blacklist = bl) # , cluster = cl) # no funciona esta funcion de cluster
end_time <- Sys.time()
end_time - start_time


save(res, file=paste("hc-tminchaar-smote-t2",Sys.time(),".RData",sep=""))

#' Aprendizaje de parametros
#' 
start_time <- Sys.time()
fitted = bn.fit(res, training.set)     # learning of parameters
end_time <- Sys.time()
end_time - start_time

#' Mostramos los parámetros para los nodos que nos interesan predecir
#' 
fitted[pred_sensores]

bn.fit.qqplot(fitted[[ncol(df)]])
bn.fit.xyplot(fitted[[ncol(df)]])
bn.fit.histogram(fitted[[ncol(df)]])

#' Markov blanket de las variables de interés para predecir
#' 
for(i in 1:length(pred_sensores))
{
  cat("Markov blanket of ",pred_sensores[i],"\n")
  print(mb(res,pred_sensores[i]))
}

#' Predicciones, evaluación en conjunto de testeo, caso regresión predicción temperaturas
#' 
df_res <- errors_regression(pred_sensores, fitted, test.set, verbose = FALSE)

df_res

#' plots de R squared
#' 
#' 
r2_plots_inline(pred_sensores, fitted, test.set)

#' llamar confusionMatrix de caret, pasar primero "a factor of predicted classes, then a factor
#'  of classes to be used as the true results

breaks.binario <- c(-10,0,50) # caso Helada y no helada
my.breaks <- c(-10,-5,0,2,5,10,50)

#' ### Caso binario: helada o no helada
#' 
conf_matrix_binario = conf_matrix(fitted,pred_sensores,test.set, breaks.binario)

#' ### Evaluación de predicción en rangos de temperaturas
#' 
conf_matrix_temp = conf_matrix(fitted,pred_sensores,test.set, my.breaks)

