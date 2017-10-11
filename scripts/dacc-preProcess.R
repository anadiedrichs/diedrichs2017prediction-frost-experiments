#' El objetivo de este documento es probar una red bayesiana para realizar predicción sobre los datos de sensores. 
#' Para ello aprendemos una red bayesiana y ajustamos sus parámetros, considerando los datos de entrada como continuos 
#' (no realizamos discretización), por eso veremos que se samplean la distribución de cada nodo/variable a una gaussiana.

#' El dataset utilizado es de valores diarios de cuatro estaciones meteorologicas del DACC. Las variables diarias son:
#' temperatura (máxima, minima y media), y humedad (maxima, minima y media)
#' 
#' 

#' ### Preprocesando el dataset
#' 
#' 
source("bnlearn-utils.R")

set.seed(147)

library(readr)
sensores <- read_csv("~/phd-repos/tmin/bnlearn/data/estaciones-dacc-diarios.csv")

#' Variables del dataset
colnames(sensores)
#' Una vista breve

head(sensores)
#' Cantidad de filas y columnas
#' 
cat("Rows: ",nrow(sensores)," Columns: ",ncol(sensores))

#' como denomino a las variables que quiero predecir

pred_sensores = colnames(sensores)[c(6,12,18,23,29)]
pred_sensores

#' reemplazo valor erroneo (temperatura máxima por los 780)
#' 
sensores[1632,30:35]
sensores[1632,33] <- 25

#' Columnas a borrar
erase <- colnames(sensores)[1:2]
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

#' del tiempo presente solo me interesa la temperatura mínima, las que quiero predecir
sensores_t <- sensores_t[,pred_sensores]
colnames(sensores_t) <- paste(colnames(sensores_t),"_t",sep="")
pred_sensores <- paste(pred_sensores,"_t",sep="") # renombro variables predictoras

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
wl <- get_whitelist(pred_sensores,colnames(df))

#' ### Training set y test dataset
#' 
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
res = hc(training.set, whitelist=wl,blacklist = bl)
end_time <- Sys.time()
end_time - start_time

#' Aprendizaje de parametros
#' 
start_time <- Sys.time()
fitted = bn.fit(res, training.set)     # learning of parameters
end_time <- Sys.time()
end_time - start_time


#' guardo modelo para más análisis o corridas posteriores
#' 
save(res, file = paste("hc-dacc-t2",Sys.time(),".RData",sep=""))

#' Mostramos los parámetros para los nodos que nos interesan predecir
#' 
fitted[pred_sensores]

bn.fit.qqplot(fitted$las_paredes.Tmin_t)
bn.fit.xyplot(fitted$las_paredes.Tmin_t)
bn.fit.histogram(fitted$las_paredes.Tmin_t)

#' Markov blanket de las variables de interés para predecir
#' 
for(i in 1:length(pred_sensores))
{
  cat("Markov blanket of ",pred_sensores[i],"\n")
  print(mb(res,pred_sensores[i]))
}

#' Predicciones, evaluación en conjunto de testeo
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


#' Test de predicción con randomForest
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
#' genero etiquetas: 1 noche de helada y 0 no helada por cada estación, ya que tienen distintos datos de heladas
#' 
#' Luego evaluamos para cada una como resultó la predicción
#' 

# for(j in 1:length(pred_sensores))
# {
#   
#   sensor <- unlist(strsplit(pred_sensores[i],split=".",fixed = TRUE))[1]
#   vars <- v[grepl( sensor, v, fixed = TRUE)] # extraigo todas las variables relacionadas con sensor
#   #vars <- vars[-length(vars)] # quito la última variable min_t
#   
# }

#' Me enfoco en el caso de la estación junin, por lo que analizaremos solo la predicción sobre la misma....
var_pred <- pred_sensores[4]
Y_class <- as.factor(with(df,ifelse(df[,pred_sensores[4]] <= 0,1,0)))

hasta <- round(nrow(df)*.67)
hasta
summary(Y_class)
summary(Y_class[hasta:length(Y_class)])
test.set <- df[hasta:nrow(df),]

#' 
library(unbalanced)

#' datos para entrenar
data_smote <- ubBalance(df[1:(hasta-1),],Y_class[1:(hasta-1)],type = "ubSMOTE",percOver = 300, percUnder = 150)

#' para visualizar la distribución de las clases
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

#' guardar modelo 
save(res, file= paste("hc-dacc-smote-t2",var_pred,"-",Sys.time(),".RData",sep=""))

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
df_res <- errors_regression(var_pred, fitted, test.set, verbose = FALSE)

df_res

#' plots de R squared
#' 
#' 
r2_plots_inline(var_pred, fitted, test.set)

#' llamar confusionMatrix de caret, pasar primero "a factor of predicted classes, then a factor
#'  of classes to be used as the true results

breaks.binario <- c(-10,0,50) # caso Helada y no helada
my.breaks <- c(-10,-5,0,2,5,10,50)

#' ### Caso binario: helada o no helada
#' 

conf_matrix_binario = conf_matrix(fitted,var_pred,test.set, breaks.binario)


#' ### Evaluación de predicción en rangos de temperaturas
#' 

conf_matrix_temp = conf_matrix(fitted,var_pred,test.set, my.breaks)


