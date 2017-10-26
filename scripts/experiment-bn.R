#'
#' 
#' FORMATO NOMBRE DE ARCHIVO SALIDA
#' <dataset>-<variable>-<T period en numero>-<S o N, S para SMOTE y N normal>-<H humedad incluida, N no incluida>-<algoritmo>-<score>


#' Librería para aprendizaje de redes bayesianas
library(bnlearn)

source("bnlearn-utils.R")
source("dataset-processing.R")

library(caret) # para confusion matrix
library(forecast) # para metodo accuracy

library(doParallel)
library(unbalanced)
set.seed(147)

packages <- c("bnlearn","caret","forecast","unbalanced","readr","xts","timeDate")
# si quiero guardar los dataset desfasados para ser usados por otras librerías.

SAVE_DATASET <- TRUE
ownVariables <- FALSE
split.train <- 0.68 # porcentaje de datos en el dataset de entremaniento

################

dataset <- c("dacc","dacc-temp","dacc-spring") #"dacc",
config.train <-c("normal","smote")
#' VEC si considera solo informacion de la misma estacion ("solo") o todas las variables
vec <- c("vec") #TODO ,"solo"
alg <- c("hc","tabu")
#' T cuantos dias anteriores tomamos
period <- c(1,2,3,4,5)

#' the multivariate Gaussian log-likelihood (loglik-g) score.
#' the corresponding Akaike Information Criterion score (aic-g).
#' the corresponding Bayesian Information Criterion score (bic-g).
#' a score equivalent Gaussian posterior density (bge).
score <- c("bic-g","loglik-g","aic-g","bge")

#' m = model or predicted values
#' o = observed or real values
#' 
RMSE = function(m, o){  sqrt(mean((m - o)^2)) } #tested
rsq <- function(x, y){ summary(lm(y~x))$r.squared } #tested


#' pred & obs are vectors or arrays with the same length
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
  return(list(rmse = rmse, r2 = r2, sens= sens, spec= spec, prec= p, acc= acc))
}


trainingNormal <- function(df,alg,sc, file.name, pred_sensores, fila,p=0.68)
{
  
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc)
  res = rr$model
  save(res, file = paste(file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  #'
  start_time <- Sys.time()
  fitted = bn.fit(rr$model, training.set)     # learning of parameters
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
  save(fitted, file = paste(file.name,"--fitted.RData",sep="")) #,Sys.time()
  #' guardo modelo para más análisis o corridas posteriores
  #'
  
  for(p in 1:length(pred_sensores))
  {
    # nombre variable a predecir
    f <- paste(file.name,pred_sensores[p],sep="--")
    #row <- c(row,pred_sensores[p]) 
    # prediccion
    pred = predict(fitted, pred_sensores[p], test.set)
    # guardar csv con valor real vs predicho
    dataset <- as.data.frame(cbind(test.set[pred_sensores[p]],pred))
    colnames(dataset)<- c("y_real","y_pred")
    write.csv(x = dataset,file = paste("./results/",f,"--Y-vs-Y_pred.csv",sep = ""))
    #evaluar resultados en testeo
    eee <- evaluate(pred,test.set[,pred_sensores[p]])
    #guardo detalles del experimento
    row <- paste(fila,rr$time,fitted.time,NA,nrow(training.set),NA,pred_sensores[p],
                 eee$rmse,eee$r2,eee$sens,eee$acc,eee$prec,eee$spec,sep=",")
    write(row, file=RESUMEN, append = TRUE)
    
  }
    
  return()
}

trainingSMOTE <- function(df,alg,sc, file.name, var, fila,p=0.68)
{
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  nfrostorig <- length(training.set[training.set[,var] <= 0,var])
  
  #' Me enfoco en el caso de una sola estación junin, por lo que analizaremos solo la predicción sobre la misma....
  Y_class <- as.factor(with(df,ifelse(df[,var] <= 0,1,0)))
  summary(Y_class)
  summary(Y_class[until:length(Y_class)])

  #' datos para entrenar
  data_smote <- ubBalance(training.set,Y_class[1:(until-1)],type = "ubSMOTE",percOver = 300, percUnder = 150)
  
  #guardar este dataset desfasado en T
  if(SAVE_DATASET){  write.csv(data_smote$X,paste(paste(file.name,var,"smote-dataset.csv",sep="--"))) }
  
  #' para visualizar la distribución de las clases
  #' 
  summary(data_smote$Y)
  training.set <- data_smote$X
  nfrost <- length(data_smote$X[data_smote$X[,var] <= 0,var])
  
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc)
  res = rr$model
  save(res, file = paste(file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  #'
  start_time <- Sys.time()
  fitted = bn.fit(rr$model, training.set)     # learning of parameters
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(end_time-start_time),3)
  save(fitted, file = paste(file.name,"--fitted.RData",sep="")) #,Sys.time()
  print(file.name)

  # nombre variable a predecir
  f <- paste(file.name,var,sep="--")
  #row <- c(row,pred_sensores[p]) 
  # prediccion
  pred = predict(fitted, var, test.set)
  # guardar csv con valor real vs predicho
  dataset <- as.data.frame(cbind(test.set[var],pred))
  colnames(dataset)<- c("y_real","y_pred")
  write.csv(x = dataset,file = paste("./results/",f,"--Y-vs-Y_pred.csv",sep = ""))
  
  
  if(length(is.na(pred))<1){
  
    #evaluar resultados en testeo
    eee <- evaluate(pred,test.set[,var])
    #guardo detalles del experimento
    row <- paste(fila,rr$time,fitted.time,nfrostorig,nrow(training.set),nfrost,var,
                 eee$rmse,eee$r2,eee$sens,eee$acc,eee$prec,eee$spec,sep=",")
    
  }else{
    # CASO EN el que los parámetros no pueden ser calculados por pocos datos
    # u otro error
    #guardo detalles del experimento
    row <- paste(fila,rr$time,fitted.time,nfrostorig,nrow(training.set),nfrost,var,
                 NA,NA,NA,NA,NA,NA,sep=",")
    
  }
  write(row, file=RESUMEN, append = TRUE)
  
  return()
}

#' funcion de aprendizaje bayesiano variables continuas
#' probada
learn.bayes <- function(df, wl=NULL,bl=NULL,alg="hc",sc="bic")
{
  time = NULL; res = NULL
  if(!which(sc %in% score)) stop("score inexistente en learn.bayes")
  
  if(alg=="hc")
  {
    start_time <- Sys.time()
    res = hc(df, whitelist=wl,blacklist = bl, score = sc)
    end_time <- Sys.time()
    time = round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
    
  }else if(alg=="tabu"){
    
    start_time <- Sys.time()
    res = tabu(df, whitelist=wl,blacklist = bl, score = sc)
    end_time <- Sys.time()
    time = round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
    
  }
  
  return(list(model=res, time = round(as.numeric(time),3)))
  
}


cl <- makeCluster(4) # colocar 12 en server
#registerDoParallel(cl)  

library(doMC)
registerDoMC(4)

RESUMEN <<- paste(Sys.time(),"--experimento.csv",sep="")
columnas <- paste("dataset","days","ncol","nrow","config_train","alg","score",
                  "t_run_s"," t_fitted","nfrostorig","ntrain","nfrostsmote","var",
                  "RMSE","r2","Sensitivity","Acc","Precision","Specificity",sep = ",")
#"ntrain", "ntest",
write(columnas,file=RESUMEN)

foreach(j = 1:3,.packages = packages) %dopar% 
#for(j in 2:3) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  sensores <- dd$data[-1] #quito columna date o primer columna
  pred_sensores_base <- dd$pred
  cat("DATASET ",dd$name,"\n")
  
  foreach(t = 1:length(period),.packages = packages) %dopar% 
  #for(t in 1:length(period))
  {
    cat("Period ",t)
    #row <- cbind.data.frame(row,t)
    #file.name <- paste(file.name,t,sep = "--")
    #' Obtengo dataset con variables desfasadas a t dias 
    #'
    aux <- desfasar.dataset.T(t,sensores, pred_sensores_base)
    pred_sensores <- aux$vars
    df <- aux$data
    
    #guardar este dataset desfasado en T
    if(SAVE_DATASET){  write.csv(aux$data,paste(paste(dd$name,t,sep = "--"),"dataset.csv",sep="--"))  }
    
    bl <- get_blacklist(pred_sensores)
    wl <- get_whitelist(pred_sensores,colnames(df))

    foreach(a = 1:length(alg),.packages = packages) %dopar% 
    #for(a in 1:length(alg))
    {
      cat("Alg ",alg[a])
      foreach(s = 1:length(score),.packages = packages) %dopar%   
      #for(s in 1:length(score))
      {
        cat("Score ",score[s])
        foreach(c = 1:length(config.train),.packages = packages) %dopar%        
          #for(c in 1:length(config.train))
          {
            u <- NULL
            #' ### Training set y test dataset
            df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
            file.name <- paste(dd$name,t,config.train[c],alg[a],score[s],sep = "--")
            
            if(config.train[c]=="normal")
            {
              
              fila <- paste(dd$name,t,ncol(df),nrow(df),config.train[c],alg[a],score[s],sep=",")
              
              trainingNormal(df, alg=alg[a],sc=score[s], file.name = file.name, pred_sensores = pred_sensores , fila)
              
            }else if(config.train[c]=="smote")
            {
              
              fila <- paste(dd$name,t,ncol(df),nrow(df),config.train[c],alg[a],score[s],sep=",")
              
              for(p in 1:length(pred_sensores))
              {
                trainingSMOTE(df, alg=alg[a],sc=score[s], file.name = file.name, var = pred_sensores[p], fila )
              }
            }
          
          }# por training config 
        }# for por score
      }# for por algoritmo  
  }#for por T
}# for por dataset

#system.time(foreach(i=1:10000) %dopar% sum(tanh(1:i)))  
#   user  system elapsed 
#  3.392   0.193   3.664 
stopCluster(cl)

# no correr!! es solo codigo copypaste
testeo.results <- function()
{
  
  #TODO 
  #levantar archivos con Y_Ypred
  # calcular resultados por cada archivo
  
  
  #' Predicciones, evaluación en conjunto de testeo
  #'
  
  df_res <- errors_regression(pred_sensores, fitted, test.set, verbose = FALSE)
  
  print(df_res)
  
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
  
}
