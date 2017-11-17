#' 
#' Experimento mínimo para chequear si con random restart mejora la performance de hc
#' Para ello restringimos la configuracion a la siguiente configuracion:
#' 1) dataset DACC
#' 2) T=1
#' 3) CONFIG SMOTE
#' 4) VARIABLE junin
#' 5) algoritmo hc
#' 6) las 4 score function
#' 
#' 
#' 
#' 
#' Librería para aprendizaje de redes bayesianas
library(bnlearn)

source("../bnlearn-utils.R")
source("../dataset-processing.R")

library(caret) # para confusion matrix
library(forecast) # para metodo accuracy

library(doParallel)
library(unbalanced)

set.seed(147)

RANDOM_RESTART <- 50
OUTPUT.FILE <- "output-bn-randomStarts-" # <- where prints go while cluster is running
FILE.RESULT.NAME <- "--experimento-bn-randomStarts-50.csv"
PATH.MODELS <- "./models_randomRestart/"
PATH.RESULTS <- "./results/"
PAR = TRUE
# si quiero guardar los dataset desfasados para ser usados por otras librerías.
SAVE_DATASET <- FALSE

packages <- c("bnlearn","caret","forecast","unbalanced","readr","xts","timeDate")
split.train <- 0.68 # porcentaje de datos en el dataset de entremaniento

################
# voy a correr solo dacc
dataset <- c("dacc")#,"dacc","dacc-temp") #,"dacc-spring") 
config.train <-c("smote")#,"normal")

# local: configuracion para armar red bayesiana con sólo las variables locales, de la propia estación
#alg <- c("local") 
alg <- c("hc") #,"tabu","local") 

#' T cuantos dias anteriores tomamos
period <- c(1)#,4,5 #1
#' the multivariate Gaussian log-likelihood (loglik-g) score.
#' the corresponding Akaike Information Criterion score (aic-g).
#' the corresponding Bayesian Information Criterion score (bic-g).
#' a score equivalent Gaussian posterior density (bge).
score <- c("bic-g","loglik-g","aic-g","bge")


Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text,..., "\n"))
  cat(msg)
}

trainingNormalOneVar <- function(df,alg,sc, file.name, var, fila,p=split.train)
{
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc,var=var)
  res = rr$model
  save(res, file = paste("./models/",file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  #'
  variables <- c(as.character(unique(wl[which(wl$to== var),"from"])),var)
  start_time <- Sys.time()
  fitted = bn.fit(rr$model, training.set[variables])     # learning of parameters
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
  save(fitted, file = paste("./models/",file.name,"--fitted.RData",sep="")) #,Sys.time()
  #' guardo modelo para más análisis o corridas posteriores
  
  
  # nombre variable a predecir
  f <- paste(file.name,var,sep="--")
  #row <- c(row,pred_sensores[p]) 
  # prediccion
  pred = predict(fitted, var, test.set[variables])
  # guardar csv con valor real vs predicho
  dataset <- as.data.frame(cbind(test.set[var],pred))
  colnames(dataset)<- c("y_real","y_pred")
  write.csv(x = dataset,file = paste(PATH.RESULTS,f,"--Y-vs-Y_pred.csv",sep = ""))
  #evaluar resultados en testeo
  eee <- evaluate(pred,test.set[,var])
  #guardo detalles del experimento
  row <- paste(fila,rr$time,fitted.time,NA,nrow(training.set),NA,var,
               eee$rmse,eee$r2,eee$sens,eee$acc,eee$prec,eee$spec,sep=",")
  write(row, file=RESUMEN, append = TRUE)
  
  
  return()
}

trainingNormal <- function(df,alg,sc, file.name, pred_sensores, fila,p=split.train)
{
  
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc)
  res = rr$model
  save(res, file = paste("./models/",file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  #'
  start_time <- Sys.time()
  fitted = bn.fit(rr$model, training.set)     # learning of parameters
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
  save(fitted, file = paste("./models/",file.name,"--fitted.RData",sep="")) #,Sys.time()
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
    write.csv(x = dataset,file = paste(PATH.RESULTS,f,"--Y-vs-Y_pred.csv",sep = ""))
    #evaluar resultados en testeo
    eee <- evaluate(pred,test.set[,pred_sensores[p]])
    #guardo detalles del experimento
    row <- paste(fila,rr$time,fitted.time,NA,nrow(training.set),NA,pred_sensores[p],
                 eee$rmse,eee$r2,eee$sens,eee$acc,eee$prec,eee$spec,sep=",")
    write(row, file=RESUMEN, append = TRUE)
    
  }
    
  return()
}

trainingSMOTE <- function(df,alg,sc, file.name, var, fila,p=split.train)
{
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  nfrostorig <- length(training.set[training.set[,var] <= 0,var])
  
  #' Me enfoco en el caso de una sola estación junin, por lo que analizaremos solo la predicción sobre la misma....
  Y_class <- as.factor(with(df,ifelse(df[,var] <= 0,1,0)))
  #summary(Y_class)
  #summary(Y_class[until:length(Y_class)])

  #' datos para entrenar
  data_smote <- ubBalance(training.set,Y_class[1:(until-1)],type = "ubSMOTE",percOver = 300, percUnder = 150)
  
  #guardar este dataset desfasado en T
  if(SAVE_DATASET){  write.csv(data_smote$X,paste(paste(file.name,var,"smote-dataset.csv",sep="--"))) }
  
  #' para visualizar la distribución de las clases
  #' 
  summary(data_smote$Y)
  training.set <- data_smote$X
  nfrost <- length(data_smote$X[data_smote$X[,var] <= 0,var])
  
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc,var=var)
  res = rr$model
  save(res, file = paste("./models/",file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  variables <- c(as.character(unique(wl[which(wl$to== var),"from"])),var)
  
  start_time <- Sys.time()
  if(alg=="local"){fitted = bn.fit(rr$model, training.set[variables])}
  else fitted = bn.fit(rr$model, training.set)
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(end_time-start_time),3)
  save(fitted, file = paste("./models/",file.name,"--fitted.RData",sep="")) #,Sys.time()
  
  # nombre variable a predecir
  f <- paste(file.name,var,sep="--")
  #row <- c(row,pred_sensores[p]) 
  # prediccion
  pred = predict(fitted, var, test.set)
  # guardar csv con valor real vs predicho
  dataset <- as.data.frame(cbind(test.set[var],pred))
  colnames(dataset)<- c("y_real","y_pred")
  write.csv(x = dataset,file = paste("./results/",f,"--Y-vs-Y_pred.csv",sep = ""))
  
  
  if(length(which(is.na(pred)==TRUE))<1){ #si la predicción regresó NA, el fitted no pudo ser calculado 
  
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
learn.bayes <- function(df, wl=NULL,bl=NULL,alg="hc",sc="bic",var=NULL)
{
  time = NULL; res = NULL
  if(alg!="local" && !which(sc %in% score)) stop("score inexistente en learn.bayes")
  
  if(alg=="hc")
  {
    start_time <- Sys.time()
    res = hc(df, whitelist=wl,blacklist = bl, score = sc,restart = RANDOM_RESTART ) # RESTART 
    end_time <- Sys.time()
    time = round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
    
  }else if(alg=="tabu"){
    
    start_time <- Sys.time()
    res = tabu(df, whitelist=wl,blacklist = bl, score = sc)
    end_time <- Sys.time()
    time = round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
    
  }else if(alg=="local")
  {
    if(is.null(var) || is.null(wl)) stop("var and wl must not be NULL in learn.bayes function")
    # recuperar variables relacionadas con el sensor
    variables <- c(as.character(unique(wl[which(wl$to== var),"from"])),var)
    res = empty.graph(variables)
    #class(e)
    arcs(res) <- sapply(wl[which(wl$to== var),],as.character)
    time = 0
  }
  
  return(list(model=res, time = round(as.numeric(time),3)))
  
}

#' Caso para manejar entrenamiento configuracion local
config.train.local <- function(config,df,file.name,fila,pred_sensores)
{
    foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% 
    #for(p in 1:length(pred_sensores))
    {
      if(config=="normal"){
        
        trainingNormalOneVar(df, alg="local",sc="ignore", file.name = file.name, var = pred_sensores[p] , fila)
      
      }else if(config=="smote"){

        trainingSMOTE(df, alg="local",sc="ignore",  file.name = file.name, var = pred_sensores[p], fila )
      }
    }
}


if(PAR==TRUE){
  cl <- makeCluster(detectCores(),outfile=paste(OUTPUT.FILE,Sys.time(),".log",sep="")) # 
  registerDoParallel(cl)
}

RESUMEN <<- paste(Sys.time(),FILE.RESULT.NAME,sep="")
columnas <- paste("dataset","days","ncol","nrow","config_train","alg","score",
                  "t_run_s"," t_fitted","nfrostorig","ntrain","nfrostsmote","var",
                  "RMSE","r2","Sensitivity","Acc","Precision","Specificity",sep = ",")
#"ntrain", "ntest",
write(columnas,file=RESUMEN)

foreach(j = 1:length(dataset),.packages = packages) %dopar% # volver 2 como 1 para correr dataset dacc
#for(j in 1:length(dataset)) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  sensores <- dd$data[-1] #quito columna date o primer columna
  pred_sensores_base <- dd$pred
  Log(paste("DATASET ",dd$name,sep = ""))
  
  foreach(t = 1:length(period),.packages = packages) %dopar% 
#  for(t in 1:length(period))
  {
    Log(paste("Period ",period[t]))
    #row <- cbind.data.frame(row,t)
    #file.name <- paste(file.name,t,sep = "--")
    #' Obtengo dataset con variables desfasadas a t dias 
    #'
    aux <- desfasar.dataset.T(period[t],sensores, pred_sensores_base)
    pred_sensores <- aux$vars
    df <- aux$data
    
    #guardar este dataset desfasado en T
    if(SAVE_DATASET){  write.csv(aux$data,paste(paste(dd$name,t,sep = "--"),"dataset.csv",sep="--"))  }
    
    bl <<- get_blacklist(pred_sensores)
    wl <<- get_whitelist(pred_sensores,colnames(df))
    print(wl)
    foreach(a = 1:length(alg),.packages = packages) %dopar% 
    #for(a in 1:length(alg))
    {
      Log("Alg ",alg[a])

     foreach(c = 1:length(config.train),.packages = packages) %dopar%  # 
     # for(c in 1:length(config.train))
      {
        u <- NULL
        #' ### Training set y test dataset
        df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
        
        if(alg[a]=="local") # ACA NO ENTRA
        {
          file.name <- paste(dd$name,period[t],config.train[c],alg[a],"ignore",sep = "--")
          fila <- paste(dd$name,period[t],ncol(df),nrow(df),config.train[c],alg[a],"ignore",sep=",")
          
          config.train.local(config.train[c],df,file.name,fila,pred_sensores)

        }else{
          
          foreach(s = 1:length(score),.packages = packages) %dopar% # 
          {
            file.name <- paste(dd$name,period[t],config.train[c],alg[a],score[s],sep = "--")
            
            fila <- paste(dd$name,period[t],ncol(df),nrow(df),config.train[c],alg[a],score[s],sep=",")
            Log("Score ",score[s])
            
            #caso vec o vecinos
            if(config.train[c]=="normal") # ACA NO ENTRA
            {
              trainingNormal(df, alg=alg[a],sc=score[s], file.name = file.name, pred_sensores = pred_sensores , fila)
              
            }else if(config.train[c]=="smote")
            {
              foreach(p = 1:1,.packages = packages) %dopar% # solo para junin length(pred_sensores)
              {
                trainingSMOTE(df, alg=alg[a],sc=score[s], file.name = file.name, var = pred_sensores[p], fila )
              }
            }
          }# fin foreach por score
          
        }# fin if si es algoritmo local o si son los otros
      }# por training config 
       
    }# for por algoritmo  
  }#for por T
}# for por dataset

if(PAR==TRUE){
  stopCluster(cl)
}
