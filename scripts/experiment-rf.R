#'
#' TODO actualizar este comment
#' FORMATO NOMBRE DE ARCHIVO SALIDA
#' <dataset>-<variable>-<T period en numero>-<S o N, S para SMOTE y N normal>-<H humedad incluida, N no incluida>-<algoritmo>-<score>

library(randomForest)

source("dataset-processing.R")

library(caret) # para confusion matrix
library(forecast) # para metodo accuracy

library(doParallel)
library(unbalanced)
set.seed(147)

packages <- c("randomForest","caret","forecast","unbalanced","readr","xts","timeDate")
# si quiero guardar los dataset desfasados para ser usados por otras librerías.
LOCAL <- TRUE
#
SAVE_DATASET <- FALSE
#
split.train <- 0.68 # porcentaje de datos en el dataset de entremaniento
dataset <- c("dacc","dacc-temp","dacc-spring") 
config.train <-c("normal","smote")
#' T cuantos dias anteriores tomamos
period <- c(1,2,3,4,5)
tunegrid <- expand.grid(.mtry=c(10:25),.ntree=seq(from=500,to=3500,by=500))
# porcentaje para train set split
porc_train = 0.68
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

#TODO testear
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
#log.socket <- make.socket(port=4000)
Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
  cat(msg)
  #write.socket(log.socket, msg)
}

#cl <- makeCluster(4,outfile=paste("output-rf-",Sys.time(),".log",sep="")) # colocar detectCores() en server  en vez de 4
#registerDoParallel(cl)


RESUMEN <<- paste(Sys.time(),"--rf--experimento.csv",sep="")
columnas <- paste("dataset","days","ncol","nrow","config_train","alg","ntree","mtry","var",
                  "t_run_s","nfrostorig","ntrain","nfrostsmote",
                  "RMSE","r2","Sensitivity","Acc","Precision","Specificity",sep = ",")
#"ntrain", "ntest",
write(columnas,file=RESUMEN)

#foreach(j = 1:length(dataset),.packages = packages) %dopar% # comentar para correr en modo debug o rstudio y descomentar linea de abajo
for(j in 1:length(dataset)) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  sensores <- dd$data[-1] #quito columna date o primer columna
  pred_sensores_base <- dd$pred
  Log("DATASET ",dd$name)
  
  #foreach(t = 1:length(period),.packages = packages) %dopar% 
  for(t in 1:length(period))
  {
    Log("Period ",t)
    #row <- cbind.data.frame(row,t)
    #file.name <- paste(file.name,t,sep = "--")
    #' Obtengo dataset con variables desfasadas a t dias 
    #'
    aux <- desfasar.dataset.T(t,sensores, pred_sensores_base)
    pred_sensores <- aux$vars
    df <- aux$data
    
    #' ### Training set y test dataset
    df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
    until <- round(nrow(df)*porc_train)
    training.set = df[1:until-1, ] # This is training set to learn the parameters
    test.set = df[until:nrow(df), ]
    nfrost <- NA
    
    #foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% # arranca en 2 para evitar procesar junin
    for(p in 1:length(pred_sensores)) #junin ya lo he realizado
    {
      Log(pred_sensores[p])
      nfrostorig <- length(training.set[training.set[,pred_sensores[p]] <= 0,pred_sensores[p]])
      
    #foreach(u = 1:nrow(tunegrid),.packages = packages) %dopar% 
    #foreach(u = 1:length(unique(tunegrid$.ntree)),.packages = packages) %dopar%  # config LOCAL
    for(u in 1:length(unique(tunegrid$.ntree)))  # CONFIG LOCAL
       # for(u in 1:nrow(tunegrid))
      {
        
        #foreach(c = 1:length(config.train),.packages = packages) %dopar%  # solo corro SMOTE, volver 2 como 1 para rollback
        for(c in 1:length(config.train))
        {
          ts <- training.set
          test <- test.set
          if(config.train[c]=="smote")
          {
            #' Me enfoco en el caso de una sola estación 
            Y_class <- as.factor(with(df,ifelse(df[,pred_sensores[p]] <= 0,1,0)))
            
            #' datos para entrenar
            data_smote <- ubBalance(ts,Y_class[1:(until-1)],type = "ubSMOTE",percOver = 300, percUnder = 150)
            
            #' para visualizar la distribución de las clases
            summary(data_smote$Y)
            ts <- data_smote$X
            nfrost <- length(data_smote$X[data_smote$X[,pred_sensores[p]] <= 0,pred_sensores[p]])
          }
          
          # config rf-local o rf-solo
          if(LOCAL==TRUE)
          {
            vars <- vars.del.sensor(pred_sensores[p],colnames(training.set))
            ts <- training.set[,vars]
            test <- test.set[,vars]
            fila <- paste(dd$name,t,ncol(df),nrow(df),config.train[c],"rf-local",
                          tunegrid[u,]$.ntree,tunegrid[u,]$.mtry,pred_sensores[p],sep=",")
            file.name <- paste(dd$name,t,config.train[c],"rf-local",unique(tunegrid$.ntree)[u],1,pred_sensores[p],sep = "--")
            
            Log(paste("starts random Forest learning ntree:",tunegrid[u,]$.ntree))
          }else{
            
            ts <- training.set
            fila <- paste(dd$name,t,ncol(df),nrow(df),config.train[c],"rf",
                          tunegrid[u,]$.ntree,tunegrid[u,]$.mtry,pred_sensores[p],sep=",")
            file.name <- paste(dd$name,t,config.train[c],"rf",tunegrid[u,]$.ntree,tunegrid[u,]$.mtry,pred_sensores[p],sep = "--")
            
            Log(paste("starts random Forest learning mtry:",tunegrid[u,]$.mtry," ntree:",tunegrid[u,]$.ntree))
            # random forest
          }
          #guardar este dataset desfasado en T de smote
          if(SAVE_DATASET && config.train[c]=="smote"){  write.csv(data_smote$X,paste(paste(file.name,pred_sensores[p],"smote-dataset.csv",sep="--"))) }
          
          Log(fila)
        
          tryCatch({
            
            
            start_time <- Sys.time()
            # model <- randomForest(y ~ ., data = ts, importance = TRUE, keep.forest=FALSE,proximity=FALSE,
            #                       do.trace=TRUE,
            #                       ntree=tunegrid[u,]$.ntree, mtry=tunegrid[u,]$.mtry)
            # 
            if(LOCAL==TRUE)
            {
              
              model <- randomForest(x =ts[,-which(colnames(ts) %in% pred_sensores[p])],
                                    y=ts[,pred_sensores[p]], 
                                    importance = TRUE, keep.forest=TRUE,proximity=FALSE,
                                    ntree=unique(tunegrid$.ntree)[u])
            }else{
              
              model <- randomForest(x =ts[,-which(colnames(ts) %in% pred_sensores[p])],
                                    y=ts[,pred_sensores[p]], 
                                    importance = TRUE, keep.forest=TRUE,proximity=FALSE,
                                    ntree=tunegrid[u,]$.ntree, mtry=tunegrid[u,]$.mtry)
            }
            
            end_time <- Sys.time()
            timerf <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
            
            write.csv(importance(model),file = paste("./results-rf/",file.name,"--importance.csv",sep = ""))
            write.csv(data.frame(mse=model$mse,rsq=model$rsq),file = paste("./results-rf/",file.name,"--mse-rsq-vs-ntree.csv",sep = ""))
          
            # guardar model
            #save(model, file = paste("./models-rf/",file.name,".RData",sep=""),compress = TRUE)
            pred <- predict(model, test)
            
            # guardar csv con valor real vs predicho
            dataset <- as.data.frame(cbind(test[pred_sensores[p]],pred))
            colnames(dataset)<- c("y_real","y_pred")
            write.csv(x = dataset,file = paste("./results-rf/",file.name,"--Y-vs-Y_pred.csv",sep = ""))
            #evaluar resultados en testeo
            eee <- evaluate(pred,test[,pred_sensores[p]])
            #guardo detalles del experimento
            row <- paste(fila,timerf,nfrostorig,nrow(training.set),nfrost,
                         eee$rmse,eee$r2,eee$sens,eee$acc,eee$prec,eee$spec,sep=",")
            write(row, file=RESUMEN, append = TRUE)
            Log(row)
            
          }, warning=function(war){# capturar errores y enviarlos al log
            Log(paste("[WARNING] ",war))
          },error=function(err){
            Log(paste("[ERROR] ",err))
          },finally={})
          
        }# por training config 
        
      }# por tunegrid parameters
      
    }# for por cada sensor o estacion a predecir la minima   
  }#for por T
}# for por dataset

stopCluster(cl)
