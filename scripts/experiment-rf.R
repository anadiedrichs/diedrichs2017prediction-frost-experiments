
library(randomForest)
library(doParallel)
library(unbalanced)
set.seed(147)

TMIN_CHAAR <-NULL
DATA <- "inta" # possible values: dacc, inta, ur, needed for dataset-processing.R
if(DATA=="inta"){TMIN_CHAAR <-TRUE}else{TMIN_CHAAR <-FALSE}
OUTPUT.FILE <- "output-rf-inta-1-" # <- where prints go while cluster is running
FILE.RESULT.NAME <- "--experimento-rf-inta-1.csv"
#PATH.MODELS <- "./models-rf/"
PATH.RESULTS <- "./results-rf/"
PATH.SAVE.DATASET <- "./datasets-rf/"

packages <- c("randomForest","caret","forecast","unbalanced","readr","xts","timeDate")
# si quiero que solo tome las variables de la misma ubicacion y no las vecinas
LOCAL <- FALSE
# si quiero guardar los dataset desfasados para ser usados por otras librerías.
SAVE_DATASET <- FALSE
# si quiero que los experimentos se ejecuten paralelamente en clusters o secuencialmente (porque estoy en debug o rstudio)
PAR <- TRUE
#
split.train <- 0.68 # porcentaje de datos en el dataset de entremaniento
dataset <- c("dacc","dacc-temp","dacc-spring") 
config.train <-c("normal","smote")
#' T cuantos dias anteriores tomamos
period <- c(1)#,2,3,4,5)
tunegrid <- expand.grid(.mtry=c(10:25),.ntree=seq(from=500,to=2500,by=500))
# porcentaje para train set split
porc_train = 0.68


source("bnlearn-utils.R")
source("dataset-processing.R")

################
# dataset <<- c("dacc")#,"dacc","dacc-temp") #,"dacc-spring") 
dataset <<- get.list.of.datasets(DATA)

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

experiment.config <- function(training.set,test.set,pred_sensor,fila_header,name_header,mtry,ntree)
{
  nfrostorig <- length(training.set[training.set[,pred_sensor] <= 0,pred_sensor])   #number of frost days in training set
  
  #foreach(c = 1:length(config.train),.packages = packages) %dopar%  # 
   for(c in 1:length(config.train))
  {
    ts <- training.set
    test <- test.set
    if(config.train[c]=="smote")
    {
      #' Me enfoco en el caso de una sola estación 
      Y_class <- as.factor(with(ts,ifelse(ts[,pred_sensor] <= 0,1,0)))
      
      #' datos para entrenar
      data_smote <- ubBalance(ts,Y_class,type = "ubSMOTE",percOver = 300, percUnder = 150)
      
      #' para visualizar la distribución de las clases
      #summary(data_smote$Y)
      ts <- data_smote$X
      nfrost <- length(data_smote$X[data_smote$X[,pred_sensor] <= 0,pred_sensor])
    }
    
    y.ts <- ts[,pred_sensor]
    # config rf-local o rf-solo
    if(LOCAL==TRUE)
    {
      vars <- vars.del.sensor(pred_sensor,colnames(ts))
      ts <- ts[,vars]
      fila <- paste(fila_header,config.train[c],"rf-local",ntree,mtry,pred_sensor,sep=",")
      file.name <- paste(name_header,config.train[c],"rf-local",ntree,mtry,pred_sensor,sep = "--")
      
    }else{
      # quitar variables en *_t, menos la del predictor
      ts <- ts[,-which(colnames(ts) %in% pred_sensores)]
      fila <- paste(fila_header,config.train[c],"rf", ntree,mtry,pred_sensor,sep=",")
      file.name <- paste(name_header,config.train[c],"rf",ntree,mtry,pred_sensor,sep = "--")
    }
    
    #guardar este dataset desfasado en T de smote
    if(SAVE_DATASET && config.train[c]=="smote"){  write.csv(data_smote$X,paste(paste(PATH.SAVE.DATASET,file.name,pred_sensores[p],"smote-dataset.csv",sep="--"))) }
    Log(fila)
    Log(paste("starts random Forest learning mtry:",mtry," ntree:",ntree))
    
    tryCatch({
      
      start_time <- Sys.time()
      if(LOCAL==TRUE)
      {
        model <- randomForest(x =ts,y=y.ts, 
                              importance = TRUE, keep.forest=TRUE,proximity=FALSE,
                              ntree=ntree)
      }else{
        model <- randomForest(x =ts,y=y.ts, 
                              importance = TRUE, keep.forest=TRUE,proximity=FALSE,
                              ntree=ntree, mtry=mtry)
      }
      
      end_time <- Sys.time()
      timerf <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
      write.csv(importance(model),file = paste(PATH.RESULTS,file.name,"--importance.csv",sep = ""))
      write.csv(data.frame(mse=model$mse,rsq=model$rsq),file = paste(PATH.RESULTS,file.name,"--mse-rsq-vs-ntree.csv",sep = ""))
      
      # guardar model
      #save(model, file = paste("./models-rf/",file.name,".RData",sep=""),compress = TRUE)
      pred <- predict(model, test)
      # guardar csv con valor real vs predicho
      dataset <- as.data.frame(cbind(test[pred_sensor],pred))
      colnames(dataset)<- c("y_real","y_pred")
      write.csv(x = dataset,file = paste("./results-rf/",file.name,"--Y-vs-Y_pred.csv",sep = ""))
      #evaluar resultados en testeo
      eee <- evaluate(pred,test[,pred_sensor])
      #guardo detalles del experimento
      row <- paste(fila,timerf,nfrostorig,nrow(training.set),nfrost,eee$rmse,eee$r2,eee$sens,eee$acc,eee$prec,eee$spec,sep=",")
      write(row, file=RESUMEN, append = TRUE)
      Log(row)
      
    }, warning=function(war){# capturar errores y enviarlos al log
      Log(paste("[WARNING] ",war))
    },error=function(err){
      Log(paste("[ERROR] ",err))
    },finally={})
    
  }# por training config 
  
}

#WARNING!! OJO! time and resource consuming! run only in dedicated server
if(PAR==TRUE){
  cl <- makeCluster(detectCores(),outfile=paste(OUTPUT.FILE ,Sys.time(),".log",sep="")) # colocar detectCores() en server  en vez de 4
  registerDoParallel(cl)
}

RESUMEN <<- paste(Sys.time(),FILE.RESULT.NAME,sep="")
columnas <- paste("dataset","days","ncol","nrow","config_train","alg","ntree","mtry","var",
                  "t_run_s","nfrostorig","ntrain","nfrostsmote",
                  "RMSE","r2","Sensitivity","Acc","Precision","Specificity",sep = ",")
write(columnas,file=RESUMEN)

foreach(j = 1:length(dataset),.packages = packages) %dopar% # comentar para correr en modo debug o rstudio y descomentar linea de abajo
#for(j in 1:length(dataset)) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  ## issue #17
  if(DATA=="dacc"){ sensores <- dd$data[-1]} #quito columna date o primer columna
  else sensores <- dd$data
  ## end of issue #17
  pred_sensores_base <- dd$pred
  Log("DATASET ",dd$name)
  
  foreach(t = 1:length(period),.packages = packages) %dopar% 
#  for(t in 1:length(period))
  {
    Log("Period ",t)
    aux <- desfasar.dataset.T(t,sensores, pred_sensores_base)
    pred_sensores <<- aux$vars # pred_sensores variable global
    df <- aux$data
    
    #' ### Training set y test dataset
    df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
    until <- round(nrow(df)*porc_train)
    training.set = df[1:until-1, ] # This is training set to learn the parameters
    test.set = df[until:nrow(df), ]
    nfrost <- NA

#    foreach(p = 1:1,.packages = packages) %dopar% # arranca en 2 para evitar procesar junin
    foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% # arranca en 2 para evitar procesar junin
#    for(p in 1:length(pred_sensores)) #junin ya lo he realizado
    {
      Log(pred_sensores[p])
      
      if(LOCAL==TRUE){
        
        foreach(u = 1:length(unique(tunegrid$.ntree)),.packages = packages) %dopar%  # config LOCAL
        #  foreach(u = 1:1,.packages = packages) %dopar%  # config LOCAL
        #for(u in 1:length(unique(tunegrid$.ntree)))  # CONFIG LOCAL
        {
          Log(paste("TuneGrid value u:",u," value ",unique(tunegrid$.ntree)[u],sep=""))
          experiment.config(training.set,test.set,pred_sensores[p],paste(dd$name,t,ncol(df),nrow(df),sep = ",")
                            ,name_header = paste(dd$name,t,sep="--"),ntree= unique(tunegrid$.ntree)[u],mtry=1)
        }# por tunegrid parameters
        
      }else{
        
        #foreach(u = 1:1,.packages = packages) %dopar% 
        foreach(u = 1:nrow(tunegrid),.packages = packages) %dopar% 
        #for(u in 1:nrow(tunegrid))
        {
          experiment.config(training.set,test.set,pred_sensores[p],paste(dd$name,t,ncol(df),nrow(df),sep = ",")
                            ,name_header = paste(dd$name,t,sep="--"),ntree= tunegrid[u,]$.ntree,mtry=tunegrid[u,]$.mtry)
        }# por tunegrid parameters
        
      }

    }# for por cada sensor o estacion a predecir la minima   
  }#for por T
}# for por dataset

if(PAR==TRUE){stopCluster(cl)}
