
library(randomForest)
library(doParallel)
library(unbalanced)
library(caret)

set.seed(147)

TMIN_CHAAR <-NULL
DATA <- "dacc" # possible values: dacc, inta, ur, needed for dataset-processing.R
if(DATA=="inta"){TMIN_CHAAR <-TRUE}else{TMIN_CHAAR <-FALSE}
OUTPUT.FILE <- "output-rf-inta-1-" # <- where prints go while cluster is running
FILE.RESULT.NAME <- "--experimento-rf-dacc.csv"
#PATH.MODELS <- "./models-rf/"
PATH.RESULTS <- "./results-rf/"
PATH.SAVE.DATASET <- "./datasets-rf/"

packages <- c("randomForest","caret","DMwR","unbalanced","readr","xts","timeDate")
# si quiero que solo tome las variables de la misma ubicacion y no las vecinas
LOCAL <- FALSE
# si quiero guardar los dataset desfasados para ser usados por otras librerías.
SAVE_DATASET <- FALSE
# si quiero que los experimentos se ejecuten paralelamente en clusters o secuencialmente (porque estoy en debug o rstudio)
PAR <- TRUE
#
dataset <- c("dacc","dacc-temp","dacc-spring") 
config.train <-c("normal","smote")
config.vars <-c("local","all") #only local variables or all variables.
#' T cuantos dias anteriores tomamos
period <- c(1,2,3,4)#,5)
tunegrid <- expand.grid(.mtry=c(10:25),.ntree=seq(from=500,to=2500,by=500))
# porcentaje para train set split
porc_train = 0.68
breaks <- c(-20,0,50) # caso Helada y no helada


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
samp = "none"
RESUMEN <<- paste(Sys.time(),FILE.RESULT.NAME,sep="")
columnas <- paste("dataset","days","ncol","nrow","config_train","alg","ntree","mtry","var",
                  "t_run_s","nfrostorig","ntrain","nfrostsmote",
                  "RMSE","r2","Sensitivity","Acc","Precision","Specificity",sep = ",")
write(columnas,file=RESUMEN)

lista <- list()

#foreach(j = 1:length(dataset),.packages = packages) %dopar% # comentar para correr en modo debug o rstudio y descomentar linea de abajo
for(j in 1:length(dataset)) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  ## issue #17
  if(DATA=="dacc"){ sensores <- dd$data[-1]} #quito columna date o primer columna
  else sensores <- dd$data
  ## end of issue #17
  pred_sensores_base <- dd$pred
  Log("DATASET ",dd$name)
  
  set.seed(3456)
  
  # si no arranco de cero no considera la primera fila, por eso el cero
  trainIndex <- createDataPartition(0:nrow(sensores), p = porc_train,list = FALSE, times = 1) 
  training.set <- as.data.frame(sensores[ trainIndex,])
  test.set  <- as.data.frame(sensores[-trainIndex,])
  
  X <- training.set[,-which(colnames(sensores) %in% pred_sensores_base)]
  #pred_sensores_base <- pred_sensores_base[1:2]
  
  #foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% # arranca en 2 para evitar procesar junin
   for(p in 1:length(pred_sensores)) #junin ya lo he realizado
  {
    
    Y <- as.numeric(training.set[,pred_sensores[p]])
    Log(pred_sensores[p])
   # foreach(c = 1:length(config.train),.packages = packages) %dopar%  # 
  for(ct in 1:length(config.train))
    {
      Log("config.train ",config.train[c])
      
      if(config.train[c] == "smote"){samp = "smote"}
      else {samp = "none"}
      
      #foreach(cvars = 1:length(config.vars),.packages = packages) %dopar%  # 
      for(cvars in 1:length(config.vars))
      {
        Log("config.vars ",config.vars[cvars])
        
        if(config.vars[cvars]=="local")
        {
          vars <- vars.del.sensor(pred_sensores[p],colnames(X))
          ts <- X[,vars]
          fila <- paste(fila_header,config.train[c],"rf-local",ntree,mtry,pred_sensor,sep=",")
          file.name <- paste(name_header,config.train[c],"rf-local",ntree,mtry,pred_sensor,sep = "--")
          
        }else{
          fila <- paste(fila_header,config.train[c],"rf", ntree,mtry,pred_sensor,sep=",")
          file.name <- paste(name_header,config.train[c],"rf",ntree,mtry,pred_sensor,sep = "--")
        }
        
        #foreach(t = 1:length(period),.packages = packages) %dopar% 
        for(t in 1:length(period))
        {
          Log("T value ",t)
          fila_header <- paste(dd$name,pred_sensores[p],config.train[c],config.vars[cvars],t,sep = ",")
          name_header <- paste(dd$name,pred_sensores[p],config.train[c],config.vars[cvars],t,sep = "--")
          
          #timeSlicesTrain <- createTimeSlices(1:nrow(training.set),initialWindow = T,horizon = 1,fixedWindow = TRUE)
          my.train.control <- trainControl(method = "cv", number = 5, 
                                           initialWindow = t, horizon = 1, fixedWindow = TRUE,
                                           sampling = samp)
          
          nfrost <- NA ## ??
          real <- test.set[,pred_sensores[p]]
          
          # por cada modelo a correr
          # modelos rf para regresion, rf clasificacion, (logistic regression classificacion) gml
          fila <- paste(fila_header,"rf-reg",sep=",")
          file.name <- paste(name_header,"rf-reg",sep = "--")
          
          # randomForest - regression
          start_time <- Sys.time()
          model <- train(x=X,y=Y,method="rf",trControl = my.train.control, metric="RMSE",tuneLength=5,importance=T)
          end_time <- Sys.time()
          timerf <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
          #plot(model)
          print(model)
          varImp(model)
          # agregar modelo a una lista 
          name <- paste(varpred,"rf",T,sep="-")
          
          # guardar model
          #save(model, file = paste("./models/",file.name,".RData",sep=""),compress = TRUE)
          pred <- predict(model,test.set)
          evaluate(pred, real)
          # guardar csv con valor real vs predicho
          dataset <- as.data.frame(cbind(test.set[pred_sensores[p]],pred))
          colnames(dataset)<- c("y_real","y_pred")
          write.csv(x = dataset,file = paste("./results/",file.name,"--Y-vs-Y_pred.csv",sep = ""))
          #evaluar resultados en testeo
          eee <- evaluate(pred,test.set[,pred_sensor])
          # guardo detalles del experimento
          row <- paste(fila,timerf,nfrostorig,nrow(training.set),nfrost,eee$rmse,eee$r2,eee$sens,eee$acc,eee$prec,eee$spec,sep=",")
          write(row, file=RESUMEN, append = TRUE)
          Log(row)
          
          
          lista[[name]] <- model
          #' random forest para clasificacion
          #' 
          y.disc <- cut(Y, breaks = breaks)
          data <- cbind(X,y.disc)
          model.rf.class <- train(x=X,y=y.disc,method="rf",trControl = my.train.control, metric="Accuracy",tuneLength=5,importance=T)
          
          pred <- predict(model.rf.class,test.set)
          evaluate(pred, cut(real, breaks = breaks)) # VER CASO classificacion
          #' logistic regresion 
          #' este método puede requerir formula, data
          
          model.lg <- train(y.disc ~ ., data = data, method="glm", family="binomial",trControl = my.train.control, metric="Accuracy")
          summary(model.lg)
          varImp(model.lg)
          
          
          pred <- predict(model.rf.class,test.set)
          evaluate(pred, cut(real, breaks = breaks)) # check
        
          # TODO ir guardando todos los resultados en un archivo!!!
          
        }# for por T
      }# for each config.vars
    }# for each config.train
  }# for por cada sensor o estacion a predecir la minima
}# for por dataset

if(PAR==TRUE){stopCluster(cl)}
