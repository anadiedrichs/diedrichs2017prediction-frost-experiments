library(doParallel)

library(randomForest)
library(caret)

source("bnlearn-utils.R")
source("dataset-processing.R")

set.seed(147)
#dataset <- c("dacc")#,"dacc","dacc-temp") #,"dacc-spring") 
#dataset <<- get.list.of.datasets(DATA)
VERBOSE <- TRUE
TMIN_CHAAR <-NULL
DATA <- "dacc" # possible values: dacc, inta, ur, needed for dataset-processing.R
if(DATA=="inta"){TMIN_CHAAR <-TRUE}else{TMIN_CHAAR <-FALSE}
OUTPUT.FILE <- "output-" # <- where prints go while cluster is running
FILE.RESULT.NAME <- "--experimento-dacc.csv"
PATH.MODELS <- "./models/"
PATH.RESULTS <- "./results/"
PATH.SAVE.DATASET <- "./datasets/"

packages <- c("randomForest","caret","DMwR","readr","xts","timeDate")
# si quiero que solo tome las variables de la misma ubicacion y no las vecinas
LOCAL <- FALSE
# si quiero guardar los modelos en .RData file 
SAVE_MODEL <- TRUE
# si quiero que los experimentos se ejecuten paralelamente en clusters o secuencialmente (porque estoy en debug o rstudio)
PAR <- FALSE
#
config.train <-c("normal","smote")
config.vars <-c("local","all") #only local variables or all variables.
#' T cuantos dias anteriores tomamos
period <- c(1)#,2,3,4)#,5)
#tunegrid <- expand.grid(.mtry=c(10:25),.ntree=seq(from=500,to=2500,by=500))
# porcentaje para train set split
porc_train = 0.68
breaks <- c(-20,0,50) # caso Helada y no helada
# rf: random forest, glm: logistic regression
models <- c("glm","rf")

samp = "none"

lista <- list()
################

# var: nombre variable a predecir,ejemplo *_tmin
# variables: colnames o conjunto de variables del dataset
vars.del.sensor <- function(var,variables,dataset_tmin_chaar=FALSE)
{
  v <- variables
  sensor <- unlist(strsplit(var,split=".",fixed = TRUE))[1]
  if(dataset_tmin_chaar==TRUE) sensor <- paste(sensor,".",sep="") # esto es lo diferente, por bug #
  
  vars <- v[grepl( sensor, v, fixed = TRUE)] # extraigo todas las variables relacionadas con sensor
  
  return(vars)
}

#' function to call each model through caret
train.models <- function(trCtrl, X, Y,data, modelName)
{
  
  model = NULL
  
  if(modelName == "rf")
  {
    # rf tune grid
    model <- train(x=X,y=Y,method="rf",trControl = trCtrl, metric="ROC",tuneLength=5,importance=T)
    
  }else if(modelName == "glm")
  {
    # ?? tune grid
    
    model <- train(y.disc ~ ., data = data, method="glm", family="binomial",trControl = trCtrl, metric="ROC")
    
  } 
  
  model     
  
}

#' Custom SMOTE function call 
smotest <- list(name = "SMOTE-k.10-p.300",
                func = function (x, y) {
                  library(DMwR)
                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y <- y
                  dat <- SMOTE(.y ~ ., data = dat, k = 10, perc.over = 300)
                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                       y = dat$.y)
                },
                first = TRUE)
#WARNING!! OJO! time and resource consuming! run only in dedicated server
if(PAR==TRUE){
  cl <- makeCluster(detectCores(),outfile=paste(OUTPUT.FILE ,Sys.time(),".log",sep="")) # colocar detectCores() en server  en vez de 4
  registerDoParallel(cl)
}

RESUMEN <<- paste(Sys.time(),FILE.RESULT.NAME,sep="")
columnas <- paste("dataset","var","config.train","config.vars","T","alg","t_run_s","FAR",
                  "Accuracy","Kappa","AccuracyLower","AccuracyUpper","AccuracyNull","AccuracyPValue",
                  "McnemarPValue","Sensitivity","Specificity","Pos Pred Value","Neg Pred Value",
                  "Precision","Recall","F1","Prevalence","Detection Rate","Detection Prevalence",
                  "Balanced Accuracy",sep = ",")

write(columnas,file=RESUMEN)


#foreach(j = 1:length(dataset),.packages = packages) %dopar% # comentar para correr en modo debug o rstudio y descomentar linea de abajo
for(j in 1:length(dataset)) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  ## issue #17
  if(DATA=="dacc"){ sensores <- dd$data[-1]} #quito columna date o primer columna
  else sensores <- dd$data
  ## end of issue #17
  pred_sensores <- dd$pred
  Log("DATASET ",dd$name)
  
  set.seed(3456)
  
  # si no arranco de cero no considera la primera fila, por eso el cero
  trainIndex <- createDataPartition(0:nrow(sensores), p = porc_train,list = FALSE, times = 1) 
  training.set <- as.data.frame(sensores[ trainIndex,])
  test.set  <- as.data.frame(sensores[-trainIndex,])
  
  X <- training.set[,-which(colnames(sensores) %in% pred_sensores)]
  #pred_sensores <- pred_sensores[1:2]
  
  #foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% # arranca en 2 para evitar procesar junin
   for(p in 1:length(pred_sensores)) 
  {
    
    Y <- as.numeric(training.set[,pred_sensores[p]])
    Log(pred_sensores[p])
   # foreach(c = 1:length(config.train),.packages = packages) %dopar%  # 
  for(ct in 1:length(config.train))
    {
      Log("config.train ",config.train[ct])
      
      if(config.train[ct] == "smote"){samp = smotest}
      else {samp = NULL}
      
      #foreach(cvars = 1:length(config.vars),.packages = packages) %dopar%  # 
      for(cvars in 1:length(config.vars))
      {
        Log("config.vars ",config.vars[cvars])
        #' discretizar valores en helada y no helada, con nombres admisibles como make.names
        real <- test.set[,pred_sensores[p]]
        real[which(real <=0)] <- 0 # frost
        real[which(real >0)] <- 1 # NO frost
        y.disc <- as.vector(Y)
        y.disc[which(y.disc <=0)] <- 0
        y.disc[which(y.disc > 0)] <- 1

        data <- cbind(X,y.disc)
        
        if(config.vars[cvars]=="local")
        {
          vars <- vars.del.sensor(pred_sensores[p],colnames(X))
          X <- X[,vars]
          data <- cbind(X,y.disc)
        }else{
          
          X <- training.set[,-which(colnames(sensores) %in% pred_sensores)]
          data <- cbind(X,y.disc)
        }
        
        #foreach(t = 1:length(period),.packages = packages) %dopar% 
        for(t in 1:length(period))
        {
          Log("T value ",t)
          
          #fila_header <- paste(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t,sep = ",")
          fila_header <- cbind(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t)
          name_header <- paste(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t,sep = "--")
          
          #timeSlicesTrain <- createTimeSlices(1:nrow(training.set),initialWindow = T,horizon = 1,fixedWindow = TRUE)
          my.train.control <- trainControl(method = "cv", number = 5, 
                                           initialWindow = t, horizon = 1, fixedWindow = TRUE,
                                           sampling = samp
                                           ,classProbs = TRUE, summaryFunction = twoClassSummary)
          
          for(mod in models)
          {
            start_time <- Sys.time()
            model <- train.models(trCtrl=my.train.control, X=X, Y=y.disc,data=data, modelName=mod)
            end_time <- Sys.time()
            runtime <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
            
            fila <- cbind(fila_header,mod,runtime)
            file.name <- paste(name_header,mod,sep = "--")
            
            # save model
            if(SAVE_MODEL){save(model, file = paste(PATH.MODELS,file.name,".RData",sep=""),compress = TRUE)}
            
            summary(model)
            print(model)
            
            #varimp model o lista de variables importantes
            vv <- varImp(model) # si tiene varImpModel
            write.csv(x = as.data.frame(vv$importance),file = paste(PATH.RESULTS,file.name,"--importance.csv",sep = ""))
            pred <- predict(model,test.set)
            
            # guardar csv con valor real vs predicho
            dat <- as.data.frame(cbind(test.set[pred_sensores[p]],pred))
            colnames(dat)<- c("y_real","y_pred")
            write.csv(x = dat,file = paste(PATH.RESULTS,file.name,"--Y-vs-Y_pred.csv",sep = ""))
            # evaluar en testeo
            eee <- evaluate.classification(pred, real)
            # guardo detalles del experimento
            #row <- paste(fila,eee$sens,eee$acc,eee$prec,eee$spec,"param1","param2",sep=",")
            row <- cbind(fila,eee$far,t(eee$cm$overall),t(eee$cm$byClass))
            write.table(row, file=RESUMEN, append = TRUE,row.names = FALSE,col.names = FALSE, sep = ",")
            Log(row)
            
            # agregar modelo a una lista 
            # name <- paste(varpred,"rf",T,sep="-")
            lista[[file.name]] <- model
            
          }# for each model
          
        }# for por T
      }# for each config.vars
    }# for each config.train
    # plot y results de resample de los modelos 
    resamps <- resamples(lista)
    summary(resamps)
    png(paste(dataset[j],pred_sensores[p],"bwplot.png",sep="--"))
    bwplot(resamps)
    dev.off()
    png(paste(dataset[j],pred_sensores[p],"dotplot.png",sep="--"))
    dotplot(resamps)
    dev.off()
    lista <- list()
  }# for por cada sensor o estacion a predecir la minima
}# for por dataset

if(PAR==TRUE){stopCluster(cl)}
