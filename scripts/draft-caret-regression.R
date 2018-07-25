library(doParallel)
library(randomForest)
library(caret)
library(rpart)
library(C50)

source("bnlearn-utils.R")
source("dataset-processing.R")

#dataset <- c("dacc")#,"dacc","dacc-temp") #,"dacc-spring") 
#dataset <<- get.list.of.datasets(DATA)
VERBOSE <- TRUE
TMIN_CHAAR <-NULL
DATA <- "dacc" # possible values: dacc, inta, ur, needed for dataset-processing.R
if(DATA=="inta"){TMIN_CHAAR <-TRUE}else{TMIN_CHAAR <-FALSE}
OUTPUT.FILE <- "output-" # <- where prints go while cluster is running
FILE.RESULT.NAME <- "--experimento-dacc.csv"
PATH.MODELS <- "./models-reg/"
PATH.RESULTS <- "./results-reg/"

packages <- c("randomForest","caret","DMwR","readr","xts","timeDate","rpart")

# si quiero guardar los modelos en .RData file 
SAVE_MODEL <- TRUE
# si quiero que los experimentos se ejecuten paralelamente en clusters o secuencialmente (porque estoy en debug o rstudio)
PAR <- FALSE
#normal: just split train and test set, smote: oversampling of the minority class.
config.train <-c("normal")#,"smote")
config.vars <-c("local","all") #only local variables or all variables.
#' T cuantos dias anteriores tomamos
period <- c(1,2,3,4)#,5)
#tunegrid <- expand.grid(.mtry=c(10:25),.ntree=seq(from=500,to=2500,by=500))
# porcentaje para train set split
porc_train = 0.68
breaks <- c(-20,0,50) # caso Helada y no helada
# rf: random forest, glm: logistic regression
models <- c("rpart","rf")
# variable cuyo valor cambia segun configuracion for
samp = "none" 
tuneParLen = 1 

SEED <- 147
seeds <- NULL
KFOLD <- 10
lista <- list()
################
# from http://jaehyeon-kim.github.io/2015/05/Setup-Random-Seeds-on-Caret-Package.html
setSeeds <- function(method = "cv", numbers = 1, repeats = 1, tunes = NULL, seed = 1237) {
  #B is the number of resamples and integer vector of M (numbers + tune length if any)
  B <- if (method == "cv") numbers
  else if(method == "repeatedcv") numbers * repeats
  else NULL
  
  if(is.null(length)) {
    seeds <- NULL
  } else {
    set.seed(seed = seed)
    seeds <- vector(mode = "list", length = B)
    seeds <- lapply(seeds, function(x) sample.int(n = 1000000, size = numbers + ifelse(is.null(tunes), 0, tunes)))
    seeds[[length(seeds) + 1]] <- sample.int(n = 1000000, size = 1)
  }
  # return seeds
  seeds
}


settingMySeeds <- function(model,tunelen)
{ 
  ss <- NULL
  
  if(model=="rpart") ss <- setSeeds(numbers=KFOLD,tunes = tunelen,seed = SEED)
  if(model=="rf")  ss <- setSeeds(numbers=KFOLD,tunes = tunelen,seed = SEED)
  # falta bayesian models 
  ss
}


#' function to call each model through caret
train.models <- function(trCtrl, X, Y,data, modelName,tp)
{
  
  model = NULL
  
  if(modelName == "rf")
  {
    # rf tune grid
    set.seed(SEED)
    model <- train(x=X,y=Y,method="rf",trControl = trCtrl, metric="RMSE",importance=T, tuneLength=tp)
    
  }else if(modelName == "rpart")
  {
    
    set.seed(SEED)
    model <- train(y.disc ~., data = data, method = "rpart", trControl=trCtrl,tuneLength = 10,
                   metric="RMSE", parms=list(split='information'))
     
  }
  
  model     
  
}


#WARNING!! OJO! time and resource consuming! run only in dedicated server
if(PAR==TRUE){
  cl <- makePSOCKcluster(detectCores(),outfile=paste(OUTPUT.FILE ,Sys.time(),".log",sep="")) # colocar detectCores() en server  en vez de 4
  registerDoParallel(cl)
}

RESUMEN <<- paste(Sys.time(),FILE.RESULT.NAME,sep="")
columnas <- paste("dataset","var","config.train","config.vars","T","alg","t_run_s","RMSE",
                  "MAE","R2",sep = ",")

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
  
  X <- training.set #[,-which(colnames(sensores) %in% pred_sensores)]
  #pred_sensores <- pred_sensores[1:2]
  
  #foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% # 
   for(p in 1:length(pred_sensores)) 
  {
    
    Y <- as.numeric(training.set[,pred_sensores[p]])
    #' discretizar valores en helada y no helada, con nombres admisibles como make.names
    real <- test.set[,pred_sensores[p]]
    
    y.disc <- as.vector(Y)
    
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
        
        if(config.vars[cvars]=="local")
        {
          vars <- vars.del.sensor(pred_sensores[p],colnames(X))
          X <- X[,vars]
          data <- cbind(X,y.disc)
          tuneParLen = 1 
          
        }else{
          
          X <- training.set[,-which(colnames(sensores) %in% pred_sensores)]
          data <- cbind(X,y.disc)
          tuneParLen = 5
        }
        
        #foreach(t = 1:length(period),.packages = packages) %dopar% 
        for(t in 1:length(period))
        {
          Log("T value ",t)
          
          #fila_header <- paste(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t,sep = ",")
          fila_header <- cbind(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t)
          name_header <- paste(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t,sep = "--")
          
          
          for(mod in models)
          {
            seeds <- settingMySeeds(mod,tuneParLen)
            #timeSlicesTrain <- createTimeSlices(1:nrow(training.set),initialWindow = T,horizon = 1,fixedWindow = TRUE)
            my.train.control <- trainControl(method = "cv", number = KFOLD, 
                                             initialWindow = t, horizon = 1, fixedWindow = TRUE,
                                             seeds = seeds)
            
            
            start_time <- Sys.time()
            model <- train.models(trCtrl=my.train.control, X=X, Y=y.disc,data=data, modelName=mod, tp = tuneParLen)
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
            dat <- as.data.frame(cbind(real,pred))
            colnames(dat)<- c("y_real","y_pred")
            write.csv(x = dat,file = paste(PATH.RESULTS,file.name,"--Y-vs-Y_pred.csv",sep = ""))
            # evaluar en testeo
            eee <- evaluate(pred, real)
            # guardo detalles del experimento
            row <- cbind(fila,eee$MAE,eee$rmse,eee$r2,t(eee$cm$overall),t(eee$cm$byClass))
            write.table(row, file=RESUMEN, append = TRUE,row.names = FALSE,col.names = FALSE, sep = ",")
            Log(row)
            
            # agregar modelo a una lista 
            lista[[file.name]] <- model
            
          }# for each model
          
        }# for por T
      }# for each config.vars
    }# for each config.train
    # plot y results de resample de los modelos 
    resamps <- resamples(lista)
    write.csv(resamps$values,file=paste(PATH.RESULTS,dataset[j],"--",pred_sensores[p],"--resamples.csv",sep=""))
    png(paste(PATH.RESULTS,dataset[j],"--",pred_sensores[p],"--bwplot.png",sep=""))
    print(bwplot(resamps))
    dev.off()
    png(paste(PATH.RESULTS,dataset[j],"--",pred_sensores[p],"--dotplot.png",sep=""))
    print(dotplot(resamps))
    dev.off()
    lista <- list()
  }# for por cada sensor o estacion a predecir helada/no helada
}# for dataset

if(PAR==TRUE){stopCluster(cl)}
