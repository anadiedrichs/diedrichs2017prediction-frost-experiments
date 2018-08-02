library(doParallel)
library(randomForest)
library(caret)
library(rpart)
library(bnlearn)

source("bnlearnRegression.R")
source("bnlearn-utils.R")
source("dataset-processing.R")

# si quiero que los experimentos se ejecuten paralelamente en clusters o secuencialmente (porque estoy en debug o rstudio)
PAR <- FALSE
#dataset <- c("dacc","dacc-temp","dacc-spring") 
#dataset <<- get.list.of.datasets(DATA)
VERBOSE <- TRUE
TMIN_CHAAR <-NULL
DATA <- "dacc" # possible values: dacc, inta, ur, needed for dataset-processing.R
if(DATA=="inta"){TMIN_CHAAR <-TRUE}else{TMIN_CHAAR <-FALSE}
OUTPUT.FILE <- "output-reg-3-" # <- where prints go while cluster is running
FILE.RESULT.NAME <- "--experimento-dacc-regression-3.csv"
PATH.MODELS <- "./models-reg/"
PATH.RESULTS <- "./results-reg/"

packages <- c("randomForest","caret","DMwR","readr","xts","timeDate","rpart")

# si quiero guardar los modelos en .RData file 
SAVE_MODEL <- TRUE
#normal: just split train and test set, smote: oversampling of the minority class.
config.train <-c("normal")#,"smote")
config.vars <-c("local","all") #only local variables or all variables.
#' T cuantos dias anteriores tomamos
period <- c(1)#,2,3,4)#,5) #TODO IN PRODUCTION
#tunegrid <- expand.grid(.mtry=c(10:25),.ntree=seq(from=500,to=2500,by=500))
# porcentaje para train set split
porc_train = 0.68
breaks <- c(-20,0,50) # caso Helada y no helada
# rf: random forest, glm: logistic regression
models <- c("bnReg","rf") #TODO IN PRODUCTION  "rpart",
# variable cuyo valor cambia segun configuracion for
samp = "none" 
tuneParLen = 1 

SEED <- 147
seeds <- NULL
KFOLD <- 3750 #TODO IN PRODUCTION
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

settingMySeeds <- function(model,tunelen) #TODO
{ 
  ss <- NULL
  
  if(model=="rpart") ss <- setSeeds(numbers=KFOLD,tunes = 10,seed = SEED) #TODO tuneLenRpart
  if(model=="rf")  ss <- setSeeds(numbers=KFOLD,tunes = tunelen,seed = SEED)
  if(model=="bnReg") ss <- setSeeds(numbers=KFOLD,tunes = 8,seed = SEED) # por defecto, 8 parametros a tunear
  ss
}


#' function to call each model through caret
train.models <- function(trCtrl, X, Y,data, modelName,tuneLen)
{
  my.metric = "RMSE"
  model = NULL
  
  if(modelName == "rf")
  {
    # rf tune grid
    set.seed(SEED)
    model <- train(x=X,y=Y,method="rf",trControl = trCtrl, metric=my.metric,importance=T, tuneLength=tuneLen)
    
  }else if(modelName == "rpart")
  {
    set.seed(SEED)
    model <- train(Y ~., data = data, method = modelName, trControl=trCtrl,tuneLength = tuneLen, #TODO tuneLenRpart
                   metric=my.metric, parms=list(split='information'))
     
  }else if(modelName == "bnReg")
  {
    set.seed(SEED)
    # por defecto entrena en modo grilla o grid
    Y <- as.vector(as.numeric(Y))
    #colnames(Y) <- "Y"
    model <- train(x= data, y = Y, data = data, 
                      method = bnReg, metric=my.metric,
                      trControl = trCtrl, 
                      node = "Y") 
  }
  model     
  
}


#WARNING!! OJO! time and resource consuming! run only in dedicated server
if(PAR==TRUE){
  cl <- makePSOCKcluster(detectCores(),outfile=paste(OUTPUT.FILE ,Sys.time(),".log",sep="")) # colocar detectCores() en server  en vez de 4
  registerDoParallel(cl)
}

RESUMEN <<- paste(Sys.time(),FILE.RESULT.NAME,sep="")
columnas <- paste("dataset","var","config.train","config.vars","T","alg","t_run_s","MAE","RMSE",
                  "R2","Accuracy","Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyNull", "AccuracyPValue", "McnemarPValue","Sensitivity",
                  "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1","Prevalence", "Detection Rate",
                  "Detection Prevalence Balanced", "Accuracy",sep = ",") 


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
  
  
  
 # foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% # 
   for(p in 1:length(pred_sensores[1])) #solo corro Junin 
  {
    
    Log(pred_sensores[p])
   
   foreach(c = 1:length(config.train),.packages = packages) %dopar%  # 
  #for(ct in 1:length(config.train))
    {
      Log("config.train ",config.train[ct])
      
      if(config.train[ct] == "smote"){samp = smotest}
      else {samp = NULL}
      
      foreach(cvars = 1:length(config.vars),.packages = packages) %dopar%  # 
      #for(cvars in 1:length(config.vars))
      {
        Log("config.vars ",config.vars[cvars])
        
        foreach(t = 1:length(period),.packages = packages) %dopar% 
        #for(t in period)
        {
          Log("T value ",t)
          
          aux <- build.dataset.for.experiment(t=period[t],dataset=sensores, pred_sensores, pred_sensores[p],config=config.vars[cvars])
          
          X <- aux$x
          Y <- aux$y
          data <- aux$data
          test.set <- aux$test
          
          #fila_header <- paste(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t,sep = ",")
          fila_header <- cbind(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t)
          name_header <- paste(dd$name,pred_sensores[p],config.train[ct],config.vars[cvars],t,sep = "--")
         foreach(m = 1:length(models),.packages = packages) %dopar% 
         # for(mod in models)
          {
            Log("Model ",mod)
            cc <- createTimeSlices(1:nrow(X),initialWindow=300,horizon=100,fixedWindow=FALSE)
            KFOLDS <- length(cc$train)
            if(ncol(X)<10) tunePar = 3
            else tunePar = 10
            
            seeds <- settingMySeeds(mod,tunePar)
            #timeSlicesTrain <- createTimeSlices(1:nrow(training.set),initialWindow = T,horizon = 1,fixedWindow = TRUE)
            my.train.control <- trainControl(method = "timeslice",# number = KFOLD,
                                             initialWindow = 300, horizon = 100, fixedWindow = TRUE,
                                             seeds = seeds)


            start_time <- Sys.time()
            model <- train.models(trCtrl=my.train.control, X=X, Y=Y,data=data, modelName=mod, tuneLen=tunePar)
            end_time <- Sys.time()
            runtime <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)

            fila <- cbind(fila_header,mod,runtime)
            file.name <- paste(name_header,mod,sep = "--")

            # save model
            if(SAVE_MODEL){save(model, file = paste(PATH.MODELS,file.name,".RData",sep=""),compress = TRUE)}
            if(VERBOSE){
              summary(model)
              print(model)
            }
            #varimp model o lista de variables importantes
            if(mod == "bnReg")
            {
              #save Markov blanket
              bnregmb <- mb(model$finalModel$network,node="Y")
              df.bnregmb <- data.frame(nodes=bnregmb)
              write.csv(x = df.bnregmb,file = paste(PATH.RESULTS,file.name,"--markovBlanket.csv",sep = ""))
              # plot Bayesian network
              png(paste(PATH.RESULTS,file.name,"--BayesianNetwork.png",sep = ""))
              print(plot(model$finalModel$network))
              dev.off()

            }else{ #variable importance list
              vv <- varImp(model) # si tiene varImpModel
              write.csv(x = as.data.frame(vv$importance),file = paste(PATH.RESULTS,file.name,"--importance.csv",sep = ""))
              png(paste(PATH.RESULTS,file.name,"--importance.png",sep = ""))
              print(plot(varImp(model)))
              dev.off()
            }
            # prediction on test.set
            pred <- predict(model,test.set)

            # guardar csv con valor real vs predicho
            dat <- as.data.frame(cbind(aux$real,pred))
            colnames(dat)<- c("y_real","y_pred")
            write.csv(x = dat,file = paste(PATH.RESULTS,file.name,"--Y-vs-Y_pred.csv",sep = ""))
            # evaluar en testeo
            eee <- evaluate(pred, aux$real)
            # guardo detalles del experimento
            row <- cbind(fila,eee$MAE,eee$rmse,eee$r2,t(eee$cm$overall),t(eee$cm$byClass))
            write.table(row, file=RESUMEN, append = TRUE,row.names = FALSE,col.names = FALSE, sep = ",")
            Log(row)

            # agregar modelo a una lista
            #lista[[file.name]] <- model
            warnings()
          }# for each model

        }# for por T
      }# for each config.vars
    }# for each config.train
    # plot y results de resample de los modelos 
#    resamps <- resamples(lista)
#    write.csv(resamps$values,file=paste(PATH.RESULTS,dataset[j],"--",pred_sensores[p],"--resamples.csv",sep=""))
#    png(paste(PATH.RESULTS,dataset[j],"--",pred_sensores[p],"--bwplot.png",sep=""))
#    print(bwplot(resamps))
#    dev.off()
#    png(paste(PATH.RESULTS,dataset[j],"--",pred_sensores[p],"--dotplot.png",sep=""))
#    print(dotplot(resamps))
#    dev.off()
#    lista <- list()
  }# for por cada sensor o estacion a predecir helada/no helada
}# for dataset

if(PAR==TRUE){stopCluster(cl)}
