library(bnlearn)
library(doParallel)
library(unbalanced)

set.seed(147)
TMIN_CHAAR <-NULL
DATA <- "ur" # possible values: dacc, inta, ur, needed for dataset-processing.R
if(DATA=="inta"){TMIN_CHAAR <-TRUE}else{TMIN_CHAAR <-FALSE}
OUTPUT.FILE <- "output-ur-local-" # <- where prints go while cluster is running
FILE.RESULT.NAME <- "--experimento-ur-.csv"
PATH.MODELS <- "./models-ur/"
PATH.RESULTS <- "./results-ur/"
PATH.SAVE.DATASET <- "./datasets-ur/"
#' True for parallel cluster execution (run only on server, not desktop), false for sequential execution
PAR <- FALSE
# si quiero guardar los dataset desfasados para ser usados por otras librerías.
SAVE_DATASET <- TRUE
# arguments for doparallel
config.train <<-c("normal","smote")
# local: configuracion para armar red bayesiana con sólo las variables locales, de la propia estación
#alg <- c("local") 
alg <<- c("local","hc","tabu")
#' T how many previous day of information we take
period <<- c(1,2,3,4,5) #,3)#,4,5 #1
score <- c("bic-g","loglik-g","aic-g","bge")

#' the multivariate Gaussian log-likelihood (loglik-g) score.
#' the corresponding Akaike Information Criterion score (aic-g).
#' the corresponding Bayesian Information Criterion score (bic-g).
#' a score equivalent Gaussian posterior density (bge).
#
exports <- c("get.list.of.datasets","trainingSMOTE","trainingNormal","trainingNormalOneVar","Log")
packages <- c("bnlearn","caret","forecast","unbalanced","readr","xts","timeDate")
split.train <<- 0.68 # porcentaje de datos en el dataset de entremaniento

source("bnlearn-utils.R")
source("dataset-processing.R")

################
# dataset <<- c("dacc")#,"dacc","dacc-temp") #,"dacc-spring") 
dataset <<- get.list.of.datasets(DATA)

#' Caso para manejar entrenamiento configuracion local
config.train.local <- function(config,df,file.name,fila,pred_sensores)
{
    foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% 
    #for(p in 1:length(pred_sensores))
    {
      fn <- paste(file.name, pred_sensores[p],sep="--")
      if(config=="normal"){
        
        trainingNormalOneVar(df, alg="local",sc="ignore", file.name = fn, var = pred_sensores[p] , fila)
      
      }else if(config=="smote"){

        trainingSMOTE(df, alg="local",sc="ignore",  file.name = fn, var = pred_sensores[p], fila )
      }
    }
}

if(PAR==TRUE){
  cl <- makeCluster(detectCores(),outfile=paste(OUTPUT.FILE,Sys.time(),".log",sep="")) # 
  registerDoParallel(cl)
  Log("EJECUTANDO EN PARALELO")
}

RESUMEN <<- paste(Sys.time(),FILE.RESULT.NAME,sep="")
columnas <- paste("dataset","days","ncol","nrow","config_train","alg","score",
                  "t_run_s"," t_fitted","nfrostorig","ntrain","nfrostsmote","var",
                  "RMSE","r2","Sensitivity","Acc","Precision","Specificity",sep = ",")
#"ntrain", "ntest",
write(columnas,file=RESUMEN)

foreach(j = 1:length(dataset),.packages = packages,.export=exports) %dopar% # volver 2 como 1 para correr dataset dacc
#for(j in 1:length(dataset)) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  ## issue #17
  if(DATA=="dacc"){ sensores <- dd$data[-1]} #quito columna date o primer columna
  else sensores <- dd$data
  ## end issue #17
  pred_sensores_base <- dd$pred
  Log(paste("DATASET ",dd$name,sep = ""))
  
foreach(t = 1:length(period),.packages = packages,.export=exports) %dopar% 
# for(t in 1:length(period))
  {
    Log(paste("Period ",period[t]))
    #' Obtengo dataset con variables desfasadas a t dias 
    aux <- desfasar.dataset.T(period[t],sensores, pred_sensores_base)
    pred_sensores <- aux$vars
    df <- aux$data
    
    #guardar este dataset desfasado en T
    if(SAVE_DATASET){  write.csv(aux$data,paste(paste(PATH.SAVE.DATASET,dd$name,t,sep = "--"),"dataset.csv",sep="--"))  }
    
    bl <<- get_blacklist(pred_sensores)
    wl <<- get_whitelist(pred_sensores,colnames(df),TMIN_CHAAR)
    print(wl)

 foreach(a = 1:length(alg),.packages = packages,.export=exports) %dopar% 
#    for(a in 1:length(alg))
    {
      Log("Alg ",alg[a])

   foreach(c = 1:length(config.train),.packages = packages,.export=exports) %dopar%  # 
#      for(c in 1:length(config.train))
      {
        u <- NULL
        #' ### Training set y test dataset
        df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
        
        if(alg[a]=="local")
        {
          file.name <- paste(dd$name,period[t],config.train[c],alg[a],"ignore",sep = "--")
          fila <- paste(dd$name,period[t],ncol(df),nrow(df),config.train[c],alg[a],"ignore",sep=",")
          
          config.train.local(config.train[c],df,file.name,fila,pred_sensores)

        }else{
          
          foreach(s = 1:length(score),.packages = packages,.export=exports) %dopar%
          #for(s in 1:length(score))
          {
            file.name <- paste(dd$name,period[t],config.train[c],alg[a],score[s],sep = "--")
            
            fila <- paste(dd$name,period[t],ncol(df),nrow(df),config.train[c],alg[a],score[s],sep=",")
            Log("Score ",score[s])
            
            #caso vec o vecinos
            if(config.train[c]=="normal")
            {
              trainingNormal(df, alg=alg[a],sc=score[s], file.name = file.name, pred_sensores = pred_sensores , fila)
              
            }else if(config.train[c]=="smote")
            {
              for(p in 1:length(pred_sensores))
              #foreach(p = 1:length(pred_sensores),.packages = packages) %dopar% 
              {
                
                fn <- paste(file.name, pred_sensores[p],sep="--")
                trainingSMOTE(df, alg=alg[a],sc=score[s], file.name = fn, var = pred_sensores[p], fila )
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
Log("END OF EXPERIMENT")