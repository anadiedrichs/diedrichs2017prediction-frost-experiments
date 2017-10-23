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

set.seed(147)


packages <- c("bnlearn","caret","forecast")
# si quiero guardar los dataset desfasados para ser usados por otras librerías.

SAVE_DATASET <- TRUE
ownVariables <- FALSE
split.train <- 0.68 # porcentaje de datos en el dataset de entremaniento

################

dataset <- c("dacc","dacc-temp","dacc-spring")
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

columnas <- c("dataset","days","ncol","nrow","config_train", "t_run_s"," t_fitted","var")

results <- data.frame(dataset=as.Date(character()),days=numeric(),ncol=numeric(),nrow=numeric(),
           config_train=character(), t_run_s = as.numeric(), t_fitted = as.numeric(),
           var = character(),stringsAsFactors=FALSE) 

training.config <- function(df,type="normal",p=0.68)
{
  if(type=="normal")
  {
    until <- round(nrow(df)*p)
    training.set = df[1:until-1, ] # This is training set to learn the parameters
    test.set = df[until:nrow(df), ]
  
    }else if(type=="smote")
  {
    
  }else stop("invalid value in argument type in training.config")
  
  return(list(train=training.set,test=test.set))
}

#' funcion de aprendizaje bayesiano variables continuas
learn.bayes <- function(df, wl=NULL,bl=NULL,alg="hc",sc="bic")
{
  time = NULL; res = NULL
  if(!which(sc %in% score)) stop("score inexistente en learn.bayes")
  
  if(alg=="hc")
  {
    start_time <- Sys.time()
    res = hc(df, whitelist=wl,blacklist = bl, score = sc)
    end_time <- Sys.time()
    time = end_time - start_time
    
  }else if(alg=="tabu"){
    
    start_time <- Sys.time()
    res = tabu(df, whitelist=wl,blacklist = bl, score = sc)
    end_time <- Sys.time()
    time = end_time - start_time
    
  }
  
  return(list(model=res, time = as.numeric(time)))
  
}


cl <- makeCluster(4)  
registerDoParallel(cl)  

for(j in 1:3) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  sensores <- dd$data[-1] #quito columna date o primer columna
  pred_sensores_base <- dd$pred
  cat("DATASET ",dd$name,"\n")
  
  for(t in 1:length(period))
  {
    #row <- cbind.data.frame(row,t)
    #file.name <- paste(file.name,t,sep = "--")
    #' Obtengo dataset con variables desfasadas a t dias 
    #'
    aux <- desfasar.dataset.T(t,sensores, pred_sensores_base)
    pred_sensores <- aux$vars
    df <- aux$data
    
    #guardar este dataset desfasado en T
    if(SAVE_DATASET){  write.csv(aux$data,paste(paste(dd$name,t,sep = "--"),"dataset.csv",sep="--"))  }
    #' nombres de las variables a usar para entrenar/testear
    #' nro de variables
    #row <- cbind.data.frame(row,ncol(df))
    #' nro de ejemplos
    #row <- cbind.data.frame(row,nrow(df))

    bl <- get_blacklist(pred_sensores)
    wl <- get_whitelist(pred_sensores,colnames(df))
    
    for(c in 1:length(config.train))
    {
      #row <- cbind.data.frame(row,config.train[c])
      #file.name <- paste(file.name,config.train[c],sep = "--")
      #' ### Training set y test dataset
      df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
      u <- training.config(df,type=config.train[c])
      training.set = u$train
      test.set = u$test
      
      for(a in 1:length(alg))
      {
        
        foreach(s = 1:length(score),.packages = packages) %dopar%
        {
          
          #file.name <- paste(file.name,score[s],sep="--")
          #row <- cbind.data.frame(row,score[s])
          rr <- learn.bayes(training.set, wl,bl,alg=alg[a],sc=score[s])
          res = rr$model
          #row <- cbind.data.frame(row,rr$time) # chequear unidad del tiempo (segundos, milisegundos, etc)
          #' Aprendizaje de parametros
          #'
          start_time <- Sys.time()
          fitted = bn.fit(rr$model, training.set)     # learning of parameters
          end_time <- Sys.time()
          #row <- cbind.data.frame(row,as.numeric(end_time - start_time)) # chequear unidad del tiempo (segundos, milisegundos, etc)
          
          #end_time - start_time
          #' guardo modelo para más análisis o corridas posteriores
          #'
          
          file.name <- paste(dd$name,t,config.train[c],alg[a],score[s],sep = "--")
          save(res, file = paste(file.name,".RData",sep="")) #,Sys.time()
          print(file.name)
          
          # RMSE y otros errores por cada variable a predecir
          err <- errors_regression(pred_sensores, fitted, test.set, verbose = TRUE)
          write.csv(x = err,file = paste("./results/",file.name,"--error.csv",sep = ""))
          
          # Confusion matrix de heladas
          confm <- conf_matrix_df_frost(fitted,pred_sensores,test.set)
          write.csv(x = confm,file = paste("./results/",file.name,"--confusionMatrixHelada.csv",sep = ""))
          print(confm)
          # realizar prediccion en test.set, guardar Y - Ypred en archivo
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
          }
          
          # agrego fila al dataset
          row <- cbind.data.frame(dd$name,t,ncol(df),nrow(df),config.train[c],alg[a],score[s],rr$time,as.numeric(end_time - start_time))
          print(row)
          results <- rbind.data.frame(results,row)
          
        }# for por score
      }# for por algoritmo  
    }# por training config 
  }#for por T
}# for por dataset

#system.time(foreach(i=1:10000) %dopar% sum(tanh(1:i)))  
#   user  system elapsed 
#  3.392   0.193   3.664 
stopCluster(cl)

colnames(results) <- columnas
write.csv(x = results,file = paste("./results/",Sys.time(),"--experimentos.csv",sep = ""))

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
