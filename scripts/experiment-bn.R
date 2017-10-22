#'
#' 
#' FORMATO NOMBRE DE ARCHIVO SALIDA
#' <dataset>-<variable>-<T period en numero>-<S o N, S para SMOTE y N normal>-<H humedad incluida, N no incluida>-<algoritmo>-<score>


#' Librería para aprendizaje de redes bayesianas
library(bnlearn)

source("bnlearn-utils.R")
source("dataset-processing.R")

set.seed(147)

# si quiero guardar los dataset desfasados para ser usados por otras librerías.

SAVE_DATASET <- TRUE
HUMEDAD <- FALSE # si quiero que quite las variables relativas a humedad
SMOTE <- FALSE # si quiero que en el entrenamiento use SMOTE para agregar más samples 
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
score <- c("bic-g","loglik-g","aic-g","bde")

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
  
  return(list(model=res, time = time))
  
}


for(j in 1:3) # POR cada uno de los datasets
{
 # traigo dataset 
  dd <-get.dataset(dataset[j])
  sensores <- dd$data
  pred_sensores <- dd$pred
  row <- c(dd$name)
  file.name <- dd$name
  cat("DATASET ",dd$name,"\n")
  
  for(t in 1:length(period))
  {
    row <- cbind.data.frame(row,t)
    file.name <- paste(file.name,t,sep = "--")
    #' Obtengo dataset con variables desfasadas a t dias 
    #'
    aux <- desfasar.dataset.T(t,sensores, pred_sensores)
    pred_sensores <- aux$vars
    df <- aux$data
    
    #guardar este dataset desfasado en T
    if(SAVE_DATASET){  write.csv(aux$data,paste(file.name,"-dataset.csv",sep=""))  }
    #' nombres de las variables a usar para entrenar/testear
    #' nro de variables
    row <- cbind.data.frame(row,ncol(df))
    #' nro de ejemplos
    row <- cbind.data.frame(row,nrow(df))

    bl <- get_blacklist(pred_sensores)
    wl <- get_whitelist(pred_sensores,colnames(df))
    
    for(c in 1:length(config.train))
    {
      row <- cbind.data.frame(row,config.train[c])
      file.name <- paste(file.name,config.train[c],sep = "--")
      #' ### Training set y test dataset
      df[,1:ncol(df)] <- lapply(df[,1:ncol(df)],as.numeric) # <- convertir a numeric
      u <- training.config(df,type=config.train[c])
      training.set = u$train
      test.set = u$test
      
      #' Aprendemos una red bayesiana usando el algoritmo hc y pasando como restricciones las white y black lists
      #TODO FOR POR ALG Y SCORE 
      for(a in 1:length(alg))
      {
        file.name <- paste(file.name,alg[a],sep="--")
        row <- cbind.data.frame(row,alg[a])
        
        for(s in 1:length(score))
        {
          
          file.name <- paste(file.name,score[s],sep="--")
          row <- cbind.data.frame(row,score[s])
          rr <- learn.bayes(training.set, wl,bl,alg=alg[a],sc=score[s])
          res = rr$model
          row <- cbind.data.frame(row,rr$time) # chequear unidad del tiempo (segundos, milisegundos, etc)
          #' Aprendizaje de parametros
          #'
          start_time <- Sys.time()
          fitted = bn.fit(rr$model, training.set)     # learning of parameters
          end_time <- Sys.time()
          row <- cbind.data.frame(row,as.numeric(end_time - start_time)) # chequear unidad del tiempo (segundos, milisegundos, etc)
          
          #end_time - start_time
          #' guardo modelo para más análisis o corridas posteriores
          #'
          save(res, file = paste(file.name,Sys.time(),".RData",sep=""))
          
          # RMSE y otros errores por cada variable a predecir
          err <- errors_regression(pred_sensores, fitted, test.set, verbose = TRUE)
          write.csv(x = err,file = paste(file.name,"-error.csv",sep = ""))
          
          # Confusion matrix de heladas
          confm <- conf_matrix_df_frost(fitted,pred_sensores,test.set)
          write.csv(x = confm,file = paste(file.name,"-confusionMatrixHelada.csv",sep = ""))
          
          # realizar prediccion en test.set, guardar Y - Ypred en archivo
          for(p in 1:length(pred_sensores))
          {
            # nombre variable a predecir
            f <- paste(file.name,pred_sensores[p],sep="--")
            #row <- c(row,pred_sensores[p]) 
            # prediccion
            pred = predict(fitted, pred_sensores[p], test.set)
            # guardar csv con valor real vs predicho
            dd <- as.data.frame(cbind(test.set[pred_sensores[p]],pred))
            colnames(dd)<- c("y_real","y_pred")
            write.csv(x = dd,file = paste(f,"-Y-vs-Y_pred.csv",sep = ""))
          }
          results <- rbind.data.frame(results,row)
          # agrego fila al dataset
          
        }# for por score
      }# for por algoritmo  
    }# por training config 
  }#for por T
}# for por dataset

write.csv(x = results,file = paste(f,"-experimentos.csv",sep = ""))



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
#' 
#' #' Mostramos los parámetros para los nodos que nos interesan predecir
#' #' 
#' fitted[pred_sensores]
#' 
#' bn.fit.qqplot(fitted$las_paredes.Tmin_t)
#' bn.fit.xyplot(fitted$las_paredes.Tmin_t)
#' bn.fit.histogram(fitted$las_paredes.Tmin_t)
#' 
#' #' Markov blanket de las variables de interés para predecir
#' #' 
#' for(i in 1:length(pred_sensores))
#' {
#'   cat("Markov blanket of ",pred_sensores[i],"\n")
#'   print(mb(res,pred_sensores[i]))
#' }
#' 
#' #' Predicciones, evaluación en conjunto de testeo
#' #' 
#' 
#' df_res <- errors_regression(pred_sensores, fitted, test.set, verbose = FALSE)
#' 
#' df_res
#' #' plots de R squared
#' #' 
#' #' 
#' 
#' r2_plots_inline(pred_sensores, fitted, test.set)
#' 
#' #' llamar confusionMatrix de caret, pasar primero "a factor of predicted classes, then a factor
#' #'  of classes to be used as the true results
#' 
#' breaks.binario <- c(-10,0,50) # caso Helada y no helada
#' my.breaks <- c(-10,-5,0,2,5,10,50)
#' 
#' #' ### Caso binario: helada o no helada
#' #' 
#' 
#' conf_matrix_binario = conf_matrix(fitted,pred_sensores,test.set, breaks.binario)
#' 
#' 
#' #' ### Evaluación de predicción en rangos de temperaturas
#' #' 
#' 
#' conf_matrix_temp = conf_matrix(fitted,pred_sensores,test.set, my.breaks)
#' 
#' 
#' #' Test de predicción con randomForest
#' #' 
#' 
#' library(randomForest)
#' library(miscTools)
#' library(ggplot2)
#' 
#' 
#' for(i in 1:length(pred_sensores))
#' {
#'   # determinar la variable predictora
#'   y_label <- pred_sensores[i]
#'   df2 <- df
#'   #' renombro variable predictora por y, para facilitar formula 
#'   colnames(df2)[which(colnames(df2)==y_label)] <- "y"
#'   #' quito las otras variables predictoras, ya que solo analizaré la que se encuentre en y_label
#'   #' 
#'   df2 <- df2[,-which(names(df2) %in% pred_sensores)]
#'   colnames(df2)
#'   
#'   # train y test set
#'   until <- round(nrow(df2)*.67)
#'   training.set = df2[1:until, ] # This is training set to learn the parameters
#'   test.set = df2[until:nrow(df2), ]
#'   
#'   
#'   model <- randomForest(y ~ ., data = training.set, importance = TRUE )
#'   pred <- predict(model, test.set)
#'   #View(cbind(pred,test.set$y))
#'   mse <- mean((test.set$y - pred)^2)
#'   r2 <- rSquared(test.set$y, test.set$y - pred)
#'   
#'   cat("Variable ",pred_sensores[i]," MSE:",mse," Rsquared: ",r2)
#'   
#'   #' Plot R_2, valores predichos vs valores reales
#'   #' 
#'   
#'   p <- ggplot(aes(x=actual, y=pred),
#'               data=data.frame(actual=test.set$y, pred=pred))
#'   p2 <- p + geom_point() +
#'     geom_abline(color="red") +
#'     ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))
#'   
#'   plot(p2)
#'   
#'   cat("Confusion matrix helada/no helada",pred_sensores[2],"\n")
#'   
#'   y <- cut(test.set[,"y"], breaks = breaks.binario)
#'   y_pred <- cut(pred, breaks = breaks.binario)
#'   print(confusionMatrix(y_pred,y))
#'   
#'   cat("Confusion matrix ",pred_sensores[i],"\n")    
#'   y <- cut(test.set[, "y"], breaks = my.breaks)
#'   y_pred <- cut(pred, breaks = my.breaks)
#'   print(confusionMatrix(y_pred,y))
#'   
#'   
#' }
#' 
#' 
#' #' ## Oversampling: generamos más datos en el training.set mediante SMOTE 
#' #' Para ello dividimos el dataset entre training y testing. En el training  set aplicamos SMOTE.
#' #' 
#' #' genero etiquetas: 1 noche de helada y 0 no helada por cada estación, ya que tienen distintos datos de heladas
#' #' 
#' #' Luego evaluamos para cada una como resultó la predicción
#' #' 
#' 
#' # for(j in 1:length(pred_sensores))
#' # {
#' #   
#' #   sensor <- unlist(strsplit(pred_sensores[i],split=".",fixed = TRUE))[1]
#' #   vars <- v[grepl( sensor, v, fixed = TRUE)] # extraigo todas las variables relacionadas con sensor
#' #   #vars <- vars[-length(vars)] # quito la última variable min_t
#' #   
#' # }
#' 
#' #' Me enfoco en el caso de la estación junin, por lo que analizaremos solo la predicción sobre la misma....
#' var_pred <- pred_sensores[4]
#' Y_class <- as.factor(with(df,ifelse(df[,pred_sensores[4]] <= 0,1,0)))
#' 
#' hasta <- round(nrow(df)*.67)
#' hasta
#' summary(Y_class)
#' summary(Y_class[hasta:length(Y_class)])
#' test.set <- df[hasta:nrow(df),]
#' 
#' #' 
#' library(unbalanced)
#' 
#' #' datos para entrenar
#' data_smote <- ubBalance(df[1:(hasta-1),],Y_class[1:(hasta-1)],type = "ubSMOTE",percOver = 300, percUnder = 150)
#' 
#' #' para visualizar la distribución de las clases
#' #' 
#' summary(data_smote$Y)
#' training.set <- data_smote$X
#' #' Procedemos a realizar las predicciones y evaluar el error.
#' #' 
#' ##' Aprendemos una red bayesiana usando el algoritmo hc y pasando como restricciones las white y black lists
#' #' 
#' start_time <- Sys.time()
#' print(start_time)
#' res = hc(training.set, whitelist=wl,blacklist = bl) # , cluster = cl) # no funciona esta funcion de cluster
#' end_time <- Sys.time()
#' end_time - start_time
#' 
#' #' guardar modelo 
#' save(res, file= paste("hc-dacc-smote-t1-spring-",var_pred,"-",Sys.time(),".RData",sep=""))
#' 
#' #' Aprendizaje de parametros
#' #' 
#' start_time <- Sys.time()
#' fitted = bn.fit(res, training.set)     # learning of parameters
#' end_time <- Sys.time()
#' end_time - start_time
#' 
#' #' Mostramos los parámetros para los nodos que nos interesan predecir
#' #' 
#' fitted[pred_sensores]
#' 
#' bn.fit.qqplot(fitted[[ncol(df)]])
#' bn.fit.xyplot(fitted[[ncol(df)]])
#' bn.fit.histogram(fitted[[ncol(df)]])
#' 
#' #' Markov blanket de las variables de interés para predecir
#' #' 
#' for(i in 1:length(pred_sensores))
#' {
#'   cat("Markov blanket of ",pred_sensores[i],"\n")
#'   print(mb(res,pred_sensores[i]))
#' }
#' 
#' #' Predicciones, evaluación en conjunto de testeo, caso regresión predicción temperaturas
#' #' 
#' df_res <- errors_regression(var_pred, fitted, test.set, verbose = FALSE)
#' 
#' df_res
#' 
#' #' plots de R squared
#' #' 
#' #' 
#' r2_plots_inline(var_pred, fitted, test.set)
#' 
#' #' llamar confusionMatrix de caret, pasar primero "a factor of predicted classes, then a factor
#' #'  of classes to be used as the true results
#' 
#' breaks.binario <- c(-10,0,50) # caso Helada y no helada
#' my.breaks <- c(-10,-5,0,2,5,10,50)
#' 
#' #' ### Caso binario: helada o no helada
#' #' 
#' 
#' conf_matrix_binario = conf_matrix(fitted,var_pred,test.set, breaks.binario)
#' 
#' 
#' #' ### Evaluación de predicción en rangos de temperaturas
#' #' 
#' 
#' conf_matrix_temp = conf_matrix(fitted,var_pred,test.set, my.breaks)
#' 
#' 
