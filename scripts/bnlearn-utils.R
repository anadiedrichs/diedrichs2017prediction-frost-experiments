#library(bnlearn)
#library(caret) # para confusion matrix
library(forecast) # para metodo accuracy


#' m = model or predicted values
#' o = observed or real values
RMSE = function(m, o){  sqrt(mean((m - o)^2)) } #tested
rsq <- function(x, y){ summary(lm(y~x))$r.squared } #tested
#' pred & obs must be numeric vectors or arrays with the same length
#' 
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
#' pred and obs are factor vectors of the same levels. 
evaluate.classification <- function(pred, obs) #tested
{
  #"RMSE","r2","Sensitivity","Acc","Precision"
#  rmse <- round(RMSE(pred,obs),2)
#  r2 <- round(rsq(obs,pred),2)
#  breaks <- c(-20,0,50) # caso Helada y no helada
  y <- obs
  y_pred <- pred
  c <- confusionMatrix(y_pred,y,mode="everything") # que otras metricas sacamos de la matriz de confusion 
  
  sens <- round(c$byClass["Sensitivity"],2)
  spec <- round(c$byClass["Specificity"],2)
  p <- round(c$byClass["Precision"],2)
  FAR <- round(c$table[1,2] / (c$table[1,2] + c$table[1,1]),2)
  acc <- round(c$overall["Accuracy"],2)
  return(list( sens= sens, spec= spec, prec= p, acc= acc, far = FAR, cm = c))
}

#' blacklist: las variables predictoras no pueden conectarse entre ellas
#' Recibe un array (pred_sensores) con nombres de variables, es decir, cada elemento del array es un string.
#' Regresa un dataframe con campos from y to.
#' tested
get_blacklist <- function(pred_sensores)
{
  bl <- data.frame(from=character(),to=character(),stringsAsFactors=FALSE)
  levels(bl$from) <- pred_sensores
  levels(bl$to) <- pred_sensores
  str(bl)
  total <- length(pred_sensores)
  
  for(i in 1:total)
  {
    bl1 = data.frame(from = rep(pred_sensores[i],total), to = pred_sensores)
    bl2 = data.frame(from = pred_sensores, to = rep(pred_sensores[i],total))
    bl = rbind.data.frame(bl,bl1,bl2)
  }
  
  #print(bl)
  return(bl)
}

#' white list de aristas, lista de arcos que si o si tienen que ir en la red Bayesiana. 
#' Consideramos arcos dirigidos desde las variables _T_1 y _T_2 hacia el t de un mismo sensor
#' 
#' funciona para dataset tmin chaar junin SI se setea dataset_tmin_chaar = TRUE
#' tested
get_whitelist <- function(pred_sensores,variables,dataset_tmin_chaar = FALSE){
  
  wl <- data.frame(from=character(),to=character(),stringsAsFactors=FALSE)
  #levels(bl$from) <- pred_sensores
  #levels(bl$to) <- pred_sensores
  str(wl)
  total <- length(pred_sensores)
  v <- variables
  
  for(i in 1:total)
  {
    sensor <- unlist(strsplit(pred_sensores[i],split=".",fixed = TRUE))[1]
    if(dataset_tmin_chaar==TRUE) sensor <- paste(sensor,".",sep="") # esto es lo diferente, por bug #
    vars <- v[grepl( sensor, v, fixed = TRUE)] # extraigo todas las variables relacionadas con sensor
    vars <- vars[-length(vars)] # quito la última variable min_t
    
    wl1 = data.frame(from = vars, to = pred_sensores[i])
    wl = rbind.data.frame(wl,wl1)
  }
  
  #print(wl)
  return(wl)
}

#Logger function
Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text,..., "\n"))
  cat(msg)
}

#' As trainingNormal. The difference: after fitting the predict function is apply only to one "var"
trainingNormalOneVar <- function(df,alg,sc, file.name, var, fila,p=split.train)
{
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc,var=var)
  res = rr$model
  save(res, file = paste(PATH.MODELS,file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  #'
  variables <- c(as.character(unique(wl[which(wl$to== var),"from"])),var)
  start_time <- Sys.time()
  fitted = bn.fit(rr$model, training.set[variables])     # learning of parameters
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
  save(fitted, file = paste(PATH.MODELS,file.name,"--fitted.RData",sep="")) #,Sys.time()
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

#' df: dataset, columns are variable or features
#' alg: values possible hc, tabu or local
#' file.name: string, how the model, results, etc files will be named
#' pred_sensores: an array with strings, names of the variables we are interested on predict
#' fila: string, values separated by comma, header_row to save in file *--experiment--
trainingNormal <- function(df,alg,sc, file.name, pred_sensores, fila,p=split.train)
{
  
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc)
  res = rr$model
  save(res, file = paste(PATH.MODELS,file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  #'
  start_time <- Sys.time()
  fitted = bn.fit(rr$model, training.set)     # learning of parameters
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)
  save(fitted, file = paste(PATH.MODELS,file.name,"--fitted.RData",sep="")) #,Sys.time()
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

#' df: dataset, columns are variable or features
#' alg: values possible hc, tabu or local
#' file.name: string, how the model, results, etc files will be named
#' pred_sensores: an array with strings, names of the variables we are interested on predict
#' fila: string, values separated by comma, header_row to save in file *--experiment--
#' 
#' This method split train and test set from df dataset, then apply SMOTE over train set. 
#' In order to use SMOTE, an y vector is created, which contains the label 1: frost event or 0; no event
trainingSMOTE <- function(df,alg,sc, file.name, var, fila,p=split.train)
{
  until <- round(nrow(df)*p)
  training.set = df[1:until-1, ] # This is training set to learn the parameters
  test.set = df[until:nrow(df), ]
  nfrostorig <- length(training.set[training.set[,var] <= 0,var])
  
  #' Me enfoco en el caso de una sola estación 
  Y_class <- as.factor(with(df,ifelse(df[,var] <= 0,1,0)))
  #summary(Y_class)
  #summary(Y_class[until:length(Y_class)])
  
  #' datos para entrenar
  data_smote <- ubBalance(training.set,Y_class[1:(until-1)],type = "ubSMOTE",percOver = 300, percUnder = 150)
  
  #guardar este dataset desfasado en T
  if(SAVE_DATASET){  write.csv(data_smote$X,paste(paste(PATH.SAVE.DATASET,file.name,var,"smote-dataset.csv",sep="--"))) }
  
  #' para visualizar la distribución de las clases
  #' 
  summary(data_smote$Y)
  training.set <- data_smote$X
  nfrost <- length(data_smote$X[data_smote$X[,var] <= 0,var])
  
  
  rr <- learn.bayes(training.set, wl,bl,alg=alg,sc=sc,var=var)
  res = rr$model
  save(res, file = paste(PATH.MODELS,file.name,"--bn.RData",sep="")) #,Sys.time()
  
  #' Aprendizaje de parametros
  variables <- c(as.character(unique(wl[which(wl$to== var),"from"])),var)
  
  start_time <- Sys.time()
  if(alg=="local"){fitted = bn.fit(rr$model, training.set[variables])}
  else fitted = bn.fit(rr$model, training.set)
  end_time <- Sys.time()
  fitted.time <- round(as.numeric(end_time-start_time),3)
  save(fitted, file = paste(PATH.MODELS,file.name,"--fitted.RData",sep="")) #,Sys.time()
  
  # nombre variable a predecir
  f <- paste(file.name,var,sep="--")
  #row <- c(row,pred_sensores[p]) 
  # prediccion
  pred = predict(fitted, var, test.set)
  # guardar csv con valor real vs predicho
  dataset <- as.data.frame(cbind(test.set[var],pred))
  colnames(dataset)<- c("y_real","y_pred")
  write.csv(x = dataset,file = paste(PATH.RESULTS ,f,"--Y-vs-Y_pred.csv",sep = ""))
  
  
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

#' Bayesian network structure learning
#' 
learn.bayes <- function(df, wl=NULL,bl=NULL,alg="hc",sc="bic",var=NULL)
{
  time = NULL; res = NULL
  if(alg!="local" && !which(sc %in% score)) stop("score inexistente en learn.bayes")
  
  if(alg=="hc")
  {
    start_time <- Sys.time()
    res = hc(df, whitelist=wl,blacklist = bl, score = sc)
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

######################################################################################3
#' Los siguientes métodos no son usados en experiment-bn*, pero si son
#' usados en los notebooks o *preProcess scripts examples

#' Matriz de confusion calculada usando libreria caret
#' fitted objeto clase bn.fit
#' pred_sensores array nombre de variables de interes a predecir
#' test.set dataset del conjunto de testeo
#' 
#' tested
conf_matrix <- function(fitted,pred_sensores,test.set, breaks, verbose = TRUE)
{
  library(caret)
  
  lista <- vector("list",length(pred_sensores))
  
  for(i in 1:length(pred_sensores))
  {
    if (verbose) cat("Variable ",pred_sensores[i],"\n")
    pred = predict(fitted, pred_sensores[i], test.set)
    y <- cut(test.set[, pred_sensores[i]], breaks = breaks)
    y_pred <- cut(pred, breaks = breaks)
    c <- confusionMatrix(y_pred,y)
    if(verbose) print(c)
    # añadir c a la lista
    lista[[i]] <- c
  }
  # regresar la lista
  return(lista)
}

#' Function
#' 

require(graph)
require(igraph)
bn2igraph <- function(g.bn){
  g <- igraph.from.graphNEL(as.graphNEL(g.bn))
}

test.plot <- function(){
  
  load(file="~/phd-repos/tmin/bnlearn/scripts/results/dacc--1--smote--hc--aic-g--bn.RData")
}


#'caso para breaks binario
conf_matrix_df_frost <- function(fitted,pred_sensores,test.set, verbose = TRUE)
{
  require(caret)
  
  breaks <- c(-20,0,50) # caso Helada y no helada
  columnas <- c("Variable","A","B","C","D","Accuracy","Sensitivity/Recall","Specificity","Precision")
  
  results <- data.frame(Variable=as.Date(character()),A=numeric(),B=numeric(),C=numeric(),
                        D=numeric(), accuracy= as.numeric(), sensitivity = as.numeric(),
                        specificity = as.numeric(), precision= as.numeric()
                        ,stringsAsFactors=FALSE) 
  
  for(i in 1:length(pred_sensores))
  {
    pred = predict(fitted, pred_sensores[i], test.set)
    y <- cut(test.set[, pred_sensores[i]], breaks = breaks)
    y_pred <- cut(pred, breaks = breaks)
    
    c <- confusionMatrix(y_pred,y)
    
    A <- c$table[1]
    B <- c$table[3]
    C <- c$table[2]
    D <- c$table[4]
    
    sens <- round(sensitivity(y_pred,y),2)
    spec <- round(specificity(y_pred, y),2)
    p <- round(precision(y_pred,y),2)
    acc <- round(c$overall["Accuracy"],2)
    
    #aux <- c(A,B,C,D,acc,sens,spec,p)
    #aux <- apply(aux,as.numeric)
    results <- rbind.data.frame(results, cbind.data.frame(pred_sensores[i],A,B,C,D,acc,sens,spec,p))
    
  }
  colnames(results) <- columnas
  # regresar la lista
  return(results)
}

#' ## REGRESSION TOOLS
#' r squared plots
#' 
#' tested

r2_plots_inline <- function(pred_sensores, fitted, test.set)
{
  library(miscTools)
  library(ggplot2)
  
  for(i in 1:length(pred_sensores))
  {
    # predicts the value of node pred_sensores[i]  given test set
    pred = predict(fitted, pred_sensores[i], test.set) 
    
    #' Plot R_2, valores predichos vs valores reales
    #' 
    (r2 <- rSquared(test.set[,pred_sensores[i]], test.set[,pred_sensores[i]] - pred))
    
    p <- ggplot(aes(x=actual, y=pred),
                data=data.frame(actual=test.set[,pred_sensores[i]], pred=pred))
    pp <- p + geom_point() +
      geom_abline(color="red") +
      ggtitle(paste("R^2=", round(r2,2)," for ",pred_sensores[i], sep=""))
    
    plot(pp)
  }
}

#' error tables
#' 
#' ### Valores de error en MAE, RMSE
#' 
#' fitted objeto clase bn.fit
#' pred_sensores array nombre de variables de interes a predecir
#' test.set dataset del conjunto de testeo
#' tested

errors_regression <- function(pred_sensores, fitted, test.set, verbose = TRUE){
  
  df_res <- data.frame(Variable=character(),ME=double(),RMSE=double(),MAE=double(),MPE=double(),MAPE=double(),stringsAsFactors=FALSE)
  levels(df_res$Variable) <- pred_sensores
  #str(df_res)
  col <- c("Variable","ME","RMSE","MAE","MPE","MAPE")
  
  library(forecast)
  
  for(i in 1:length(pred_sensores))
  {
    #cat("Testing on ",pred_sensores[i],"\n")
    # predicts the value of node pred_sensores[i]  given test set
    pred = predict(fitted, pred_sensores[i], test.set)  
    # compare the actual and predicted, then print the values
    aux <- accuracy(f = pred, x = test.set[,pred_sensores[i]])
    aux <- round(aux,2)
    #print(cbind(pred, real=test.set[, pred_sensores[i]]))
    
    r <- c(as.character(pred_sensores[i]),apply(aux,2,as.double))
    df_res <- rbind.data.frame(df_res,r,stringsAsFactors = FALSE) 
  }
  
  colnames(df_res) <- col
  if(verbose) print(df_res)
  return(df_res)
}

#' 
#' 
#' testeado unitariamente
#' 
#' 
yreal_ypred_plot_inline <- function(pred_sensores, fitted, test.set)
{
  library(miscTools)
  library(ggplot2)
  x_ <- seq(from=1,to=nrow(test.set),by=1)
  
  for(i in 1:length(pred_sensores))
  {
    # predicts the value of node pred_sensores[i]  given test set
    pred = predict(fitted, pred_sensores[i], test.set) 
    
    # ggplot(test_data, aes(x)) + 
    #   geom_line(aes(y = var0, colour = "var0")) + 
    #   geom_line(aes(y = var1, colour = "var1"))
    # 
    # 
    p <- ggplot(aes(x),
                data=data.frame(x=x_, actual=test.set[,pred_sensores[i]], pred=pred))
    pp <- p + geom_line(aes(y = actual, colour = "real")) + 
      geom_line(aes(y = pred, colour = "prediction"))  +
      ggtitle(paste("Valor real vs predicción en ", pred_sensores[i], sep=""))
    
    plot(pp)
  }
}
