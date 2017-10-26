
library(bnlearn)
library(forecast)
library(caret)


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
