#' metrics file
#' 
#' m = model or predicted values
#' o = observed or real values
RMSE = function(m, o){  sqrt(mean((m - o)^2)) } #tested
rsq <- function(x, y){ summary(lm(y~x))$r.squared } #tested
MAE <- function(m, o){  mean(abs(m - o))} #tested


#' pred & obs must be numeric vectors or arrays with the same length
#' 
evaluate <- function(pred, obs) #tested
{
  #"RMSE","r2","Sensitivity","Acc","Precision"
  rmse <- round(RMSE(pred,obs),2)
  r2 <- round(rsq(obs,pred),2)
  mae <- round(MAE(obs,pred),2)
  breaks <- c(-20,0,50) # caso Helada y no helada
  y <- cut(obs, breaks = breaks)
  y_pred <- cut(pred, breaks = breaks)
  sens <- round(sensitivity(y_pred,y),2)
  spec <- round(specificity(y_pred, y),2)
  p <- round(precision(y_pred,y),2)
  c <- confusionMatrix(y_pred,y,mode="everything")
  acc <- round(c$overall["Accuracy"],2)
  return(list(rmse = rmse, r2 = r2, sens= sens, spec= spec, prec= p, acc= acc,cm = c, MAE= mae))
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

#Logger function
Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text,..., "\n"))
  cat(msg)
}