
require(readr)
require(xts)
require(timeDate)

dacc.spring <- function(){
  
  library(readr)
  data <- read_csv("~/phd-repos/tmin/bnlearn/data/estaciones-dacc-diarios.csv")
  # data[1632,30:35]
  # reemplazo valor erroneo 784
  data[1632,33] <- 25
  
  library(xts)
  library(timeDate)
  #' Variables del dataset
  #' 
  dfx <- xts(data[3:ncol(data)], order.by=as.timeDate(strptime(data$date,"%Y-%m-%d")))
  dd <- dfx[.indexmon(dfx) %in% c(7,8,10,11),] # mes agosto, setiembre, octubre, noviembre
  
  return(dd)
  #' Una vista breve
  
}

dacc <- function()
{
  
  sensores <- read_csv("~/phd-repos/tmin/bnlearn/data/estaciones-dacc-diarios.csv")
  
  #' Variables del dataset
  #colnames(sensores)
  #' Una vista breve
  
  #head(sensores)
  #' Cantidad de filas y columnas
  #' 
  #cat("Rows: ",nrow(sensores)," Columns: ",ncol(sensores))
  
  #' como denomino a las variables que quiero predecir
  
  pred_sensores = colnames(sensores)[c(6,12,18,23,29)]
  
  #' reemplazo valor erroneo (temperatura máxima por los 780)
  #' 
  sensores[1632,30:35]
  sensores[1632,33] <- 25
  
  #' Columnas a borrar
  erase <- colnames(sensores)[1:2]
  erase
  sensores <- sensores[,-which(names(sensores) %in% erase)]
  colnames(sensores)
  
  return(sensores)
}

desfasar.dataset.t2 <- function(sensores, pred_sensores){
  
  #' Procedemos a armar un dataset con las variables, colocando los datos de hace dos días, 
  #'  luego hace un día y luego día presente. Por ello, desfazamos el dataset para que 
  #'  queden primero las variables en T-2, T-1 y luego en t o tiempo presente.
  
  sensores_T_2 <- sensores[1:(nrow(sensores)-2),] 
  sensores_T_1 <- sensores[2:(nrow(sensores)-1),] # no incluyo la primera fila
  sensores_t <- sensores[3:nrow(sensores),] 
  
  #' renombro las columnas 
  colnames(sensores_T_2) <- paste(colnames(sensores_T_2),"_T_2",sep="")
  colnames(sensores_T_1) <- paste(colnames(sensores_T_1),"_T_1",sep="")
  
  #' del tiempo presente solo me interesa la temperatura mínima, las que quiero predecir
  sensores_t <- sensores_t[,pred_sensores]
  colnames(sensores_t) <- paste(colnames(sensores_t),"_t",sep="")
  pred_sensores <- paste(pred_sensores,"_t",sep="") # renombro variables predictoras
  
  #' creo dataset de datos de T-2, T-1 y t
  df <- cbind.data.frame(sensores_T_2,sensores_T_1,sensores_t)
  
  
  return(list(data=df,vars=pred_sensores))
}

desfasar.dataset.t1 <- function(sensores, pred_sensores){
  
  #' Procedemos a armar un dataset con las variables, colocando los datos de hace dos días, 
  #'  luego hace un día y luego día presente. Por ello, desfazamos el dataset para que 
  #'  queden primero las variables en T-2, T-1 y luego en t o tiempo presente.
  
  sensores_T_1 <- sensores[1:(nrow(sensores)-1),] 
  sensores_t <- sensores[2:nrow(sensores),] # no incluyo la primera fila
  
  #' renombro las columnas 
  colnames(sensores_T_1) <- paste(colnames(sensores_T_1),"_T_1",sep="")
  
  #' del tiempo presente solo me interesa la temperatura mínima, las que quiero predecir
  sensores_t <- sensores_t[,pred_sensores]
  colnames(sensores_t) <- paste(colnames(sensores_t),"_t",sep="")
  pred_sensores <- paste(pred_sensores,"_t",sep="") # renombro variables predictoras
  
  #' creo dataset de datos de T-2, T-1 y t
  df <- cbind.data.frame(sensores_T_1,sensores_t)
  
  return(list(data=df,vars=pred_sensores))
}