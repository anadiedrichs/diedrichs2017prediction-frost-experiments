
require(readr)
require(xts)
require(timeDate)


dataset <- c("dacc","dacc-temp","dacc-spring")

#' Get a dataset by index
#' 
#' 
get.dataset <- function(d)
{
  if(d==dataset[1]) return(dacc())
  else if(d==dataset[2]) return(dacc.spring())
  else if(d==dataset[3]) return(dacc.temp())
  else stop("ERROR get.dataset, you muss pass the correct argument value")
}

# probado
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
  
  #TODO cambiar return a 
  
  #return(list(data=sensores, pred= pred_sensores,name="dacc"))
  return(dd)
  #' Una vista breve
  
}

# probado
dacc <- function()
{
  
  sensores <- read_csv("~/phd-repos/tmin/bnlearn/data/estaciones-dacc-diarios.csv")
  
  #pred_sensores = colnames(sensores)[c(6,12,18,23,29)]
  
  #' reemplazo valor erroneo (temperatura máxima por los 780)
  #' 
  sensores[1632,33] <- 25
  
  #' Columnas a borrar
  erase <- colnames(sensores)[1:2]
  erase
  sensores <- sensores[,-which(names(sensores) %in% erase)]
  colnames(sensores)
  
  #' como denomino a las variables que quiero predecir
  pred_sensores = colnames(sensores)[c(4,10,16,21,27)]
  
  return(list(data=sensores, pred= pred_sensores,name="dacc"))
}

# TODO HACER!!!
dacc.temp <- function()
{
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
 
  #' Quitar columnas de humedad
  #' las columnas con humedad tienen los caractere Hm
  #' 
  h <- colnames(dfx)[grepl("Hm", colnames(dfx))]
  d <- dfx[,-which(names(dfx) %in% h)]
  
  # lso valores minimos diarios son los que nos interesa predecir
  pred_sensores = colnames(sensores)[c(1,4,7,12,18)]
  
  return(list(data = d, pred = pred_sensores, name="dacc-temp"))
}


# tested
desfasar.dataset.T <- function(T_value,sensores, pred_sensores){
  
  #' Procedemos a armar un dataset con las variables, colocando los datos de hace dos días, 
  #'  luego hace un día y luego día presente. Por ello, desfazamos el dataset para que 
  #'  queden primero las variables en T-2, T-1 y luego en t o tiempo presente, si T=2.
  #'  
  
  df <- NULL
  fin <- T_value
  
  for(t in 1:T_value)
  {
    aux <- sensores[t:(nrow(sensores)-fin),] 
    colnames(aux) <- paste(colnames(aux),"_T_",fin,sep="")
    if(is.null(df)){df <- aux}
    else df <- cbind.data.frame(df,aux)
    fin <- fin - 1
    
  }
  
  #' del tiempo presente solo me interesa la temperatura mínima, las que quiero predecir
  sensores_t <- sensores[(T_value+1):(nrow(sensores)),pred_sensores] 
  
  #sensores_t <- sensores_t[,pred_sensores]
  colnames(sensores_t) <- paste(colnames(sensores_t),"_t",sep="")
  pred_sensores <- paste(pred_sensores,"_t",sep="") # renombro variables predictoras
  
  #' creo dataset de datos de T-2, T-1 y t, caso T=2
  df <- cbind.data.frame(df,sensores_t)
  
  return(list(data=df,vars=pred_sensores))
}

# deprecated, usar desfasar.dataset.T
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

# deprecated, usar desfasar.dataset.T
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