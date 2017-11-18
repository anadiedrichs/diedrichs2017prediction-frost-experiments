
library(readr)
library(xts)
library(timeDate)

#' Dataset dacc (año 2007 hasta 2014) son los datos sumarizados diarios de las estaciones:
#' 
#' * Junín
#' * Tunuyán
#' * Agua Amarga
#' * Las Paredes
#' * La Llave
#' 
#' Dataset dacc-temp es el dataset dacc pero solo con información de temperaturas
#' 
#' Dataset dacc-spring es el dataset dacc pero solo con datos de dias de agosto hasta noviembre inclusive de cada año
#' 
#' Dataset inta-junin : 20 sensores de temperatura en parcela 20x20
#' 

all.dataset.list <<- c("dacc","dacc-temp","dacc-spring","inta") # all available datasets
dataset.list <<- NULL
dataset <<- c("dacc")#,"dacc","dacc-temp") #,"dacc-spring") 

get.list.of.datasets <- function(name){
  
  if(name=="dacc"){dataset.list <<- c("dacc","dacc-temp","dacc-spring")}
  else if(name=="inta") {dataset.list <<- c("inta")}
  else if(name=="ur") {dataset.list <<- NULL}
  else stop("ERROR get.list.of.datasets, possible values are dacc, inta or ur")
  
  return(dataset.list)
}
#' Get a dataset by name

get.dataset <- function(d)
{
  if(d==all.dataset.list[1]) return(dacc_v2())
  else if(d==all.dataset.list[2]) return(dacc.temp_v2())
  else if(d==all.dataset.list[3]) return(dacc.spring_v2())
  else if(d==all.dataset.list[4]) return(inta_data()) #TODO
  else stop("ERROR get.dataset, you muss pass the correct argument value")
}

inta_data <- function()
{
  sensores <- read_csv("~/phd-repos/tmin/bnlearn/data/sensores.csv")
  #' Columnas a borrar "X1" 
  #sensores <- sensores[-1]
  #' dejar columnas de max, min y media
  sensores <- sensores[-which(grepl("Est",colnames(sensores),fixed=TRUE))] # quito columnas de la estacion
  colmin <- colnames(sensores)[grepl("min",colnames(sensores),fixed=TRUE)]
  colmax <- colnames(sensores)[grepl("max",colnames(sensores),fixed=TRUE)]
  colavg <- c(colnames(sensores)[grepl("media",colnames(sensores),fixed=TRUE)],
              colnames(sensores)[grepl("temp_med",colnames(sensores),fixed=TRUE)])
  sensores <- sensores[c(colmin,colmax,colavg)]
  #' como denomino a las variables que quiero predecir, temperaturas minimas
  #pred_sensores <- colmin[-21] # quito la minima de humedad de la estacion
  pred_sensores <- colmin
  return(list(data=sensores, pred= pred_sensores,name="inta"))
}

# probado
dacc_v2 <- function()
{
  
  sensores <- suppressWarnings(read_csv("~/phd-repos/tmin/bnlearn/data/dacc-daily-tmin.csv"))
  
  #' Columnas a borrar "X1" 
  sensores <- sensores[-1]
  
  #' como denomino a las variables que quiero predecir
  pred_sensores <- colnames(sensores)[c(7,14,21,28,35)]
  
  #' quito radiación, no la he usado en planteo original y hay valores muy negativos, extraños
  h <- colnames(sensores)[grepl("radiacion", colnames(sensores))]
  sensores <- sensores[,-which(names(sensores) %in% h)]
  
  return(list(data=sensores, pred= pred_sensores,name="dacc"))
  
}
#tested
dacc.spring_v2 <- function()
{
  
  d <- dacc_v2()
  data <- d$data
  
  #' Variables del dataset
  #' 
  dfx <- xts(data[3:ncol(data)], order.by=as.Date(strptime(data$date,"%Y-%m-%d")))
  dd <- dfx[.indexmon(dfx) %in% c(7,8,9,10),] # mes agosto, setiembre, octubre, noviembre #BUG me incluia diciembre y el 20xx-01-01 , solved
  d1 <- data.frame(date=index(dd), coredata(dd))
  
  return(list(data = d1, pred = d$pred, name="dacc-spring"))
}

dacc.temp_v2 <- function()
{
  d <- dacc_v2()
  data <- d$data
  
  #' Quitar columnas de humedad
  #' las columnas con humedad tienen los caracteres Hm
  #' 
  h <- colnames(data)[grepl("humedad", colnames(data))]
  data <- data[,-which(names(data) %in% h)]
  
  return(list(data = data, pred = d$pred, name="dacc-temp"))
}

# probado
# deprecated at 30 oct 2017
dacc <- function()
{
  
  sensores <- read_csv("~/phd-repos/tmin/bnlearn/data/estaciones-dacc-diarios.csv")
  
  #' reemplazo valor erroneo (temperatura máxima por los 780)
  #' 
  sensores[1632,33] <- 25
  
  #' Columnas a borrar "X1"
  sensores <- sensores[,-1]
  
  #' como denomino a las variables que quiero predecir
  pred_sensores = colnames(sensores)[c(5,11,17,22,28)]
  
  #' quito datos estacion el marcado
  #' 
  h <- colnames(sensores)[grepl("el_marcado", colnames(sensores))]
  sensores <- sensores[,-which(names(sensores) %in% h)]
  
  return(list(data=sensores, pred= pred_sensores,name="dacc"))
}

# tested
# deprecated at 30 oct 2017
dacc.spring <- function(){
  
  d <- dacc()
  data <- d$data
  
  #' Variables del dataset
  #' 
  dfx <- xts(data[3:ncol(data)], order.by=as.Date(strptime(data$date,"%Y-%m-%d")))
  dd <- dfx[.indexmon(dfx) %in% c(7,8,10,11),] # mes agosto, setiembre, octubre, noviembre
  d1 <- data.frame(date=index(dd), coredata(dd))
  
  return(list(data = d1, pred = d$pred, name="dacc-spring"))
}

# probado
# deprecated at 30 oct 2017
dacc.temp <- function()
{
  d <- dacc()
  data <- d$data
  
  #' Quitar columnas de humedad
  #' las columnas con humedad tienen los caracteres Hm
  #' 
  h <- colnames(data)[grepl("Hm", colnames(data))]
  data <- data[,-which(names(data) %in% h)]
  
  return(list(data = data, pred = d$pred, name="dacc-temp"))
}

#' slide window method
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
