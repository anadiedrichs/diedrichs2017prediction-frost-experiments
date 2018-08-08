
library(caret)
library(bnlearn)
source("metrics.R")
source("reproducibility.R")
SEED <- 147

settingMySeeds <- function(model,tunelen) #TODO
{ 
  ss <- NULL
  
  if(model=="rpart") ss <- setSeeds(numbers=KFOLDS,tunes = 10,seed = SEED) #TODO tuneLenRpart
  if(model=="rf")  ss <- setSeeds(numbers=KFOLDS,tunes = tunelen,seed = SEED)
  if(model=="bnReg") ss <- setSeeds(numbers=KFOLDS,tunes = 8,seed = SEED) # por defecto, 8 parametros a tunear
  ss
}

source("bnlearnRegression.R")
source("metrics.R")

data(learning.test)
data(gaussian.test)
net = hc(gaussian.test)
# deberia chequear que net tenga todos arcos dirigidos
# sino el fit no va a funcionar, tirar error

fit = bn.fit(net, gaussian.test)

# ejemplo o intento con caret 
# VER PORQUE DEMORA TANTO BNREGRESSION
cc <- createTimeSlices(1:3600,initialWindow=250,horizon=100,fixedWindow=FALSE)
KFOLDS <<- length(cc$train)
seeds <- settingMySeeds("bnReg",8)

#timeSlicesTrain <- createTimeSlices(1:nrow(training.set),initialWindow = T,horizon = 1,fixedWindow = TRUE)
fitControl <- trainControl(method = "timeslice",# number = KFOLD,
                                 initialWindow = 250, horizon = 100, fixedWindow = TRUE,
                                 seeds = seeds)


start_time <- Sys.time()
bn.model <- caret::train(x= gaussian.test[1:3600,], # dataset con todas las variables
                         y = gaussian.test[1:3600,]$A, # pasar cualquier variable numerica, es node
                         data = gaussian.test, 
                         #preProc = c("center", "scale"),
                   method = bnReg, 
                   trControl = fitControl, 
                   node = "A") # importante, sobre cual variable se quiere mejorar la prediccion
end_time <- Sys.time()
runtime <- round(as.numeric(difftime(end_time, start_time, units = "secs")),3)

plot(bn.model$finalModel$network)
print(bn.model)



# varImp no funciona, requiere regresar variables y un número que indique su importancia.
#vvv <- varImp(bn.model,node="A")

pred <- predict(bn.model,gaussian.test,node="A")
real <- gaussian.test$A
MAE(real,pred)
# EXTRACt de markov blanket of the network
mb(bn.model$finalModel$network,node="A")


# PRUEBA CON SERIE TEMPORAL 

fitControl <- trainControl(method = "cv",number = 10,search = "grid",
                           initialWindow = 2, 
                           horizon = 1, 
                           fixedWindow = TRUE)
set.seed(825)
# NO PUEDO IGNORAR y ARGUMENTO pues en caso de serie temporal deberia
# realizar un join entre x e y para pasarlo luego 
# a bnlear
bn.model <- caret::train(x= gaussian.test, # dataset con todas las variables
                         y = gaussian.test$A, # pasar cualquier variable numerica, sera ignorada
                         data = gaussian.test, 
                         preProc = c("center", "scale"),
                         method = bnReg, 
                         trControl = fitControl, 
                         node = "A") # importante, sobre cual variable se quiere mejorar la prediccion
plot(bn.model$finalModel$network)
print(bn.model)
# EXTRACt de markov blanket of the network
mb(bn.model$finalModel$network,node="A")

varImp(bn.model,useModel=FALSE)


