library(randomForest)
library(caret)

source("bnlearnRegression.R")
source("bnlearn-utils.R")
source("dataset-processing.R")


# Load dataset dacc
dd <-get.dataset(dataset[1])

sensores <- dd$data[-1] #quito columna date o primer columna

pred_sensores <- dd$pred

set.seed(3456)
porc_train = 0.68

#' ### Training set y test dataset
until <- round(nrow(sensores)*porc_train)
training.set = as.data.frame(sensores[1:until-1, ]) # This is training set to learn the parameters
test.set = as.data.frame(sensores[until:nrow(sensores), ])

X <- training.set 
Y <- as.numeric(training.set[,pred_sensores[1]])
varpred <- pred_sensores[1]
print(varpred)
#real <- test.set[,pred_sensores[1]]

#' MOdelo entrenado usando caret + timeslices
load("../dacc--junin.temp_min--normal--all--1--rf.RData")

print(model$bestTune)
# da mtry = 2 y ntree=500 by default, parametros que luego usamos para entrenar los modelos.

#' ### Test modelo random forest entrenado por caret para predecir usando datos test.set, sin convertirlos a timeslices: ¿los convierte caret por si mismo?
#' pruebo prediccion pasando test.set asi como viene, vemos qeu hace caret dentro.
#' Es como pasan test.set en muchos ejemplos de caret. ¿es lo mismo para timeseries?
pred.caret <- predict(model,test.set)
evaluate(pred.caret,test.set$junin.temp_min)
plot(test.set$junin.temp_min, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred.caret,test.set$junin.temp_min)))
points(pred.caret, col = "blue") 

#' Testeando modelo caret pero pasando lso datos test.set en formato timeSlices
#' pruebo pasando el test.set como timeslices, al mismo modelo entrenado por caret, el anterior

timeSlicesTest <- createTimeSlices(1:nrow(test.set), 
                                   initialWindow = 1, horizon = 1, fixedWindow = TRUE)

xSlices <- timeSlicesTest[[1]]
ySlices <- timeSlicesTest[[2]]
head(xSlices)
head(ySlices)
pred.caret2 <- predict(model,test.set[unlist(xSlices),])
evaluate(pred.caret2,test.set[unlist(ySlices),varpred])

plot(test.set[unlist(ySlices),varpred], col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred.caret2,test.set[unlist(ySlices),varpred])))
points(pred.caret2, col = "blue") 
#' ¿Será que caret ya lo vuelve a convertir dentro en timeSlices y rompo el formato?
#' 
#' ### Entrenando y prediciendo modelo randomForest con mi método de desfasar dataset T
#' 
#' Entrenar randomForest con mtry=2 usando T=1, usando mi metodo de desfasar dataset.
#' Este metodo renombra las variables temporales pprevias, por lo que a fines del modelo de 
#' random Forest se crean más variables. Por este motivo creo que el resultado es diferente
#' 
train.data <- desfasar.dataset.T(T_value = 1,training.set,pred_sensores = pred_sensores)

print(train.data$vars)
print(head(train.data$data))
# entrenar random forest
model.rf1 <- randomForest(x = train.data$data[,-which(colnames(train.data$data) %in% train.data$vars)],y=train.data$data[,train.data$vars[1]], 
                      importance = TRUE, proximity=FALSE,
                      ntree=500, mtry=2)
# armar testset
test.data <- desfasar.dataset.T(T_value = 1,test.set,pred_sensores = pred_sensores)

# predecir
pred.rf <- predict(model.rf1,test.data$data[,-which(colnames(test.data$data) %in% test.data$vars)] )
# evaluar resultados
real <- test.data$data[,test.data$vars[1]]

df <- data.frame(pred=pred.rf, obs= real)

eee <- evaluate(pred.rf, real)
print(eee)

plot(real, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred.rf,real)))
points(pred.rf, col = "blue") 

#' ### Entreno randomForest (sin caret) usando timeSlices
#' ahora si uso timeSlices para entrenar randomForest model y para predecir.

timeSlices <- createTimeSlices(1:nrow(training.set), 
                               initialWindow = 1, horizon = 1, fixedWindow = TRUE)

str(timeSlices,max.level = 1)

Xx <- timeSlices[[1]]
Yx <- timeSlices[[2]]

model.rf <- randomForest(x = training.set[unlist(Xx),],y=training.set[unlist(Yx),varpred], 
                         importance = TRUE, proximity=FALSE,
                         ntree=500, mtry=2)


pred <- predict(model.rf,test.set[unlist(xSlices),])

true <- test.set[unlist(ySlices),varpred]

plot(true, col = "red", ylab = "true (red) , pred (blue)", ylim = range(c(pred,true)))
points(pred, col = "blue") 


print(evaluate(pred, true))

#' ### ¿cuál de todos los enfoques usar?