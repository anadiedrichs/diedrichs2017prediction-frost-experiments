
library(caret)
library(bnlearn)

RMSE = function(m, o){  sqrt(mean((m - o)^2)) } #tested
rsq <- function(x, y){ summary(lm(y~x))$r.squared } #tested
MAE <- function(m, o){  mean(abs(m - o))} #tested


data(learning.test)

data(gaussian.test)
net = hc(gaussian.test)
# deberia chequear que net tenga todos arcos dirigidos
# sino el fit no va a funcionar, tirar error

fit = bn.fit(net, gaussian.test)

# ejemplo o intento con caret 

fitControl <- trainControl(method = "cv",
                           number = 5,
                           search = "grid")

set.seed(825)
bn.model <- caret::train(x= gaussian.test, # dataset con todas las variables
                         y = gaussian.test$A, # pasar cualquier variable numerica, sera ignorada
                         data = gaussian.test, 
                         preProc = c("center", "scale"),
                   method = bnReg, 
                   trControl = fitControl, 
                   node = "A") # importante, sobre cual variable se quiere mejorar la prediccion
plot(bn.model$finalModel$network)
print(bn.model)
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


