#' Probando bnlearn para crear redes bayesianas a partir de sensores.csv dataset
#' Utilizar los datos de origen sin discretizar, por lo que la red bayesiana trabajara con datos continuos
#' un pequeño test para T+1 o prediccion al dia siguiente en un punto
#' 


#' Ejemplo o prueba de uso de datos continuos
#' 
library(bnlearn)                       
library(forecast)

data(gaussian.test)
training.set = gaussian.test[1:4000, ] # This is training set to learn the parameters
test.set = gaussian.test[4001:4010, ]  # This is test set to give as evidence
res = hc(training.set)                 # learn BN structure on training set data 
fitted = bn.fit(res, training.set)     # learning of parameters
pred = predict(fitted, "C", test.set)  # predicts the value of node C given test set
cbind(pred, test.set[, "C"])           # compare the actual and predicted
accuracy(f = pred, x = test.set[, "C"])

#' caso dataset sensores.csv
#' 

library(readr)
sensores <- read_csv("../data/sensores.csv")

sensores_T <- sensores[2:nrow(sensores),] # no incluyo la primera fila
# renombro las columnas agregando T mayuscula al final
colnames(sensores_T) <- paste(colnames(sensores_T),"_T",sep="")
# quito algunos sensores para simplicar el problema

df <- cbind(sensores[1:(nrow(sensores)-1),-1],sensores_T[,-1])


training.set = df[1:350, ] # This is training set to learn the parameters
test.set = df[351:nrow(df), ]  # This is test set to give as evidence

#res2 = si.hiton.pc(training.set,alpha = 0.01) # <-- it takes a while running...
# si uso lo anterior, da error al tratar de usar bn.fit, error:
# Error in bn.fit(res, training.set) : the graph is only partially directed
res3 <- inter.iamb(training.set, test = "smc-cor", B = 100, alpha = 0.01)
dag = cpdag(res2)
dag = pdag2dag(res2)
# lo siguiente tarda en correr
res = hc(training.set)                 # learn BN structure on training set data 
fitted = bn.fit(res, training.set)     # learning of parameters
pred = predict(fitted, "S14.min", test.set)  # predicts the value of node S14.min given test set
cbind(pred, test.set[, "S14.min"])           # compare the actual and predicted
accuracy(f = pred, x = test.set[, "S14.min"])

# graficar red bayesiana

# plot solo

# usando igraph libreria
library(igraph)

g <- graph.adjacency(amat(res), mode="directed",add.rownames="code")
#'   jpeg(file = paste(PLOT_PATH,"/",x$name,".jpg",sep = "")) # descomentar para guardar imagen del grafo red bayesiana
#' layout =layout.fruchterman.reingold
l <-layout.spring(g) 
plot(g,layout = l,vertex.size=4,vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.3)#edge.arrow.size=.5, edge.width=1,vertex.label.font=.51)

