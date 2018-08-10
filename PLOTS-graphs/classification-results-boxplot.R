#' ## Resultados clasificación
#' 
library(readr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(lazyeval)

dataset <- read_csv("classification-results.csv")
dataset[,8:ncol(dataset)] <- round(dataset[,8:ncol(dataset)],digits = 3)

#' ESTACION: valores posibles en la columna vars. Son las estaciones meteorológicas
print(unique(dataset$var))
#' DATASET: valores posibles "dacc", faltan casos de "dacc-temp","dacc-spring"
#' 
#' Caso columna o dato que evalue si colabora o no el enfoque agregar vecinos.
#' 

stations <- unique(dataset$var)
algoritmos <- unique(dataset$alg)
metrics <- c("FAR","Sensitivity","Specificity","Accuracy","Kappa","F1","Precision")

#' ## Carga dataset
#' Creación de dataset genérico para resultados clasificación
df <- dataset %>%
  unite(dataset, 
        col=label,c("dataset","var","config.train","config.vars","T","alg"),
        sep = "-",remove=FALSE) %>% 
  select(label,dataset,var,config.train,config.vars,T,alg,FAR,Sensitivity,Specificity,Accuracy,Kappa,F1,Precision) 

#' ## Comportamiento general de local vs all según métrica
p <-ggplot(aes(y = Sensitivity, x = var, fill = config.vars), data = df) + 
  geom_boxplot()  + coord_flip()
print(p)

p <-ggplot(aes(y = F1, x = var, fill = config.vars), data = df) + geom_boxplot() + coord_flip()
print(p)

p <-ggplot(aes(y = Precision , x = var, fill = config.vars), data = df) + geom_boxplot() + coord_flip()
print(p)

#' ## Según cuantos días anteriores de información sumamos.
#' 
p <-ggplot(aes(y = Precision , x = var, fill = as.factor(T)), data = df) + geom_boxplot() + coord_flip()
print(p)

p <-ggplot(aes(y = Sensitivity , x = var, fill = as.factor(T)), data = df) + geom_boxplot() + coord_flip()
print(p)

p <-ggplot(aes(y = F1 , x = var, fill = as.factor(T)), data = df) + 
  geom_boxplot() + coord_flip() + labs(fill="T")
print(p)

#' ## Comportamiento general de local vs all por cada estación según métrica

for(s in stations){
  
  df1 <- df %>% filter( var == s ) 
  
  p <-ggplot(aes(y = F1 , x = alg, fill = config.vars), data = df1) + 
    geom_boxplot() + coord_flip() + labs(title=paste("Estación",s,sep="  "))
  print(p)
  p <-ggplot(aes(y = Sensitivity , x = alg, fill = config.vars), data = df1) + 
    geom_boxplot() + coord_flip() + labs(title=paste("Estación",s,sep="  "))
  print(p)
  p <-ggplot(aes(y = Precision , x = alg, fill = config.vars), data = df1) + 
    geom_boxplot() + coord_flip() + labs(title=paste("Estación",s,sep="  "))
  print(p)

}

library(reshape)
df3 <-  df %>% filter( dataset == "dacc") %>% select(-one_of(c("FAR")))

#  melt(df3,id.vars="label")
df4 <- melt(as.data.frame(df3),
            id.vars =(c("label","dataset","var","config.train","config.vars","T","alg")),
            measure.vars = metrics[-1])

p <-ggplot(aes(y = value , x = var, fill = variable), data = df4) + 
  geom_boxplot() + coord_flip() + labs(title="Variabilidad de las métricas por las estaciones")
print(p)

  
df4 <- df %>% filter( dataset == "dacc")   
df4 <- melt(as.data.frame(df4),
            id.vars =(c("label","dataset","var","config.train","config.vars","T","alg")),
            measure.vars = metrics)

for(a in algoritmos)
{
  for(m in metrics)
  {
    df5 <- df4 %>% filter( alg == a & variable == m) 
    p <-ggplot(aes(y = value , x = var, fill = config.vars), data = df5) + 
      geom_boxplot() + coord_flip() + labs(title=paste("Comportamiento de ",m," en modelo ",a,sep=""))
    print(p)
  }
  # for(s in stations){
  #      
  #   df5 <- df4 %>% filter( var == s & alg == a) 
  #   p <-ggplot(aes(y = value , x = variable, fill = config.vars), data = df5) + 
  #     geom_boxplot() + coord_flip() + labs(title=paste("Estacion ",s," y algoritmo ",a,sep=""))
  #   print(p)
  #  }
}