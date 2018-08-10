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

ESTACION <- "junin.temp_min"
DATASET <- "dacc"
ALGORITMO <- "glm"

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


#' # Gráficos barras comparativos por recall, precision, F1, sensitivity, etc, indicando local - all
plot_ranking_alg <- function(df,m,s,alg)
{
  df2 <- df 
  # renombrar columna con la métrica
  colnames(df2)[which(colnames(df2) == m)] <- c("metric")
  
  p <- ggplot(data=df2, aes(x= reorder(label,metric), y=metric, 
                       color=config.vars)) +
    geom_bar(stat="identity",fill="white")+
    geom_text(aes(label=metric), vjust=1.3, color="black", size=3) +
    coord_flip()+
    theme_minimal() +
    labs(x = "Models",y=m, title=paste(s,alg,m,sep="--"))
  print(p)
  #plot(p)
  
}
#' # Comparativas de sensitivity, precision, F1,
#'

#' #' ## MOSTRAR RANKING POR recall, precision, F1, sensitivity, etc
plot_comparativo <- function(df,s,alg)
{
 
    df3 <- melt(df,id=(c("label","dataset","var","config.train","config.vars","T","alg")))
    
    
   p<-  ggplot(data=df3, aes(x= reorder(label,value), y=value, fill=variable)) +
      geom_bar(stat="identity",position=position_dodge())+
      coord_flip()+
      theme(legend.position="bottom") +
      labs(x = alg, y = "metricas", title=paste("comparacion",alg,s,sep="-"))
   print(p)
}

library(reshape2)


metrics <- c("Sensitivity","Specificity","Accuracy","Kappa","F1","Precision") #"FAR",

for(s in stations)
{
  for(a in algoritmos)
  {
    
    df3 <- df %>%
      select(label,dataset,var,config.train,config.vars,T,alg,Sensitivity,Specificity,Accuracy,Kappa,F1,Precision) %>%
      filter(dataset == DATASET & var == ESTACION  & alg==ALGORITMO) # & T == 1 ) %>%
    
    plot_comparativo(df3,s,a)
    
    for(m in metrics){
      plot_ranking_alg(df3,m,s,a)
      
    }
  }
}




