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

#' ## ENFOQUE local vs all
#' 
#' Vistazo del dataset del que hacemos la resta:
#' 

df %>%
  select(dataset,var,config.train,config.vars,T,alg,FAR) %>%
  filter(dataset == DATASET & var == ESTACION ) %>% # & alg=="glm" & T == 1 ) %>%
  group_by(dataset,var,config.train,T,alg) %>% 
  arrange(dataset,var,config.train,T,alg,desc(config.vars)) %>% 
  mutate(local_vs_all = lag(FAR) - FAR)

#' para crear columna local vs all
mutate_call_ <- function(df, col1, col2, new_col_name) {
  mutate_call = lazyeval::interp(~ round(lag(a) - b,2), a = as.name(col1), b = as.name(col2))
 df %>% mutate_(.dots = setNames(list(mutate_call), new_col_name))
}
#' df data.frame
#' 
#' NO IMPLEMENTADO filtro: filtro a pasar a ggplot en filter
#' 
#' m metrica, valores posibles o referencia array metrics o mirar dataset.
#' 
#' s: character, nombre de la estacion o variable predecida
plot_local_vs_all <- function(df,m,s)
{
  df1 <- df %>%
    select(dataset,var,config.train,config.vars,T,alg,m) %>%
    group_by(dataset,var,config.train,T,alg) %>% 
    arrange(dataset,var,config.train,T,alg,desc(config.vars)) %>% 
    mutate_call_(m, m, "local_vs_all") %>% 
    filter(!is.na(local_vs_all))  %>%
    unite(col=label,c("dataset","var","config.train","T","alg"),
          sep = "-",remove=FALSE) 
  
  
  p <- ggplot(data=df1, aes(x= reorder(label,-local_vs_all), y=local_vs_all)) +
          geom_bar(stat="identity",fill="green")+ 
          geom_text(aes(label=local_vs_all), vjust=1.3, color="black", size=3) + 
          coord_flip()+
          theme_minimal() +
          labs(x = "Models",title=paste(s,m,sep="--"))
  print(p)
  return(p)
}
#' corro para cada estación para cada una de las métricas
#' 
#' ## IMPORTANTE, las barras representan la resta de local - all para alguna métrica. 
#' ## Barra negativa significa  que config ALL es mayor a local 
#' (info de las otras estaciones)
lista <- NULL
for(s in stations)
{
  df1 <- df %>%
    #select(dataset,var,config.train,config.vars,T,alg,m) %>%
    filter(dataset == DATASET & var == s ) # & alg=="glm" & T == 1 ) %>%
  for(m in metrics){
    lista[[paste(s,m,sep="--")]] <- plot_local_vs_all(df1,m,s)
  }
}

#grid.arrange(grobs = lista, ncol = 2)

#' #' ## MOSTRAR RANKING POR recall, precision, F1, sensitivity, etc
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




