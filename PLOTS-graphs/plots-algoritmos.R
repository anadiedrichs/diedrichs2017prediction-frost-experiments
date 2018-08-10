suppressMessages(library(readr))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(tidyverse))
suppressMessages(library(lazyeval))

dataset <- suppressMessages(read_csv("classification-results.csv"))
dataset[,8:ncol(dataset)] <- round(dataset[,8:ncol(dataset)],digits = 3)

stations <- unique(dataset$var)
algoritmos <- unique(dataset$alg)
metrics <- c("FAR","Sensitivity","Specificity","Accuracy","Kappa","F1","Precision")

#' ## Carga dataset
#' dataset genérico para resultados clasificación
df <- dataset %>%
  unite(dataset, 
        col=label,c("dataset","var","config.train","config.vars","T","alg"),
        sep = "-",remove=FALSE) %>% 
  select(label,dataset,var,config.train,config.vars,T,alg,FAR,Sensitivity,Specificity,Accuracy,Kappa,F1,Precision) 

var_table <- table(df$var)
var_levels <- names(var_table)[order(var_table)]
df$var <- factor(df$var, levels = var_levels)


#' ## estaciones x algoritmos segun métricas
#' 
#' 
df1 <- df %>% filter( dataset == "dacc" & T==1) 

p <-ggplot(aes(y = Precision , x = sort(var), fill = alg), data = df1) + geom_boxplot() + coord_flip()
print(p)

p <-ggplot(aes(y = Sensitivity , x = sort(var), fill = alg), data = df1) + geom_boxplot() + coord_flip()
print(p)

p <-ggplot(aes(y = F1 , x = sort(var), fill = alg), data = df) + 
  geom_boxplot() + coord_flip() #+ labs(fill="T")
print(p)

df2 <- df  %>% filter( dataset == "dacc" & T==1 & alg == "rf" ) 
titulo <- "Random Forest por estación"
# Random forest por estaciones
p <-ggplot(aes(y = Precision , x = var, fill = config.vars), data = df2) + 
  geom_boxplot() + coord_flip() + labs(title=titulo)
print(p)

p <-ggplot(aes(y = Sensitivity , x = var, fill = config.vars), data = df2) + 
  geom_boxplot() + coord_flip() + labs(title=titulo)
print(p)

p <-ggplot(aes(y = F1 , x = var, fill = config.vars), data = df2) + 
  geom_boxplot() + coord_flip() + labs(title=titulo)
print(p)

df2 <- df  %>% filter( dataset == "dacc" & T==1  & alg == "glm" ) 
titulo <- "Logistic regression (glm) por estación"
# Random forest por estaciones
p <-ggplot(aes(y = Precision , x = var, fill = config.vars), data = df2) + 
  geom_boxplot() + coord_flip() + labs(title=titulo)
print(p)

p <-ggplot(aes(y = Sensitivity , x = var, fill = config.vars), data = df2) + 
  geom_boxplot() + coord_flip() + labs(title=titulo)
print(p)

p <-ggplot(aes(y = F1 , x = var, fill = config.vars), data = df2) + 
  geom_boxplot() + coord_flip() + labs(title=titulo)
print(p)


