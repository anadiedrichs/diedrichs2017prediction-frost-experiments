source("ui.R")

library(caret)
library(forecast)
library(readr)

dataset <- c("dacc","dacc-temp","dacc-spring")
config.train <-c("normal","smote")
vec <- c("vec") #TODO ,"solo"
alg <- c("hc","tabu","local")
period <- c(1,2,3,4,5)
score <- c("bic-g","loglik-g","aic-g","bge")
variables <- c("tunuyan.temp_min_t","las_paredes.temp_min_t","la_llave.temp_min_t","junin.temp_min_t","agua_amarga.temp_min_t")
lista <- list.files(path=".",pattern="*.csv")


RMSE = function(m, o){  sqrt(mean((m - o)^2)) } #tested
rsq <- function(x, y){ summary(lm(y~x))$r.squared } #tested


function(input, output, session){
  
  
  output$elegirDataset<- renderUI({
    selectInput("dataset", "dataset", as.list(dataset))
  })
  
  output$elegirT<- renderUI({
    selectInput("T", "T value", as.list(period))
  })
  
  output$elegirConfig<- renderUI({
    selectInput("config","Training Config", as.list(config.train))
  })
  
  output$elegirAlg<- renderUI({
    selectInput("alg", "Algoritmo", as.list(alg))
  })
  
  
  output$elegirScore<- renderUI({
    selectInput("score", "Score", as.list(score))
  })
  
  output$elegirVariable<- renderUI({
    selectInput("variable", "Variable a Predecir", as.list(variables))
  })
  
  output$text1 <- renderText({
    paste(input$dataset,input$T,input$config,input$alg,input$score,input$variable,is.null(input$variable),sep = "--")
  })
  
  
  model <- reactiveValues(data = NULL)
  tablita <- reactiveValues(data = NULL)
  
  
  #lista de modelos segun eleccion de variable
  modelos <- reactive({
    
    if(is.null(input$variable)){return()}
    findthis <- paste(input$dataset,input$T,input$config,input$alg,input$score,input$variable,sep = "--")
    archivo <- read.csv(lista[grep(findthis, lista,fixed=TRUE)],sep=",")
    #cumm <- cumm[-1] #quito la primer columna nombre x
    model$data <- sapply(archivo,as.numeric)
    return(archivo)
  })
  
  
  observe({
    
    if(is.null(input$dataset) || is.null(input$T) || is.null(input$config)
       || is.null(input$alg) || is.null(input$score) || is.null(input$variable)){return()}
    modelos()
    
  })
  
  
  output$text2 <- renderText({
    if(!is.null(model$data))
    {
      colnames(model$data)
    }
  })
  

  output$tbl <- renderDataTable({
    
    if(is.null(model) || is.null(model$data)){return(0)}
    model$data
    })  
  
  output$plot <- renderPlotly({
    
    if(is.null(model) || is.null(model$data)){return()}
    p <- plot_ly() %>%
         add_trace( x = model$data[,1], y = model$data[,2], mode = "lines",name=colnames(model$data)[2]) %>%
         add_trace(p, x = model$data[,1], y = model$data[,3], mode = "lines",name=colnames(model$data)[3])
  
    p
  })
  
  #
  output$errors <- renderDataTable({
    
    pred = model$data[,3]
    obs = model$data[,2]
    
    if(is.null(model) || is.null(model$data)){return()}
    #aux <- accuracy(f = pred, x = obs)
    
    rmse <- round(RMSE(pred,obs),2)
    r2 <- round(rsq(obs,pred),2)
    d <- cbind.data.frame(rmse,r2)
    d
    
  })

  # #Matriz de confusion caso de heladas
  output$confMatFrost <- renderPrint({


    if(is.null(model) || is.null(model$data)){return(0)}
    breaks <- c(-20,0,50) # caso Helada y no helada
    y <- cut(model$data[,2], breaks = breaks)
    y_pred <- cut(model$data[,3], breaks = breaks)
    confusionMatrix(y_pred,y,mode="everything")

  })

  #Matriz de confusion analizando rango de temperaturas
  output$confMatTemps <- renderPrint({


    if(is.null(model) || is.null(model$data)){return(0)}
     breaks <- c(-20,-5,0,2,5,10,50) # caso Helada y no helada
    y <- cut(model$data[,2], breaks = breaks)
    y_pred <- cut(model$data[,3], breaks = breaks)
    c <- confusionMatrix(y_pred,y,mode="everything")
    c
  })
  # diff <- abs(round(y - pred,2))
  
  output$cummulative <- renderPlot({
    
    
    if(is.null(model) || is.null(model$data)){return(0)}
    
    breaks <- seq(from=0,to = 3, by= 0.5) # caso Helada y no helada
    
    diff <- abs(round(model$data[,2] - model$data[,3],2))
    
    y <- cut(diff, breaks = breaks)
    
    plot(y,type="h")
    
  })
  
  ## probar lo de abajo
  output$cumm <- renderDataTable({
    
    
    if(is.null(model) || is.null(model$data)){return(0)}
    
    diff <- abs(round(model$data[,2] - model$data[,3],2))
    
    total <- length(diff)
    max.value <- 3
    interval <- 0.5
    error <- matrix(data = 0,nrow=max.value/interval,ncol = 3)
    rows <- c()
    data <- list()
    j <- 0.5
    i <- 1
    
    while(j <= max.value)
      #for(j = 0.5,i = 1;j < max.value; )
    {
      error[i,1] <- length(which(diff<=j))
      error[i,2] <- error[i,1]/total * 100
      rows <- c(rows, j)
      j <- j+0.5; i <- i+1
    }
    d <- as.data.frame(cbind(rows,error[,2]))
    colnames(d) <- c("intervals","error")
    
    d
  })
  
}