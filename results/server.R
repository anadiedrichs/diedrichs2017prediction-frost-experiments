source("ui.R")

library(caret)
library(forecast)

dataset <- c("dacc","dacc-temp","dacc-spring")
config.train <-c("normal","smote")
vec <- c("vec") #TODO ,"solo"
alg <- c("hc","tabu")
period <- c(1,2,3,4,5)
score <- c("bic-g","loglik-g","aic-g","bge")
variables <- c("tunuyan.Tmin_t","las_paredes.Tmin_t","la_llave.Tmin_t","junin.Tmin_t","agua_amarga.Tmin_t")
lista <- list.files(path=".",pattern="*.csv")

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
    archivo <- read.csv(files[grep(findthis, files,fixed=TRUE)])
    #cumm <- cumm[-1] #quito la primer columna nombre x
    model$data <- archivo
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
  
  
  # tabla <- reactive({
  # 
  #     if(is.null(input$modelTrain) || is.null(input$configVecinos)|| is.null(model$data)){ return()}
  #     col <- colnames(model$data)
  #     n <- grep("nnet",col)
  #     nn <- grep("MI",col)
  #     isolate(tablita$data <- model$data[,intersect(n,nn)])
  #     isolate(tablita$data)
  # })
  

  output$tbl <- renderDataTable(model$data)  
  
  output$plot <- renderPlotly({
    
    if(is.null(model) || is.null(model$data)){return(0)}
    p <- plot_ly() %>%
         add_trace( x = model$data$X, y = model$data[,2], mode = "lines",name=colnames(model$data)[2]) %>%
         add_trace(p, x = model$data$X, y = model$data[,3], mode = "lines",name=colnames(model$data)[3])
  
    p
  })
  
  #TODO
  output$errors <- renderDataTable({
    
    aux <- accuracy(f = model$data[,3], x = model$data[,2])
    aux <- round(aux,2)
    aux
    
  })

  #TODO
  output$confMatFrost <- renderPrint({
    
    breaks <- c(-20,0,50) # caso Helada y no helada
    y <- cut(model$data[,2], breaks = breaks)
    y_pred <- cut(model$data[,3], breaks = breaks)
    confusionMatrix(y_pred,y)
    
  })
  
  
  output$confMatTemps <- renderPrint({
   
     breaks <- c(-20,-5,0,2,5,10,50) # caso Helada y no helada
    y <- cut(model$data[,2], breaks = breaks)
    y_pred <- cut(model$data[,3], breaks = breaks)
    c <- confusionMatrix(y_pred,y)
    c
  })
  
  
  # output$plot1 <- renderPlot({
  #   if(is.null(input$variable) || is.null(input$elegirModelo)){return()}
  #   p <- plot_ly() %>%
  #     add_trace(x = index, y =model$data[,input$elegirModelo], mode = "lines")
  #   p
  #   })
  
}