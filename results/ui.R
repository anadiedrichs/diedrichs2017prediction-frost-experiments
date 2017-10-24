library(shiny)
library(plotly)
library(ggplot2)

file <- NULL
cumm <- NULL

shinyUI(fluidPage(
  
  titlePanel("Análisis de resultados"),
  
  sidebarLayout(
    
    sidebarPanel(
      # Inputs excluded for brevity
      htmlOutput("elegirDataset"),
      htmlOutput("elegirT"),
      htmlOutput("elegirConfig"),
      htmlOutput("elegirAlg"),
      htmlOutput("elegirScore"),
      htmlOutput("elegirVariable"),
      textOutput("text1"),
      textOutput("text2"),
      width = 2
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("ConfusionMatrixHelada",verbatimTextOutput("confMatFrost")),
        tabPanel("ConfusionMatrixTemps",verbatimTextOutput("confMatTemps")),
        
        tabPanel("Errors regression", dataTableOutput("errors")),
        
        tabPanel("Plot", plotlyOutput("plot")), 
        # tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", dataTableOutput("tbl"))
      )
    )
  )
))

# fluidPage(
#     titlePanel("Análisis de resultados"),
#     fluidRow(
#       column(4, htmlOutput("elegirDataset")),
#       column(4, htmlOutput("elegirT")),
#       column(4, htmlOutput("elegirConfig")),
#       column(4, htmlOutput("elegirAlg")),
#       column(4, htmlOutput("elegirScore")),
#       column(4, htmlOutput("elegirVariable")),
#       # column(4, selectInput("modelTrain", "MODELO",choices = opcionesTrain)),
#       # column(4,
#       #        selectInput("configVecinos", "Tecnica seleccion variables",choices = opcionesVecinos),
#       textOutput("text1"),
#       textOutput("text2")
#     ),
#     # ,
#     # fluidRow(
#     #   plotlyOutput("plot2")
#     # ),
#     # Create a new row for the table.
#     fluidRow(
#       dataTableOutput("tbl")
#     )
#   )
