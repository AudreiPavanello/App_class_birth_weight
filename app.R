library(shiny)
library(tidymodels)
library(tibble)
library(randomForest)

# Load the trained RandomForest model
model <- readRDS("rf_model.rds")

ui <- fluidPage(
  titlePanel("Classificação do peso ao nascimento"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ESCMAE", "Nível de escolaridade materno", choices = c("1 a 3 anos", "4 a 7 anos",
                                                                         "8 a 11 anos", "12 anos ou mais",
                                                                         "Nenhum", "0")),
      selectInput("RACACORMAE", "Raça ou cor materna", choices = c("Branca", "Preta", "Amarela", "Indígena",
                                                                   "Parda", "9")),
      selectInput("GRAVIDEZ", "Tipo de gravidez", choices = c("única", "Dupla", "Tripla e mais")),
      selectInput("ESTCIVMAE", "Estado Civil materno", choices = c("Casada", "Separada judicialmente",
                                                                   "Solteira", "União consensual",
                                                                   "Viúva")),
      numericInput("IDADEMAE", "Idade materna", value = 0),
      numericInput("QTDFILVIVO", "Número de filhos vivos", value = 0),
      numericInput("QTDFILMORT", "Número de filhos mortos", value = 0),
      numericInput("SEMAGESTAC", "Semana gestacional", value = 0),
      numericInput("QTDGESTANT", "Quantidade gestações", value = 0),
      numericInput("QTDPARTCES", "Quantidade de partos de cesárea", value = 0),
      numericInput("QTDPARTNOR", "Quantidade de partos vaginais", value = 0),
      actionButton("predictButton", "Predict")
    ),
    mainPanel(
      textOutput("prediction_text")
    )
  )
)

server <- function(input, output, session) {
  
  getPrediction <- function() {
    df <- tibble(
      ESCMAE = as.factor(input$ESCMAE),
      RACACORMAE = as.factor(input$RACACORMAE),
      GRAVIDEZ = as.factor(input$GRAVIDEZ),
      ESTCIVMAE = as.factor(input$ESTCIVMAE),
      IDADEMAE = as.numeric(input$IDADEMAE),
      QTDFILVIVO = as.numeric(input$QTDFILVIVO),
      QTDFILMORT = as.numeric(input$QTDFILMORT),
      SEMAGESTAC = as.numeric(input$SEMAGESTAC),
      QTDGESTANT = as.numeric(input$QTDGESTANT),
      QTDPARTCES = as.numeric(input$QTDPARTCES),
      QTDPARTNOR = as.numeric(input$QTDPARTNOR)
    )
    
    prediction <- predict(model, new_data = df)
    prediction$.pred_class
  }
  
  observeEvent(input$predictButton, {
    prediction <- getPrediction()
    output$prediction_text <- renderText({
      paste("Predicted Class:", prediction)
    })
  })
}

shinyApp(ui = ui, server = server)