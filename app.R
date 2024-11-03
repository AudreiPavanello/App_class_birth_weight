library(shiny)
library(tidymodels)
library(tibble)
library(randomForest)
library(bslib)
library(shinyvalidate)
library(shinyjs)

# Load the trained RandomForest model
model <- readRDS("rf_model.rds")

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  title = "Classificação do peso ao nascimento",
  
  sidebar = sidebar(
    title = "Dados de Entrada",
    
    # Demographic Information Card
    card(
      card_header("Informações Demográficas"),
      selectInput("ESCMAE", "Nível de escolaridade materno",
                  choices = c("1 a 3 anos" = "1 a 3 anos",
                              "4 a 7 anos" = "4 a 7 anos",
                              "8 a 11 anos" = "8 a 11 anos",
                              "12 anos ou mais" = "12 anos ou mais",
                              "Nenhum" = "Nenhum",
                              "Não informado" = "0")),
      
      selectInput("RACACORMAE", "Raça ou cor materna",
                  choices = c("Branca" = "Branca",
                              "Preta" = "Preta",
                              "Amarela" = "Amarela",
                              "Indígena" = "Indígena",
                              "Parda" = "Parda",
                              "Não informado" = "9")),
      
      selectInput("ESTCIVMAE", "Estado Civil materno",
                  choices = c("Casada" = "Casada",
                              "Separada judicialmente" = "Separada judicialmente",
                              "Solteira" = "Solteira",
                              "União consensual" = "União consensual",
                              "Viúva" = "Viúva")),
      
      numericInput("IDADEMAE", "Idade materna", value = 25, min = 10, max = 60)
    ),
    
    # Pregnancy Information Card
    card(
      card_header("Informações da Gestação"),
      selectInput("GRAVIDEZ", "Tipo de gravidez",
                  choices = c("Única" = "única",
                              "Dupla" = "Dupla",
                              "Tripla ou mais" = "Tripla e mais")),
      
      numericInput("SEMAGESTAC", "Semana gestacional",
                   value = 38, min = 20, max = 45),
      
      numericInput("QTDGESTANT", "Quantidade de gestações",
                   value = 1, min = 0, max = 20)
    ),
    
    # Previous Births Card
    card(
      card_header("Histórico de Partos"),
      numericInput("QTDFILVIVO", "Número de filhos vivos",
                   value = 0, min = 0, max = 20),
      numericInput("QTDFILMORT", "Número de filhos mortos",
                   value = 0, min = 0, max = 20),
      numericInput("QTDPARTCES", "Quantidade de partos de cesárea",
                   value = 0, min = 0, max = 20),
      numericInput("QTDPARTNOR", "Quantidade de partos vaginais",
                   value = 0, min = 0, max = 20)
    ),
    
    actionButton("predictButton", "Realizar Predição",
                 class = "btn-primary w-100",
                 icon = icon("calculator"))
  ),
  
  # Main panel content
  card(
    card_header("Resultado da Predição"),
    value_box(
      title = "Classificação Prevista",
      value = textOutput("prediction_text"),
      showcase = icon("weight-scale"),
      theme = "primary",
      full_width = TRUE
    ),
    card_body(
      HTML("<b>Nota:</b> Esta é uma previsão baseada em um modelo estatístico 
           e deve ser utilizada apenas como referência.")
    )
  )
)

server <- function(input, output, session) {
  # Input validation
  iv <- InputValidator$new()
  iv$add_rule("IDADEMAE", sv_required())
  iv$add_rule("IDADEMAE", sv_between(10, 60))
  iv$add_rule("SEMAGESTAC", sv_required())
  iv$add_rule("SEMAGESTAC", sv_between(20, 45))
  iv$enable()
  
  getPrediction <- function() {
    # Data validation
    validate(
      need(input$IDADEMAE >= 10 && input$IDADEMAE <= 60,
           "Idade materna deve estar entre 10 e 60 anos"),
      need(input$SEMAGESTAC >= 20 && input$SEMAGESTAC <= 45,
           "Semanas de gestação deve estar entre 20 e 45")
    )
    
    tryCatch({
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
      
    }, error = function(e) {
      showNotification(
        "Erro ao fazer a predição. Verifique os dados inseridos.",
        type = "error"
      )
      return(NULL)
    })
  }
  
  observeEvent(input$predictButton, {
    if (iv$is_valid()) {
      prediction <- getPrediction()
      if (!is.null(prediction)) {
        output$prediction_text <- renderText({
          if (prediction == "Normal") {
            "Peso Normal"
          } else {
            "Baixo Peso"
          }
        })
      }
    }
  })
}

shinyApp(ui = ui, server = server)