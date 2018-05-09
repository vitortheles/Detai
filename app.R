#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Front end da aplicação
ui <- fluidPage(
  
  # Título da Aplicação
  titlePanel("DETAI - Determinação Ágil de Investimento"),

  # Linha <HR>
  tags$hr(),
    
  # Corpo da aplicação
  sidebarLayout(

    
    # Menu principal do sistema
    sidebarPanel(
      
      # Orientação ao usuário
      tags$h5("Indique, a seguir, seus critérios para que possamos buscar e calcular a carteira ótima para seu investimento."),
      
      
      # Listagem de ativos
      checkboxGroupInput("checkGroup", label = "Ativos (investimentos):", 
                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                         selected = 1),
      
      # Data inicial para pesquisa de histórico
      dateInput("date", label = "Data inicial p/ busca de histórico de rentabilidade dos ativos:", value = "2017-05-01"),         
      
      # Retorno mínimo esperado
      sliderInput("retorno",
                  "Retorno Mínimo Esperado (%):",
                  min = 1,
                  max = 100,
                  value = 50),         
      
      # Risco Esperado
      sliderInput("risco", "Risco Aceitável, min e max (%):",
                  min = 1, max = 100,
                  value = c(20,80)),         
      
      actionButton("calcular", "Definir Carteira Otimizada", class = "btn-primary center")         
      
    ),
    
    # Janela com os resultados / definição da carteira ótima
    mainPanel(
      
      tags$h3("Critérios Definidos"),
      
      fluidRow(
        
        #Primeira coluna com a exibição dos critérios definidos pelo usuário
        column(6, wellPanel(
          
          tags$h5("Ativos (investimentos):"),
          verbatimTextOutput("ativos"),
          
          tags$h5("Data inicial p/ busca de histórico de rentabilidade dos ativos:"),
          verbatimTextOutput("data")          
          
        )),
        
        #Segunda coluna com a exibição dos critérios definidos pelo usuário
        column(6, wellPanel(
          
          tags$h5("Retorno mínimo esperado (%):"),
          verbatimTextOutput("retorno"),        
          
          tags$h5("Risco mínimo aceitável (%):"),
          verbatimTextOutput("riscoMinimo"),
          
          tags$h5("Risco máximo aceitável (%):"),
          verbatimTextOutput("riscoMaximo")          
          
        ))
      ),            

      tags$h3("Carteira Otimizada"),
      
      #fluidRow(
      #  column(12, wellPanel(
      #    verbatimTextOutput("saida")
      #  ))
      #)
      
      # Esta informação é exibida somente após o usuário clicar no botão
      verbatimTextOutput("saida")
      
    )
  )  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Temporário - Mostra os valores selecionaos no checkGroup (listagem dos ativos)
  output$ativos <- renderPrint({ input$checkGroup })
  
  # Temporário - Mostra o valor da data
  output$data <- renderPrint({ input$date })   
  
  # Temporário - Mostra o valor do retorno
  output$retorno <- renderPrint({ input$retorno })
  
  # Temporário - Mostra o valor dos riscos
  output$riscoMinimo <- renderPrint({ paste(input$risco[1]) })   
  output$riscoMaximo <- renderPrint({ paste(input$risco[2]) })   
  
  # O campo SAIDA so eh preenchido depois que o usuario clica no botao de calcular
  observeEvent(input$calcular, {
    output$saida <- renderPrint({ "Aqui devem ser colocados os resultados da carteira otimizada." })   
  })  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

