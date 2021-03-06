#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("tidyverse")
library("tidyquant")
library("PortfolioAnalytics")
library("quantmod")
library("PerformanceAnalytics")
library("zoo")
library("plotly")
library("ggthemes")
library("timetk")
library("DEoptim")
library("ROI")
require("ROI.plugin.glpk")
require("ROI.plugin.quadprog")
library("shiny")

#Gerar ativos da carteira
ativosDisponiveis<-gerarAtivos()

# Front end da aplicação
ui <- fluidPage(
  
  # Título da Aplicação
  titlePanel("DETAI - Determinação Ágil de Investimento"),
  
  # Linha <HR>
  tags$hr(),
  
  # Corpo da aplica??o
  sidebarLayout(
    
    
    # Menu principal do sistema
    sidebarPanel(
      
      # Orientação ao usuário
      tags$h5("Indique, a seguir, seus critérios para que possamos buscar e calcular a carteira ótima para seu investimento."),
      
      
      # Listagem de ativos
      verbatimTextOutput('listaAtivos'),
      selectInput('ativos',  label ="Ativos (investimentos):", ativosDisponiveis[,2], multiple=TRUE, selectize=TRUE),
      
      # Data inicial para pesquisa de histórico
      dateInput("dataInicial", label = "Data inicial p/ busca de histórico de rentabilidade dos ativos:", value = '2017-01-01'),         
      
      # Retorno mínimo esperado
      sliderInput("risco",
                  "Aversão ao risco (%):",
                  min = 1,
                  max = 100,
                  value = 25),
      
      sliderInput("retorno", "Retorno mínimo e máximo para cada um dos ativos (%): ",
                  min = 0, max = 100, value = c(5, 70)),

      actionButton("calcularCarteira", "Definir Carteira Otimizada", class = "btn-primary btn-block")         
      
    ),
    
    # Janela com os resultados / definidos da carteira ótima
    mainPanel(
      
      
      fluidRow(
        
        #Primeira coluna com a exibição dos critérios definidos pelo usuário
        column(5,
           tags$h3("Ativos"),
             verbatimTextOutput("ativos", placeholder = TRUE)
          ),
        
        #Segunda coluna com a exibição dos critérios definidos pelo usuário
        column(7,
          tags$h3("Critérios Definidos"),
               
          verbatimTextOutput("dataInicial"),
               
          verbatimTextOutput("risco"),
          
          verbatimTextOutput("retorno")
          )
      ),            
      
      tags$h3("Carteira Otimizada"),
      
      
      # Esta informação é exibida somente após o usuário clicar no botão
      tableOutput("saida")
    )
  ),#rodapé da página
  tags$footer("Copyright © 2018 DETAI. Adriane Gomes, Erick Moreira e Victor Costa.", align = "center", style = "
              position:relative;
              bottom:0;
              width:95%;
              height:50px;
              padding:10px;
              color: black;
              z-index: 1000;")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    #Mostrar na tela o valor mínimo esperado escolhido
    output$retorno <- renderText({
      paste("Retorno mínimo e máximo para cada um dos ativos (%): ", 
            paste(as.character(input$retorno), collapse = " % até "),
            paste("%")
      )
    })
    
    output$risco <- renderText({
      paste("Aversão ao risco: ", as.character(input$risco),"%")
    })
    
    
    #Mostrar na tela a data inicial escolhida
    output$dataInicial  <- renderText({
      paste("Data inicial: ", as.character(input$dataInicial))
    })
    
    #Mostrar na tela os ativos selecionados
    output$ativos <- renderText({
      paste(as.character(input$ativos),collapse = '\n')
    })
    
    saida <- eventReactive(input$calcularCarteira, {
      listaAtivos<-input$ativos
      dataInicio<-input$dataInicial
      risco<-input$risco
      retorno<-input$retorno
      otimizarCarteira(listaAtivos,dataInicio,risco,retorno)
    })
    
    output$saida <- renderTable({
      # Criar barra de carregando
      progress <- shiny::Progress$new()
      progress$set(message = "Carregando...", value = 0)
      # Fechar barra de progresso ao clicar no x
      on.exit(progress$close())
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      compute_data(updateProgress)
      saida()
    })
    
   
}


# Run the application 
shinyApp(ui = ui, server = server)

