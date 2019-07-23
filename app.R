# Load packages ----
library(shiny)

# User interface ----
ui <- fluidPage(
  titlePanel("title panel"),
    fluidRow(
      sidebarLayout(
        
        sidebarPanel("sidebar panel"),
        mainPanel(
            column(9, 
                   selectInput("userInput_ano",
                               label = "Escolha o ano",
                          choices = list("2019", 
                                         "2018",
                                         "2017" 
                          ),
                          selected = "2019"
              ),
              selectInput("userInput_produto", 
                          label = "Escolha o Produto",
                          choices = list("ETANOL HIDRATADO",
                                         "GASOLINA COMUM",
                                         "GLP",
                                         "GNV",
                                         "ÓLEO DIESE"
                          ),
                          selected = "GASOLINA COMUM"
              ),
              selectInput("userInput_estado", 
                          label = "Escolha o Estado",
                          choices = list("ACRE",
                                         "ALAGOAS",
                                         "AMAP",
                                         "AMAZONAS",
                                         "BAHIA",
                                         "CEARA",
                                         "DISTRITO FEDERAL",
                                         "ESPIRITO SANTO",
                                         "GOIAS",
                                         "MARANHAO",
                                         "MATO GROSSO",
                                         "MATO GROSSO DO SUL",
                                         "MINAS GERAIS",
                                         "PARA",
                                         "PARAIBA",
                                         "PARANA",
                                         "PERNAMBUCO",
                                         "PIAUI",
                                         "RIO DE JANEIRO",
                                         "RIO GRANDE DO NORTE", 
                                         "RIO GRANDE DO SUL",
                                         "RONDONIA RORAIMA",
                                         "SANTA CATARINA",
                                         "SAO PAULO",
                                         "SERGIPE",
                                         "TOCANTINS"
                          ),
                          selected = "ACRE"
              )
            ),
            textOutput("ano_escolhido")
          )
        )
      )
)

# Server logic
server <- function(input, output) {
  output$ano_escolhido <- renderText({paste("No ano de", input$userInput_ano, 
                                            "com o produto", input$userInput_produto, 
                                            "no estado de ", input$userInput_estado )})
  
}

# Run the app
shinyApp(ui, server)
