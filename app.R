# Load packages ----
library(shiny)
library(ggplot2)

# User interface ----
ui <- fluidPage(
  titlePanel("title panel"),
    
  sidebarLayout(
           sidebarPanel("sidebar panel"),
           
    mainPanel(
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
                                        "ÓLEO DIESEL"
                         ),
                         selected = "GASOLINA COMUM"
             ),
             selectInput("userInput_estado", 
                         label = "Escolha o Estado",
                         choices = list("ACRE",
                                        "ALAGOAS",
                                        "AMAPÁ" = "AMAPA",
                                        "AMAZONAS",
                                        "BAHIA",
                                        "CEARÁ" = "CEARA" ,
                                        "DISTRITO FEDERAL",
                                        "ESPÍRITO SANTO" = "ESPIRITO SANTO",
                                        "GOIÁS" = "GOIAS",
                                        "MARANHÃO" = "MARANHAO",
                                        "MATO GROSSO",
                                        "MATO GROSSO DO SUL",
                                        "MINAS GERAIS",
                                        "PARÁ" = "PARA",
                                        "PARAÍBA" = "PARAIBA",
                                        "PARANÁ" = "PARANA",
                                        "PERNAMBUCO",
                                        "PIAUÍ" = "PIAUI",
                                        "RIO DE JANEIRO",
                                        "RIO GRANDE DO NORTE", 
                                        "RIO GRANDE DO SUL",
                                        "RONDÔNIA" = "RONDONIA",
                                        "RORÂIMA" = "RORAIMA",
                                        "SANTA CATARINA",
                                        "SÃO PAULO" = "SAO PAULO",
                                        "SERGIPE",
                                        "TOCANTINS"
                         ),
                         selected = "ACRE"
             ),
             plotOutput('plot')
      )
  )
)

# Server logic

server <- function(input, output) {
  df <- read.csv('data/2004-2019.tsv', sep='\t')
  
  
  output$plot <- renderPlot({
    
    d <- df[df$ANO == input$userInput_ano & 
              df$PRODUTO == input$userInput_produto &
              df$ESTADO == input$userInput_estado, ]
    
    d <- aggregate(d$PREÇO.MÉDIO.REVENDA, by=list(MÊS = d$MÊS), mean)
    
    g <- ggplot(d, aes(x=MÊS, y=x)) +
          geom_line() +
          scale_x_continuous("MÊS", labels = as.character(d$MÊS), breaks = d$MÊS) + 
          geom_point() + 
          ylim(c(2, 5.6))
    
    plot(g)
    })
  
}

# Run the app
shinyApp(ui, server)
