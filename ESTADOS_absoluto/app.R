# Load packages ----
library(shiny)
library(ggplot2)
library(plotly)

# User interface ----
ui <- fluidPage(

      fluidRow(
        column(3,""),
        column(3,
            selectInput("userInput_ano",
                         label = "Escolha o ano",
                         choices = list("2019", 
                                        "2018",
                                        "2017" 
                         ),
                         selected = "2019"
             )
        ),
        column(3,
             selectInput("userInput_produto", 
                         label = "Escolha o Produto",
                         choices = list("ETANOL HIDRATADO",
                                        "GASOLINA COMUM",
                                        "ÓLEO DIESEL"
                         ),
                         selected = "GASOLINA COMUM"
             )
        )
        
      ),
      fluidRow(
        column(10, offset=1,
            checkboxGroupInput("userInput_estado", "Estados",
                               c("ACRE",
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
              selected=c('RIO GRANDE DO SUL', 'SAO PAULO', 'MATO GROSSO', 'BAHIA', 'AMAZONAS'),
              inline = TRUE)
        )
      ),
      
      htmlOutput("newline"),
            
      plotlyOutput('plot')
              
      )


# Server logic

server <- function(input, output) {
  
  output$newline<- renderUI({
    HTML("<br/><br/>")
  })
  
  
  df <- read.csv('../data/2004-2019.tsv', sep='\t')
  meses_code <- c('Janeiro' = 1, 
                  'Fevereiro' = 2, 
                  'Março' = 3, 
                  'Abril' = 4, 
                  'Maio' = 5, 
                  'Junho' = 6, 
                  'Julho' = 7, 
                  'Agosto' = 8, 
                  'Setembro' = 9, 
                  'Outubro' = 10, 
                  'Novembro' = 11, 
                  'Dezembro' = 12)
  
  output$estados <- renderText({input$userInput_estado2})
  
  output$plot <- renderPlotly({
    
    d <- df[df$ANO == input$userInput_ano & 
              df$PRODUTO == input$userInput_produto &
              df$ESTADO %in% input$userInput_estado, ]
    
    d <- aggregate(d$PREÇO.MÉDIO.REVENDA, by=list(MÊS = d$MÊS, ESTADOS=d$ESTADO), mean)
    
    g <- ggplot(d, aes(x=MÊS, y=x, group = ESTADOS, colour = ESTADOS)) +
          geom_line() +
          scale_x_continuous(paste("Meses observados de ", input$userInput_ano), 
                             labels = names(meses_code)[match(d$MÊS, meses_code)], 
                             breaks = d$MÊS) + 
          ylim(c(2, 5.6)) + 
          labs(title=sprintf("Histórico de Preços Médios (%s) por Estado", input$userInput_produto), y="Preço (em valores absolutos)") + 
          geom_point(aes(text=sprintf("%s<br>%s<br>Preço Médio: R$ %.3f<br>Mês: %s", d$ESTADO, input$userInput_produto, d$x, names(meses_code)[match(d$MÊS, meses_code)] ))) 
          

    gg <- ggplotly(g, tooltip="text")
    
    (gg)
    })
  
}

# Run the app
shinyApp(ui, server)
