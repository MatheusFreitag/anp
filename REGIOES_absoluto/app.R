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
                         label = "Escolha o Ano",
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
            checkboxGroupInput("userInput_regiao", "Escolha as Regiões",
                               c("SUL",
                                 "SUDESTE",
                                 "CENTRO OESTE",
                                 "NORDESTE",
                                 "NORTE"
                               ),
                selected=c('SUL'),
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
  
  output$plot <- renderPlotly({
    
    d <- df[df$ANO == input$userInput_ano & 
              df$PRODUTO == input$userInput_produto &
              df$REGIÃO %in% input$userInput_regiao, ]
    
    d <- aggregate(d$PREÇO.MÉDIO.REVENDA, by=list(MÊS = d$MÊS, REGIÃO=d$REGIÃO), mean)
    
    g <- ggplot(d, aes(x=MÊS, y=x, group = REGIÃO, colour = REGIÃO)) +
          geom_line() +
          scale_x_continuous(paste("Meses observados de ", input$userInput_ano), 
                             labels = names(meses_code)[match(d$MÊS, meses_code)], 
                             breaks = d$MÊS) + 
          ylim(c(2, 5.6)) + 
          labs(title=sprintf("Histórico de Preços Médios (%s) por Região", input$userInput_produto), y="Preço (em valores absolutos)") + 
          geom_point(aes(text=sprintf("%s<br>Preço Médio: R$ %.3f<br>Mês: %s", d$REGIÃO, d$x, names(meses_code)[match(d$MÊS, meses_code)] ))) 
          

    gg <- ggplotly(g, tooltip="text")
    
    (gg)
    })
  
}

# Run the app
shinyApp(ui, server)
