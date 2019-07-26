# Load packages ----
library(shiny)
library(ggplot2)
library(plotly)

# User interface ----
ui <- fluidPage(

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
              plotlyOutput('plot')
      )


# Server logic

server <- function(input, output) {
  df <- read.csv('data/2004-2019.tsv', sep='\t')
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
              df$ESTADO == input$userInput_estado, ]
    
    d <- aggregate(d$PREÇO.MÉDIO.REVENDA, by=list(MÊS = d$MÊS), mean)
    
    g <- ggplot(d, aes(x=MÊS, y=x)) +
          geom_line() +
          scale_x_continuous(paste("Meses observados de ", input$userInput_ano), 
                             labels = names(meses_code)[match(d$MÊS, meses_code)], 
                             breaks = d$MÊS) + 
          ylim(c(2, 5.6)) + 
          labs(title="Histórico de Preços", y="Preço (em valores absolutos)") + 
          geom_point(aes(text=sprintf("Preço: R$%.3f<br>Mês: %s", d$x, names(meses_code)[match(d$MÊS, meses_code)] ))) 
          

    gg <- ggplotly(g, tooltip="text")
    
    (gg)
    })
  
}

# Run the app
shinyApp(ui, server)
