# Load packages ----
library(shiny)
library(ggplot2)
library(plotly)

# User interface ----
ui <- fluidPage(

      fluidRow(
        column(2,""),
        column(3,
             selectInput("userInput_produto", 
                         label = "Escolha o Produto",
                         choices = list("ETANOL HIDRATADO",
                                        "GASOLINA COMUM",
                                        "ÓLEO DIESEL"
                         ),
                         selected = "GASOLINA COMUM"
             )
        ),
        column(5,
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
              selected='RIO GRANDE DO SUL')
        )
      ),
      
      htmlOutput("newline"),
      
      dataTableOutput('table'),
      
      htmlOutput("newline2"),
            
      plotlyOutput('plot')
              
      )


# Server logic

server <- function(input, output) {
  
  output$newline<- renderUI({
    HTML("<br/><br/>")
  })
  
  output$newline2<- renderUI({
    HTML("<br/><br/>")
  })
  
  
  df <- read.csv('./data/2004-2019.tsv', sep='\t')
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
  
  
  semana1 = c('2019-01-06', '2019-02-03', '2019-03-03', '2019-04-07', '2019-05-05', '2019-06-02')
  semana2 = c('2019-01-13', '2019-02-10', '2019-03-10', '2019-04-14', '2019-05-12', '2019-06-09')
  semana3 = c('2019-01-20', '2019-02-17', '2019-03-17', '2019-04-21', '2019-05-19', '2019-06-16')
  semana4 = c('2019-01-27', '2019-02-24', '2019-03-24', '2019-04-28', '2019-05-26', '2019-06-23')
  
  
  
  output$plot <- renderPlotly({
    
    d <- df[df$ANO == 2019 &
            df$PRODUTO == input$userInput_produto &
            df$ESTADO == input$userInput_estado, ]
    
   
    d1 <- d[d$DATA.INICIAL %in% semana1, ]
    d2 <- d[d$DATA.INICIAL %in% semana2, ]
    d3 <- d[d$DATA.INICIAL %in% semana3, ]
    d4 <- d[d$DATA.INICIAL %in% semana4, ]
    
    d1$SEMANA = '1'
    d2$SEMANA = '2'
    d3$SEMANA = '3'
    d4$SEMANA = '4'
    
    d_final = rbind(d1,d2,d3,d4)
    
    d <- d_final
    
    g <- ggplot(d, aes(x=MÊS, y=PREÇO.MÉDIO.REVENDA, group = SEMANA, colour = SEMANA)) +
          geom_line() +
          scale_x_continuous(paste("Meses observados de 2019 para ", input$userInput_estado), 
                             labels = names(meses_code)[match(d$MÊS, meses_code)], 
                             breaks = d$MÊS) + 
          ylim(c(3, 5.6)) + 
          labs(title=sprintf("Preço médio semanal (%s) por mês de 2019",  input$userInput_produto ), y="Preço (em valores absolutos)") + 
          geom_point(aes(text=sprintf("%s<br>Preço Médio : R$%.3f<br>Mês: %s<br>SEMANA: %sª do mês", d$ESTADO, d$PREÇO.MÉDIO.REVENDA, names(meses_code)[match(d$MÊS, meses_code)], d$SEMANA ))) 
          

    gg <- ggplotly(g, tooltip="text")
    
    (gg)
    
})
  
  
  output$table <- renderDataTable({
    d <- df[df$ANO == 2019 &
            df$PRODUTO == input$userInput_produto &
            df$ESTADO == input$userInput_estado, ]
    
    
    d1 <- d[d$DATA.INICIAL %in% semana1, ]
    d2 <- d[d$DATA.INICIAL %in% semana2, ]
    d3 <- d[d$DATA.INICIAL %in% semana3, ]
    d4 <- d[d$DATA.INICIAL %in% semana4, ]
    
    trial <- matrix(c(round(mean(d1$PREÇO.MÉDIO.REVENDA), 3),
                      round(mean(d2$PREÇO.MÉDIO.REVENDA), 3),
                      round(mean(d3$PREÇO.MÉDIO.REVENDA), 3),
                      round(mean(d4$PREÇO.MÉDIO.REVENDA), 3)), ncol=4)
    colnames(trial) <- c('Semana 1', 'Semana 2', 'Semana 3', 'Semana 4')
    rownames(trial) <- c('Preço Médio')
    
    trial <- as.table(trial)
    
    trial
    
  }, options = list(paging = FALSE, searching = FALSE))
  
}

# Run the app
shinyApp(ui, server)
