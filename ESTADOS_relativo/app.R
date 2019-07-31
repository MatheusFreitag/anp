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
            checkboxGroupInput("userInput_estado", "Escolha os Estados",
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
  
  df <- read.csv('./data/2004-2019.tsv', sep='\t')
  meses_code <- c('Inicio do ano' = 1, 
                  'Fim do ano' = 2)
  
  output$estados <- renderText({input$userInput_estado2})
  
  output$plot <- renderPlotly({
    
    
    d <- df[df$ANO == input$userInput_ano & 
              df$PRODUTO == input$userInput_produto &
              df$ESTADO %in% input$userInput_estado, ]
    
    crescimento_relativo <- aggregate(d$PREÇO.MÉDIO.REVENDA, by=list(MÊS = d$MÊS, ESTADO = d$ESTADO), mean)
    
    d_final <- data.frame()
    
    for (cada_regiao in unique(crescimento_relativo$ESTADO)){
      d_r <- crescimento_relativo[crescimento_relativo$ESTADO == cada_regiao, ]
      d_final <- rbind(d_final, data.frame('MÊS' = 1, 'ESTADO' = cada_regiao, 'TAXA'= 0))
      taxa = (tail(d_r, 1)$x - head(d_r, 1)$x) / head(d_r, 1)$x * 100
      d_final <- rbind(d_final, data.frame('MÊS' = 2, 'ESTADO' = cada_regiao, 'TAXA'=  taxa))
    }
    
    d <- d_final
    
    g <- ggplot(d, aes(x=MÊS, y=TAXA, group = ESTADO, colour = ESTADO)) +
      geom_line() +
      scale_x_continuous(paste("Meses observados de ", input$userInput_ano), 
                         labels = names(meses_code)[match(d$MÊS, meses_code)], 
                         breaks = d$MÊS) + 
      ylim(c(-20, 20)) + 
      labs(title=sprintf("Crescimento acumulado de preços (%s) por Estado", input$userInput_produto), y="Crescimento (valores relativos)") + 
      geom_point(aes(text=sprintf("%s<br>%s<br>Crescimento: %.3f%s<br>Período: %s", d$ESTADO, input$userInput_produto,d$TAXA, '%', names(meses_code)[match(d$MÊS, meses_code)] ))) 
    
    
    gg <- ggplotly(g, tooltip="text")
    
    (gg)
    })
  
}

# Run the app
shinyApp(ui, server)
