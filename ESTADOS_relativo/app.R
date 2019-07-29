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
    
    crescimento_relativo <- aggregate(d$PREÇO.MÉDIO.REVENDA, by=list(MÊS = d$MÊS, ESTADO = d$ESTADO), mean)
    
    d_final <- data.frame()
    
    for (cada_estado in unique(crescimento_relativo$ESTADO)){
      d_r <- crescimento_relativo[crescimento_relativo$ESTADO == cada_estado, ]
      
      x <- d_r$x
      taxa_de_crescimento <- c(0)
      i <- 1
      for (value in x){
        if (i != 1){
          taxa = taxa_de_crescimento[i-1] + ((x[i] - x[i-1]) / x[i] * 100)
          taxa_de_crescimento = c(taxa_de_crescimento, taxa)
        }
        i <- i+1
      }
      
      d_r$x <- taxa_de_crescimento
      
      d_final <- rbind(d_final, d_r)
    }
    
    d <- d_final
    
    g <- ggplot(d, aes(x=MÊS, y=x, group = ESTADO, colour = ESTADO)) +
          geom_line() +
          scale_x_continuous(paste("Meses observados de ", input$userInput_ano), 
                             labels = names(meses_code)[match(d$MÊS, meses_code)], 
                             breaks = d$MÊS) + 
          ylim(c(-20, 20)) + 
          labs(title="Histórico de alteração acumulada relativa de preços por Estado", y="Crescimento (valores relativos acumulados)") + 
          geom_point(aes(text=sprintf("%s<br>Crescimento: %f%s<br>Mês: %s", d$ESTADO, d$x, '%', names(meses_code)[match(d$MÊS, meses_code)] ))) 
          

    gg <- ggplotly(g, tooltip="text")
    
    (gg)
    })
  
}

# Run the app
shinyApp(ui, server)