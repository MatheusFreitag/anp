# Load packages ----
library(shiny)

# User interface ----
ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(
    sidebarPanel("sidebar panel"),
    mainPanel("main panel")
  )
)

# Server logic
server <- function(input, output) {
  
  
}

# Run the app
shinyApp(ui, server)
