library(shiny)

ui <- fluidPage(
  h1("Render werkt!")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)