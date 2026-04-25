library(shiny)
library(plotly)
library(tidyverse)

training <- read_csv("data/training/training_fe.csv") %>%
  mutate(previous_crop = as.factor(previous_crop),
         year = as.integer(year)) %>%
  drop_na()

ui <- navbarPage("Test",
                 tabPanel("Tab 1", h3("This is Tab 1")),
                 tabPanel("Tab 2",
                          mainPanel(
                            plotlyOutput("soil_plot", height = "400px")
                          )
                 )
)

server <- function(input, output, session) {
  output$soil_plot <- renderPlotly({
    plot_ly(training %>% slice_sample(n = 5000),
            x = ~soilpH, y = ~yield_mg_ha,
            type = "scatter", mode = "markers",
            marker = list(color = "#52B788", size = 3)) %>%
      layout(xaxis = list(title = "Soil pH"),
             yaxis = list(title = "Yield (Mg/ha)"))
  })
}

shinyApp(ui, server)