library(tidyverse)
library(plotly)

training <- read_csv("data/training/training_fe.csv") %>%
  mutate(previous_crop = as.factor(previous_crop),
         year          = as.integer(year)) %>%
  drop_na()

# Test soil plot
x_var <- "soilpH"
x_lab <- "Soil pH"
df <- training %>% filter(!is.na(.data[[x_var]]))

p1 <- plot_ly(df, x = ~soilpH, y = ~yield_mg_ha,
              type = "scatter", mode = "markers",
              marker = list(color = "#52B788", size = 3, opacity = 0.4)) %>%
  layout(xaxis = list(title = x_lab),
         yaxis = list(title = "Yield (Mg/ha)"))
p1

# Test weather plot
p2 <- plot_ly(training, x = ~gdd_season, y = ~yield_mg_ha,
              color = ~as.factor(year),
              type = "scatter", mode = "markers",
              marker = list(size = 3, opacity = 0.5)) %>%
  layout(xaxis = list(title = "GDD"),
         yaxis = list(title = "Yield (Mg/ha)"))
p2

# Test elevation plot
p3 <- plot_ly(training, x = ~elevation, y = ~yield_mg_ha,
              type = "scatter", mode = "markers",
              marker = list(color = "#52B788", size = 4, opacity = 0.5)) %>%
  layout(xaxis = list(title = "Elevation (m)"),
         yaxis = list(title = "Yield (Mg/ha)"))
p3

file.copy("data", "App/", recursive = TRUE, overwrite = TRUE)
library(shiny)
runApp("App")