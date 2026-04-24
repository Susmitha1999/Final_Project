library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(DT)

# ── Load data ────────────────────────────────────────────────────────────────
training <- read_csv("data/training/training_fe.csv") %>%
  mutate(
    previous_crop = as.factor(previous_crop),
    year          = as.integer(year)
  ) %>%
  drop_na()

# ── Color palette ─────────────────────────────────────────────────────────────
COL_GREEN    <- "#2D6A4F"
COL_TEAL     <- "#52B788"
COL_GOLD     <- "#D4A017"
COL_RED      <- "#C1440E"
COL_BLUE     <- "#1A6B9A"
COL_PURPLE   <- "#7B2D8B"
COL_ORANGE   <- "#E07B39"
COL_LIGHT    <- "#B7E4C7"
COL_BG       <- "#FAFAF7"
COL_SIDEBAR  <- "#F0F7F0"

# Professional diverging palette for sites/years
SITE_COLORS  <- c("#2D6A4F","#52B788","#1A6B9A","#D4A017","#C1440E",
                  "#7B2D8B","#E07B39","#3A86FF","#FB5607","#8338EC",
                  "#06D6A0","#118AB2","#FFD166","#EF476F","#073B4C")

YEAR_COLORS  <- c("#1A6B9A","#2D6A4F","#52B788","#D4A017","#E07B39",
                  "#C1440E","#7B2D8B","#3A86FF","#FB5607","#06D6A0")

# ── Variable lists ────────────────────────────────────────────────────────────
weather_vars <- c(
  "GDD (Season)"       = "gdd_season",
  "Precipitation (mm)" = "prcp_total_mm",
  "Max Temp (°C)"      = "tmax_mean",
  "Min Temp (°C)"      = "tmin_mean",
  "Solar Radiation"    = "srad_mean",
  "Vapor Pressure"     = "vp_mean",
  "Heat Stress Days"   = "heat_stress_days"
)

soil_vars <- c(
  "Soil pH"            = "soilpH",
  "Organic Matter (%)" = "om_pct",
  "Soil K (ppm)"       = "soilk_ppm",
  "Soil P (ppm)"       = "soilp_ppm"
)

color_vars <- c(
  "Year"          = "year",
  "Site"          = "site",
  "Previous Crop" = "previous_crop"
)

corr_vars <- c("yield_mg_ha","gdd_season","prcp_total_mm","tmax_mean",
               "tmin_mean","heat_stress_days","soilpH","om_pct",
               "soilk_ppm","soilp_ppm","elevation")

corr_labels <- c("Yield","GDD","Precip","Tmax","Tmin",
                 "Heat Stress","Soil pH","Org Matter",
                 "Soil K","Soil P","Elevation")

# ── Theme ─────────────────────────────────────────────────────────────────────
corn_theme <- bs_theme(
  bg           = COL_BG,
  fg           = "#1C1C1C",
  primary      = COL_GREEN,
  secondary    = COL_TEAL,
  success      = COL_TEAL,
  warning      = COL_GOLD,
  danger       = COL_RED,
  base_font    = font_google("IBM Plex Sans"),
  heading_font = font_google("Playfair Display"),
  font_scale   = 0.9
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = span(
    style = "font-family:'Playfair Display',serif; font-weight:700; color:#2D6A4F; font-size:1.1rem;",
    "🌽 Corn Yield Prediction Dashboard"
  ),
  theme = corn_theme,
  bg    = "#1C3829",
  
  header = div(
    style = "padding:10px 20px; background:#1C3829;",
    layout_columns(
      fill = FALSE,
      value_box("Total Observations", scales::comma(nrow(training)),
                showcase = bsicons::bs_icon("database"),
                style = "background: linear-gradient(135deg,#2D6A4F,#52B788); color:white; border:none;"),
      value_box("Sites", n_distinct(training$site),
                showcase = bsicons::bs_icon("geo-alt"),
                style = "background: linear-gradient(135deg,#1A6B9A,#3A86FF); color:white; border:none;"),
      value_box("Years", paste(min(training$year), "–", max(training$year)),
                showcase = bsicons::bs_icon("calendar3"),
                style = "background: linear-gradient(135deg,#7B2D8B,#C77DFF); color:white; border:none;"),
      value_box("Mean Yield (Mg/ha)", round(mean(training$yield_mg_ha), 2),
                showcase = bsicons::bs_icon("bar-chart"),
                style = "background: linear-gradient(135deg,#D4A017,#FFD166); color:white; border:none;"),
      value_box("Hybrids", scales::comma(n_distinct(training$hybrid)),
                showcase = bsicons::bs_icon("flower1"),
                style = "background: linear-gradient(135deg,#C1440E,#E07B39); color:white; border:none;")
    )
  ),
  
  # ════════════════════════════════════════════════════════════════
  # TAB 1 — Yield Explorer
  # ════════════════════════════════════════════════════════════════
  nav_panel(
    title = "📊 Yield Explorer",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = COL_SIDEBAR,
        
        div(style = "background:#2D6A4F; color:white; padding:8px 12px; border-radius:6px; font-weight:700; margin-bottom:10px;",
            "🎛️ Filters & Options"),
        
        sliderInput("year_range", "Year Range",
                    min = min(training$year), max = max(training$year),
                    value = c(min(training$year), max(training$year)),
                    step = 1, sep = ""),
        
        selectInput("site_filter", "Filter by Site",
                    choices  = c("All Sites" = "all", sort(unique(training$site))),
                    selected = "all"),
        
        hr(style = "border-color:#B7E4C7;"),
        
        radioButtons("crop_plot_type", "Previous Crop Chart Type",
                     choices  = c("Boxplot" = "box", "Violin" = "violin"),
                     selected = "box", inline = TRUE),
        
        checkboxInput("show_site_trends", "Overlay individual site trends", FALSE),
        
        hr(style = "border-color:#B7E4C7;"),
        
        div(style = "background:#F8F4E8; border-left:4px solid #D4A017; padding:10px; border-radius:4px;",
            h6(style = "color:#D4A017; font-weight:700; margin:0 0 6px 0;", "📋 Summary Stats"),
            uiOutput("summary_stats_ui"))
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        # Yield distribution
        card(
          card_header(
            style = paste0("background:linear-gradient(135deg,",COL_GREEN,",",COL_TEAL,"); color:white; font-weight:600;"),
            "📊 Yield Distribution"
          ),
          plotlyOutput("yield_hist", height = "300px")
        ),
        
        # Mean yield by year
        card(
          card_header(
            style = paste0("background:linear-gradient(135deg,",COL_BLUE,",#3A86FF); color:white; font-weight:600;"),
            "📈 Mean Yield by Year"
          ),
          plotlyOutput("yield_year", height = "300px")
        ),
        
        # Yield by site
        card(
          card_header(
            style = paste0("background:linear-gradient(135deg,",COL_PURPLE,",#C77DFF); color:white; font-weight:600;"),
            "🗺️ Yield by Site"
          ),
          card_body(
            selectizeInput("site_select", "Select Sites to Compare",
                           choices  = sort(unique(training$site)),
                           selected = sort(unique(training$site))[1:10],
                           multiple = TRUE,
                           options  = list(maxItems = 43)),
            plotlyOutput("yield_site", height = "300px")
          )
        ),
        
        # Yield by previous crop
        card(
          card_header(
            style = paste0("background:linear-gradient(135deg,",COL_GOLD,",#FFD166); color:white; font-weight:600;"),
            "🌾 Yield by Previous Crop"
          ),
          plotlyOutput("yield_crop", height = "300px")
        )
      )
    )
  ),
  
  # ════════════════════════════════════════════════════════════════
  # TAB 2 — Environment Explorer
  # ════════════════════════════════════════════════════════════════
  nav_panel(
    title = "🌍 Environment Explorer",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 270, bg = COL_SIDEBAR,
        
        div(style = "background:#1A6B9A; color:white; padding:8px 12px; border-radius:6px; font-weight:700; margin-bottom:10px;",
            "🎛️ Controls"),
        
        div(style = "background:#E8F5E9; border-left:4px solid #52B788; padding:8px; border-radius:4px; margin-bottom:8px;",
            h6(style = "color:#2D6A4F; font-weight:700;", "🌱 Soil"),
            selectInput("soil_var", NULL, choices = soil_vars, selected = "soilpH"),
            sliderInput("soil_year_range", "Filter Year",
                        min = min(training$year), max = max(training$year),
                        value = c(min(training$year), max(training$year)),
                        step = 1, sep = "")
        ),
        
        div(style = "background:#E3F2FD; border-left:4px solid #1A6B9A; padding:8px; border-radius:4px; margin-bottom:8px;",
            h6(style = "color:#1A6B9A; font-weight:700;", "🌦️ Weather"),
            selectInput("weather_var", NULL, choices = weather_vars, selected = "gdd_season"),
            radioButtons("weather_color", "Color By",
                         choices = color_vars, selected = "year"),
            checkboxInput("weather_smooth", "Add loess smoother", TRUE)
        ),
        
        div(style = "background:#FFF3E0; border-left:4px solid #E07B39; padding:8px; border-radius:4px;",
            h6(style = "color:#E07B39; font-weight:700;", "⛰️ Elevation"),
            selectInput("elev_color_var", "Color By Soil Var",
                        choices = soil_vars, selected = "soilpH")
        )
      ),
      
      layout_columns(
        col_widths = c(4, 4, 4),
        
        card(
          card_header(
            style = "background:linear-gradient(135deg,#2D6A4F,#52B788); color:white; font-weight:600;",
            "🌱 Soil vs Yield"
          ),
          plotlyOutput("soil_plot", height = "300px"),
          uiOutput("soil_corr_ui")
        ),
        
        card(
          card_header(
            style = "background:linear-gradient(135deg,#1A6B9A,#3A86FF); color:white; font-weight:600;",
            "🌦️ Weather vs Yield"
          ),
          plotlyOutput("weather_plot", height = "300px")
        ),
        
        card(
          card_header(
            style = "background:linear-gradient(135deg,#E07B39,#FFD166); color:white; font-weight:600;",
            "⛰️ Elevation vs Yield"
          ),
          plotlyOutput("elev_plot", height = "300px")
        )
      ),
      
      card(
        card_header(
          style = "background:linear-gradient(135deg,#7B2D8B,#C77DFF); color:white; font-weight:600;",
          "🔥 Correlation Matrix — Click a cell to explore that relationship"
        ),
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput("corr_matrix", height = "400px"),
          plotlyOutput("corr_detail", height = "400px")
        )
      )
    )
  ),
  
  # ════════════════════════════════════════════════════════════════
  # PLACEHOLDER TABS
  # ════════════════════════════════════════════════════════════════
  nav_panel(
    title = "⚔️ Model Comparison",
    card(card_body(div(
      style = "text-align:center; padding:80px; color:#74C69D;",
      h3("Coming soon — XGBoost results pending")
    )))
  ),
  nav_panel(
    title = "🏆 Final Model",
    card(card_body(div(
      style = "text-align:center; padding:80px; color:#74C69D;",
      h3("Coming soon — Final model selection pending")
    )))
  ),
  nav_panel(
    title = "🔍 Variable Importance",
    card(card_body(div(
      style = "text-align:center; padding:80px; color:#74C69D;",
      h3("Coming soon")
    )))
  ),
  nav_panel(
    title = "🔮 2024 Predictions",
    card(card_body(div(
      style = "text-align:center; padding:80px; color:#74C69D;",
      h3("Coming soon — Predictions pending")
    )))
  )
)


# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Tab 1 filtered data ───────────────────────────────────────────────────
  tab1_data <- reactive({
    df <- training %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    if (input$site_filter != "all") df <- df %>% filter(site == input$site_filter)
    df
  })
  
  # ── Summary stats ─────────────────────────────────────────────────────────
  output$summary_stats_ui <- renderUI({
    df <- tab1_data()
    tags$table(
      style = "width:100%; font-size:0.82rem;",
      tags$tr(tags$td(style="color:#666;","Mean:"),
              tags$td(strong(style="color:#2D6A4F;", round(mean(df$yield_mg_ha),2), " Mg/ha"))),
      tags$tr(tags$td(style="color:#666;","Median:"),
              tags$td(strong(style="color:#2D6A4F;", round(median(df$yield_mg_ha),2), " Mg/ha"))),
      tags$tr(tags$td(style="color:#666;","Min:"),
              tags$td(strong(style="color:#C1440E;", round(min(df$yield_mg_ha),2), " Mg/ha"))),
      tags$tr(tags$td(style="color:#666;","Max:"),
              tags$td(strong(style="color:#1A6B9A;", round(max(df$yield_mg_ha),2), " Mg/ha"))),
      tags$tr(tags$td(style="color:#666;","N:"),
              tags$td(strong(scales::comma(nrow(df)))))
    )
  })
  
  # ── Yield histogram ───────────────────────────────────────────────────────
  output$yield_hist <- renderPlotly({
    plot_ly(tab1_data(), x = ~yield_mg_ha, type = "histogram", nbinsx = 50,
            marker = list(
              color = "#52B788",
              line  = list(color = "#2D6A4F", width = 0.5)
            )) %>%
      layout(xaxis = list(title = "Yield (Mg/ha)"),
             yaxis = list(title = "Count"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)")
  })
  
  # ── Mean yield by year ────────────────────────────────────────────────────
  output$yield_year <- renderPlotly({
    df <- tab1_data()
    overall <- df %>%
      group_by(year) %>%
      summarise(mean_yield = mean(yield_mg_ha), .groups = "drop")
    
    p <- plot_ly()
    
    if (isTRUE(input$show_site_trends)) {
      site_trends <- df %>%
        group_by(year, site) %>%
        summarise(mean_yield = mean(yield_mg_ha), .groups = "drop")
      sites <- unique(site_trends$site)
      for (i in seq_along(sites)) {
        s  <- sites[i]
        sd <- site_trends %>% filter(site == s)
        p  <- p %>% add_trace(
          data = sd, x = ~year, y = ~mean_yield,
          type = "scatter", mode = "lines",
          line = list(color = SITE_COLORS[(i - 1) %% length(SITE_COLORS) + 1], width = 1),
          opacity = 0.4, showlegend = FALSE,
          hoverinfo = "text",
          text = ~paste(s, "| Year:", year, "| Yield:", round(mean_yield, 2))
        )
      }
    }
    
    p %>%
      add_trace(data = overall, x = ~year, y = ~mean_yield,
                type = "scatter", mode = "lines+markers",
                line   = list(color = COL_BLUE, width = 3),
                marker = list(color = COL_GOLD, size = 10,
                              line = list(color = "white", width = 2)),
                name = "Overall Mean",
                hoverinfo = "text",
                text = ~paste("Year:", year, "<br>Mean:", round(mean_yield, 2), "Mg/ha")) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Mean Yield (Mg/ha)"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)")
  })
  
  # ── Yield by site ─────────────────────────────────────────────────────────
  output$yield_site <- renderPlotly({
    req(input$site_select)
    df <- tab1_data() %>% filter(site %in% input$site_select)
    site_order <- df %>%
      group_by(site) %>%
      summarise(med = median(yield_mg_ha)) %>%
      arrange(med) %>% pull(site)
    n_sites <- length(unique(df$site))
    df %>%
      mutate(site = factor(site, levels = site_order)) %>%
      plot_ly(x = ~yield_mg_ha, y = ~site, type = "box",
              color = ~site,
              colors = rep(SITE_COLORS, length.out = n_sites),
              showlegend = FALSE) %>%
      layout(xaxis = list(title = "Yield (Mg/ha)"),
             yaxis = list(title = ""),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)")
  })
  
  # ── Yield by previous crop ────────────────────────────────────────────────
  output$yield_crop <- renderPlotly({
    df <- tab1_data()
    crop_order <- df %>%
      group_by(previous_crop) %>%
      summarise(med = median(yield_mg_ha)) %>%
      arrange(med) %>% pull(previous_crop)
    df <- df %>%
      mutate(previous_crop = factor(as.character(previous_crop),
                                    levels = as.character(crop_order)))
    n_crops <- n_distinct(df$previous_crop)
    crop_colors <- rep(SITE_COLORS, length.out = n_crops)
    
    if (input$crop_plot_type == "box") {
      plot_ly(df, x = ~previous_crop, y = ~yield_mg_ha,
              type = "box", color = ~previous_crop,
              colors = crop_colors, showlegend = FALSE) %>%
        layout(xaxis = list(title = "Previous Crop"),
               yaxis = list(title = "Yield (Mg/ha)"),
               paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor  = "rgba(0,0,0,0)")
    } else {
      plot_ly(df, x = ~previous_crop, y = ~yield_mg_ha,
              type = "violin", color = ~previous_crop,
              colors = crop_colors, showlegend = FALSE,
              box = list(visible = TRUE),
              meanline = list(visible = TRUE)) %>%
        layout(xaxis = list(title = "Previous Crop"),
               yaxis = list(title = "Yield (Mg/ha)"),
               paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor  = "rgba(0,0,0,0)")
    }
  })
  
  # ── Soil plot ─────────────────────────────────────────────────────────────
  output$soil_plot <- renderPlotly({
    df    <- training %>%
      filter(year >= input$soil_year_range[1],
             year <= input$soil_year_range[2])
    x_var <- input$soil_var
    x_lab <- names(soil_vars)[soil_vars == x_var]
    corr  <- round(cor(df[[x_var]], df$yield_mg_ha, use = "complete.obs"), 3)
    
    df_clean <- df %>% filter(!is.na(.data[[x_var]]))
    fit <- tryCatch(
      loess(as.formula(paste("yield_mg_ha ~", x_var)), data = df_clean, span = 0.5),
      error = function(e) NULL
    )
    
    p <- plot_ly(df_clean,
                 x = ~get(x_var), y = ~yield_mg_ha,
                 type = "scatter", mode = "markers",
                 marker = list(color = COL_TEAL, size = 3, opacity = 0.4),
                 hoverinfo = "text",
                 text = ~paste(x_lab, ":", round(get(x_var), 2),
                               "<br>Yield:", round(yield_mg_ha, 2))) %>%
      layout(xaxis = list(title = x_lab),
             yaxis = list(title = "Yield (Mg/ha)"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             annotations = list(list(
               x = 0.05, y = 0.95, xref = "paper", yref = "paper",
               text = paste("r =", corr), showarrow = FALSE,
               font = list(size = 13, color = COL_GREEN, family = "IBM Plex Mono")
             )))
    
    if (!is.null(fit)) {
      ord <- order(df_clean[[x_var]])
      p <- p %>% add_lines(
        x = df_clean[[x_var]][ord],
        y = fitted(fit)[ord],
        line = list(color = COL_RED, width = 2.5),
        showlegend = FALSE, inherit = FALSE
      )
    }
    p
  })
  
  output$soil_corr_ui <- renderUI({
    df   <- training %>%
      filter(year >= input$soil_year_range[1],
             year <= input$soil_year_range[2])
    corr <- round(cor(df[[input$soil_var]], df$yield_mg_ha, use = "complete.obs"), 3)
    div(style = "text-align:center; padding:4px; font-size:0.8rem;",
        span(style = paste0("color:", COL_GREEN, "; font-weight:600;"),
             paste("Pearson r =", corr)),
        span(style = "color:#666; margin-left:8px;",
             paste("| n =", scales::comma(sum(!is.na(df[[input$soil_var]])))))
    )
  })
  
  # ── Weather plot ──────────────────────────────────────────────────────────
  output$weather_plot <- renderPlotly({
    x_var   <- input$weather_var
    col_var <- input$weather_color
    x_lab   <- names(weather_vars)[weather_vars == x_var]
    col_lab <- names(color_vars)[color_vars == col_var]
    smooth  <- input$weather_smooth
    
    df <- training %>% filter(!is.na(.data[[x_var]]))
    
    n_levels <- n_distinct(df[[col_var]])
    pal      <- rep(SITE_COLORS, length.out = n_levels)
    
    p <- plot_ly(df,
                 x     = ~get(x_var),
                 y     = ~yield_mg_ha,
                 color = ~as.factor(get(col_var)),
                 colors = pal,
                 type  = "scatter", mode = "markers",
                 marker = list(size = 3, opacity = 0.5),
                 hoverinfo = "text",
                 text = ~paste(x_lab, ":", round(get(x_var), 2),
                               "<br>Yield:", round(yield_mg_ha, 2),
                               "<br>", col_lab, ":", get(col_var))) %>%
      layout(xaxis = list(title = x_lab),
             yaxis = list(title = "Yield (Mg/ha)"),
             legend = list(title = list(text = col_lab)),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)")
    
    if (smooth) {
      fit <- tryCatch(
        loess(as.formula(paste("yield_mg_ha ~", x_var)), data = df, span = 0.4),
        error = function(e) NULL
      )
      if (!is.null(fit)) {
        ord <- order(df[[x_var]])
        p <- p %>% add_lines(
          x = df[[x_var]][ord], y = fitted(fit)[ord],
          line = list(color = COL_RED, width = 2.5),
          name = "Trend", showlegend = FALSE, inherit = FALSE
        )
      }
    }
    p
  })
  
  # ── Elevation plot ────────────────────────────────────────────────────────
  output$elev_plot <- renderPlotly({
    col_var <- input$elev_color_var
    col_lab <- names(soil_vars)[soil_vars == col_var]
    df      <- training %>% filter(!is.na(elevation), !is.na(.data[[col_var]]))
    fit     <- tryCatch(
      loess(yield_mg_ha ~ elevation, data = df, span = 0.5),
      error = function(e) NULL
    )
    ord <- order(df$elevation)
    
    p <- plot_ly(df, x = ~elevation, y = ~yield_mg_ha,
                 color = ~get(col_var),
                 colors = c(COL_LIGHT, COL_GREEN),
                 type = "scatter", mode = "markers",
                 marker = list(size = 4, opacity = 0.5),
                 hoverinfo = "text",
                 text = ~paste("Elevation:", round(elevation, 1), "m",
                               "<br>Yield:", round(yield_mg_ha, 2),
                               "<br>", col_lab, ":", round(get(col_var), 2),
                               "<br>Site:", site)) %>%
      layout(xaxis = list(title = "Elevation (m)"),
             yaxis = list(title = "Yield (Mg/ha)"),
             paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)")
    
    if (!is.null(fit)) {
      p <- p %>% add_lines(
        x = df$elevation[ord], y = fitted(fit)[ord],
        line = list(color = COL_RED, width = 2.5),
        name = "Trend", showlegend = FALSE, inherit = FALSE
      )
    }
    p
  })
  
  # ── Correlation matrix ─────────────────────────────────────────────────────
  corr_mat <- cor(training[, corr_vars], use = "pairwise.complete.obs")
  
  output$corr_matrix <- renderPlotly({
    plot_ly(x = corr_labels, y = rev(corr_labels),
            z = corr_mat[nrow(corr_mat):1, ],
            type = "heatmap",
            colorscale = list(
              c(0,   COL_RED),
              c(0.5, "#FAFAF7"),
              c(1,   COL_GREEN)
            ),
            zmin = -1, zmax = 1,
            hovertemplate = "%{y} vs %{x}<br>r = %{z:.2f}<extra></extra>",
            source = "corr_click") %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)",
             plot_bgcolor  = "rgba(0,0,0,0)",
             xaxis = list(tickfont = list(size = 9), tickangle = -35),
             yaxis = list(tickfont = list(size = 9))) %>%
      event_register("plotly_click")
  })
  
  output$corr_detail <- renderPlotly({
    click <- event_data("plotly_click", source = "corr_click")
    
    if (is.null(click)) {
      return(
        plotly_empty() %>%
          layout(paper_bgcolor = "rgba(0,0,0,0)",
                 plot_bgcolor  = "rgba(0,0,0,0)",
                 annotations = list(list(
                   text = "👆 Click any cell in the<br>matrix to explore that<br>relationship here",
                   showarrow = FALSE,
                   font = list(size = 14, color = COL_TEAL),
                   xref = "paper", yref = "paper", x = 0.5, y = 0.5
                 )))
      )
    }
    
    x_idx <- which(corr_labels == click$x)
    y_idx <- which(rev(corr_labels) == click$y)
    if (length(x_idx) == 0 || length(y_idx) == 0) return(plotly_empty())
    
    x_col <- corr_vars[x_idx]
    y_col <- corr_vars[which(corr_labels == click$y)]
    if (length(y_col) == 0) y_col <- corr_vars[y_idx]
    
    corr_val <- round(cor(training[[x_col]], training[[y_col]], use = "complete.obs"), 3)
    df_c <- training %>% filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]]))
    fit  <- tryCatch(
      loess(as.formula(paste(y_col, "~", x_col)), data = df_c, span = 0.4),
      error = function(e) NULL
    )
    ord <- order(df_c[[x_col]])
    
    p <- plot_ly(df_c,
                 x = ~get(x_col), y = ~get(y_col),
                 type = "scatter", mode = "markers",
                 marker = list(color = COL_TEAL, size = 3, opacity = 0.4),
                 hoverinfo = "text",
                 text = ~paste(click$x, ":", round(get(x_col), 2),
                               "<br>", click$y, ":", round(get(y_col), 2))) %>%
      layout(title = list(
        text = paste0(click$x, " vs ", click$y, "  |  r = ", corr_val),
        font = list(size = 12, color = COL_GREEN)
      ),
      xaxis = list(title = click$x),
      yaxis = list(title = click$y),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)")
    
    if (!is.null(fit)) {
      p <- p %>% add_lines(
        x = df_c[[x_col]][ord], y = fitted(fit)[ord],
        line = list(color = COL_RED, width = 2.5),
        showlegend = FALSE, inherit = FALSE
      )
    }
    p
  })
}

shinyApp(ui = ui, server = server)