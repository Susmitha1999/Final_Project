library(shiny)
library(tidyverse)
library(plotly)
library(DT)

# ── Load training data ────────────────────────────────────────────────────────
training <- read_csv("data/training/training_fe.csv") %>%
  mutate(previous_crop = as.factor(previous_crop),
         year          = as.integer(year)) %>%
  drop_na()

# ── Load Cubist outputs ───────────────────────────────────────────────────────
cubist_test_preds    <- read_csv("output/cubist_test_predictions.csv")
cubist_test_metrics  <- read_csv("output/cubist_test_metrics.csv")
cubist_train_metrics <- read_csv("output/cubist_train_metrics.csv")
cubist_importance    <- read_csv("output/cubist_importance.csv")
cubist_resid_site    <- read_csv("output/cubist_residuals_by_site.csv")
cubist_resid_year    <- read_csv("output/cubist_residuals_by_year.csv")
cubist_preds_2024    <- read_csv("output/testing_final_pred_Cubist_no_gm.csv")

# ── Load XGBoost outputs ──────────────────────────────────────────────────────
xgb_test_preds    <- read_csv("output/xgboost_test_predictions.csv")
xgb_test_metrics  <- read_csv("output/xgboost_test_metrics.csv") %>% select(.metric,.estimator,.estimate)
xgb_train_metrics <- read_csv("output/xgboost_train_metrics.csv") %>% select(.metric,.estimator,.estimate)
xgb_importance    <- read_csv("output/xgboost_importance.csv")
xgb_resid_site    <- read_csv("output/xgboost_residuals_by_site.csv")
xgb_resid_year    <- read_csv("output/xgboost_residuals_by_year.csv")

# ── Colors ────────────────────────────────────────────────────────────────────
COL_GREEN  <- "#2D6A4F"
COL_TEAL   <- "#52B788"
COL_GOLD   <- "#D4A017"
COL_RED    <- "#C1440E"
COL_BLUE   <- "#1A6B9A"
COL_PURPLE <- "#7B2D8B"
COL_ORANGE <- "#E07B39"
COL_LIGHT  <- "#B7E4C7"

SITE_COLORS <- c("#2D6A4F","#52B788","#1A6B9A","#D4A017","#C1440E",
                 "#7B2D8B","#E07B39","#3A86FF","#FB5607","#8338EC",
                 "#06D6A0","#118AB2","#FFD166","#EF476F","#073B4C")

YEAR_COLORS <- c("#023E8A","#0077B6","#0096C7","#00B4D8","#48CAE4",
                 "#90E0EF","#ADE8F4","#CAF0F8","#D4A017","#C1440E")

# ── Variable lists ────────────────────────────────────────────────────────────
weather_vars <- c("GDD (Season)"="gdd_season","Precipitation (mm)"="prcp_total_mm",
                  "Max Temp (°C)"="tmax_mean","Min Temp (°C)"="tmin_mean",
                  "Solar Radiation"="srad_mean","Vapor Pressure"="vp_mean",
                  "Heat Stress Days"="heat_stress_days")

soil_vars <- c("Soil pH"="soilpH","Organic Matter (%)"="om_pct",
               "Soil K (ppm)"="soilk_ppm","Soil P (ppm)"="soilp_ppm")

corr_vars   <- c("yield_mg_ha","gdd_season","prcp_total_mm","tmax_mean","tmin_mean",
                 "heat_stress_days","soilpH","om_pct","soilk_ppm","soilp_ppm","elevation")
corr_labels <- c("Yield","GDD","Precip","Tmax","Tmin","Heat Stress",
                 "Soil pH","Org Matter","Soil K","Soil P","Elevation")

corr_mat <- cor(training[, corr_vars], use = "pairwise.complete.obs")

site_elev <- training %>%
  group_by(site) %>%
  summarise(mean_yield=mean(yield_mg_ha),
            elevation=median(elevation,na.rm=TRUE), .groups="drop")

# ── Process importance ────────────────────────────────────────────────────────
cubist_importance <- cubist_importance %>%
  filter(Importance > 0) %>%
  mutate(
    Group = case_when(
      Variable %in% c("gdd_season","prcp_total_mm","tmax_mean","tmin_mean",
                      "srad_mean","vp_mean","heat_stress_days") ~ "Weather",
      Variable %in% c("soilpH","om_pct","soilk_ppm","soilp_ppm")         ~ "Soil",
      Variable %in% c("longitude","latitude","elevation","site")           ~ "Location",
      Variable %in% c("year","previous_crop","hybrid")                     ~ "Management",
      TRUE ~ "Trial Design"
    ),
    group_color = case_when(
      Group == "Weather"      ~ "#1A6B9A",
      Group == "Soil"         ~ "#7B3F00",
      Group == "Location"     ~ "#2D6A4F",
      Group == "Management"   ~ "#D4A017",
      TRUE                    ~ "#7B2D8B"
    )
  ) %>% arrange(desc(Importance))

# Collapse XGBoost dummy-encoded variables to top-level
xgb_importance_clean <- xgb_importance %>%
  mutate(Variable = case_when(
    str_starts(Variable, "site_")          ~ "site",
    str_starts(Variable, "previous_crop_") ~ "previous_crop",
    Variable == "year_factor"              ~ "year",
    Variable == "soilp_h"                 ~ "soilpH",
    TRUE ~ Variable
  )) %>%
  group_by(Variable) %>%
  summarise(Importance = sum(Importance), .groups = "drop") %>%
  arrange(desc(Importance)) %>%
  mutate(
    Group = case_when(
      Variable %in% c("gdd_season","prcp_total_mm","tmax_mean","tmin_mean",
                      "srad_mean","vp_mean","heat_stress_days") ~ "Weather",
      Variable %in% c("soilpH","om_pct","soilk_ppm","soilp_ppm")         ~ "Soil",
      Variable %in% c("longitude","latitude","elevation","site")           ~ "Location",
      Variable %in% c("year","previous_crop","hybrid")                     ~ "Management",
      TRUE ~ "Trial Design"
    ),
    group_color = case_when(
      Group == "Weather"      ~ "#1A6B9A",
      Group == "Soil"         ~ "#7B3F00",
      Group == "Location"     ~ "#2D6A4F",
      Group == "Management"   ~ "#D4A017",
      TRUE                    ~ "#7B2D8B"
    )
  )

# ── Metrics helpers ───────────────────────────────────────────────────────────
get_metric <- function(df, metric) {
  round(df %>% filter(.metric == metric) %>% pull(.estimate), 3)
}

c_test_rmse  <- get_metric(cubist_test_metrics, "rmse")
c_test_rsq   <- get_metric(cubist_test_metrics, "rsq")
c_test_mae   <- get_metric(cubist_test_metrics, "mae")
c_train_rmse <- get_metric(cubist_train_metrics, "rmse")
c_train_rsq  <- get_metric(cubist_train_metrics, "rsq")

x_test_rmse  <- get_metric(xgb_test_metrics, "rmse")
x_test_rsq   <- get_metric(xgb_test_metrics, "rsq")
x_test_mae   <- get_metric(xgb_test_metrics, "mae")
x_train_rmse <- get_metric(xgb_train_metrics, "rmse")
x_train_rsq  <- get_metric(xgb_train_metrics, "rsq")

# ── CSS ───────────────────────────────────────────────────────────────────────
my_css <- "
body { background-color:#FAFAF7; font-family:'Segoe UI',sans-serif; }
.navbar { background-color:#1C3829 !important; border:none; }
.navbar-brand { color:#D8F3DC !important; font-weight:700; font-size:1.1rem; }
.navbar-nav > li > a { color:#B7E4C7 !important; }
.navbar-nav > li.active > a,
.navbar-nav > li > a:hover { background-color:#2D6A4F !important; color:white !important; }
.value-row { display:flex; gap:10px; padding:10px 15px; background:#1C3829; flex-wrap:wrap; }
.vbox { flex:1; min-width:150px; border-radius:8px; padding:12px 16px; color:white; }
.vbox .vb-title { font-size:0.75rem; opacity:0.85; margin-bottom:4px; }
.vbox .vb-value { font-size:1.3rem; font-weight:700; }
.card-hdr { padding:7px 12px; color:white; font-weight:600; font-size:0.88rem; border-radius:5px 5px 0 0; }
.plot-card { border:1px solid #dee2e6; border-radius:6px; margin-bottom:14px; overflow:hidden; background:white; }
.sidebar-box { padding:8px; border-radius:5px; margin-bottom:8px; }
.sec-hdr { padding:6px 11px; border-radius:4px; font-weight:700; font-size:0.82rem; margin-bottom:10px; color:white; }
.metric-box { border-radius:8px; padding:14px; text-align:center; margin-bottom:10px; }
.metric-val { font-size:1.6rem; font-weight:700; }
.metric-lbl { font-size:0.75rem; opacity:0.85; }
.winner-badge { background:#2D6A4F; color:white; padding:3px 8px; border-radius:12px;
                font-size:0.75rem; font-weight:700; margin-left:6px; }
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "🌽 Corn Yield Prediction Dashboard",
  id    = "main_nav",
  
  header = tagList(
    tags$head(tags$style(my_css)),
    div(class="value-row",
        div(class="vbox",style="background:linear-gradient(135deg,#2D6A4F,#52B788);",
            div(class="vb-title","🗄️ Observations"),
            div(class="vb-value",scales::comma(nrow(training)))),
        div(class="vbox",style="background:linear-gradient(135deg,#1A6B9A,#3A86FF);",
            div(class="vb-title","📍 Sites"),
            div(class="vb-value",n_distinct(training$site))),
        div(class="vbox",style="background:linear-gradient(135deg,#7B2D8B,#C77DFF);",
            div(class="vb-title","📅 Years"),
            div(class="vb-value",paste(min(training$year),"–",max(training$year)))),
        div(class="vbox",style="background:linear-gradient(135deg,#D4A017,#FFD166);",
            div(class="vb-title","📊 Mean Yield (Mg/ha)"),
            div(class="vb-value",round(mean(training$yield_mg_ha),2))),
        div(class="vbox",style="background:linear-gradient(135deg,#C1440E,#E07B39);",
            div(class="vb-title","🌱 Hybrids"),
            div(class="vb-value",scales::comma(n_distinct(training$hybrid))))
    )
  ),
  
  # ══════════════════════════════════════════════════════════════
  # TAB 1 — Yield Explorer
  # ══════════════════════════════════════════════════════════════
  tabPanel("📊 Yield Explorer",
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="sec-hdr",style="background:#2D6A4F;","🎛️ Filters & Options"),
                          sliderInput("year_range","Year Range",
                                      min=min(training$year),max=max(training$year),
                                      value=c(min(training$year),max(training$year)),step=1,sep=""),
                          selectInput("site_filter","Filter by Site",
                                      choices=c("All Sites"="all",sort(unique(training$site))),selected="all"),
                          hr(),
                          radioButtons("crop_plot_type","Previous Crop Chart",
                                       choices=c("Boxplot"="box","Violin"="violin"),selected="box",inline=TRUE),
                          checkboxInput("show_site_trends","Overlay site trends",FALSE),
                          hr(),
                          div(style="background:#F8F4E8;border-left:4px solid #D4A017;padding:10px;border-radius:4px;",
                              strong(style="color:#D4A017;","📋 Summary Stats"),
                              uiOutput("summary_stats_ui"))
             ),
             mainPanel(width=9,
                       fluidRow(
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#2D6A4F,#52B788);","📊 Yield Distribution"),
                                      plotlyOutput("yield_hist",height="280px"))),
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#1A6B9A,#3A86FF);","📈 Mean Yield by Year"),
                                      plotlyOutput("yield_year",height="280px")))
                       ),
                       fluidRow(
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#7B2D8B,#C77DFF);","🗺️ Yield by Site"),
                                      div(style="padding:8px;",
                                          selectizeInput("site_select","Select Sites",
                                                         choices=sort(unique(training$site)),
                                                         selected=sort(unique(training$site))[1:10],
                                                         multiple=TRUE,options=list(maxItems=43))),
                                      plotlyOutput("yield_site",height="260px"))),
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#D4A017,#FFD166);","🌾 Yield by Previous Crop"),
                                      plotlyOutput("yield_crop",height="300px")))
                       )
             )
           )
  ),
  
  # ══════════════════════════════════════════════════════════════
  # TAB 2 — Environment Explorer
  # ══════════════════════════════════════════════════════════════
  tabPanel("🌍 Environment Explorer",
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="sec-hdr",style="background:#1A6B9A;","🎛️ Controls"),
                          div(class="sidebar-box",style="background:#E8F5E9;border-left:3px solid #52B788;",
                              strong(style="color:#2D6A4F;font-size:.85rem;","🌱 Soil"),
                              selectInput("soil_var",NULL,choices=soil_vars,selected="soilpH"),
                              sliderInput("soil_year_range","Filter Year",
                                          min=min(training$year),max=max(training$year),
                                          value=c(min(training$year),max(training$year)),step=1,sep=""),
                              p(style="font-size:0.75rem;color:#666;margin:0;","Mean yield per soil bin with error bars")),
                          div(class="sidebar-box",style="background:#E3F2FD;border-left:3px solid #1A6B9A;",
                              strong(style="color:#1A6B9A;font-size:.85rem;","🌦️ Weather"),
                              selectInput("weather_var",NULL,choices=weather_vars,selected="gdd_season"),
                              checkboxInput("weather_smooth","Add trend line",TRUE),
                              p(style="font-size:0.75rem;color:#666;margin:0;","Points colored by year")),
                          div(class="sidebar-box",style="background:#FFF3E0;border-left:3px solid #E07B39;",
                              strong(style="color:#E07B39;font-size:.85rem;","⛰️ Elevation"),
                              selectInput("elev_site_filter","Highlight Site",
                                          choices=c("All Sites"="all",sort(unique(site_elev$site))),selected="all"),
                              p(style="font-size:0.75rem;color:#666;margin:4px 0 0 0;","Mean yield per site vs elevation"))
             ),
             mainPanel(width=9,
                       fluidRow(
                         column(4,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#2D6A4F,#52B788);","🌱 Soil vs Yield (Binned Means)"),
                                      plotlyOutput("soil_plot",height="300px"),uiOutput("soil_corr_ui"))),
                         column(4,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#1A6B9A,#3A86FF);","🌦️ Weather vs Yield"),
                                      plotlyOutput("weather_plot",height="300px"))),
                         column(4,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#E07B39,#FFD166);","⛰️ Mean Yield by Site Elevation"),
                                      plotlyOutput("elev_plot",height="300px")))
                       ),
                       fluidRow(
                         column(12,div(class="plot-card",
                                       div(class="card-hdr",style="background:linear-gradient(135deg,#7B2D8B,#C77DFF);","🔥 Correlation Matrix — Click a cell to explore"),
                                       fluidRow(
                                         column(6,plotlyOutput("corr_matrix",height="380px")),
                                         column(6,plotlyOutput("corr_detail",height="380px"))
                                       )))
                       )
             )
           )
  ),
  
  # ══════════════════════════════════════════════════════════════
  # TAB 3 — Model Comparison
  # ══════════════════════════════════════════════════════════════
  tabPanel("⚔️ Model Comparison",
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="sec-hdr",style="background:#7B2D8B;","⚔️ Model Comparison"),
                          div(style="background:#E8F5E9;border:2px solid #2D6A4F;border-radius:8px;padding:12px;margin-bottom:10px;",
                              div(style="display:flex;align-items:center;gap:6px;margin-bottom:8px;",
                                  strong(style="color:#2D6A4F;font-size:.95rem;","🏆 Cubist"),
                                  span(class="winner-badge","WINNER")),
                              tags$table(style="width:100%;font-size:.82rem;",
                                         tags$tr(tags$td(style="color:#555;","RMSE:"),tags$td(strong(style="color:#2D6A4F;",c_test_rmse," Mg/ha"))),
                                         tags$tr(tags$td(style="color:#555;","R²:"),  tags$td(strong(style="color:#2D6A4F;",c_test_rsq))),
                                         tags$tr(tags$td(style="color:#555;","MAE:"), tags$td(strong(style="color:#2D6A4F;",c_test_mae," Mg/ha")))
                              )),
                          div(style="background:#FFF5F5;border:1px solid #dee2e6;border-radius:8px;padding:12px;margin-bottom:10px;",
                              strong(style="color:#555;font-size:.95rem;","XGBoost"),
                              tags$table(style="width:100%;font-size:.82rem;margin-top:6px;",
                                         tags$tr(tags$td(style="color:#555;","RMSE:"),tags$td(strong(style="color:#C1440E;",x_test_rmse," Mg/ha"))),
                                         tags$tr(tags$td(style="color:#555;","R²:"),  tags$td(strong(style="color:#C1440E;",x_test_rsq))),
                                         tags$tr(tags$td(style="color:#555;","MAE:"), tags$td(strong(style="color:#C1440E;",x_test_mae," Mg/ha")))
                              )),
                          hr(),
                          p(style="font-size:0.78rem;color:#777;",
                            "Both models evaluated on the same 30% holdout test set (set.seed 931), stratified by yield. Cubist outperforms XGBoost on all three metrics.")
             ),
             mainPanel(width=9,
                       fluidRow(
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#2D6A4F,#52B788);","🏆 Cubist — Predicted vs Actual"),
                                      plotlyOutput("cubist_pred_actual",height="320px"))),
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#888,#555);","XGBoost — Predicted vs Actual"),
                                      plotlyOutput("xgb_pred_actual",height="320px")))
                       ),
                       fluidRow(
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#2D6A4F,#52B788);","🏆 Cubist — RMSE by Site"),
                                      plotlyOutput("cubist_resid_site",height="300px"))),
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#888,#555);","XGBoost — RMSE by Site"),
                                      plotlyOutput("xgb_resid_site",height="300px")))
                       ),
                       fluidRow(
                         column(12,div(class="plot-card",
                                       div(class="card-hdr",style="background:linear-gradient(135deg,#7B2D8B,#C77DFF);","📅 RMSE by Year — Cubist vs XGBoost"),
                                       plotlyOutput("rmse_year_comparison",height="280px")))
                       )
             )
           )
  ),
  
  # ══════════════════════════════════════════════════════════════
  # TAB 4 — Variable Importance
  # ══════════════════════════════════════════════════════════════
  tabPanel("🔍 Variable Importance",
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="sec-hdr",style="background:#D4A017;","🔍 Controls"),
                          radioButtons("imp_model","Select Model",
                                       choices=c("🏆 Cubist (Winner)"="cubist","XGBoost"="xgb"),
                                       selected="cubist"),
                          sliderInput("n_vars","Show Top N Variables",min=5,max=20,value=15,step=1),
                          hr(),
                          h6(style="color:#555;font-weight:700;","Variable Groups"),
                          div(style="font-size:0.82rem;",
                              div(style="display:flex;align-items:center;gap:6px;margin-bottom:4px;",
                                  div(style="width:12px;height:12px;border-radius:2px;background:#1A6B9A;"),"Weather"),
                              div(style="display:flex;align-items:center;gap:6px;margin-bottom:4px;",
                                  div(style="width:12px;height:12px;border-radius:2px;background:#7B3F00;"),"Soil"),
                              div(style="display:flex;align-items:center;gap:6px;margin-bottom:4px;",
                                  div(style="width:12px;height:12px;border-radius:2px;background:#2D6A4F;"),"Location"),
                              div(style="display:flex;align-items:center;gap:6px;margin-bottom:4px;",
                                  div(style="width:12px;height:12px;border-radius:2px;background:#D4A017;"),"Management"),
                              div(style="display:flex;align-items:center;gap:6px;",
                                  div(style="width:12px;height:12px;border-radius:2px;background:#7B2D8B;"),"Trial Design")
                          )
             ),
             mainPanel(width=9,
                       div(class="plot-card",
                           div(class="card-hdr",style="background:linear-gradient(135deg,#D4A017,#FFD166);","🔍 Variable Importance"),
                           plotlyOutput("vip_plot",height="500px"))
             )
           )
  ),
  
  # ══════════════════════════════════════════════════════════════
  # TAB 5 — Final Model Performance (Cubist)
  # ══════════════════════════════════════════════════════════════
  tabPanel("🏆 Final Model",
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="sec-hdr",style="background:#2D6A4F;","🏆 Cubist — Final Model"),
                          p(style="font-size:0.82rem;color:#555;",
                            "Cubist selected as final model based on superior performance on all three metrics vs XGBoost."),
                          hr(),
                          h6(style="color:#2D6A4F;font-weight:700;","🎯 Holdout Test Set (30%)"),
                          div(class="metric-box",style="background:linear-gradient(135deg,#2D6A4F,#52B788);color:white;",
                              div(class="metric-lbl","RMSE (Mg/ha)"),div(class="metric-val",c_test_rmse)),
                          div(class="metric-box",style="background:linear-gradient(135deg,#1A6B9A,#3A86FF);color:white;",
                              div(class="metric-lbl","R²"),div(class="metric-val",c_test_rsq)),
                          div(class="metric-box",style="background:linear-gradient(135deg,#D4A017,#FFD166);color:white;",
                              div(class="metric-lbl","MAE (Mg/ha)"),div(class="metric-val",c_test_mae)),
                          hr(),
                          h6(style="color:#2D6A4F;font-weight:700;","🏋️ Training Set"),
                          div(class="metric-box",style="background:#F0F7F0;border:1px solid #B7E4C7;",
                              div(class="metric-lbl",style="color:#555;","RMSE"),
                              div(class="metric-val",style="color:#2D6A4F;",c_train_rmse)),
                          div(class="metric-box",style="background:#F0F7F0;border:1px solid #B7E4C7;",
                              div(class="metric-lbl",style="color:#555;","R²"),
                              div(class="metric-val",style="color:#2D6A4F;",c_train_rsq))
             ),
             mainPanel(width=9,
                       fluidRow(
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#2D6A4F,#52B788);","🎯 Predicted vs Actual — Holdout Test Set"),
                                      plotlyOutput("final_pred_actual",height="360px"))),
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#C1440E,#E07B39);","📉 Residual Distribution"),
                                      plotlyOutput("final_resid_dist",height="360px")))
                       ),
                       fluidRow(
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#1A6B9A,#3A86FF);","📍 RMSE by Site"),
                                      plotlyOutput("final_resid_site",height="300px"))),
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#7B2D8B,#C77DFF);","📅 RMSE by Year"),
                                      plotlyOutput("final_resid_year",height="300px")))
                       )
             )
           )
  ),
  
  # ══════════════════════════════════════════════════════════════
  # TAB 6 — 2024 Predictions
  # ══════════════════════════════════════════════════════════════
  tabPanel("🔮 2024 Predictions",
           sidebarLayout(
             sidebarPanel(width=3,
                          div(class="sec-hdr",style="background:#C1440E;","🔮 2024 Controls"),
                          selectInput("pred_site","Filter by Site",
                                      choices=c("All Sites"="all",sort(unique(cubist_preds_2024$site))),selected="all"),
                          hr(),
                          h6(style="color:#555;font-weight:700;","📋 Summary"),
                          uiOutput("pred_summary_ui"),
                          hr(),
                          p(style="font-size:0.78rem;color:#777;",
                            "Predictions from Cubist model (winner), refitted on full 2014–2023 training data."),
                          hr(),
                          div(style="background:#FFF3E0;border-left:3px solid #E07B39;padding:8px;border-radius:4px;",
                              strong(style="color:#E07B39;font-size:.8rem;","⚠️ Grain Moisture Note"),
                              p(style="font-size:0.75rem;color:#666;margin:4px 0 0 0;",
                                "Grain moisture was the #2 most important predictor but is unavailable for 2024 
       as it is measured at harvest. Final model was refit excluding grain moisture."))
                          
             ),
             mainPanel(width=9,
                       fluidRow(
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#C1440E,#E07B39);","🔮 2024 Predicted Yield Distribution"),
                                      plotlyOutput("pred_dist",height="300px"))),
                         column(6,div(class="plot-card",
                                      div(class="card-hdr",style="background:linear-gradient(135deg,#2D6A4F,#52B788);","📍 Mean Predicted Yield by Site"),
                                      plotlyOutput("pred_by_site",height="300px")))
                       ),
                       fluidRow(
                         column(12,div(class="plot-card",
                                       div(class="card-hdr",style="background:linear-gradient(135deg,#1A6B9A,#3A86FF);","📋 2024 Predictions Table"),
                                       DTOutput("pred_table")))
                       )
             )
           )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Tab 1 ────────────────────────────────────────────────────────────────────
  tab1_data <- reactive({
    df <- training %>% filter(year>=input$year_range[1],year<=input$year_range[2])
    if (input$site_filter!="all") df <- df %>% filter(site==input$site_filter)
    df
  })
  
  output$summary_stats_ui <- renderUI({
    df <- tab1_data()
    tags$table(style="width:100%;font-size:.82rem;",
               tags$tr(tags$td(style="color:#666;","Mean:"),tags$td(strong(style="color:#2D6A4F;",round(mean(df$yield_mg_ha),2)," Mg/ha"))),
               tags$tr(tags$td(style="color:#666;","Median:"),tags$td(strong(style="color:#2D6A4F;",round(median(df$yield_mg_ha),2)," Mg/ha"))),
               tags$tr(tags$td(style="color:#666;","Min:"),tags$td(strong(style="color:#C1440E;",round(min(df$yield_mg_ha),2)," Mg/ha"))),
               tags$tr(tags$td(style="color:#666;","Max:"),tags$td(strong(style="color:#1A6B9A;",round(max(df$yield_mg_ha),2)," Mg/ha"))),
               tags$tr(tags$td(style="color:#666;","N:"),tags$td(strong(scales::comma(nrow(df)))))
    )
  })
  
  output$yield_hist <- renderPlotly({
    plot_ly(tab1_data(),x=~yield_mg_ha,type="histogram",nbinsx=50,
            marker=list(color=COL_TEAL,line=list(color=COL_GREEN,width=0.5))) %>%
      layout(xaxis=list(title="Yield (Mg/ha)"),yaxis=list(title="Count"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$yield_year <- renderPlotly({
    df <- tab1_data()
    overall <- df %>% group_by(year) %>% summarise(mean_yield=mean(yield_mg_ha),.groups="drop")
    p <- plot_ly()
    if (isTRUE(input$show_site_trends)) {
      st <- df %>% group_by(year,site) %>% summarise(mean_yield=mean(yield_mg_ha),.groups="drop")
      for (i in seq_along(unique(st$site))) {
        s <- unique(st$site)[i]; sd <- st %>% filter(site==s)
        p <- p %>% add_trace(data=sd,x=~year,y=~mean_yield,type="scatter",mode="lines",
                             line=list(color=SITE_COLORS[(i-1)%%length(SITE_COLORS)+1],width=1),
                             opacity=0.4,showlegend=FALSE,hoverinfo="text",
                             text=~paste(s,"| Year:",year,"| Yield:",round(mean_yield,2)))
      }
    }
    p %>% add_trace(data=overall,x=~year,y=~mean_yield,type="scatter",mode="lines+markers",
                    line=list(color=COL_BLUE,width=3),
                    marker=list(color=COL_GOLD,size=10,line=list(color="white",width=2)),
                    name="Overall Mean",hoverinfo="text",
                    text=~paste("Year:",year,"<br>Mean:",round(mean_yield,2),"Mg/ha")) %>%
      layout(xaxis=list(title="Year"),yaxis=list(title="Mean Yield (Mg/ha)"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$yield_site <- renderPlotly({
    req(input$site_select)
    df <- tab1_data() %>% filter(site%in%input$site_select); req(nrow(df)>0)
    site_order <- df %>% group_by(site) %>% summarise(med=median(yield_mg_ha)) %>% arrange(med) %>% pull(site)
    n <- length(unique(df$site))
    df %>% mutate(site=factor(site,levels=site_order)) %>%
      plot_ly(x=~yield_mg_ha,y=~site,type="box",color=~site,
              colors=rep(SITE_COLORS,length.out=n),showlegend=FALSE) %>%
      layout(xaxis=list(title="Yield (Mg/ha)"),yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$yield_crop <- renderPlotly({
    df <- tab1_data()
    crop_order <- df %>% group_by(previous_crop) %>% summarise(med=median(yield_mg_ha)) %>% arrange(med) %>% pull(previous_crop)
    df <- df %>% mutate(previous_crop=factor(as.character(previous_crop),levels=as.character(crop_order)))
    n <- n_distinct(df$previous_crop); cols <- rep(SITE_COLORS,length.out=n)
    if (input$crop_plot_type=="box") {
      plot_ly(df,x=~previous_crop,y=~yield_mg_ha,type="box",color=~previous_crop,colors=cols,showlegend=FALSE) %>%
        layout(xaxis=list(title="Previous Crop"),yaxis=list(title="Yield (Mg/ha)"),
               paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
    } else {
      plot_ly(df,x=~previous_crop,y=~yield_mg_ha,type="violin",color=~previous_crop,colors=cols,showlegend=FALSE,
              box=list(visible=TRUE),meanline=list(visible=TRUE)) %>%
        layout(xaxis=list(title="Previous Crop"),yaxis=list(title="Yield (Mg/ha)"),
               paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
    }
  })
  
  # ── Tab 2 ────────────────────────────────────────────────────────────────────
  output$soil_plot <- renderPlotly({
    x_var <- input$soil_var; x_lab <- names(soil_vars)[soil_vars==x_var]
    df <- training %>% filter(year>=input$soil_year_range[1],year<=input$soil_year_range[2],!is.na(.data[[x_var]]))
    df <- df %>% mutate(bin=cut(.data[[x_var]],breaks=10,include.lowest=TRUE))
    binned <- df %>% group_by(bin) %>%
      summarise(mean_yield=mean(yield_mg_ha),se=sd(yield_mg_ha)/sqrt(n()),mid=mean(.data[[x_var]]),.groups="drop") %>% filter(!is.na(bin))
    corr <- round(cor(df[[x_var]],df$yield_mg_ha,use="complete.obs"),3)
    plot_ly(binned,x=~mid,y=~mean_yield,type="scatter",mode="markers+lines",
            error_y=list(type="data",array=~se,visible=TRUE,color=COL_GREEN),
            marker=list(color=COL_TEAL,size=8),line=list(color=COL_GREEN,width=2),
            hoverinfo="text",text=~paste(x_lab,":",round(mid,2),"<br>Mean Yield:",round(mean_yield,2),"<br>SE:",round(se,3))) %>%
      layout(xaxis=list(title=x_lab),yaxis=list(title="Mean Yield (Mg/ha)"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             annotations=list(list(x=0.05,y=0.95,xref="paper",yref="paper",
                                   text=paste("r =",corr),showarrow=FALSE,font=list(size=13,color=COL_GREEN))))
  })
  
  output$soil_corr_ui <- renderUI({
    df <- training %>% filter(year>=input$soil_year_range[1],year<=input$soil_year_range[2])
    corr <- round(cor(df[[input$soil_var]],df$yield_mg_ha,use="complete.obs"),3)
    div(style="text-align:center;padding:3px;font-size:.8rem;",
        span(style=paste0("color:",COL_GREEN,";font-weight:600;"),paste("Pearson r =",corr)),
        span(style="color:#888;margin-left:6px;",paste("n =",scales::comma(sum(!is.na(df[[input$soil_var]]))))))
  })
  
  output$weather_plot <- renderPlotly({
    x_var <- input$weather_var; x_lab <- names(weather_vars)[weather_vars==x_var]
    df <- training %>% filter(!is.na(.data[[x_var]])) %>% slice_sample(n=min(15000,nrow(.)))
    p <- plot_ly(df,x=~get(x_var),y=~yield_mg_ha,color=~as.factor(year),colors=YEAR_COLORS,
                 type="scatter",mode="markers",marker=list(size=3,opacity=0.5),
                 hoverinfo="text",text=~paste(x_lab,":",round(get(x_var),2),"<br>Yield:",round(yield_mg_ha,2),"<br>Year:",year)) %>%
      layout(xaxis=list(title=x_lab),yaxis=list(title="Yield (Mg/ha)"),
             legend=list(title=list(text="Year")),paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
    if (input$weather_smooth) {
      fit <- tryCatch(loess(as.formula(paste("yield_mg_ha ~",x_var)),data=df,span=0.4),error=function(e) NULL)
      if (!is.null(fit)) { ord <- order(df[[x_var]])
      p <- p %>% add_lines(x=df[[x_var]][ord],y=fitted(fit)[ord],line=list(color=COL_RED,width=2.5),name="Trend",showlegend=FALSE,inherit=FALSE) }
    }
    p
  })
  
  output$elev_plot <- renderPlotly({
    df_elev <- site_elev %>%
      mutate(highlight=ifelse(input$elev_site_filter=="all"|site==input$elev_site_filter,TRUE,FALSE),
             opacity=ifelse(highlight,1,0.2),size=ifelse(highlight,14,8))
    plot_ly(df_elev,x=~elevation,y=~mean_yield,type="scatter",mode="markers",
            marker=list(color=~mean_yield,colorscale=list(c(0,"#C1440E"),c(0.5,"#FFD166"),c(1,"#2D6A4F")),
                        showscale=TRUE,colorbar=list(title="Mean<br>Yield"),size=~size,opacity=~opacity,
                        line=list(color="white",width=1.5)),
            hoverinfo="text",hovertext=~paste0("<b>",site,"</b>","<br>Elevation:",round(elevation,1)," m","<br>Mean Yield:",round(mean_yield,2)," Mg/ha")) %>%
      add_annotations(x=~elevation,y=~mean_yield,text=~ifelse(input$elev_site_filter!="all"&highlight,site,""),
                      xref="x",yref="y",showarrow=FALSE,xanchor="left",yanchor="bottom",
                      font=list(size=8,color="#333"),xshift=5) %>%
      layout(xaxis=list(title="Site Elevation (m)"),yaxis=list(title="Mean Yield (Mg/ha)"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$corr_matrix <- renderPlotly({
    plot_ly(x=corr_labels,y=rev(corr_labels),z=corr_mat[nrow(corr_mat):1,],type="heatmap",
            colorscale=list(c(0,COL_RED),c(0.5,"#FAFAF7"),c(1,COL_GREEN)),zmin=-1,zmax=1,
            hovertemplate="%{y} vs %{x}<br>r = %{z:.2f}<extra></extra>",source="corr_click") %>%
      layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             xaxis=list(tickfont=list(size=9),tickangle=-35),yaxis=list(tickfont=list(size=9))) %>%
      event_register("plotly_click")
  })
  
  output$corr_detail <- renderPlotly({
    click <- event_data("plotly_click",source="corr_click")
    if (is.null(click)) return(plotly_empty() %>% layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
                                                         annotations=list(list(text="👆 Click a cell to explore",showarrow=FALSE,font=list(size=14,color=COL_TEAL),xref="paper",yref="paper",x=0.5,y=0.5))))
    x_idx <- which(corr_labels==click$x); y_idx <- which(corr_labels==click$y)
    if (length(x_idx)==0||length(y_idx)==0) return(plotly_empty())
    x_col <- corr_vars[x_idx]; y_col <- corr_vars[y_idx]
    corr_val <- round(cor(training[[x_col]],training[[y_col]],use="complete.obs"),3)
    df_c <- training %>% filter(!is.na(.data[[x_col]]),!is.na(.data[[y_col]])) %>% slice_sample(n=min(10000,nrow(.)))
    fit  <- tryCatch(loess(as.formula(paste(y_col,"~",x_col)),data=df_c,span=0.4),error=function(e) NULL)
    ord  <- order(df_c[[x_col]])
    p <- plot_ly(df_c,x=~get(x_col),y=~get(y_col),type="scatter",mode="markers",
                 marker=list(color=COL_TEAL,size=3,opacity=0.4),hoverinfo="text",
                 text=~paste(click$x,":",round(get(x_col),2),"<br>",click$y,":",round(get(y_col),2))) %>%
      layout(title=list(text=paste0(click$x," vs ",click$y," | r = ",corr_val),font=list(size=12,color=COL_GREEN)),
             xaxis=list(title=click$x),yaxis=list(title=click$y),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
    if (!is.null(fit)) p <- p %>% add_lines(x=df_c[[x_col]][ord],y=fitted(fit)[ord],line=list(color=COL_RED,width=2),showlegend=FALSE,inherit=FALSE)
    p
  })
  
  # ── Helper: pred vs actual plot ───────────────────────────────────────────────
  pred_actual_plot <- function(preds, rmse, rsq, color) {
    plot_ly(preds,x=~yield_mg_ha,y=~.pred,type="scatter",mode="markers",
            marker=list(color=color,size=4,opacity=0.35),
            hoverinfo="text",
            text=~paste("Actual:",round(yield_mg_ha,2),"<br>Predicted:",round(.pred,2),"<br>Site:",site,"| Year:",year)) %>%
      add_lines(x=c(0,25),y=c(0,25),line=list(color=COL_RED,width=2,dash="dash"),showlegend=FALSE,inherit=FALSE) %>%
      layout(xaxis=list(title="Actual Yield (Mg/ha)",range=c(0,25)),
             yaxis=list(title="Predicted Yield (Mg/ha)",range=c(0,25)),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             annotations=list(list(x=0.05,y=0.95,xref="paper",yref="paper",showarrow=FALSE,
                                   text=paste0("RMSE = ",rmse,"<br>R² = ",rsq),
                                   font=list(size=12,color=color),
                                   bgcolor="rgba(255,255,255,0.8)",bordercolor=color,borderwidth=1)))
  }
  
  # ── Tab 3 — Model Comparison ──────────────────────────────────────────────────
  output$cubist_pred_actual <- renderPlotly({
    pred_actual_plot(cubist_test_preds, c_test_rmse, c_test_rsq, COL_GREEN)
  })
  
  output$xgb_pred_actual <- renderPlotly({
    pred_actual_plot(xgb_test_preds, x_test_rmse, x_test_rsq, "#888888")
  })
  
  output$cubist_resid_site <- renderPlotly({
    df <- cubist_resid_site %>% arrange(rmse)
    plot_ly(df,x=~rmse,y=~reorder(site,rmse),type="bar",orientation="h",
            marker=list(color=COL_TEAL),hoverinfo="text",
            text=~paste("Site:",site,"<br>RMSE:",round(rmse,3))) %>%
      layout(xaxis=list(title="RMSE (Mg/ha)"),yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$xgb_resid_site <- renderPlotly({
    df <- xgb_resid_site %>% arrange(rmse)
    plot_ly(df,x=~rmse,y=~reorder(site,rmse),type="bar",orientation="h",
            marker=list(color="#888888"),hoverinfo="text",
            text=~paste("Site:",site,"<br>RMSE:",round(rmse,3))) %>%
      layout(xaxis=list(title="RMSE (Mg/ha)"),yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$rmse_year_comparison <- renderPlotly({
    df_c <- cubist_resid_year %>% mutate(model="Cubist")
    df_x <- xgb_resid_year    %>% mutate(model="XGBoost")
    df   <- bind_rows(df_c, df_x)
    plot_ly(df,x=~year,y=~rmse,color=~model,
            colors=c("Cubist"=COL_GREEN,"XGBoost"="#888888"),
            type="scatter",mode="lines+markers",
            marker=list(size=8),line=list(width=2),
            hoverinfo="text",
            text=~paste(model,"| Year:",year,"<br>RMSE:",round(rmse,3))) %>%
      layout(xaxis=list(title="Year"),yaxis=list(title="RMSE (Mg/ha)"),
             legend=list(title=list(text="Model")),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  # ── Tab 4 — Variable Importance ───────────────────────────────────────────────
  output$vip_plot <- renderPlotly({
    df <- if (input$imp_model=="cubist") cubist_importance else xgb_importance_clean
    df <- df %>% slice_head(n=input$n_vars) %>% arrange(Importance)
    plot_ly(df,x=~Importance,y=~reorder(Variable,Importance),type="bar",orientation="h",
            marker=list(color=~group_color,line=list(color="white",width=0.5)),
            hoverinfo="text",
            text=~paste("Variable:",Variable,"<br>Group:",Group,"<br>Importance:",round(Importance,3))) %>%
      layout(xaxis=list(title="Importance Score"),yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  # ── Tab 5 — Final Model ───────────────────────────────────────────────────────
  output$final_pred_actual <- renderPlotly({
    pred_actual_plot(cubist_test_preds, c_test_rmse, c_test_rsq, COL_GREEN)
  })
  
  output$final_resid_dist <- renderPlotly({
    resid_df <- cubist_test_preds %>% mutate(residual=yield_mg_ha-.pred)
    plot_ly(resid_df,x=~residual,type="histogram",nbinsx=50,
            marker=list(color=COL_ORANGE,line=list(color=COL_RED,width=0.5))) %>%
      add_lines(x=c(0,0),y=c(0,5000),line=list(color=COL_RED,width=2,dash="dash"),showlegend=FALSE,inherit=FALSE) %>%
      layout(xaxis=list(title="Residual (Actual − Predicted)"),yaxis=list(title="Count"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$final_resid_site <- renderPlotly({
    df <- cubist_resid_site %>% arrange(desc(rmse))
    plot_ly(df,x=~rmse,y=~reorder(site,rmse),type="bar",orientation="h",
            marker=list(color=~rmse,colorscale=list(c(0,COL_TEAL),c(1,COL_RED)),showscale=FALSE),
            hoverinfo="text",text=~paste("Site:",site,"<br>RMSE:",round(rmse,3),"<br>Mean Residual:",round(mean_residual,3))) %>%
      layout(xaxis=list(title="RMSE (Mg/ha)"),yaxis=list(title=""),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$final_resid_year <- renderPlotly({
    plot_ly(cubist_resid_year,x=~year,y=~rmse,type="scatter",mode="lines+markers",
            line=list(color=COL_PURPLE,width=2),marker=list(color=COL_PURPLE,size=8),
            hoverinfo="text",text=~paste("Year:",year,"<br>RMSE:",round(rmse,3),"<br>Mean Residual:",round(mean_residual,3))) %>%
      layout(xaxis=list(title="Year"),yaxis=list(title="RMSE (Mg/ha)"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  # ── Tab 6 — 2024 Predictions ──────────────────────────────────────────────────
  pred_data <- reactive({
    if (input$pred_site=="all") cubist_preds_2024
    else cubist_preds_2024 %>% filter(site==input$pred_site)
  })
  
  output$pred_summary_ui <- renderUI({
    df <- pred_data()
    tags$table(style="width:100%;font-size:.82rem;",
               tags$tr(tags$td(style="color:#666;","Hybrids:"),tags$td(strong(scales::comma(nrow(df))))),
               tags$tr(tags$td(style="color:#666;","Mean:"),tags$td(strong(style="color:#2D6A4F;",round(mean(df$.pred),2)," Mg/ha"))),
               tags$tr(tags$td(style="color:#666;","Min:"),tags$td(strong(style="color:#C1440E;",round(min(df$.pred),2)," Mg/ha"))),
               tags$tr(tags$td(style="color:#666;","Max:"),tags$td(strong(style="color:#1A6B9A;",round(max(df$.pred),2)," Mg/ha")))
    )
  })
  
  output$pred_dist <- renderPlotly({
    plot_ly(pred_data(),x=~.pred,type="histogram",nbinsx=40,
            marker=list(color=COL_ORANGE,line=list(color=COL_RED,width=0.5))) %>%
      layout(xaxis=list(title="Predicted Yield (Mg/ha)"),yaxis=list(title="Count"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$pred_by_site <- renderPlotly({
    df <- cubist_preds_2024 %>% group_by(site) %>% summarise(mean_pred=mean(.pred),.groups="drop") %>% arrange(desc(mean_pred))
    plot_ly(df,x=~reorder(site,mean_pred),y=~mean_pred,type="bar",
            marker=list(color=~mean_pred,colorscale=list(c(0,"#C1440E"),c(0.5,"#FFD166"),c(1,"#2D6A4F")),showscale=FALSE),
            hoverinfo="text",text=~paste("Site:",site,"<br>Mean Pred:",round(mean_pred,2),"Mg/ha")) %>%
      layout(xaxis=list(title="Site",tickangle=-45),yaxis=list(title="Mean Predicted Yield (Mg/ha)"),
             paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$pred_table <- renderDT({
    pred_data() %>% select(site,hybrid,year,.pred) %>%
      rename(Site=site,Hybrid=hybrid,Year=year,`Predicted Yield (Mg/ha)`=.pred) %>%
      mutate(`Predicted Yield (Mg/ha)`=round(`Predicted Yield (Mg/ha)`,3)) %>%
      datatable(filter="top",rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))
  })
}

shinyApp(ui=ui, server=server)