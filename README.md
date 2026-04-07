# Final_Project

## Overview
This project uses machine learning to predict corn hybrid yield (bu/ac) for a 2024 test dataset,
trained on 10 years of corn variety trial data (2014–2023). The dataset contains over 164,000 
observations spanning 45 sites across the USA (270 site-years) and more than 5,000 corn hybrids. 

In addition to the provided trait, soil, and metadata, we incorporate open-source weather and 
elevation data to improve model performance. Results are communicated through an interactive 
Shiny dashboard.

## Team 
1. Elizabeth Abati
2. Susmitha Kalli 

## Project Goals

1. Set up the GitHub repository, folder structure, and collaborator access :Susmitha Kalli
2. Retrieve open-source data useful for yield prediction (weather, elevation) : Susmitha Kalli
3. Merge all data sources — trait, soil, meta, and open-source — into a unified dataset: Elizabeth Abati
4. Perform data wrangling, assess missingness patterns, engineer features, and check assumptions: Susmitha Kalli, Elizabeth Abati
5. Conduct exploratory data analysis (EDA) to guide model selection: Susmitha Kalli, Elizabeth Abati
6. Train and tune two ML models: XGBoost and one additional model of our choice
7. Compare model performance and select the best model based on RMSE and R²
8. Generate yield predictions for the 2024 test dataset using the final model
9. Build and publish an interactive Shiny dashboard to communicate results
10. Prepare a single presentation slide summarizing our approach and findings
11. Submit: GitHub repository URL, completed testing_submission.csv, Shiny app URL, 
    and presentation slide by April 30, 2026
    

## Initial data inspection

1. We started by pulling two open-source datasets, weather from Daymet and elevation from USGS using R packages. 
2. Daymet gave us daily weather readings for any location in the US, and USGS gave us the elevation in meters for any coordinate we provide.
3. When we downloaded the weather data for a sample corn belt location, we got 8 daily variables back,
temperature (max and min), precipitation, solar radiation, vapor pressure, snow water equivalent, and day length. 
4. When we downloaded elevation, we got a single clean number, the height of that location above sea level in meters.
5. We then loaded our actual project files. The training data has 164,000 hybrid observations across 43 sites and 10 years, with yield recorded in mg/ha. 
The meta file has longitude and latitude for every site-year, which is exactly what we need to pull weather and elevation for each location.
6. When we checked whether everything could be connected, we confirmed that site and year appear in all three training files,
trait, soil, and meta, so they all join together without any issues. The meta file's coordinates are the bridge to the external data.

So here is how we plan to combine everything. We use site and year to link trait, soil, and meta together. 
Then we use the longitude and latitude from meta to pull Daymet weather for each site across all 10 years. 
Elevation joins on site alone since it never changes over time. We believe this combined dataset will improve yield prediction, 
because weather explains why yield goes up or down from year to year at the same site, and elevation explains why some sites consistently perform 
differently from others even with the same hybrid. Together with soil properties and hybrid identity, this gives the model a much richer picture 
of what drives yield variation across 43 sites and 10 years.


