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

## Data Merging

1. I pulled Daymet weather for all 66 site-years across both training and testing using the longitude and latitude from the meta file. 
This gave daily records for each site and year which I then summarised into eight site-year level variables, growing season GDD, total precipitation, 
mean maximum and minimum temperature, mean solar radiation, mean vapor pressure, and heat stress days. 
Elevation was downloaded once per site and joined on site alone since it does not change over time.

2. I then merged all sources into a single training and testing dataset. Trait, meta, soil, weather, and elevation were joined sequentially 
using site and year as keys. A few cleaning steps were needed along the way, the meta file had three duplicate site-years which I removed before joining, 
and the soil file had site names stored with the year appended, which I stripped before the join.

3. The final training dataset has 164,477 rows and 24 columns. The final testing dataset has 10,057 rows and 19 columns, 
with yield as NA since that is what the model will predict.

4. One issue I noted is that soil data is partially missing in training, 2014 has no soil data at all, 
and coverage varies across sites in later years. This is a data availability issue rather than a join error. 
Since XGBoost handles missing values natively, I left these as is for now and will revisit during feature engineering if needed.

## Data Cleaning

1. Identified sites missing coordinates; recovered by computing site-level median from other years where coordinates existed.

2. Sub-trial variants IAH1a/b/c and TXH1-Dry/Early/Late assigned coordinates from parent sites IAH1 and TXH1. 
Rows with no recoverable coordinates (TXH4) dropped.

3. Site GEH1 (Germany) dropped — outside Daymet coverage. All remaining sites confirmed within continental US bounds.
4. Re-pulled Daymet weather for 29 site-years that were missing weather due to previously missing coordinates.

5. Soil variables (soilpH, om_pct, soilk_ppm, soilp_ppm) imputed using site-level medians; global median used as fallback for sites with no soil data in any year.

6. Elevation for sub-trial variants borrowed from parent sites IAH1 and TXH1 using median elevation.

7. previous_crop standardized — lowercased, trimmed, collapsed into clean categories (soybean, corn, wheat, sorghum, cotton, peanut, sugar beet, fallow, rye, other). 
Missing values assigned explicit "unknown" factor level.

8. True within-plot duplicates (same year + site + hybrid + replicate + block) resolved by averaging yield_mg_ha and grain_moisture. Replicate and block structure preserved.

9. Range validation confirmed all numeric variables within agronomically plausible bounds — no rows filtered.

10. Hybrid and site name columns checked for whitespace issues — none found.

11. Dates parsed from %m/%d/%y format; zero invalid date pairs found.

12. Column types coerced — year to integer, replicate and block to factor, previous_crop to factor, hybrid and site to character.

## EDA Section

## Model-1 XGBoost


## Model-2 Cubist

**Model Selection**

1. Originally planned to use Support Vector Machine (SVM) with RBF kernel as the second model.

2. SVM was abandoned after running for 12+ hours on a 100GB HPC cluster node and completing only 11 of 250 tuning fits, computationally infeasible within the project timeline.

3. Switched to Cubist, a rule-based regression model not covered in class, available via the rules package in tidymodels.

**What is Cubist?**

4. Cubist builds a set of if-then rules, each with a linear regression model at the leaf node.

5. It combines the interpretability of decision trees with the predictive power of linear models.

6. At prediction time, it optionally uses nearest-neighbor adjustments to refine predictions.

7. Ensembling is done through committees, each successive committee corrects errors from the previous one, similar in spirit to boosting but fundamentally different from XGBoost's gradient-based approach.

**Data Preparation**

8. Loaded training_fe.csv with all engineered features.

9. Coerced previous_crop to factor and year to integer; removed rows with any remaining NAs. Total rows used for modeling: 150,509.

**Data Split**

10. 70/30 stratified split by yield_mg_ha using set.seed(931) — same split as XGBoost for fair comparison. Train set: ~105,353 rows; Test set: ~45,156 rows.

**Preprocessing**

11. Removed date_planted and date_harvested, replaced by engineered DOY and season length features.

12. All hybrid levels retained, step_zv() applied to remove zero-variance predictors.

13. No dummy encoding or normalization applied, Cubist handles categorical variables natively.

**Hyperparameter Tuning**

14. Tuned two parameters: committees (range 1–20) and neighbors (range 0–9).

15. Regular grid search with 5 levels each = 25 combinations total.

16. 10-fold cross validation stratified by yield_mg_ha.

17. Best model selected using RMSE as the selection metric.

18. Tuning run on full 70% train_data (~105,353 rows) with parallelization across all available cores minus one.

**Model Fitting**

19. Best hyperparameters selected using select_best() on RMSE.

20. Evaluation model fit on full 70% train_data with best parameters for honest holdout evaluation.

21. Final model refit on complete 2014–2023 data (150,509 rows) for generating 2024 yield predictions.

**Results**

22. Tuning identified the best parameters as committees = 20 and neighbors = 9 based on lowest CV RMSE.

23. On the 30% holdout test set, Cubist achieved RMSE of 1.86 Mg/ha, R² of 0.638, and MAE of 1.42 Mg/ha.

24. On the training set, Cubist achieved RMSE of 1.72 Mg/ha, R² of 0.693, and MAE of 1.30 Mg/ha.

25. The small train-test gap (RMSE difference of 0.14 Mg/ha, R² difference of 0.055) indicates the model generalizes well with minimal overfitting.

26. Cubist explains approximately 64% of yield variance on unseen data across all sites, 10 years, and full hybrid diversity.

## Model Selection


## Final Yield Prediction on 2024 data


## R-Shiny dashboard


