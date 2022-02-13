# TwitterCovidDietScripts
 R scripts and data for running analysis for the manuscript:<br />
 "Diet During the COVID-19 Pandemic: An Analysis of Twitter data"
 
 Manuscript Authors: Mark A. Hernandez, MPH,  Shagun Modi, MPH,  Kanisha Mittal, MPH,  Pallavi Dwivedi, MPH,  Quynh C. Nguyen, PhD,  Nina L. Cesare, PhD and Elaine O. Nsoesie, PhD
 
 Corresponding author: Mark A. Hernandez, mark.hernandez@ll.mit.edu, markhernandez512@gmail.com


# Scripts

**run_tweet_classifier.R** <br />
Imports (1) raw tweet data and (2) subsample of labeled tweets to indicate food consumption. 
Trains Random Forest model to identify tweets indicating food consumption. Runs model across the all tweet data and outputs labels identifying food consumption tweets.

**importStateCountyVar.R** <br />
Imports and merges county-level covariates from (1) Google Mobility (2) American Community Survey and (3) Community Business Patterns

**run_tweet_stats.R** <br />
Calculates changes in shares of healthy food tweets, fast food tweets, and alcohol tweets nationally and by state. Also calculates county-level shares in food tweet categories during pre-pandemic period.

**stateAnalysis.R** <br />
Plots changes in state food tweets.

**countyAnalysis.R** <br />
Runs logistic regression models for each primary outcome at the county level and plots results.

**tweetClassifier.R** <br />
Evaluates the performance of three separate models in identifying food consumption tweets: Random Forest, linear SVM, and logistic regression.

**twitter_food_functions.R** <br />
Contains functions used by multiple R scripts.



# Data

We provide analytic datasets containing tweet IDs, food/alcohol tweet coding, and area-level measures. 
The authors also provide data that were used to generate the Figures 1, 2, and 3 in the manuscript.<br />
Original tweet text reported in this study cannot be deposited in a public repository because of privacy concerns and Twitter data sharing policy. 
However, tweet IDs can be used to retrieve original tweet data via Twitter’s public application programming interface (API). 


## Analytic Dataset

We provide two datasets with the food tweet data used for the paper's analyses.

**Data/ProcessedData/df_forAnalysis.csv** <br />
Tweet-level data used to (1) calculate changes in food tweets before and during the pandemic, and (2) fit the regression model quantifying associations between county-level characteristics and changes in food tweets.
Incudes tweet IDs, food labels, county code, state, and period. 
Excludes original tweet text and metadata -- as per Twitter data policy.

**Data/Finallabel_data.csv** <br />
Table with the 2,878 labels tweets that were used to train the classifier that identifies tweets indicating food consumption. 
Incudes tweet IDs and manual label to indicate tweets that indicate food consumption (1) and those that do not (0). <br />
Excludes original tweet text (can be retrieved using the Twitter API)

## Imported Data

We provide datasets that we imported from public sources (all referenced below) including the Community Business Patterns (CBP), Google Mobility, and the American Community Survey (ACS).

The ACS data were imported into the R using the tidycensus package.

**Data/ImportedData/2020_US_Region_Mobility_Report.csv** <br />
**Data/ImportedData/2021_US_Region_Mobility_Report.csv** <br />
Google COVID-19 Community Mobility Reports data to characterize county-level changes in time spent in places of residence during the pandemic.<br />
Source: https://www.google.com/covid19/mobility/<br />
Date Accessed: March 27, 2021

**Data/ImportedData/cbp_2018.csv** <br />
Community Business Patterns data for 2017 and 2018 used to characterize number of food establishments per capita by county.<br />
Source: https://www.census.gov/programs-surveys/cbp.html<br />
Date Accessed: March 30, 2021

**Data/ImportedData/us_states_hexgrid.shp** <br />
Shapefile containing the hexagon shapes for mapping United States in Figure 2.<br />
Source: https://github.com/holtzy/D3-graph-gallery/blob/master/DATA/us_states_hexgrid.geojson.json<br />
Date Accessed: March 9, 2021


## Results Data

**Data/ProcessedData/df_filt_geo_Difference_BeforeAfter_All.csv** <br />
Results data used to produce Figure 1.

**Data/ProcessedData/df_filt_geo_Difference_BeforeAfter_State.csv** <br />
Results data used to produce Figure 2.

**Data/ProcessedData/df_filt_geo_LogRegression_coef_alcohol_FINAL_edit.csv** <br />
**Data/ProcessedData/df_filt_geo_LogRegression_coef_fastfood_FINAL_edit.csv** <br />
**Data/ProcessedData/df_filt_geo_LogRegression_coef_healthy_FINAL_edit.csv** <br />
Results data used to produce Figure 3.