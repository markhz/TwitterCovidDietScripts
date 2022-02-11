# TwitterCovidDietScripts
 R scripts and data for running analysis for the manuscript:
 "Diet During the COVID-19 Pandemic: An Analysis of Twitter data"
 
 Manuscrip Authors: Mark A. Hernandez, MPH,  Shagun Modi, MPH,  Kanisha Mittal, MPH,  Pallavi Dwivedi, MPH,  Quynh C. Nguyen, PhD,  Nina L. Cesare, PhD and Elaine O. Nsoesie, PhD
 
 Corresponding author: Mark A. Hernandez, mark.hernandez@ll.mit.edu, markhernandez512@gmail.com


# Scripts

run_tweet_classifier.R 
Imports (1) raw tweet data and (2) subsample of labeled tweets to indicate food consumption. 
Trains Random Forest model to identify tweets indicating food consumption. Runs model across the all tweet data and outputs labels identifying food consumption tweets.

importStateCountyVar.R
Imports and merges county-level covariates from (1) Google Mobility (2) American Community Survey and (3) Community Business Patterns

run_tweet_stats.R
Calculates changes in shares of healthy food tweets, fast food tweets, and alcohol tweets nationally and by state. Also calculates county-level shares in food tweet categories during pre-pandemic period.

stateAnalysis.R
Plots changes in state food tweets.

countyAnalysis.R
Runs logistic regression models for each primary outcome at the county level and plots results.

tweetClassifier.R
Evaluates the performance of three separate models in identifying food consumption tweets: Random Forest, linear SVM, and logistic regression.

twitter_food_functions.R
Contains functions used by multiple R scripts.



