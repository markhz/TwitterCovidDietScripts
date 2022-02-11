
rm(list=ls())

# set working directory to script
wd <- "/projectnb2/sph795/markhz/TwitterScripts"
setwd(wd)

# import packages and functions
library(tidyverse)
library(tictoc)
library(zoo) # date time variables
library(lubridate) # date time variables
# library(sf)
# library(tidycensus)
library(broom) # for tidy
library(RColorBrewer)
library(parallel)

library(caTools)
library(randomForest)

source("twitter_food_functions.R")

# Add census api key
# census_api_key("b25f910867b5786e08dec5cba9253be2e6d376f6", install = TRUE)



# set path to input data
dataPath <- "/project/sph795/Data/"

filePath2019 <- file.path(dataPath, 
                          "Twitter_food_2019",
                          "food_tweets_feb_2019_jan_2020.csv")

filePath2020 <- file.path(dataPath, 
                          "Twitter_food_2020",
                          "food_tweets_feb_2020_jan_2021.csv")

filePath_labeled <- file.path("/projectnb/sph795/markhz/TwitterScripts/Data/Finallabel_data.csv")


# filePathTweetClassifier <- file.path("Classifiers/TweetClassifier.RData")  

filePathHexShp <- "us_states_hexgrid/us_states_hexgrid.shp"


# load classifier
# load(filePathTweetClassifier)

# load csv with 2019 Twitter food data
tic()
df19 <- read.csv(filePath2019, fileEncoding="latin1") %>%
  mutate(datasetYear = 2019)
toc()

# load csv with 2020 Twitter food data
tic()
df20 <- read.csv(filePath2020, fileEncoding="latin1") %>%
  mutate(datasetYear = 2020)
toc()

df_in <- df19 %>%
  bind_rows(df20)

rm(df19)
rm(df20)





# -------------------------------------------------------------------------
# classify and filter
# -------------------------------------------------------------------------


#Text mining packages
library(tm)
library(SnowballC)



generateCorpus <- function(tweets){
  #Creating text corpus
  corpus = Corpus(VectorSource(tweets))
  
  #Conversion to lower case
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, tolower)
  
  #Remove punctuation
  corpus = tm_map(corpus, removePunctuation)
  
  #Remove stop words
  corpus= tm_map(corpus, removeWords, c(stopwords("english")))
  
  # Stemming- To reduce the number of inflectional forms of words appearing in the text
  corpus = tm_map(corpus, stemDocument)
  
  return(corpus)
}

# load labeled training data
t1 <- read.csv(filePath_labeled)

# generate corpus from tweets
corpus <- generateCorpus(t1$tweet)

#Creating Document term matrix
frequencies = DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.995)

tSparse = as.data.frame(as.matrix(sparse))
# colnames(tSparse) = make.names(colnames(tSparse))
# tSparse$recommended_id = as.logical(t1$FINALLABEL)
tSparse$recommended_id = t1$FINALLABEL

train_terms <- colnames(tSparse)[1:ncol(tSparse)-1]


# -------------------------------------------------------------------------



#creating training and test dataset

set.seed(100)
split = sample.split(tSparse$recommended_id, SplitRatio = 0.7)
trainSparse = subset(tSparse, split==TRUE)
testSparse = subset(tSparse, split==FALSE)


trainn <- select(trainSparse,-c(recommended_id))
datlabels_train <-  trainSparse$recommended_id

testt <- select(testSparse,-c(recommended_id))
datlabels_test <- testSparse$recommended_id


#tune the parameters to find the best model
fit_tune <- tuneRF(trainn, 
                   as.factor(datlabels_train) , 
                   mtrystart = 2, 
                   ntreeTry = 500, 
                   stepFactor=1.2, 
                   plot=FALSE,
                   trace=FALSE) #tune your RF to find the best parameters
rowd <- which(fit_tune == min(fit_tune), arr.ind = TRUE)
val <- fit_tune[rowd[1]]
val
fit_forest =  randomForest(trainn, factor(datlabels_train), classwt = c(1,1)/2, mtry = val, ntree=500)
# fit_forest =  randomForest(trainn, factor(datlabels_train), classwt = c(1,1)/2, ntree=500)
# pred_test = predict(fit_forest, newdata = testt)
pred_test = predict(fit_forest, newdata = testt)

confusion_matrix_RF_tune<-as.data.frame.matrix(table(datlabels_test, pred_test))
accuracy_RF_tune<-(confusion_matrix_RF_tune[[1]][1]+confusion_matrix_RF_tune[[2]][2])/sum(confusion_matrix_RF_tune)

#F1 score
f1_RF_tune <- (confusion_matrix_RF_tune[[2]][2]) / (confusion_matrix_RF_tune[[2]][2] + 0.5 *(confusion_matrix_RF_tune[[1]][2] + confusion_matrix_RF_tune[[2]][1]) )





# -------------------------------------------------------------------------


# find how many tweets have invalid symbols


# replace invalid symbols that cause errors
tic()
df_in <- df_in %>% 
  mutate(tweet = str_replace(tweet, "\xf0\x9f\x98", ""))
toc()


tSparseRef <- data.frame(matrix(ncol = length(train_terms), nrow = 0))
colnames(tSparseRef) <- train_terms




# -------------------------------------------------------------------------
# predict one tweet at a time
# -------------------------------------------------------------------------


predictTweet <- function(tweet1, fit_forest, tSparseRef, train_terms){
  corpus1 <- generateCorpus(tweet1)

  # generate document term matrix for single tweet
  frequencies1 = DocumentTermMatrix(corpus1)
  tSparse1 = as.data.frame(as.matrix(frequencies1))
  # colnames(tSparse1) = make.names(colnames(tSparse1))

  # fit tweet DTM to training terms
  excludeTerms <- setdiff( colnames(tSparse1) , train_terms)
  tSparse_1 <- bind_rows(tSparseRef, tSparse1) %>%
    select(-excludeTerms) %>%
    replace(is.na(.), 0)

  # predict
  pred_test_1 = predict(fit_forest, newdata = tSparse_1)
}




tic()
y_consumed <- mclapply( df_in$tweet, 
       predictTweet, 
       fit_forest = fit_forest,
       tSparseRef = tSparseRef,
       train_terms = train_terms,
       mc.cores = 8)
       # simplify = TRUE, 
       # USE.NAMES = FALSE)
toc()

# conert to numeric
y_consumed_out <- as.numeric( as.character(unlist(y_consumed)) )

# percentage of tweets where food was consumed
sum(y_consumed_out) / length(y_consumed_out)


write.csv(y_consumed_out, "Data/ProcessedData/prediction_full.csv",
          row.names = FALSE)


