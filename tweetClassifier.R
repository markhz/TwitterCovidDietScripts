library(readr)
library(dplyr)

#Text mining packages
library(tm)
library(SnowballC)

#loading the data
t1 <- read_csv("~/Desktop/Finallabel_data.csv")
glimpse(t1)  

#Creating text corpus
corpus = Corpus(VectorSource(t1$tweet))
corpus[[1]][1]
t1$FINALLABEL[1]

#Conversion to lower case
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus[[1]][1]  

#Remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]][1]

#Remove stop words
corpus= tm_map(corpus, removeWords, c(stopwords("english")))
corpus[[1]][1] 

#Stemming- To reduce the number of inflectional forms of words appearing in the text
corpus = tm_map(corpus, stemDocument)
corpus[[1]][1] 

#Creating Document term matrix
frequencies = DocumentTermMatrix(corpus)

sparse = removeSparseTerms(frequencies, 0.995)

tSparse = as.data.frame(as.matrix(sparse))
colnames(tSparse) = make.names(colnames(tSparse))
tSparse$recommended_id = t1$FINALLABEL

prop.table(table(tSparse$recommended_id))

#creating training and test dataset
library(caTools)
set.seed(100)
split = sample.split(tSparse$recommended_id, SplitRatio = 0.7)
trainSparse = subset(tSparse, split==TRUE)
testSparse = subset(tSparse, split==FALSE)

#Random Forest
library(randomForest)
set.seed(100)
trainSparse$recommended_id = as.factor(trainSparse$recommended_id)
testSparse$recommended_id = as.factor(testSparse$recommended_id )

RF_model = randomForest(recommended_id ~ ., data=trainSparse)
predictRF = predict(RF_model, newdata=testSparse)
confusion_matrix_RF<-as.data.frame.matrix(table(testSparse$recommended_id, predictRF))
accuracy_RF<-(confusion_matrix_RF[[1]][1]+confusion_matrix_RF[[2]][2])/sum(confusion_matrix_RF)
recall_RF<- confusion_matrix_RF[[2]][2]/(confusion_matrix_RF[[1]][2]+confusion_matrix_RF[[2]][2])
specificity_RF<- confusion_matrix_RF[[1]][1]/(confusion_matrix_RF[[1]][1]+confusion_matrix_RF[[2]][1])

######################## Hyper parameter tuning RF ##################

trainn <- select(trainSparse,-c(recommended_id))
# trainn <- trainSparse 
datlabels_train <-  trainSparse$recommended_id

testt <- select(testSparse,-c(recommended_id))
# testt <- testSparse
datlabels_test <- testSparse$recommended_id

#*******fit random forest model*******************
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


#SVM
library(caTools)
set.seed(100)
library(e1071)
#hyper parameter tuning
fit_tune_svm <- tune.svm(trainn, as.factor(datlabels_train), kernel = "linear")
best.linear <- fit_tune_svm$best.model
tune.test <- predict(best.linear, newdata = testt)
confusion_matrix_svm_tune<-as.data.frame.matrix(table(datlabels_test, tune.test))
accuracy_svm_tune<-(confusion_matrix_svm_tune[[1]][1]+confusion_matrix_svm_tune[[2]][2])/sum(confusion_matrix_svm_tune)

#F1 score
f1_svm_tune <- (confusion_matrix_svm_tune[[2]][2]) / (confusion_matrix_svm_tune[[2]][2] + 0.5 *(confusion_matrix_svm_tune[[1]][2] + confusion_matrix_svm_tune[[2]][1]) )


classifier_SVM = svm(formula = recommended_id ~ .,data = trainSparse,type = 'C-classification',kernel = 'linear')
predictSVM = predict(classifier_SVM, newdata=testSparse)
confusion_matrix_SVM<-as.data.frame.matrix(table(testSparse$recommended_id, predictSVM))
accuracy_SVM<-(confusion_matrix_SVM[[1]][1]+confusion_matrix_SVM[[2]][2])/sum(confusion_matrix_SVM)
recall_SVM<- confusion_matrix_SVM[[2]][2]/(confusion_matrix_SVM[[1]][2]+confusion_matrix_SVM[[2]][2])
specificity_SVM<- confusion_matrix_SVM[[1]][1]/(confusion_matrix_SVM[[1]][1]+confusion_matrix_SVM[[2]][1])

# Logistic Regression
lr_model <- glm(recommended_id ~.,family=binomial(link='logit'),data=trainSparse)
fitted.results <- predict(lr_model,newdata=testSparse,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
confusion_matrix_LR<-as.data.frame.matrix(table(testSparse$recommended_id, fitted.results))
accuracy_LR<-(confusion_matrix_LR[[1]][1]+confusion_matrix_LR[[2]][2])/sum(confusion_matrix_LR)
recall_LR<- confusion_matrix_LR[[2]][2]/(confusion_matrix_LR[[1]][2]+confusion_matrix_LR[[2]][2])
specificity_LR<- confusion_matrix_LR[[1]][1]/(confusion_matrix_LR[[1]][1]+confusion_matrix_LR[[2]][1])

                       
libray(gbm)  
gbm_model<-gbm(formula = recommended_id ~ ., distribution = "bernoulli", data = trainSparse, n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 
#Generating a Prediction matrix for each Tree
predmatrix<-predict(gbm_model,testSparse,n.trees = n.trees)
dim(predmatrix) #dimensions of the Prediction Matrix
#Calculating The Mean squared Test Error
test.error<-with(testSparse,apply((predmatrix-recommended_id)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged
#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.err),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)

