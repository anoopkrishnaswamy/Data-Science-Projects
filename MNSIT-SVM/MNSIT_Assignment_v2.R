############################ SVM Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
#  4.3 Polynomial Kernel
# 5 Conclusions

#####################################################################################

# 1. Business Understanding: 

#The objective is to  correctly identify the digit (between 0-9) written in a 
#digit image submitted by a user via a scanner, a tablet, or other digital devices  
# using pixel data

#####################################################################################

# 2. Data Understanding: 
# MNSIT data which is a large database of handwritten digits where we have 
#     pixel values of each digit along with its label. 784 attributes represent values in each pixel. 
#     having 28 by 28 matrix
# Number of Instances: 60,000 plus 10000 test
# Number of Attributes: 785 

#####################################################################################

#3. Data Preparation: 


#Loading Neccessary libraries

install.packages("caret")
install.packages("kernlab")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("gridExtra")

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

#Loading data
mnist_train <- read.csv("mnist_train.csv", stringsAsFactors = F, header = F)
mnist_test <- read.csv("mnist_test.csv", stringsAsFactors = F, header = F)


#Understanding Dimensions

dim(mnist_train)
dim(mnist_test)

#Structure of the dataset

str(mnist_train)
str(mnist_test)

#printing first few rows

head(mnist_train)
head(mnist_test)

#Exploring the data

summary(mnist_train)
summary(mnist_test)


# Data has no column names


names(mnist_train)[1] <- "digit"
names(mnist_test)[1] <- "digit"
View(mnist_train)
View(mnist_test)

# Duplicated rows

sum(duplicated(mnist_train)) # no duplicate rows
sum(duplicated(mnist_test)) # no duplicate rows

# Checking for NAs

sum(sapply(mnist_train, function(x) sum(is.na(x)))) # There are no missing values
sum(sapply(mnist_test, function(x) sum(is.na(x)))) # There are no missing values


#Making our target class to factor

mnist_train$digit<-factor(mnist_train$digit)
mnist_test$digit<-factor(mnist_test$digit)


# Split the data into train and test set

set.seed(100)
sample_indices <- sample(1: nrow(mnist_train), 10000) # extracting subset of 10000(approx 15%) samples for modelling
train <- mnist_train[sample_indices, ]

# Scaling data 

max(train[ ,2:ncol(train)]) # max pixel value is 255, lets use this to scale data
train[ , 2:ncol(train)] <- train[ , 2:ncol(train)]/255

test <- cbind(digit = mnist_test[ ,1], mnist_test[ , 2:ncol(mnist_test)]/255)
sample_indices_test <- sample(1: nrow(mnist_test), 2000) # extracting subset of 2000(approx 20%) samples for evaluation
test <- test[sample_indices_test,]

#Explorary Data Analysis

plot1 <- ggplot(mnist_train, aes(x = digit, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Frequency", title = "mnist_train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot2 <- ggplot(mnist_test, aes(x = digit, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Frequency", title = "mnist_test dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot3 <- ggplot(train, aes(x = digit, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Frequency", title = "train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot4 <- ggplot(test, aes(x = digit, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Frequency", title = "test dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

grid.arrange(plot1, plot2, plot3, plot4, nrow = 4)

#Data distribution is similar across  train and test data sets

#####################################################################################

# 4. Model  Buliding: 

#to allow parellel processing
library(doParallel)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
# machine learning code goes in here

#4.1 Using Linear Kernel
Model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot", C=1)
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)

# Observations:
# Overall accuracy of 92.1%
# Specificities quite high > 98%
# Sensitivities good > 87%

## Linear kernel using stricter C
Model_linear_2 <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot", C=10)
Eval_linear_2<- predict(Model_linear_2, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear_2,test$digit)


# Observations:
# Overall accuracy of 92.05%
# Specificities quite high > 98%
# Sensitivities good > 86%

# Model performance has slightly decreased, model may be overfitting

## Using cross validation to optimise C

grid_linear <- expand.grid(C= c(0.001, 0.1 ,1 ,10 ,100)) # defining range of C

fit.linear <- train(digit ~ ., data = train, metric = "Accuracy", method = "svmLinear",
                    tuneGrid = grid_linear, preProcess = NULL,
                    trControl = trainControl(method = "cv", number = 5), allowParallel=TRUE)

# printing results of 5 cross validation
print(fit.linear) 
plot(fit.linear)

# Observations:
# Best accuracy is about 92.77% at C = 0.1
# Higher values of C are overfitting and lower values are giving simple models

eval_cv_linear <- predict(fit.linear, newdata = test)
confusionMatrix(eval_cv_linear, test$digit)

# Observations:
# Overall accuracy of 92.9%
# Specificities quite high > 98.6%
# Sensitivities > 89%, improved from model1 by making model more generic i.e. lower C 

#4.2 RBF Kernel

#Using RBF Kernel
Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot", C =1, kpar = "automatic")
Eval_RBF<- predict(Model_RBF, test)
print(Model_RBF) 


#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit)
# Observations:
# Overall accuracy of 95.25%
# Specificities quite high > 99%
# Sensitivities > 91.9%, improved from linear model indicating there is non linearity in the data set
# values with C = 1 and sigma = 0.0176

#RBF Kernel with higher sigma
Model_RBF_2 <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot", C =1, kpar = list(sigma = 0.5))
Eval_RBF<- predict(Model_RBF_2, test)
print(Model_RBF_2) 

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit)

#Accuracy has reduced a lot, Clearly model was a overfit

## cross validation to optimization using C and sigma
grid_rbf = expand.grid(C= c(0.01, 0.1, 1, 5, 10), sigma = c(0.001, 0.01, 0.1, 1, 5)) 

Model_RBF_3 <- train(digit ~ ., data = train, metric = "Accuracy", method = "svmRadial",tuneGrid = grid_rbf,
                     trControl = trainControl(method = "cv", number = 2), preProcess = NULL, allowParallel=TRUE)


#grid_rbf
print(Model_RBF_3) 
plot(Model_RBF_3) 

#Observations
#The best accuracy of the model is at c value of 5 and sigma 0.01 which is about 96%. 
#This model has the best accurary among rdf and linear

eval_cv_rbf <- predict(Model_RBF_3, newdata = test)
confusionMatrix(eval_cv_rbf, test$digit)

#Observations
#Model Accuracy is quite high about 96 %
#Specificty is very high with 99%
#senstivity is also better than previous models which is greater than 94%

#4.3 Polynomial Kernel
Model_poly <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "polydot", C=1, kpar = list(degree = 2, scale = 1, offset = 1))
Eval_poly<- predict(Model_poly, test)

#confusion matrix - polynomial Kernel
confusionMatrix(Eval_poly,test$digit)


#Observations
# Overall accuracy of model is around 95.7 which is quite good and close to rbf best model


Model_poly_2 <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "polydot", C=1, kpar = list(degree = 2, scale = 10, offset = 1))
Eval_poly_2<- predict(Model_poly_2, test)

#confusion matrix - polynomial Kernel
confusionMatrix(Eval_poly_2,test$digit)

#Observations
# Overall accuracy of model is around 95.7 which does change at all by varying scale 
# so we can assign constant value and arrive at best fit degree using cross validation



## Grid search to optimise hyperparameters

grid_poly = expand.grid(C= c(0.01, 0.1, 1, 10), degree = c(1, 2, 3, 4, 5), scale =c(1) )

grid_poly

fit.poly <- train(digit ~ ., data = train, metric = "Accuracy", method = "svmPoly",tuneGrid = grid_poly,
                  trControl = trainControl(method = "cv", number = 2), preProcess = NULL, allowParallel=TRUE)

# printing results of cross validation
print(fit.poly) 
plot(fit.poly)

eval_cv_poly <- predict(fit.poly, newdata = test)
confusionMatrix(eval_cv_poly, test$digit)

#Degree 2 has the best accuracy and is almost same for varying c value
#Overall accuracy of 95.7 
#sensitivity above 94%
#Specificity above 99%



stopCluster(cl)


#####################################################################################
# 5 Conclusions


# Best model derived based on Accuracy metric is Model_RBF_3 which has accuracy of 95.8
# at c value of 5 and sigma 0.01. Also, polynomial model with degree 2 has 95.7 accuracy 
# which is good and close to the best model. The run time of polynomial is better than 
# rbf kernel. The final model can be chosen between Model_RBF_3 or fit.poly
# Note that the train and test dataset was sampled to pick only 10000 and 2000 records
# respectively so that the performance of modelling does not become a burden. The model 
# accuracy is close enough on the complete dataset of mnsit_train which was done separetly
# and not submitted as part of this R file.

#####################################################################################
