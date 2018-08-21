########################### Assignment- Linear Regression #############################

#Set the directory Path
#setwd("D:/PGDDM/Course 3/Assignment")

#Install necessary Packages
install.packages("car")
install.packages("MASS")
install.packages("tidyr")

#Load the libraries
library(car)
library(MASS)
library(tidyr)

#######################################################################################

#Import the data file

CarPrice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)
str(CarPrice)
summary(CarPrice)
View(CarPrice)

###############################DATA PREPARATION#####################################


#Check for duplicates
sum(duplicated(CarPrice$car_ID)) #No Duplicates

#Check for NA values in dataframe
sum(is.na(CarPrice))  # No NA's

# Given in problem statement: There is a variable named CarName which is comprised of two parts - the first word is the name of 'car company' and the second is the 'car model'. For example, chevrolet impala has 'chevrolet' as the car company name and 'impala' as the car model name. You need to consider only company name as the independent variable for the model building. 
CarPrice <- separate(CarPrice, CarName, into = c("CompanyName","ModelName"),  sep = " ", extra = "merge", fill = "right")
 View(CarPrice)

unique(CarPrice$CompanyName) 
#We can see spilling mistakes in the data frame for car names: volkswagen is misplet as vokswagen 
#and also abbrevated to vw in some cases,mazda is missplet as maxda in some cases, 
#porsche is mispelt as porcshce, toyota is misspelt as toyouta and also mismatch in names due to difference in 
#upper case and lower case

CarPrice$CompanyName <- tolower(CarPrice$CompanyName)
#Correcting spelling mistakes
CarPrice$CompanyName <- gsub("vokswagen", "volkswagen", CarPrice$CompanyName)
CarPrice$CompanyName <- gsub("vw", "volkswagen", CarPrice$CompanyName)
CarPrice$CompanyName <- gsub("maxda", "mazda", CarPrice$CompanyName)
CarPrice$CompanyName <- gsub("porcshce", "porsche", CarPrice$CompanyName)
CarPrice$CompanyName <- gsub("toyouta", "toyota", CarPrice$CompanyName)
CarPrice$CompanyName <- gsub("alfa-romero", "alfa-romeo", CarPrice$CompanyName)



summary(CarPrice$CompanyName)

#As mentioned in problem statement, You need to consider only company name as the independent variable for the model building. we can remove the model no extracted from df
CarPrice <- CarPrice[,-4]

#convert all character fields to factors to fit in to linear regression
CarPrice$symboling <- as.factor(CarPrice$symboling)
CarPrice$CompanyName <- as.factor(CarPrice$CompanyName)
CarPrice$fueltype <- as.factor(CarPrice$fueltype)
CarPrice$aspiration <- as.factor(CarPrice$aspiration)
CarPrice$doornumber <- as.factor(CarPrice$doornumber)
CarPrice$carbody <- as.factor(CarPrice$carbody)
CarPrice$drivewheel <- as.factor(CarPrice$drivewheel)
CarPrice$enginelocation <- as.factor(CarPrice$enginelocation)
CarPrice$enginetype <- as.factor(CarPrice$enginetype)
CarPrice$cylindernumber <- as.factor(CarPrice$cylindernumber)
CarPrice$fuelsystem <- as.factor(CarPrice$fuelsystem)


#Check the dataframe 
str(CarPrice)

#Check the categorical variables
summary(CarPrice$symboling)
summary(CarPrice$fueltype)
summary(CarPrice$aspiration)
summary(CarPrice$doornumber)
summary(CarPrice$carbody)
summary(CarPrice$drivewheel)
summary(CarPrice$enginelocation)
summary(CarPrice$enginetype)
summary(CarPrice$cylindernumber)
summary(CarPrice$fuelsystem)


#####################################################################################################

##Creating Dummy Variables
#Symboling
dummy <- data.frame(model.matrix( ~symboling, data = CarPrice))
View(dummy)
dummy <- dummy[,-1]

# Combine the dummy variable into a new dataset called CarPrice_new
CarPrice_new <- cbind(CarPrice[,-2], dummy)

#CompanyName
dummy <- data.frame(model.matrix( ~CompanyName, data = CarPrice_new))
View(dummy)
dummy <- dummy[,-1]

# Combine the dummy variable into a  CarPrice_new
CarPrice_new <- cbind(CarPrice_new[,-2], dummy)

#fueltype
levels(CarPrice_new$fueltype)<-c(1,0)
CarPrice_new$fueltype <- as.numeric(levels(CarPrice_new$fueltype))[CarPrice_new$fueltype]

#aspiration
levels(CarPrice_new$aspiration)<-c(1,0)
CarPrice_new$aspiration <- as.numeric(levels(CarPrice_new$aspiration))[CarPrice_new$aspiration]

#doornumber
levels(CarPrice_new$doornumber)<-c(1,0)
CarPrice_new$doornumber <- as.numeric(levels(CarPrice_new$doornumber))[CarPrice_new$doornumber]

#carbody
summary(CarPrice_new$carbody)
dummy <- data.frame(model.matrix( ~carbody, data = CarPrice_new))
View(dummy)
dummy <- dummy[,-1]

# Combine the dummy variable into a  CarPrice_new
CarPrice_new <- cbind(CarPrice_new[,-5], dummy)

#drivewheel
summary(CarPrice_new$drivewheel)
dummy <- data.frame(model.matrix( ~drivewheel, data = CarPrice_new))
View(dummy)
dummy <- dummy[,-1]

# Combine the dummy variable into a  CarPrice_new
CarPrice_new <- cbind(CarPrice_new[,-5], dummy)

#enginelocation
summary(CarPrice_new$enginelocation)
levels(CarPrice_new$enginelocation)<-c(1,0)
CarPrice_new$enginelocation <- as.numeric(levels(CarPrice_new$enginelocation))[CarPrice_new$enginelocation]

#enginetype
summary(CarPrice_new$enginetype)
dummy <- data.frame(model.matrix( ~enginetype, data = CarPrice_new))
View(dummy)
dummy <- dummy[,-1]

# Combine the dummy variable into a  CarPrice_new
CarPrice_new <- cbind(CarPrice_new[,-11], dummy)

#cylindernumber
summary(CarPrice_new$cylindernumber)
dummy <- data.frame(model.matrix( ~cylindernumber, data = CarPrice_new))
View(dummy)
dummy <- dummy[,-1]

# Combine the dummy variable into a  CarPrice_new
CarPrice_new <- cbind(CarPrice_new[,-11], dummy)

#fuelsystem
summary(CarPrice_new$fuelsystem)
dummy <- data.frame(model.matrix( ~fuelsystem, data = CarPrice_new))
View(dummy)
dummy <- dummy[,-1]
# Combine the dummy variable into a  CarPrice_new
CarPrice_new <- cbind(CarPrice_new[,-12], dummy)

#rm(CarPrice_new)

#####################################################################################################

#Check the numeric variables for outliers
#wheelbase
boxplot(CarPrice_new$wheelbase)
quantile(CarPrice_new$wheelbase,seq(0,1,0.01))

#There is steep jumps between 95th percentile and 100th percentile
CarPrice_new$wheelbase[which(CarPrice_new$wheelbase >110.00)]<- 110.00
boxplot(CarPrice_new$wheelbase)


#Car Length
boxplot(CarPrice_new$carlength)
quantile(CarPrice_new$carlength,seq(0,1,0.01))
#There is steep jumps between 99th percentile and 100th percentile and 0th percentile to 3rd percentile
CarPrice_new$carlength[which(CarPrice_new$carlength <156.06)]<- 156.06
CarPrice_new$carlength[which(CarPrice_new$carlength >202.48)]<- 202.48
boxplot(CarPrice_new$carlength)


#carwidth
boxplot(CarPrice_new$carwidth)
quantile(CarPrice_new$carwidth,seq(0,1,0.01))
#Removing few outliers
CarPrice_new$carwidth[which(CarPrice_new$carwidth >70.852)]<- 70.852
boxplot(CarPrice_new$carwidth)

#carheight
boxplot(CarPrice_new$carheight)
quantile(CarPrice_new$carheight,seq(0,1,0.01))
#No outliers

#curbweight
boxplot(CarPrice_new$curbweight)
quantile(CarPrice_new$curbweight,seq(0,1,0.01))
#one outlier at 0th percentile
CarPrice_new$curbweight[which(CarPrice_new$curbweight <1819.72)]<- 1819.72
boxplot(CarPrice_new$curbweight)

#enginesize
boxplot(CarPrice_new$enginesize)
quantile(CarPrice_new$enginesize,seq(0,1,0.01))
#outliers 
CarPrice_new$enginesize[which(CarPrice_new$enginesize <90.00)]<- 90.00
CarPrice_new$enginesize[which(CarPrice_new$enginesize >201.20)]<- 201.20
boxplot(CarPrice_new$enginesize)

#boreratio
boxplot(CarPrice_new$boreratio)
quantile(CarPrice_new$boreratio,seq(0,1,0.01))
#No outliers

#stroke
boxplot(CarPrice_new$stroke)
quantile(CarPrice_new$stroke,seq(0,1,0.01))
#outlier treatment
CarPrice_new$stroke[which(CarPrice_new$stroke <2.64)]<- 2.64
CarPrice_new$stroke[which(CarPrice_new$stroke >3.90)]<- 3.90
boxplot(CarPrice_new$stroke)

#compressionratio
boxplot(CarPrice_new$compressionratio)
quantile(CarPrice_new$compressionratio,seq(0,1,0.01))
#outliers
CarPrice_new$compressionratio[which(CarPrice_new$compressionratio >10.94)]<- 10.94
boxplot(CarPrice_new$compressionratio)

#horsepower
boxplot(CarPrice_new$horsepower)
quantile(CarPrice_new$horsepower,seq(0,1,0.01))
CarPrice_new$horsepower[which(CarPrice_new$horsepower >207)]<- 207

#peakrpm
boxplot(CarPrice_new$peakrpm)
quantile(CarPrice_new$peakrpm,seq(0,1,0.01))
CarPrice_new$peakrpm[which(CarPrice_new$peakrpm >6000)]<- 6000

#citympg
boxplot(CarPrice_new$citympg)
quantile(CarPrice_new$citympg,seq(0,1,0.01))
CarPrice_new$citympg[which(CarPrice_new$citympg >38.00)]<- 38.00

#highwaympg
boxplot(CarPrice_new$highwaympg)
quantile(CarPrice_new$highwaympg,seq(0,1,0.01))
CarPrice_new$highwaympg[which(CarPrice_new$highwaympg >46.92)]<- 46.92


##########################################################################################
##Derived Metrics

#city mpg to highway mpg ratio
CarPrice_new$citytohighwaympg <- CarPrice_new$citympg/CarPrice_new$highwaympg
# highway mpg to weight ratio
CarPrice_new$weighttohighwaympg <- CarPrice_new$highwaympg/CarPrice_new$curbweight
#horse power to wieght ratio
CarPrice_new$powertoweight <- CarPrice_new$horsepower/CarPrice_new$curbweight

#Car ID is only used to identify unique car. This is not useful for modelling.
CarPrice_new <- CarPrice_new[,-1]

##########################################################################################

###################################MODELLING##############################################

set.seed(100)
trainindices= sample(1:nrow(CarPrice_new), 0.7*nrow(CarPrice_new))
train = CarPrice_new[trainindices,]
test = CarPrice_new[-trainindices,]

#Creating linear model on the final data set
model_1 <- lm(price~., data = train)
summary(model_1)
# R-squared:  0.9805,	Adjusted R-squared:  0.9653

#Using Step AIC to identify insignificant parameters
step <- stepAIC(model_1, direction="both")

#Model_2
model_2 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + citytohighwaympg + weighttohighwaympg + 
                powertoweight , data = train)
summary(model_2)
vif(model_2)
cor(train$citytohighwaympg, train$highwaympg)
#Multiple R-squared:  0.978,	Adjusted R-squared:  0.9697

#cityto highway has insignificant p score as well as vif on higher side

#Model_3
model_3 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi  + weighttohighwaympg + 
                powertoweight , data = train)
summary(model_3)
vif(model_3)
#Multiple R-squared:  0.9777,	Adjusted R-squared:  0.9696 
#fule system mpfi has insignificant p score as well as higher vif

#Model_4
model_4 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix   + weighttohighwaympg + 
                powertoweight , data = train)
summary(model_4)
vif(model_4)
#Multiple R-squared:  0.9774,	Adjusted R-squared:  0.9694
#drive wheel rwd has insignificant p score as well as multicolinearity issue


#Model_5
model_5 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                carbodyhatchback + carbodysedan + carbodywagon  + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix   + weighttohighwaympg + 
                powertoweight , data = train)
summary(model_5)
vif(model_5)
#Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9689
#carbodysedan and carbodyhatchback has multicolinearity issue and the p value is insignificant


#Model_6
model_6 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                carbodywagon  + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix   + weighttohighwaympg + 
                powertoweight , data = train)
summary(model_6)
vif(model_6)
#Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9688
#engine type ohc has the p value  insignificant

#Model_7
model_7 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                carbodywagon  + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix   + weighttohighwaympg + 
                powertoweight , data = train)
summary(model_7)
vif(model_7)
#Multiple R-squared:  0.9748,	Adjusted R-squared:  0.9672

#car body wagon has  p value  insignificant

#Model_8
model_8 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix   + weighttohighwaympg + 
                powertoweight  , data = train)
summary(model_8)
vif(model_8)
#Multiple R-squared:  0.9736,	Adjusted R-squared:  0.9659 


#comnaby name saab p value  insignificant

#Model_9
model_9 <- lm(price~ aspiration + enginelocation + wheelbase + carwidth + 
                enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault +  CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix   + weighttohighwaympg + 
                powertoweight  , data = train)
summary(model_9)
vif(model_9)
#Multiple R-squared:  0.9727,	Adjusted R-squared:  0.965

#wheelbase has insignificant p value


#Model_10
model_10 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamedodge + CompanyNamehonda + CompanyNameisuzu + 
                 CompanyNamemazda + CompanyNamemercury + CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight  , data = train)
summary(model_10)
vif(model_10)
#Multiple R-squared:  0.9719,	Adjusted R-squared:  0.9644
#company name volvo isuzu and mercury has insignificant p value

#Model_11
model_11 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamedodge + CompanyNamehonda +  
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota + CompanyNamevolkswagen +  
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight , data = train)
summary(model_11)
vif(model_11)
cor(train$horsepower, train$powertoweight)
#Multiple R-squared:  0.9655,	Adjusted R-squared:  0.9575

#company name volkswagon has insignificant p value
#Model_12
model_12 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamedodge + CompanyNamehonda +  
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight
                  , data = train)
summary(model_12)
vif(model_12)
#Multiple R-squared:  0.9642,	Adjusted R-squared:  0.9562 
#R squared and adjusted R squared is still similar to previous models

#Comany name honda has insignificant p value
#Model_13
model_13 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 enginesize + horsepower + highwaympg + symboling0 + symboling1 + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamedodge +   
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight   
               , data = train)
summary(model_13)
vif(model_13)
#Multiple R-squared:  0.9624,	Adjusted R-squared:  0.9544

#engine size has insignificant p value

#Model_14
model_14 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 horsepower + highwaympg + symboling0 + symboling1 + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamedodge +   
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight      
               , data = train)
summary(model_14)
vif(model_14)
#Multiple R-squared:  0.9616,	Adjusted R-squared:  0.9538
# symboling 0 has insignificant p value

#Model_15
model_15 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 horsepower + highwaympg +  symboling1 + 
                 CompanyNamebmw + CompanyNamebuick + CompanyNamechevrolet + 
                 CompanyNamedodge +   
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight    
               , data = train)
summary(model_15)
vif(model_15)
#Multiple R-squared:  0.9611,	Adjusted R-squared:  0.9536
# Chevrolet has insignificant p value


#Model_16
model_16 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 horsepower + highwaympg +  symboling1 + 
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamedodge +   
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamenissan + CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight
               , data = train)
summary(model_16)
vif(model_16)
#Multiple R-squared:   0.96,	Adjusted R-squared:  0.9527

# company nissan has insignificant p value and multicolinearity issue


#Model_17
model_17 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 horsepower + highwaympg +  symboling1 + 
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamedodge +   
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight
               , data = train)
summary(model_17)
vif(model_17)
#Multiple R-squared:  0.9586,	Adjusted R-squared:  0.9514


#symboling 1 has insignificant p value

#Model_18
model_18 <- lm(price~ aspiration + enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamedodge +   
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight
               , data = train)
summary(model_18)
vif(model_18)
# Multiple R-squared:  0.9575,	Adjusted R-squared:  0.9506 r squared and adjusted r squared is still on the higher side
#aspiration has insignificant p value

#Model_19
model_19 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamedodge +   
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight
               , data = train)
summary(model_19)
vif(model_19)
#Multiple R-squared:  0.9575,	Adjusted R-squared:  0.951


#company name dodge  has insignificant p value

#Model_20
model_20 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  CompanyNamesubaru + 
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight
               , data = train)
summary(model_20)
vif(model_20)
#Multiple R-squared:  0.9566,	Adjusted R-squared:  0.9503

#company name subaru has insignificant p value. Check if removing the same has significant imapct on adjusted r squared
#Model_21
model_21 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamepeugeot + CompanyNameplymouth + 
                 CompanyNamerenault +  
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_21)
vif(model_21)
#Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9492


#plymouth car as high p value. Check if removing the same has significant imapct on adjusted r squared
#Model_22
model_22 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamemazda +  CompanyNamemitsubishi + 
                 CompanyNamepeugeot +  
                 CompanyNamerenault +  
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_22)
vif(model_22)
#Multiple R-squared:  0.9545,	Adjusted R-squared:  0.9488 

#mazdo car as high p value. Check if removing the same has significant imapct on adjusted r squared
#Model_23
model_23 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamemitsubishi + 
                 CompanyNamepeugeot +  
                 CompanyNamerenault +  
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_23)
vif(model_23)
#Multiple R-squared:  0.9532,	Adjusted R-squared:  0.9477


#renault car as high p value. Check if removing the same has significant imapct on adjusted r squared
#Model_24
model_24 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamemitsubishi + 
                 CompanyNamepeugeot +  
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_24)
vif(model_24)
#Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9459

#mitisbushi as high p value. Check if removing the same has significant imapct on adjusted r squared
#Model_25
model_25 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamepeugeot +  
                 CompanyNametoyota +   
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_25)
vif(model_25)
#Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9459

#toyata as high p value. Check if removing the same has significant imapct on adjusted r squared
#Model_26
model_26 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 CompanyNamepeugeot +  
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_26)
vif(model_26)
#Multiple R-squared:  0.9496,	Adjusted R-squared:  0.945

#peugeot as high p value. Check if removing the same has significant imapct on adjusted r squared
#Model_27
model_27 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_27)
vif(model_27)
#Multiple R-squared:  0.9475,	Adjusted R-squared:  0.943


#Model_28
model_28 <- lm(price~  enginelocation +  carwidth + 
                 horsepower + highwaympg +  
                 CompanyNamebmw + CompanyNamebuick +  
                 cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_28)
vif(model_28)
#Multiple R-squared:  0.9455,	Adjusted R-squared:  0.9413

#model_29
model_29 <- lm(price~  enginelocation +  carwidth + 
                 horsepower +   
                 CompanyNamebmw + CompanyNamebuick +  
                 cylindernumberfour + 
                 cylindernumbersix   + weighttohighwaympg + 
                 powertoweight 
               , data = train)
summary(model_29)
vif(model_29)
#Multiple R-squared:  0.9377,	Adjusted R-squared:  0.9335

#model_30
model_30 <- lm(price~  enginelocation +  carwidth + 
                 horsepower +   
                 CompanyNamebmw + CompanyNamebuick +  
                 cylindernumberfour + 
                 cylindernumbersix   +  
                 powertoweight 
               , data = train)
summary(model_30)
vif(model_30)
#Multiple R-squared:  0.9347,	Adjusted R-squared:  0.9308

#model_31
model_31 <- lm(price~  enginelocation +  carwidth + 
                 horsepower +   
                 CompanyNamebmw + CompanyNamebuick +   
                 powertoweight 
               , data = train)
summary(model_31)
vif(model_31)
#Multiple R-squared:  0.9313,	Adjusted R-squared:  0.9283
#addressing the multicolinearity issue for horse power

#model_32
model_32 <- lm(price~  enginelocation +  carwidth + horsepower +
                 CompanyNamebmw + CompanyNamebuick 
               , data = train)
summary(model_32)
vif(model_32)
#Multiple R-squared:  0.9044,	Adjusted R-squared:  0.9009


##########################################################################################
## Model evaluation

#predicting price using model_32

predict_price <- predict(model_32,test[,-18])
test$test_price <- predict_price
r <- cor(test$test_price,test$price)
rsquared <- r^2
#r square of  0.8575 

summary(model_32)
#Multiple R-squared:  0.9044,	Adjusted R-squared:  0.9009
#The final r squared and adjusted r squared has values which is quite close

#r squared from test dataset is 0.8575 and model data is 0.9009 The difference is about 5%, which is reasonably an accurate model

# Final variables used in the model:
#   enginelocation 
#   carwidth
#   horsepower  
#   CompanyNamebmw
#   CompanyNamebuick

#checking the model precision by plotting actual price vs prediction
plot(test$price,type = "l",col="red")
lines(test$test_price, type = "l",col="blue")

# #The final model predicts the price with sufficient accuracy as seen in the graph. The final model has all the 
# variables with significant p value and the multicolinearity is fairly low and insignificant.
# 
# We can conclude that the follwoing parameters control the pricing of cars significantly:
#   Enginelocation: price varies with location of engine - rear/front
#   Car width: the dimensions of the car have an impact on car price.
#   Horsepower: The horsepower of the car controls the price of the car. higher horseower cars are generally priced higher.
#   Company name: luxury brands like bmw and buick have higher prices.

#Depending on these variables the Geely Auto manufacturers can concentrate on building car models accordingly.

#*******************************************************************************************************

