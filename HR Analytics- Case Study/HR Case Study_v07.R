## Set working directory


# Install and Load the required packages
install.packages("MASS")
install.packages("car")
install.packages("e1071", dependencies=TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("cowplot")
install.packages("GGally")
install.packages("tidyr")
install.packages("dplyr")
install.packages("chron")
install.packages("lubridate")
install.packages('lazyeval')
install.packages('RcppRoll')
install.packages('backports')
install.packages('ddalpha')
install.packages('robustbase')
install.packages('DEoptimR')
install.packages('dimRed')
install.packages('gower')
install.packages("strigr")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(tidyr)
library(dplyr)
library(chron)
library(lubridate)
library(lazyeval)
library(RcppRoll)
library(backports)
library(robustbase)
library(DEoptimR)
library(ddalpha)
library(dimRed)
library(gower)
library(stringr)


## Load data files

general.data <- read.csv("general_data.csv", stringsAsFactors=FALSE)
emp.survey <- read.csv("employee_survey_data.csv", stringsAsFactors=FALSE)
manager.survey <- read.csv("manager_survey_data.csv", stringsAsFactors=FALSE)
in.time <- read.csv("in_time.csv", stringsAsFactors=FALSE)
out.time <- read.csv("out_time.csv", stringsAsFactors=FALSE)

in.time.na <- in.time
out.time.na <- out.time

## Creating Master File by merging various data files

#Pre checks

length(unique(tolower(general.data$EmployeeID)))    
length(unique(tolower(emp.survey$EmployeeID))) 
length(unique(tolower(manager.survey$EmployeeID))) 

setdiff(general.data$EmployeeID, emp.survey$EmployeeID) # Identical customerID across these datasets
setdiff(general.data$EmployeeID, manager.survey$EmployeeID) # Identical customerID across these datasets

master.data <- merge(general.data,emp.survey, by = "EmployeeID", all = TRUE)
master.data <- merge(master.data,manager.survey, by = "EmployeeID", all = TRUE)


## Changing the format into Long Data format for In Time and Out Time after naming first column as EmployeeID

colnames(in.time)[1] <- "EmployeeID"
colnames(out.time)[1] <- "EmployeeID"

in.time <- gather(in.time, in.date.time, in.value, 2:262, na.rm = TRUE)
out.time <- gather(out.time, out.date.time, out.value, 2:262, na.rm = TRUE)

## Separating Date and Time from Long Format data

in.time <- in.time[,-2]
out.time <- out.time [, -2]

in.time <- separate(in.time, in.value, into = c("in.date", "in.time"), sep = " ", remove = TRUE)

out.time <- separate(out.time, out.value, into = c("out.date", "out.time"), sep = " ", remove = TRUE)

## Merging In.Time and Out.Time dataframes into a single file and changing format

emp.time <- merge(in.time, out.time, by.x = c("EmployeeID", "in.date"), by.y = c("EmployeeID", "out.date"))

emp.time$in.time <- chron(times = emp.time$in.time, format = c(times = "h:m:s"))

emp.time$out.time <- chron(times = emp.time$out.time, format = c(times = "h:m:s"))

str(emp.time)

## Finding the total time worked by each employee and storing it as seconds

emp.time$hoursworked <- emp.time$out.time - emp.time$in.time
emp.time$hoursworked <- period_to_seconds(hms(emp.time$hoursworked))

## Removing unwanted columns from emp.time datafile

emp.time <- emp.time[, c(-2,-3,-4)]

## Average hours worked by each employee during the time

emp.time <- aggregate(emp.time$hoursworked, by=list(emp.time$EmployeeID), FUN = mean)
colnames(emp.time)[1] <- "EmployeeID"
colnames(emp.time)[2] <- "Avg_Hours_worked"

#Convert back to average hours worked/day.
emp.time$Avg_Hours_worked <- emp.time$Avg_Hours_worked/3600

## Merging emp.time with master.file

master.data <- merge(master.data, emp.time, by = "EmployeeID", all = TRUE)

View(master.data)

################################################################

### Data Preparation & Exploratory Data Analysis

# Remove Over18 column. It has the same value for all rows.
master.data <- master.data[,colnames(master.data) != "Over18"]

# Calculate leaves taken by each employee, by treating NA as leave.
in.time.na <- in.time.na[,!colSums(is.na(in.time.na)) == nrow(in.time.na)] # remove holidays.

for(i in 1:nrow(in.time.na))
{
  master.data$leaves[i]<-sum(is.na(in.time.na[i,]))
}


# Overtime? Compare with Standard time.
master.data$Std_time_Act <- ifelse(master.data$Avg_Hours_worked > master.data$StandardHours,"Over",
                                   ifelse(master.data$Avg_Hours_worked == master.data$StandardHours,"Std","Less"))

# Remove EmployeeID and StandardHours column. EmployeeID doesnt add any value and standard 
# hours are same for all rows.
master.data <- master.data[,!colnames(master.data) %in% c("EmployeeID","StandardHours")]

# Understanding the structure of the collated file
str(master.data) #4410 obs. of  31 variables

#Missing value?
sapply(master.data, function(x) sum(is.na(x))) 

# We have NA's in NumCompaniesWorked, TotalWorkingYears, EnvironmentSatisfaction,JobSatisfaction
# and WorkLifeBalance. 
# Let's set all the NA's to mean of the column. CHECK IF THIS IS THE RIGHT APPROACH


#Treating NAs in the TotalWorkingYears.
#if NumCompaniesWorked =0 , then totalworkingyears can be the yearsinthecompany.
master.data$TotalWorkingYears <- ifelse(is.na(master.data$TotalWorkingYears) , ifelse(master.data$NumCompaniesWorked == 0 , master.data$YearsAtCompany, master.data$TotalWorkingYears),master.data$TotalWorkingYears)

#assuming a person works for atleast 1 yr in he company, so multiplying numofcompanies worked with 1 and adding to yearsincompany to get totalworkingyears.
master.data$TotalWorkingYears <- ifelse(is.na(master.data$TotalWorkingYears) ,master.data$NumCompaniesWorked+master.data$YearsAtCompany ,master.data$TotalWorkingYears)

#Treating NAs for Numcompaniesworked. Keeping the same assumption of a person spending atleast 1 yr in a company, numcompaniesworked can be totalworkingyears - yearsinthecomapny.
master.data$NumCompaniesWorked <- ifelse(is.na(master.data$NumCompaniesWorked) ,master.data$TotalWorkingYears-master.data$YearsAtCompany ,master.data$NumCompaniesWorked)

#for the NA value in survey columns , replacing it with median value (so as to not introduce another level)
master.data$EnvironmentSatisfaction[which(is.na(master.data$EnvironmentSatisfaction))]<-median(master.data$EnvironmentSatisfaction,na.rm = TRUE)

master.data$JobSatisfaction[which(is.na(master.data$JobSatisfaction))]<-median(master.data$JobSatisfaction,na.rm = TRUE)

master.data$WorkLifeBalance[which(is.na(master.data$WorkLifeBalance))]<-median(master.data$WorkLifeBalance,na.rm = TRUE)

str(master.data)
master.data_copy <- master.data
#master.data <- master.data_copy
#write.csv(master.data , "master_data.csv")

####################################################################################
#Plots for categorical variables checking the impact on attrition for each
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot1 <- ggplot(master.data, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar()+bar_theme1

plot2 <- ggplot(master.data, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1

plot3 <- ggplot(master.data, aes(x=factor(EducationField),fill=Attrition))+ geom_bar()+bar_theme1

plot4 <- ggplot(master.data, aes(x=factor(Gender),fill=Attrition))+ geom_bar()+bar_theme1

plot5 <- ggplot(master.data, aes(x=factor(JobLevel),fill=Attrition))+ geom_bar()+bar_theme1

plot6 <- ggplot(master.data, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1

plot7 <- ggplot(master.data, aes(x= factor(StockOptionLevel),fill=Attrition))+ geom_bar()+bar_theme1

plot8 <- ggplot(master.data, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1

plot9 <- ggplot(master.data, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1

plot10 <- ggplot(master.data, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1

plot11 <- ggplot(master.data, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1

plot12 <- ggplot(master.data, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar()+bar_theme1


plot_grid(plot1, plot2, plot3, plot4, 
          align = "h")   

plot_grid(plot5, plot6, plot7, plot8, 
          align = "h")   

plot_grid(plot9, plot10, plot11, plot12, 
          align = "h")   


#Checking for outliers in numeric variables
#Percent Salary Hike
boxplot(master.data$PercentSalaryHike)#No ouliers

boxplot(master.data$YearsWithCurrManager)#Few outliers

boxplot(master.data$DistanceFromHome)#No outliers

boxplot(master.data$YearsAtCompany)# there are ouliers, though the spread of data is valid for the business case at hand

boxplot(master.data$YearsSinceLastPromotion)# there are ouliers, though the spread of data is valid for the business case at hand

boxplot(master.data$MonthlyIncome)


#Overall Attrition Plot
#library(stringr)
data_over_all <- master.data %>% group_by(Attrition) %>% summarise(count1 = n())
data_over_all$count1 <- 100 * data_over_all$count1/nrow(master.data)
data_over_all$count2 <- str_c(round(data_over_all$count1,2),"%")
plot_attrition <- ggplot(data_over_all,aes(x=Attrition,y=count1,fill=Attrition)) + geom_bar(stat="identity") +
  geom_text(aes(label=count2),vjust = 2)
plot_attrition
#Conclusion : About 16.12% of employees left the company

#Plotting in figures
data_over_all2     <- master.data %>% group_by(Attrition) %>% summarise(count = n())
plot_attrition_fig <- ggplot(data_over_all2,aes(Attrition,y=count,fill=Attrition))+geom_bar(stat="identity") +
  geom_text(aes(label=count),vjust = 2)
plot_attrition_fig

#########################################################################################
  #** Creation of DUmmy variables for model building ****

#COnverting categorical variables.

#Gender
str(master.data$Gender)
master.data$Gender <- as.factor(master.data$Gender) # converting to factor to apply levels function.
summary(factor(master.data$Gender))
levels(master.data$Gender) <- c(1,0)
master.data$Gender <- as.numeric(levels(factor(master.data$Gender)))[master.data$Gender]
str(master.data$Gender)

#Std_time_Act
str(master.data$Std_time_Act)
master.data$Std_time_Act <- as.factor(master.data$Std_time_Act)
summary(factor(master.data$Std_time_Act))
levels(master.data$Std_time_Act) <- c(0,1)
master.data$Std_time_Act <- as.numeric(levels(master.data$Std_time_Act))[master.data$Std_time_Act]
str(master.data$Std_time_Act)

#Performance rating
str(master.data$PerformanceRating)
master.data$PerformanceRating <- as.factor(master.data$PerformanceRating)
summary(factor(master.data$PerformanceRating))
levels(master.data$PerformanceRating) <- c(0,1)
master.data$PerformanceRating <- as.numeric(levels(master.data$PerformanceRating))[master.data$PerformanceRating]
str(master.data$PerformanceRating)

#Create a dataframe of categorical features (More than 2 levels)
master.chr <- master.data[,c("Department","EducationField", "Education","EnvironmentSatisfaction",
                               "JobSatisfaction","WorkLifeBalance","BusinessTravel",
                               "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                               "StockOptionLevel")]

# converting categorical attributes to factor
master.fact <- data.frame(sapply(master.chr, function(x) factor(x)))
str(master.fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(master.fact, 
                            function(x) data.frame(model.matrix(~x-1,data =master.fact))[,-1]))

# Final Dataset
master.data_3 <- cbind(master.data[,!colnames(master.data) %in% c("Department","EducationField", "Education","EnvironmentSatisfaction",
                                        "JobSatisfaction","WorkLifeBalance","BusinessTravel",
                                        "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                                        "StockOptionLevel")], dummies)

################################################################
# Feature standardisation

str(master.data_3)

# Normalising continuous features 
master.data_3$Age<- scale(master.data_3$Age) 
master.data_3$DistanceFromHome<- scale(master.data_3$DistanceFromHome) 
master.data_3$MonthlyIncome<- scale(master.data_3$MonthlyIncome) 
master.data_3$PercentSalaryHike <- scale(master.data_3$PercentSalaryHike)
master.data_3$TotalWorkingYears <- scale(master.data_3$TotalWorkingYears)
master.data_3$TrainingTimesLastYear <- scale(master.data_3$TrainingTimesLastYear)
master.data_3$YearsAtCompany <- scale(master.data_3$YearsAtCompany)
master.data_3$YearsSinceLastPromotion <- scale(master.data_3$YearsSinceLastPromotion)
master.data_3$YearsWithCurrManager <- scale(master.data_3$YearsWithCurrManager)
master.data_3$Avg_Hours_worked <- scale(master.data_3$Avg_Hours_worked)
master.data_3$leaves <- scale(master.data_3$leaves)
master.data_3$NumCompaniesWorked <- scale(master.data_3$NumCompaniesWorked)

# converting target variable attrition from No/Yes character to factorwith levels 0/1 
master.data_3$Attrition <- ifelse(master.data_3$Attrition =="Yes",1,0)

str(master.data_3) # 4410 obs. of  59 variables
View(master.data_3)

# library(corrplot)
# checking for correlation between Attrition and other variables
# correlation_matrix <- cor(master.data_3)
#  corrplot(correlation_matrix, method = "number", title = "Correlation Map", mar=c(0,0,1,0),
#           type = "lower", order = "FPC",col = c("red", "orange", "blue", "green"), number.cex = .5, tl.cex = 0.5)


########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(master.data_3$Attrition, SplitRatio = 0.7)

train = master.data_3[indices,]

test = master.data_3[!(indices),]

########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 4150.1....31 coeff..nullDev 5699.5...resDev 4102.1

# Stepwise selection
#library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#Remmoving EducationField.xLife.Sciences due to high VIF as well as insignificant p value
model_3 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x3 + Education.x4 + Education.x5 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)
summary(model_3)


vif(model_3)
sort((vif(model_3)), decreasing = TRUE)

#YearsAtCompany at has high vif and also has higher p value

model_4 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x3 + Education.x4 + Education.x5 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)
summary(model_4)


sort((vif(model_4)), decreasing = TRUE)

#p value of Education Field Medical is highly insignificant


model_5 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x3 + Education.x4 + Education.x5 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)
summary(model_5)


sort((vif(model_5)), decreasing = TRUE)

#p value is insignificant for Education x3

model_6 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x4 + Education.x5 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)

summary(model_6)


sort((vif(model_6)), decreasing = TRUE)

#p value insignificant for Education.x4  

model_7 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Education.x5 + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)

summary(model_7)


sort((vif(model_7)), decreasing = TRUE)

#p value insignificant for Education.x5  

model_8 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)

summary(model_8)


sort((vif(model_8)), decreasing = TRUE)



#p value insignificant for EducationField.xTechnical.Degree  

model_9 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xMarketing + 
                 EducationField.xOther + 
                 EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)

summary(model_9)


sort((vif(model_9)), decreasing = TRUE)



#p value insignificant for EducationField.xMarketing  

model_10 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                 Std_time_Act + Department.xResearch...Development + Department.xSales + 
                 EducationField.xOther + 
                 EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                 JobInvolvement.x3 + JobLevel.x2 + StockOptionLevel.x1, family = "binomial", 
               data = train)

summary(model_10)


sort((vif(model_10)), decreasing = TRUE)


#p value insignificant for StockOptionLevel.x1  

model_11 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EducationField.xOther + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                  JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_11)


sort((vif(model_11)), decreasing = TRUE)



#p value insignificant for JobInvolvement.x2  

model_12 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EducationField.xOther + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xMarried + MaritalStatus.xSingle +  
                  JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_12)


sort((vif(model_12)), decreasing = TRUE)



#p value insignificant for MaritalStatus.xMarried  

model_13 <- glm(Attrition~  Age + DistanceFromHome + MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EducationField.xOther + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_13)

sort((vif(model_13)), decreasing = TRUE)


#p value insignificant for DistanceFromHome 

model_14 <- glm(Attrition~  Age +  MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EducationField.xOther + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_14)

sort((vif(model_14)), decreasing = TRUE)



#p value insignificant for EducationField.xOther 

model_15 <- glm(Attrition~  Age +  MonthlyIncome + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_15)

sort((vif(model_15)), decreasing = TRUE)



#p value insignificant for MonthlyIncome 

model_16 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsAtCompany + YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_16)

sort((vif(model_16)), decreasing = TRUE)


#p value insignificant for YearsAtCompany and alos has high vif 

model_17 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobInvolvement.x3 + JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_17)

sort((vif(model_17)), decreasing = TRUE)



#p value insignificant for JobInvolvement.x3

model_18 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                 JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_18)

sort((vif(model_18)), decreasing = TRUE)



#p value insignificant for JobRole.xLaboratory.Technician

model_19 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_19)

sort((vif(model_19)), decreasing = TRUE)



#p value insignificant for JobRole.xResearch.Scientist

model_20 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_20)

sort((vif(model_20)), decreasing = TRUE)

#p value for JobRole.xResearch.Director is slightly higher

model_21 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +  
                  JobLevel.x2 , family = "binomial", 
                data = train)

summary(model_21)

sort((vif(model_21)), decreasing = TRUE)



#p value for JobLevel.x2 is slightly higher

model_22 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobRole.xSales.Executive + 
                  MaritalStatus.xSingle  
                 , family = "binomial", 
                data = train)

summary(model_22)

sort((vif(model_22)), decreasing = TRUE)


#p value for JobRole.xSales.Executive is slightly higher

model_23 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_23)

sort((vif(model_23)), decreasing = TRUE)

#p value for JobSatisfaction.x3 is slightly higher

model_24 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x2 + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 +  JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_24)

sort((vif(model_24)), decreasing = TRUE)


#p value for EnvironmentSatisfaction.x2 is slightly higher

model_25 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x2 +  JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_25)

sort((vif(model_25)), decreasing = TRUE)


#p value for JobSatisfaction.x2 is slightly higher

model_26 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_26)

sort((vif(model_26)), decreasing = TRUE)


#p value for TrainingTimesLastYear is slightly higher

model_27 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_27)


sort((vif(model_27)), decreasing = TRUE)

########################################################################################################


final_model<- model_27

########################################################################################################


### Model Evaluation

### Test Data ####

########################################################################################################

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 
summary(test_pred)

test$prob <- test_pred

# Let's use the probability cutoff of 50%.
test_pred_Attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_Attrition,test_pred_Attrition)

################################################################################################################
# finding the optimal probalility cutoff value
###############################################################################################################
perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff #0.1616162

# Let's choose a cutoff value of 0.1616162 
test_cutoff_Attrition <- factor(ifelse(test_pred >=0.1616162, "Yes", "No"))

conf_final <- caret::confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

conf_final

acc <- conf_final$overall[1]
acc

sens <- conf_final$byClass[1]
sens

spec <- conf_final$byClass[2]
spec

library(Information)
library(InformationValue)
library(ROCR)

##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attr <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_attr <- ifelse(test_actual_Attrition=="Yes",1,0)

sum(test_cutoff_attr)
sum(test_actual_attr)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attr, test_actual_attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.522

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attr_decile = lift(test_actual_attr, test_pred, groups = 10)

Attr_decile


#---------------------------------------------------------------------------------
#plot the lift chart 
################################################################################################################

plot(Attr_decile$Cumlift, type="l", lwd=2, col="red",
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)


# ################################################################################################################
 ks_plot(test_actual_attr, test_cutoff_attr) # Gain chart plot


########################################################################################################

##Alternate Model

########################################################################################################

summary(model_27)
vif(model_27)

#WorkLifeBalance.x4 has slightly high vif and p value is also silghtly higher


model_28 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x2 + WorkLifeBalance.x3 +  
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_28)

sort((vif(model_28)), decreasing = TRUE)

#WorkLifeBalance.x2  has higher p value compared to others

model_29 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 +  
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_29)

sort((vif(model_29)), decreasing = TRUE)


#EnvironmentSatisfaction.x3  has higher p value compared to others

model_30 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 +  
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_30)

sort((vif(model_30)), decreasing = TRUE)

cor(master.data_3$BusinessTravel.xTravel_Frequently, master.data_3$BusinessTravel.xTravel_Rarely)

#High correlation between BusinessTravel.xTravel_Frequently and BusinessTravel.xTravel_Rarely. Removing BusinessTravel.xTravel_Rarely from the model


model_31 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 +  
                  BusinessTravel.xTravel_Rarely + 
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_31)

sort((vif(model_31)), decreasing = TRUE)



#BusinessTravel.xTravel_Rarely  has higher p value now which can be ignored

model_32 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act + Department.xResearch...Development + Department.xSales + 
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 +  
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_32)

sort((vif(model_32)), decreasing = TRUE)




cor(master.data_3$Department.xResearch...Development, master.data_3$Department.xSales)



# #Since there is high correlation between department research&development and sales. Also,
# Department.xResearch...Development has lower attrition compared to sales, we can remove the Department.xResearch...Development
# to keep the senstivity of the model high



model_33 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act +  Department.xSales + 
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 +  
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_33)

sort((vif(model_33)), decreasing = TRUE)


#Department.xSales has higher p value now which can be ignored

model_34 <- glm(Attrition~  Age +   
                  NumCompaniesWorked + TotalWorkingYears +  
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  Std_time_Act +   
                  EnvironmentSatisfaction.x4 + 
                  JobSatisfaction.x4 + 
                  WorkLifeBalance.x3 +  
                  MaritalStatus.xSingle  
                , family = "binomial", 
                data = train)

summary(model_34)

sort((vif(model_34)), decreasing = TRUE)



########################################################################################################


final_model_2<- model_34

########################################################################################################


### Model Evaluation

### Test Data ####

########################################################################################################

test_pred_2 = predict(final_model_2, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 
summary(test_pred_2)

test$prob <- test_pred_2

# Let's use the probability cutoff of 50%.
test_pred_Attrition_2 <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition_2 <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test_actual_Attrition_2,test_pred_Attrition_2)

################################################################################################################
# finding the optimal probalility cutoff value
###############################################################################################################
perform_fn <- function(cutoff) 
{
  predicted_Attrition_2 <- factor(ifelse(test_pred_2 >= cutoff, "Yes", "No"))
  conf_2 <- caret::confusionMatrix(predicted_Attrition_2, test_actual_Attrition_2, positive = "Yes")
  acc <- conf_2$overall[1]
  sens <- conf_2$byClass[1]
  spec <- conf_2$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred_2)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff #is coming as 0 

#This model has bad accuracy as seen in confusion matrix. we can assume that initial model is better
