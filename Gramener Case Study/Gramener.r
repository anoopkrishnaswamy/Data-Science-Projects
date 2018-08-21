## Set working directory to the relevant folder containing the data file
install.packages("gridExtra")
install.packages("lubridate")
install.packages("vcd")
require("vcd")
require(lubridate)
require(dplyr)
require(ggplot2)
require(scales)
require(plyr)
require(tidyr)
require(grid)
require(gridExtra)
require(zoo) 

## Loading the file to R server

loan_master <- read.csv("loan.csv",stringsAsFactors = F, na.strings=c("", " ", "NA", "N/A", "n/a"))

## To check if there are any duplicate ids

length(loan_master$id)
length(unique(loan_master$id))
length(unique(loan_master$member_id))

############################ Data Cleaning Start ##############################

# Remove the columns with all Na's

loan_master <- loan_master[, colSums(is.na(loan_master)) != nrow(loan_master)]

# Remove columns that have the same value for all the rows.

loan_master <- loan_master[,!apply(loan_master,2,function (x) {length(levels(factor(x)))})==1]


# Remove the columns manually, that we see, will not be used for analysis. 

loan_master <- loan_master[,!names(loan_master) %in% c("desc","url","zip_code", "emp_title", "title", "funded_amnt_inv", "next_pymnt_d")]


#Remove '%' values in coulmns and convert to numeric values
loan_master$int_rate <- as.numeric(gsub("%", "", loan_master$int_rate))
loan_master$revol_util <- as.numeric(gsub("%", "", loan_master$revol_util))

#Convert Term column into numeric after removing months
loan_master$term <- as.numeric(gsub(" months", "", loan_master$term))

#Convert Employment Length into Factors after removing strings
loan_master$emp_length <- as.factor(gsub(" years", "", loan_master$emp_length))
loan_master$emp_length <- as.factor(gsub(" year", "", loan_master$emp_length))

#Date Conversions
loan_master$issue_d <- as.Date(as.yearmon(loan_master$issue_d,"%b-%y"))
loan_master$last_pymnt_d <- as.Date(as.yearmon(loan_master$last_pymnt_d,"%b-%y"))

#Capture the year from earliest credit line as the dates are below 1970 and direct conversion to dates fail in R
loan_master$earliest_cr_line_yr  <- as.numeric(substr(loan_master$earliest_cr_line, 5, 6)) #extract the year 
loan_master$earliest_cr_line_yr <- ifelse(loan_master$earliest_cr_line_yr >20 & loan_master$earliest_cr_line_yr <=99, loan_master$earliest_cr_line_yr+1900, loan_master$earliest_cr_line_yr+2000) #add 1900 or 2000

## Adding Derived Metrics

# Calculating FOIR (Fixed Obligations to Income Ratio, ie., what percentage of monthly income is paid as instalments)

loan_master$FOIR <- round(loan_master$installment*12*100/loan_master$annual_inc, digits = 2)

# Calculating leverage for the data set. Leverage will be the ratio of funded amount to annual income

loan_master$leverage <- round(loan_master$funded_amnt*100/loan_master$annual_inc, digits = 2)


# Income category
boxplot(loan_master$annual_inc)
summary(loan_master$annual_inc)
quantile(loan_master$annual_inc,c(0, 0.95))

# Around 95% of rows have annual income less than 142000. Hence, we'll remove the outliers.
inc_sub <- loan_master[which(loan_master$annual_inc < 142000),]

#Remove the Current status loans as this has no significance on required analysis
inc_sub = inc_sub[inc_sub$loan_status!="Current",]

boxplot(inc_sub$annual_inc)
summary(inc_sub$annual_inc)

#Categorise the annual income for analysis
inc_sub$annual_inc_cat[inc_sub$annual_inc<35000] <- "Low"
inc_sub$annual_inc_cat[inc_sub$annual_inc>=35000 & inc_sub$annual_inc<70000] <- "Lower-Middle"
inc_sub$annual_inc_cat[inc_sub$annual_inc>=70000 & inc_sub$annual_inc<100000] <- "Higher-Middle"
inc_sub$annual_inc_cat[inc_sub$annual_inc>=100000] <- "High"


############################ Data Cleaning End ##############################

############################ Data Analysis Start ##############################

#Univariate Analysis
#To Calculate the mean of charged off loans
loan_master$loan_status_value <- ifelse(loan_master$loan_status=="Charged Off",1,0)
mean(loan_master$loan_status_value) 
#0.1416-- about 14% of loans are charged off

#Loan Status Distribution
ggplot(loan_master, aes(x= loan_status)) + ggtitle("Loan Status Distribution")+
  geom_bar(aes(y = (..count..)/sum(..count..)), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  labs(x= "Loan Status" , y = "Percent") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

#Loan Purpose Distribution
ggplot(loan_master, aes(x= purpose)) + ggtitle("Loan Purpose Distribution")+
  geom_bar(aes(y = (..count..)/sum(..count..)), stat="count") +
  geom_text(aes( label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  labs(x= "Loan Purpose" , y = "Percent") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

#Considering charged off loans for analysis- creating a subset
chargedoffloan<-loan_master[which(toupper(loan_master$loan_status) == "CHARGED OFF"), ]

# COnsidering only charged off loans for purpose analysis
ggplot(chargedoffloan, aes(x= purpose)) + ggtitle("Charged off purpose wise")+
  geom_bar(aes(y = (..count..)/sum(..count..)), stat="count") +
  labs(x= "Purpose" , y = "Percent") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

# Imp observation- We can see that the debt consolidation constitutes close to 50% of total defaults


#Address State wise
ggplot(chargedoffloan, aes(x= addr_state)) + ggtitle("Charged off loan state wise")+
  geom_bar(aes(y = (..count..)/sum(..count..)), stat="count") +
  labs(x= "State" , y = "Percent") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

# We can see that the there are high number of charged off loans in few states - CA, FL, NY, TX, NJ


#===================Bivariate Analysis============================#
# Sub-grade vs Loan status
ggplot(inc_sub,aes(x=sub_grade,fill=loan_status)) + 
  geom_bar(stat="count",position="fill") + 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1)) +
  ggtitle("Sub grade Distribution") +
  labs(x= "Sub-grade" , y = "Percent") 

scale_x <- scale_x_discrete(limits=c("Low","Lower-Middle","Higher-Middle","High"))


# Annual income vs Loan status 
ggplot(inc_sub,aes(x=annual_inc_cat,fill=loan_status)) + 
  geom_bar(position="fill") + 
  theme(axis.text.x = element_text( vjust = 0.5, hjust=1)) +
  ggtitle("Income Distribution") +
  labs(x= "Income Categories" , y = "Income") +
  scale_x

loan_term36 <- inc_sub[which(inc_sub$term ==36),]
loan_term60 <- inc_sub[which(inc_sub$term ==60),]
plot_36 <- ggplot(loan_term36,aes(x=annual_inc_cat,fill=loan_status)) + 
  geom_bar(position="fill") +
  theme(axis.text.x = element_text( vjust = 0.5, hjust=0.5)) +
  ggtitle("Income Distribution at term 36") +
  labs(x= "Income Categories" , y = "Percent") +
  scale_x
plot_60 <- ggplot(loan_term60,aes(x=annual_inc_cat,fill=loan_status)) + 
  geom_bar(position="fill") +
  theme(axis.text.x = element_text( vjust = 0.5, hjust=0.5)) +
  ggtitle("Income Distribution at term 60") +
  labs(x= "Income Categories" , y = "Percent") +
  scale_x
grid.arrange(plot_36,plot_60)

inc_vs_term= structable(loan_status~annual_inc_cat+term,data=inc_sub)

# Calculate percentage of defaults across terms.
prop.table(ftable(inc_vs_term),1)
# As seen from the comparison above, default rate is inversly proportional to income and 
# directly proportional to term. Hence, chances of default are higher, when low income people
# take loans with term of 60.

#Home ownership vs Loan Status
ggplot(inc_sub, aes(x= home_ownership, fill=loan_status)) + ggtitle("Home Ownership vs Status")+
  geom_bar(aes(y = (..count..)/sum(..count..)),  stat="count") +
  labs(x= "Home Ownership" , y = "Percent", fill= "Status") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))
#Rented borrowers have higher charged off loans

# Address State vs Loan status
ggplot(loan_master, aes(x= addr_state, fill=loan_status)) + ggtitle("Address State vs Status")+
  geom_bar(aes(y = (..count..)/sum(..count..)), stat="count") +
  labs(x= "State" , y = "Percent", fill= "Status") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

# Start - To be removed
loan_addr <- inc_sub[which(inc_sub$addr_state %in% c("CA","FL","NJ","NY","TX","NE")),]

# As seen below, charged off cases are near the average of 14.2% for most states, except for states
# like NE.
ggplot(loan_master, aes(x= addr_state, fill=loan_status)) + ggtitle("Address State vs Status")+
  geom_bar(position="fill") +
  labs(x= "State" , y = "Percent", fill= "Status") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))
# End - To be removed

#Purpose Vs Status
ggplot(chargedoffloan, aes(x= purpose)) + ggtitle("Charged off - Loan Purpose Distribution")+
  geom_bar(aes(y = (..count..)/sum(..count..)), stat="count") +
  labs(x= "Loan Purpose" , y = "Percent", fill= "Status") +
  scale_y_continuous(labels=percent)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

#dti Vs Status
ggplot(loan_master, aes(x= dti, fill=loan_status)) + ggtitle("DTI Distribution")+
  geom_histogram(binwidth = 5)+
  labs(x= "DTI" , y = "Loans", fill= "Status") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

#Dti for the subset which has outliers removed
ggplot(inc_sub, aes(x= dti, fill=loan_status)) + ggtitle("DTI Distribution")+
  geom_histogram(binwidth = 5)+
  labs(x= "DTI" , y = "Loans", fill= "Status") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))
# This trend is similar to overall dataset

#Open Accounts Vs Status
boxplot(loan_master$open_acc)
summary(loan_master$open_acc)
#We can see that the no of accounts greater than 20~22 are outliers. 

ggplot(loan_master, aes(x= open_acc, fill=loan_status)) + ggtitle("Open Accounts Distribution")+
  geom_bar(aes(y = (..count..)), stat="count") +
  labs(x= "Open Accounts" , y = "Loans", fill= "Status") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

#There is steady increase in no of charged off as the no of accounts increases initially and reduces gradually beyond 10 acccounts

#Sub Grade vs Loan Status
ggplot(loan_master, aes(x= loan_master$sub_grade, fill=loan_status)) + ggtitle("Sub Grade Distribution")+
  geom_bar(aes(y = (..count..)), stat="count") +
  labs(x= "Open Accounts" , y = "Loans", fill= "Status") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

#Revol_util Analysis
ggplot(loan_master,aes(x=loan_status,y=revol_util)) + geom_boxplot() + ggtitle("Revol util vs Loan status")
#Observation: Charged off loans has higher utilisation of the revolving credit

#For charged off loans
ggplot(chargedoffloan, aes(x= revol_util, y = (..count..))) + ggtitle("Charged off loans revol utilization")+
  #geom_bar(aes(y = (..count..)), stat="count") +
  geom_histogram(binwidth = 5)+
  labs(x= "revol util" , y = "count") +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))
#We can see in histogram that concentration of charged off loans are higher with revol_util higher percentages


#Finding relation of grades with annual income
ggplot(inc_sub,aes(x=grade,y=annual_inc)) + geom_boxplot() +
  ggtitle("Annual Income by Grade")+
  labs(x = "Grade",
       y = "Annual income")

# Strangely, verified source has more default rates.
ggplot(loan_master,aes(x=loan_master$verification_status,fill=factor(loan_master$loan_status))) +
  geom_bar(position = "fill") +
  ggtitle("Verification status vs Loan status") +
  labs(x="Verification status",y="Percent") +
  labs(fill= "Loan Status")

# Let's combine Source verified and verified.
inc_sub$verified <- ifelse(inc_sub$verification_status %in% 
                             c("Source Verified", "Verified"), "Verified","Not verified")
# Still, the verified loans perform bad when comared to the not verified ones.
ggplot(inc_sub,aes(x=verified,fill=factor(loan_status))) +
  geom_bar(position = "fill") +
  ggtitle("Verification status vs Loan status") +
  labs(x="Verification status",y="Percent") +
  labs(fill= "Loan Status")

verif_vs_status = structable(loan_status~verified,data=inc_sub)
prop.table(ftable(verif_vs_status),1)

#Verification status vs delinquencies in past 2 years for charged off loans
ggplot(chargedoffloan, aes(x= verification_status , group= delinq_2yrs)) + ggtitle("Verification Status vs Delinquencies")+
  geom_bar(aes(y = (delinq_2yrs), fill = factor(verification_status)), stat="identity") +
  labs(x = "Verification Status",  y = "Delinquencies in past 2 years", fill= "Status") 

################# Writing file for Tableau representation #################
write.csv(loan_master,file = "loan_master.csv", na = "")
write.csv(inc_sub,file = "loan_data_outlier_treated.csv", na = "")
write.csv(chargedoffloan,file = "chargedoffloan.csv", na = "")

###################### End of R Code  #######################
#############################################################
