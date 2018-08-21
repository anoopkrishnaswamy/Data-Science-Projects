#Set Working Directory to the folder which contains the data files using setwd("") option

#install packages and load library of tidyr, dplyr and stringr
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
library(tidyr)
library(dplyr)
library(stringr)


#loading the companies and rounds2 data   
companies<- read.delim("companies.txt", sep = "\t", stringsAsFactors = FALSE)
rounds2<-read.csv("rounds2.csv", stringsAsFactors = FALSE)

# -------------------------------------------------------------------  
#Cleaning Data
  
#Change the name of the Key ID in rounds file to permalink for easy merge.
colnames(rounds2)[1] <- "permalink" 
  
#Change the case of field Permalink to lower
companies$permalink <- tolower(companies$permalink)
rounds2$permalink <- tolower(rounds2$permalink)
  
# ------------------------------------------------------------------- 
#Checkpoint 1    
#How many unique companies are present in rounds2?
count(distinct(rounds2, permalink))
### Alternate method (using base functions) 
rounds_unq <- length(unique(rounds2$permalink))
rounds_unq  

#How many unique companies are present in the companies file?
count(distinct(companies, permalink))
### Alternate Method (using base functions)
Comp_unq <- length(unique(companies$permalink))
Comp_unq  

  
#In the companies data frame, which column can be used as the unique key for each company? 
#Write the name of the column.
#Answer : Permalink 
  
  
#Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
diff_count <- setdiff(companies$permalink ,rounds2$permalink )
length(diff_count)  
#Answer: N
  
  
#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame ?
  
# Use Inner Join to merge both the data frames using the common column "company_permalink"
  
master_frame <- merge(rounds2,companies,by="permalink")
  
  
#Use Length to find the number of observations in the merged data frame
length(master_frame$permalink)
  
# ------------------------------------------------------------------- 

#Checkpoint 2

#Create a Data Frame with Type of Funding and Avg Funding Received
funding_avg<-group_by(master_frame,funding_round_type)
funding_summ<-summarise(funding_avg, raised_amount_avg=mean(raised_amount_usd, na.rm=T)) # Ignoring the rows where amount is NULL

###Currently, for calculating the Avg, even companies which has 0 value for a particular funding type are included. Should we exclude them as per problem statement?

#Average funding amount of venture type
filter(funding_summ, funding_round_type=='venture')

#Average funding amount of angel type
filter(funding_summ, funding_round_type=='angel')

#Average funding amount of seed type
filter(funding_summ, funding_round_type=='seed')

#Average funding amount of private equity type
filter(funding_summ, funding_round_type=='private_equity')

#Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, which investment type is the most suitable for them?
colnames(funding_summ)[2] <- "Avg_Funding_Amt"  #Naming the column
funding_summary_in_millions <- mutate(funding_summ , avg_fund_in_million = Avg_Funding_Amt / 1000000)  #Converting the average funded amount to millions
Suitable_funding_type <- filter(funding_summary_in_millions , avg_fund_in_million >= 5 & avg_fund_in_million <= 15)
Suitable_funding_type

#Answer: Venture



# ------------------------------------------------------------------- 

#Checkpoint 3
#Create a Data Frame with Total Funding Received for each country for Venture type of funding
country_avg<-group_by(master_frame,country_code) %>% filter( funding_round_type=='venture' && country_code!='')
country_summ<-summarise(country_avg, Total_Funding_Received=sum(raised_amount_usd, na.rm=T))
View(country_summ)

###top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)

top9 <- head(arrange(country_summ, desc(Total_Funding_Received)), n = 9) 
View(top9)

#Referring the list of countries with English as official language , we arrived at the top three English-speaking countries in the data frame top9.
#Answer: 
#1. United States
#2. United Kingdom
#3. India


# ------------------------------------------------------------------- 


#Checkpoint 4

#load mapping data into dataframe
mapping_wide<-read.csv("mapping.csv", stringsAsFactors = FALSE)


#Converting wide data to long format, removing NULL/ZERO Values and removing junk columns
mapping <- gather(mapping_wide, main_sector, Main_Sector_Value, 2:10)
mapping<-mapping[!(mapping$Main_Sector_Value == 0),]
mapping<-mapping[,-3]

#Renaming the columns for easier merging of data
colnames(mapping)[1] <- "primary_sector" 


#Extract the primary sector of each company from the category_list column in the Master Frame dataframe and store it in separate column "primary_sector"
master_frame$primary_sector <- sapply(str_split(master_frame$category_list, pattern = "\\|"), "[", 1)

# Mapping dataframe has 0 inplace of na in the category names. Hence correcting the data wherever applicable. 
# Also, we need to replace Enterprise 2.na  back to Enterprise 2.0 in the corrected data
#The case on the common column "primary_sector is changed to LOWER for proper merging

mapping$primary_sector<-tolower(str_replace(mapping$primary_sector,"0","na"))
mapping$primary_sector<-str_replace(mapping$primary_sector,"2.na","2.0")
master_frame$primary_sector<-tolower(master_frame$primary_sector)

#merge the mapping df to include the main category in master_frame
master_frame_new <- merge(master_frame, mapping, by = "primary_sector", all=TRUE)

#Rearranging columns for better readability of merged file
master_frame_new <- master_frame_new[c(2:9,1,17,10:16)]


# ------------------------------------------------------------------- 

#Checkpoint 5
#create dataframes D1, D2 and D3 for each of the country USA, Great Britain and India respectively for Funding Type venture and funding amount between 5 and 15 million with all the attributes of master_frame
D1 <- filter(master_frame_new, funding_round_type=='venture' , country_code=='USA', raised_amount_usd>=5000000, raised_amount_usd<=15000000, main_sector != 'Blanks', main_sector != 'NA') 
D2 <- filter(master_frame_new, funding_round_type=='venture' , country_code=='GBR', raised_amount_usd>=5000000, raised_amount_usd<=15000000, main_sector != 'Blanks', main_sector != 'NA') 
D3 <- filter(master_frame_new, funding_round_type=='venture' , country_code=='IND', raised_amount_usd>=5000000, raised_amount_usd<=15000000, main_sector != 'Blanks', main_sector != 'NA') 


# To find the Top Three Sectors along with the Number of Investments in each of these sectors, we use the Summarise function and sort the data in descending order of No of Investments

D1_grp<-group_by(D1,main_sector)
D1_summ<-summarise(D1_grp, No_of_Investments = n(), Total_Funding_Amount = sum(raised_amount_usd))
D1_summ<-arrange(D1_summ,desc(No_of_Investments))


D2_grp<-group_by(D2,main_sector)
D2_summ<-summarise(D2_grp, No_of_Investments = n(), Total_Funding_Amount = sum(raised_amount_usd))
D2_summ<-arrange(D2_summ,desc(No_of_Investments))


D3_grp<-group_by(D3,main_sector)
D3_summ<-summarise(D3_grp, No_of_Investments = n(), Total_Funding_Amount = sum(raised_amount_usd))
D3_summ<-arrange(D3_summ,desc(No_of_Investments))



#merge the count and sum of investments in to main dataframes of each country

D1 <- merge(D1, D1_summ, by = "main_sector")
D2 <- merge(D2, D2_summ, by = "main_sector")
D3 <- merge(D3, D3_summ, by = "main_sector")


#------------

#Find the Total Number of investments and Total Amount Invested (in USD) for each of the country chosen for Venture Funding Type and Funding amount between 5 to 15 million USD

D1_sum<-sum(D1$raised_amount_usd)
D1_sum
D1_count<-count(D1)
D1_count

D2_sum<-sum(D2$raised_amount_usd)
D2_sum
D2_count<-count(D2)
D2_count

D3_sum<-sum(D3$raised_amount_usd)
D3_sum
D3_count<-count(D3)
D3_count

### Alternate Method


#Q1
#Total number of Investments (count) for each country
length(D1$raised_amount_usd)
length(D2$raised_amount_usd)
length(D3$raised_amount_usd)

#Q2
#sum of Investments (count) for each country
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)

#Q3-Q8 finding top 3 main sectors for each country
head(arrange(D1_summ, desc(No_of_Investments)),n=3)
head(arrange(D2_summ, desc(No_of_Investments)),n=3)
head(arrange(D3_summ, desc(No_of_Investments)),n=3)


### For the purpose of finding the company which received the highest investment in the top two sectors for each country, we've grouped the data by the company name as some companies have received Venture funding more than one time in the same investment bracket.

#Q9 which company received the highest investment for top sector
#For country 1- USA, the top sector is Others
D1_S1_sort<-filter(D1, main_sector=="Others") #Creating a subset containing companies belonging to Others sector.
D1_S1_sort<-group_by(D1_S1_sort, name)
D1_S1_sort_summ<-summarise(D1_S1_sort,Amount_Raised=sum(raised_amount_usd))
D1_S1_sort_summ<-arrange(D1_S1_sort_summ, desc(Amount_Raised))
D1_S1_sort_summ[which.max(D1_S1_sort_summ$Amount_Raised),"name"] # finding the company which has highest amt of investments



#For country 2- GBR, the top sector is Others
D2_S1_sort<-filter(D2, main_sector=="Others") #Creating a subset containing companies belonging to Others sector.
D2_S1_sort<-group_by(D2_S1_sort, name)
D2_S1_sort_summ<-summarise(D2_S1_sort,Amount_Raised=sum(raised_amount_usd))
D2_S1_sort_summ<-arrange(D2_S1_sort_summ, desc(Amount_Raised))
D2_S1_sort_summ[which.max(D2_S1_sort_summ$Amount_Raised),"name"] # finding the company which has highest amt of investments


#For country 3- IND, the top sector is Others
D3_S1_sort<-filter(D3, main_sector=="Others") #Creating a subset containing companies belonging to Others sector.
D3_S1_sort<-group_by(D3_S1_sort, name)
D3_S1_sort_summ<-summarise(D3_S1_sort,Amount_Raised=sum(raised_amount_usd))
D3_S1_sort_summ<-arrange(D3_S1_sort_summ, desc(Amount_Raised))
D3_S1_sort_summ[which.max(D3_S1_sort_summ$Amount_Raised),"name"] # finding the company which has highest amt of investments



#Q10 which company received the highest investment for second sector

#For country 1- USA, the second sector is Social, Finance, Analytics & Advertising

D1_S2_sort<-filter(D1, main_sector == "Social..Finance..Analytics..Advertising") #Creating a subset containing companies belonging to second best sector.
D1_S2_sort<-group_by(D1_S2_sort, name)
D1_S2_sort_summ<-summarise(D1_S2_sort,Amount_Raised=sum(raised_amount_usd))
D1_S2_sort_summ<-arrange(D1_S2_sort_summ, desc(Amount_Raised))
D1_S2_sort_summ[which.max(D1_S2_sort_summ$Amount_Raised),"name"] # Finding the company with highest amt of investments


#For country 2- GBR, the second sector is Social, Finance, Analytics & Advertising

D2_S2_sort<-filter(D2, main_sector == "Social..Finance..Analytics..Advertising") #Creating a subset containing companies belonging to second best sector.
D2_S2_sort<-group_by(D2_S2_sort, name)
D2_S2_sort_summ<-summarise(D2_S2_sort,Amount_Raised=sum(raised_amount_usd))
D2_S2_sort_summ<-arrange(D2_S2_sort_summ, desc(Amount_Raised))
D2_S2_sort_summ[which.max(D2_S2_sort_summ$Amount_Raised),"name"] # Finding the company with highest amt of investments



#For country 3- IND, the second sector is Social, Finance, Analytics & Advertising

D3_S2_sort<-filter(D3, main_sector == "Social..Finance..Analytics..Advertising") #Creating a subset containing companies belonging to second best sector.
D3_S2_sort<-group_by(D3_S2_sort, name)
D3_S2_sort_summ<-summarise(D3_S2_sort,Amount_Raised=sum(raised_amount_usd))
D3_S2_sort_summ<-arrange(D3_S2_sort_summ, desc(Amount_Raised))
D3_S2_sort_summ[which.max(D3_S2_sort_summ$Amount_Raised),"name"] # Finding the company with highest amt of investments

# ------------------------------------------------------------------- 

#Checkpoint 6

#Export dataframes required for plots in Tableau

#Plot 1
write.csv(master_frame_new, "master_frame_new.csv")

#Plot 2
write.csv(top9, "top9.csv")

#Plot 3
#combine the Dataframes D1, D2 and D3
countries_df <- rbind(D1, D2, D3)
write.csv(countries_df, "countries_df.csv")

# ------------------------------------------------------------------- 