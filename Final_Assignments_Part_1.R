# ---
# title: "MSCA 31007 Statistical Analysis - Final Assignment Part 1"
# author: Prinu Mathew, Qingwei Zhang"
# start_date: "11/25/2022"
# last_revision_date: "12/09/2022"
# ---

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('GGally')
install.packages('Amelia')
install.packages('Hmisc')
install.packages('MASS')
install.packages('faraway')
install.packages('lmtest')
install.packages('reshape2')
install.packages("LambertW")
library(LambertW)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally) ##plot correlation
library(Amelia)
library(Hmisc)
library(lmtest)
library(MASS)
library(faraway)
library(reshape2)

# setwd('C:\\Prinu\\Personal\\Studies\\Masters\\UChicago\\After Admission\\Courses\\Statistical Analysis\\Assignments\\Final Assignment - Part1\\data')

setwd("C:/Users/QZHAN101/Downloads/play/UCHICAGO/MSCA_31007_ON01_Autumn_2022_Statistical_Analysis/Statistical_Analysis_final_project")

#*******************************Step 1: Import and prepare the data for analysis*******************************#


#1.1 Bring the data into R
#Using R, bring all five datasets into your workspace.  
# Notice that all five datasets have 21 columns, 
#with similar (but not identical) column names.  Please use the following
# vector of column names to standardize the data.

brooklyn_2016 <- read.csv('2016_brooklyn.csv')
brooklyn_2017 <- read.csv('2017_brooklyn.csv')
brooklyn_2018 <- read.csv('2018_brooklyn.csv')
brooklyn_2019 <- read.csv('2019_brooklyn.csv')
brooklyn_2020 <- read.csv('2020_brooklyn.csv')

#removing first 'n' rows from csv files as those are description entries
brooklyn_2016 <- tail(brooklyn_2016, -4)
brooklyn_2017 <- tail(brooklyn_2017, -4)
brooklyn_2018 <- tail(brooklyn_2018, -4)
brooklyn_2019 <- tail(brooklyn_2019, -4)
brooklyn_2020 <- tail(brooklyn_2020, -7)

#define column names for each dataframes
colnames <- c(
  "borough",
  "neighborhood",
  "bldclasscat",
  "taxclasscurr",
  "block",
  "lot",
  "easement",
  "bldclasscurr",
  "address",
  "aptnum",
  "zip",
  "resunits",
  "comunits",
  "totunits",
  "landsqft",
  "grosssqft",
  "yrbuilt",
  "taxclasssale",
  "bldclasssale",
  "price",
  "date"
)

colnames(brooklyn_2016) <- colnames
colnames(brooklyn_2017) <- colnames
colnames(brooklyn_2018) <- colnames
colnames(brooklyn_2019) <- colnames
colnames(brooklyn_2020) <- colnames

#define rows names for each dataframes
rownames(brooklyn_2016) <- 1:nrow(brooklyn_2016)
rownames(brooklyn_2017) <- 1:nrow(brooklyn_2017)
rownames(brooklyn_2018) <- 1:nrow(brooklyn_2018)
rownames(brooklyn_2019) <- 1:nrow(brooklyn_2019)
rownames(brooklyn_2020) <- 1:nrow(brooklyn_2020)


#1.2 Join the data and make it usable for analysis
#There are some data cleaning steps and transformations that would be 
# necessary or helpful to almost any analysis.  
#Consider your data carefully, column by column.  Re-format, change data 
# types, pay attention to white space and 
#special characters.  Datasets kept over multiple years are not always 
# created in exactly the same way, so take 
#care that your data is standardized across years.  When you are done, 
# create a new datasets which joins all 
#five yearly datasets.  This step will likely take a large amount of time.
# Do not assume you can complete it quickly.  
#The resulting dataset should have roughly 119,000 rows.


#remove empty rows
brooklyn_2016 <- brooklyn_2016[!apply(brooklyn_2016 == "", 1, all),]
brooklyn_2017 <- brooklyn_2017[!apply(brooklyn_2017 == "", 1, all),]
brooklyn_2018 <- brooklyn_2018[!apply(brooklyn_2018 == "", 1, all),]
brooklyn_2019 <- brooklyn_2019[!apply(brooklyn_2019 == "", 1, all),]
brooklyn_2020 <- brooklyn_2020[!apply(brooklyn_2020 == "", 1, all),]

#since we removed blank rows from brooklyn 2019 & 2020, we reduced 
# from 119351 rows to 117151
total_observations <- nrow(brooklyn_2016) + 
  nrow(brooklyn_2017) + 
  nrow(brooklyn_2018) + 
  nrow(brooklyn_2019) +
  nrow(brooklyn_2020)

#define functions. 
#This one is for trim white space.
func.df.trim <- function(df, colnames) {
  for (colname in colnames) {
    df[[colname]] <- trimws(df[[colname]])
  }
  
  return(df)
}

func.df.replace <- function(df, colnames, oldvalue, newvalue) {
  for (colname in colnames) {
    df[[colname]][df[[colname]] == oldvalue] <- newvalue
  }
  
  return(df)
}

# convert element to integer.
func.df.ToInt <- function(df, colnames) {
  for (colname in colnames) {
    df[[colname]] <- as.integer(df[[colname]])
  }
  
  return(df)
}

# remove the comma in elements, e.g., "1,003" will be "1003"
func.df.ToNum <- function(df, colnames) {
  for (colname in colnames) {
    df[[colname]] <- str_replace_all(df[[colname]], "[^0-9.]", "")
    df[[colname]] <- suppressWarnings(
      as.numeric(gsub(",", "", format(df[[colname]], scientific = F)))
    )
  }
  
  return(df)
}

# convert the date to a specific format.
func.df.ToDate <- function(df, colnames, format) {
  for (colname in colnames) {
    date_formatted <- as.POSIXct(df[[colname]], format=format)
    df[[colname]] <- as.Date(date_formatted, format=format)
  }
  
  return(df)
}

#trim leading and trailing white spaces for columns
brooklyn_2016 <- func.df.trim(brooklyn_2016, colnames)
brooklyn_2017 <- func.df.trim(brooklyn_2017, colnames)
brooklyn_2018 <- func.df.trim(brooklyn_2018, colnames)
brooklyn_2019 <- func.df.trim(brooklyn_2019, colnames)
brooklyn_2020 <- func.df.trim(brooklyn_2020, colnames)


#replace column value '-' with empty
columns_with_hyphen = c('borough','resunits','comunits','totunits','landsqft','grosssqft','taxclasssale')

brooklyn_2016 <- func.df.replace(brooklyn_2016, columns_with_hyphen, '-', '')
brooklyn_2017 <- func.df.replace(brooklyn_2017, columns_with_hyphen, '-', '')
brooklyn_2018 <- func.df.replace(brooklyn_2018, columns_with_hyphen, '-', '')
brooklyn_2019 <- func.df.replace(brooklyn_2019, columns_with_hyphen, '-', '')
brooklyn_2020 <- func.df.replace(brooklyn_2020, columns_with_hyphen, '-', '')

#change data types for following columns
brooklyn_2016 <- func.df.ToInt(brooklyn_2016,list('borough','taxclasscurr','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale','zip'))
brooklyn_2016 <- func.df.ToNum(brooklyn_2016,list('landsqft','grosssqft','price'))
brooklyn_2016 <- func.df.ToDate(brooklyn_2016,list('date'),format="%m/%d/%Y")
brooklyn_2016 <- brooklyn_2016 %>% filter(!is.na(price))

brooklyn_2017 <- func.df.ToInt(brooklyn_2017,list('borough','taxclasscurr','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale','zip'))
brooklyn_2017 <- func.df.ToNum(brooklyn_2017,list('landsqft','grosssqft','price'))
brooklyn_2017 <- func.df.ToDate(brooklyn_2017,list('date'),format="%m/%d/%y")
brooklyn_2017 <- brooklyn_2017 %>% filter(!is.na(price))

brooklyn_2018 <- func.df.ToInt(brooklyn_2018,list('borough','taxclasscurr','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale','zip'))
brooklyn_2018 <- func.df.ToNum(brooklyn_2018,list('landsqft','grosssqft','price'))
brooklyn_2018 <- func.df.ToDate(brooklyn_2018,list('date'),format="%m/%d/%y")
brooklyn_2018 <- brooklyn_2018 %>% filter(!is.na(price))

brooklyn_2019 <- func.df.ToInt(brooklyn_2019,list('borough','taxclasscurr','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale','zip'))
brooklyn_2019 <- func.df.ToNum(brooklyn_2019,list('landsqft','grosssqft','price'))
brooklyn_2019 <- func.df.ToDate(brooklyn_2019,list('date'),format="%m/%d/%y")
brooklyn_2019 <- brooklyn_2019 %>% filter(!is.na(price))

brooklyn_2020 <- func.df.ToInt(brooklyn_2020,list('borough','taxclasscurr','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale','zip'))
brooklyn_2020 <- func.df.ToNum(brooklyn_2020,list('landsqft','grosssqft','price'))
brooklyn_2020 <- func.df.ToDate(brooklyn_2020,list('date'),format="%m/%d/%y")
brooklyn_2020 <- brooklyn_2020 %>% filter(!is.na(price))

#After doing data cleaning for brooklyn 2019 & 2020, we reduced from 117151 rows to 107270
total_observations_after_cleanup <- nrow(brooklyn_2016) + 
  nrow(brooklyn_2017) + 
  nrow(brooklyn_2018) + 
  nrow(brooklyn_2019) +
  nrow(brooklyn_2020)

#merge the dataframes
brooklyn_2016_2020_list <- list(
  brooklyn_2016, 
  brooklyn_2017, 
  brooklyn_2018, 
  brooklyn_2019, 
  brooklyn_2020
)
brooklyn_2016_2020 <- brooklyn_2016_2020_list %>% reduce(full_join)
remove(brooklyn_2016_2020_list)



#1.3 Filter the data and make transformations specific to this analysis 

#For the purposes of this analysis, we will only consider purchases of single-family residences and single-unit apartments 
#or condos.  Restrict the data to purchases where the building class at the time of sale starts with ‘A’ or ‘R’ and where 
#the number of total units and the number of residential units are both 1.  Additionally restrict the data to observations 
#where gross square footage is more than 0 and sale price is non-missing.  The resulting dataset should have roughly 19,000 rows. 


#filter observations considering purchases of single-family residences and single-unit apartments or condos
#Restrict the data to purchases where the building class at the time of sale starts with ‘A’ or ‘R’
brooklyn_2016_2020_final <- brooklyn_2016_2020 %>% 
  filter(str_detect(bldclasssale, "^A") | str_detect(bldclasssale, "^R"))

#the number of total units and the number of residential units are both 1
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(resunits == 1 & totunits == 1)

#additionally restrict the data to observation where gross square footage is more than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(grosssqft > 0 & !is.na(grosssqft))

#additionally restrict the data to observation where sale price is non-missing
#brooklyn_2016_2020_final[["price"]][is.na(brooklyn_2016_2020_final[["price"]])] <- 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(!is.na(price))

#additionally restrict the data to observation where Year Built is more than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(yrbuilt > 0)

#additionally restrict the data to observation where price is less than 100 million. Anything outside of that, I would consider those as outliers
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(price <= 10000000)

#additionally restrict the data to observation where grosssqft is less than 20k. Anything outside of that, I would consider those as outliers
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(grosssqft < 20000)


#*******************************Step 2: EDA and feature engineering *******************************#
#Your goal will be to use linear regression to explain Brooklyn housing prices within the 2016-2020 window.  
#You will be asked to make predictions for the sale prices within the dataset.  You are encouraged to think of ways 
#to get the most explanatory power out of your current variables. 


#2.1 Exploratory data analysis 

#Consider price as a potential response variable.  Examine how it is distributed, and how it associates with the other variables 
#in your data.  Think about how you would use these other variables to explain price.  Consider whether each variable should 
#enter as a continuous numeric predictor, or as a factor.  Consider transformations of your response variable, transformations 
#of your predictors, or both.  Use this exploratory data analysis to revisit your initial data cleaning steps, which might need revision. 


#2.1.1.1 - Statistics summary
dim(brooklyn_2016_2020_final)
summary(brooklyn_2016_2020_final)


#2.1.1.2 - Check for any NA’s in the dataframe
missmap(brooklyn_2016_2020_final,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)
colSums(is.na(brooklyn_2016_2020_final))


#2.1.1.3 - Data Cleansing — Handle missing data
brooklyn_2016_2020_final[["taxclasscurr"]][str_detect(brooklyn_2016_2020_final[["bldclasssale"]], "^A") & is.na(brooklyn_2016_2020_final[["taxclasscurr"]])] <- 1
brooklyn_2016_2020_final[["taxclasscurr"]][str_detect(brooklyn_2016_2020_final[["bldclasssale"]], "^R") & is.na(brooklyn_2016_2020_final[["taxclasscurr"]])] <- 2
brooklyn_2016_2020_final[["comunits"]][is.na(brooklyn_2016_2020_final[["comunits"]])] <- 0


#2.1.1.4 - Correlations
#A positive correlation indicates the extent to which those variables increase or decrease in parallel; 
#a negative correlation indicates the extent to which one variable increases as the other decreases.
ggcorr(brooklyn_2016_2020_final, label = T, hjust = 1, layout.exp = 3)



#The correlation between each independent variable with the target variable must not be weak. 
#However, the correlation between two independent variables must not be too strong. Multicollinearity occurs 
#when independent variables in a regression model are correlated. This correlation is a problem because 
#independent variables should be independent. If the degree of correlation between variables is high enough, 
#it can cause problems when you fit the model and interpret the results.

#By looking at the correlation coefficient of the independent variables 'taxclasscurr', 'taxclasssale'
#'landsqft', 'lot' with the target variable 'price' (0.1, 0.1, 0.1, 0.1 respectively) are weak correlations, 
#'therefore we can exclude these two independent variable from our model.


#2.1.1.5 - visualizing the distribution of the target variable 'price'
brooklyn_2016_2020_final %>% 
  ggplot(aes(price)) +
  stat_density() + 
  theme_bw()


#2.1.1.6 - effect of the predictor variables on target variable 'price'
brooklyn_2016_2020_final %>%
  dplyr::select(c(price,zip,neighborhood,block,grosssqft,landsqft,yrbuilt,bldclasssale,taxclasssale)) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Price ($1000s)") +
  theme_minimal()


#2.1.3.1 - Additionally restrict the data to observation where price is greater than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(price > 0 & !is.na(price))

#2.1.3.2 - Additionally restrict the data to observation where zip is greater than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(zip > 0)


#2.1.3.3 - visualizing the distribution of the target variable 'price'
brooklyn_2016_2020_final %>% 
  ggplot(aes(price)) +
  stat_density() + 
  theme_bw()

#2.1.3.4 - Create a histogram of housing prices
ggplot(data=brooklyn_2016_2020_final) + geom_histogram(mapping = aes(price))
ggplot(data=brooklyn_2016_2020_final) +                         
  geom_histogram(mapping = aes(price/100000), 
                 breaks=seq(0, 7, by = 1), col="red", fill="lightblue") + 
  geom_density(mapping = aes(x=price/100000, y = (..count..)))  +   
  labs(title="Housing Prices in Brooklyn, NY (in $100,000)", 
       x="Sale Price of Individual Homes/Condos")   

ggplot(data=brooklyn_2016_2020_final) + geom_point(mapping= aes(x=grosssqft, y=price))
ggplot(data=brooklyn_2016_2020_final) + geom_point(mapping= aes(x=log(grosssqft), y=price))
ggplot(data=brooklyn_2016_2020_final) + geom_point(mapping= aes(x=yrbuilt, y=price))
ggplot(data=brooklyn_2016_2020_final) + geom_point(mapping= aes(x=price, y=bldclasssale))
ggplot(data=brooklyn_2016_2020_final) + geom_point(mapping= aes(x=price, y=zip))
ggplot(data=brooklyn_2016_2020_final) + geom_point(mapping= aes(x=price, y=block))
ggplot(data=brooklyn_2016_2020_final) + geom_point(mapping= aes(x=price, y=neighborhood))


#2.1.3.5 - effect of the predictor variables on target variable 'price'
brooklyn_2016_2020_final %>%
  dplyr::select(c(price,zip,neighborhood,block,grosssqft,landsqft,yrbuilt,bldclasssale,taxclasssale)) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Price ($1000s)") +
  theme_minimal()


#2.1.4.1 - check the model summary without any transformation
brooklyn_2016_2020_final.lm.native <- lm(formula = price~
                                           factor(bldclasssale)+
                                           factor(zip)+
                                           grosssqft+
                                           block+
                                           yrbuilt,
                                         brooklyn_2016_2020_final)
brooklyn_2016_2020_final.lm.native.summary <- summary(brooklyn_2016_2020_final.lm.native)
brooklyn_2016_2020_final.lm.native.summary

#get metrics from native model summary
brooklyn_2016_2020_final.lm.native.summary.metric <- data.frame(
  R2 = brooklyn_2016_2020_final.lm.native.summary$r.squared,
  Adj.R2 = brooklyn_2016_2020_final.lm.native.summary$adj.r.squared
)

RMSE_native_model <- sqrt(mean(brooklyn_2016_2020_final.lm.native.summary$residuals^2))
sprintf("Root Mean Square Error(RMSE) for Native Model : %s", round(RMSE_native_model, digits = 4))


#2.1.4.2 - Diagnostic plots with multiple predictors before transformation
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(brooklyn_2016_2020_final.lm.native)


#2.1.5.3 - Test IID assumptions

#Kolmogorov-Smirnov test for normality
hist(brooklyn_2016_2020_final.lm.native$residuals)
ks.test(brooklyn_2016_2020_final.lm.native$residuals/summary(brooklyn_2016_2020_final.lm.native)$sigma, pnorm)

#Breusch-Pagan test for normality heteroscedasticity
#The Breusch-Pagan test is used to determine whether or not heteroscedasticity is present in a regression model.

#The test uses the following null and alternative hypotheses:

#Null Hypothesis (H0): Homoscedasticity is present (the residuals are distributed with equal variance)
#Alternative Hypothesis (HA): Heteroscedasticity is present (the residuals are not distributed with equal variance)

#If the p-value of the test is less than some significance level (i.e. α = .05) then we reject the null hypothesis 
#and conclude that heteroscedasticity is present in the regression model.
bptest(brooklyn_2016_2020_final.lm.native)

#If the residuals become more spread out at higher values in the plot, this is a tell-tale sign that heteroscedasticity is present.
plot(brooklyn_2016_2020_final.lm.native$fitted.values, brooklyn_2016_2020_final.lm.native$residuals, col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#it still seems very clear that the constant variance assumption is violated.

#2.2 Pre-modeling and feature engineering

#Begin to construct linear models explaining price (or a transformation of price).  Consider your total model degrees of 
#freedom, your adjusted R^2, and your RMSE (root means square error).  Also consider whether your models show severe violations 
#of the OLS model assumptions, or merely slight violations of the OLS model assumptions.

#feature engineering

#2.2.1.1 - find the average price of each neighborhood and assign that price to price having 0 for those matching neighborhood
unique_neighborhoods <- as.list(unique(brooklyn_2016_2020_final$neighborhood))
unique_zips <- as.list(unique(brooklyn_2016_2020_final$zip))


#2.2.1.2 - Remove duplicates based on columns (neighborhood,bldclasscat,block,zip,resunits,totunits,landsqft,grosssqft,yrbuilt,bldclasssale,price)
brooklyn_2016_2020_final <- brooklyn_2016_2020_final[!duplicated(brooklyn_2016_2020_final, 
                                                                      by=c('neighborhood','bldclasscat','block',
                                                                           'zip','resunits','totunits','landsqft',
                                                                           'grosssqft','yrbuilt','bldclasssale',
                                                                           'price')), ]


#2.2.1.3 - find duplicate rows with same values for column (neighborhood,bldclasscat,block,zip,resunits,totunits,landsqft,grosssqft,yrbuilt,bldclasssale)
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  group_by(neighborhood,address,bldclasscat,block,zip,resunits,totunits,landsqft,grosssqft,yrbuilt,bldclasssale) %>% 
  mutate(duplicate_row = case_when(
    n()>1  ~ 1,
    TRUE  ~ 0
  ))
brooklyn_2016_2020_final <- func.df.ToInt(brooklyn_2016_2020_final,list('duplicate_row'))


#2.2.1.4 - find the average price of each neighborhood and corresponding zip and assign that average price to rows 
#that has the price between 1 and 10000 for those matching neighborhood and zip
func.df.adjPrice <- function(df, neighborhoods, zips) {
  df$adjprice = df$price
  colname_price = 'adjprice'
  colname_neighborhood = 'neighborhood'
  colname_zip = 'zip'
  
  group_by_neighbour_zip <- df %>% 
    filter(is.element(neighborhood, unique_neighborhoods) & price > 3000) %>%  
    group_by(neighborhood, zip) %>% 
    summarise(mean_price=floor(mean(price)), .groups = 'drop') %>%
    as.data.frame()
  
  for(i in 1:nrow(group_by_neighbour_zip)) {
    row <- group_by_neighbour_zip[i,]
    col_neighborhood_val <- row[,1]
    col_zip_val <- row[,2]
    col_mean_price_val <- row[,3]
    
    df[[colname_price]][df[[colname_price]] > 0 & 
                        df[[colname_price]] <= 3000 & 
                        df[[colname_neighborhood]] == col_neighborhood_val & 
                        df[[colname_zip]] == col_zip_val] <- col_mean_price_val
  }
  
  return(df)
}
brooklyn_2016_2020_final <- func.df.adjPrice(brooklyn_2016_2020_final, unique_neighborhoods, unique_zips)

#2.2.1.5 - Set landsqft = grosssqft if it's 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  mutate(adjlandsqft = case_when(
    landsqft <= 0  ~ grosssqft,
    TRUE  ~ landsqft
  ))
brooklyn_2016_2020_final <- func.df.ToNum(brooklyn_2016_2020_final,list('adjlandsqft'))

#2.2.2.1 - extract year from sale date
brooklyn_2016_2020_final$yrsold <- format(brooklyn_2016_2020_final$date,"%Y")
brooklyn_2016_2020_final <- func.df.ToInt(brooklyn_2016_2020_final,list('yrsold'))


#2.2.3.1 - adding decade as new column. Also as the year build increases the house price decreases
brooklyn_2016_2020_final$decade <- 10*floor(brooklyn_2016_2020_final$yrbuilt/10)
brooklyn_2016_2020_final$decade[brooklyn_2016_2020_final$decade<1970] <- 1970


#2.2.3.2 - adding yrbuiltbycategory by dividing the year built
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>%
  mutate(yrbuiltbycategory = case_when(
    yrbuilt <= 1900  ~ 0,
    yrbuilt > 1900 & yrbuilt <= 1970  ~ 1,
    yrbuilt > 1970 & yrbuilt <= 2000  ~ 2,
    yrbuilt > 2000  ~ 3
  ))
brooklyn_2016_2020_final <- func.df.ToInt(brooklyn_2016_2020_final,list('yrbuiltbycategory'))


#2.2.4.1 - group all "A5" "A1" "A9" "A4" "A3" "A2" "A0" "A7" "A6" to "A"
#2.2.4.2 - group all "R3" "R2" "R4" "R1" "R6" "RR" to "R"
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>%
  mutate(bldclasssalecategory = case_when(
    str_detect(bldclasssale, "^A")  ~ "0",
    str_detect(bldclasssale, "^R")  ~ "1"
  ))
brooklyn_2016_2020_final <- func.df.ToInt(brooklyn_2016_2020_final,list('bldclasssalecategory'))


#2.2.5.1 - log transformations of predictors
brooklyn_2016_2020_final$logage <- log(brooklyn_2016_2020_final$yrsold-brooklyn_2016_2020_final$yrbuilt+0.1)


#2.2.5.2 - check the model summary after transformations
brooklyn_2016_2020_final.lm.transform.v1 <- lm(formula = adjprice~
                                              factor(bldclasssalecategory)+
                                              factor(zip)+
                                              grosssqft+
                                              factor(decade)+
                                              factor(yrbuiltbycategory)+
                                              block+
                                              logage,
                                            brooklyn_2016_2020_final)
brooklyn_2016_2020_final.lm.transform.v1.summary <- summary(brooklyn_2016_2020_final.lm.transform.v1)
brooklyn_2016_2020_final.lm.transform.v1.summary


RMSE_transform_v1_model <- sqrt(mean(brooklyn_2016_2020_final.lm.transform.v1.summary$residuals^2))
sprintf("Root Mean Square Error(RMSE) for Transformed V1 Model : %s", round(RMSE_transform_v1_model, digits = 4))


#2.2.5.3 - Diagnostic plots with multiple predictors before transformation
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(brooklyn_2016_2020_final.lm.transform.v1)

#2.2.5.4 - Test IID assumptions

#Kolmogorov-Smirnov test for normality
hist(brooklyn_2016_2020_final.lm.transform.v1$residuals)
ks.test(brooklyn_2016_2020_final.lm.transform.v1$residuals/summary(brooklyn_2016_2020_final.lm.transform.v1)$sigma, pnorm)

#Breusch-Pagan test for normality heteroscedasticity
#The Breusch-Pagan test is used to determine whether or not heteroscedasticity is present in a regression model.

#The test uses the following null and alternative hypotheses:

#Null Hypothesis (H0): Homoscedasticity is present (the residuals are distributed with equal variance)
#Alternative Hypothesis (HA): Heteroscedasticity is present (the residuals are not distributed with equal variance)

#If the p-value of the test is less than some significance level (i.e. α = .05) then we reject the null hypothesis 
#and conclude that heteroscedasticity is present in the regression model.
bptest(brooklyn_2016_2020_final.lm.transform.v1)

#If the residuals become more spread out at higher values in the plot, this is a tell-tale sign that heteroscedasticity is present.
plot(fitted(brooklyn_2016_2020_final.lm.transform.v1), resid(brooklyn_2016_2020_final.lm.transform.v1), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


#2.2.5.5 - a scale-location plot
ggplot(brooklyn_2016_2020_final.lm.transform.v1, aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  ggtitle("Scale-Location plot : Standardized Residual vs Fitted values")

#2.2.5.6 - normal QQ plot
ggplot(brooklyn_2016_2020_final, aes(sample=brooklyn_2016_2020_final.lm.transform.v1$residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of BC Model")



#2.2.6.1 - Let's identify significance level from interaction between variables
summary(lm(formula = adjprice~
             (factor(bldclasssalecategory)+
                factor(zip)+
                factor(yrbuiltbycategory)+
                factor(decade)+
                grosssqft+
                adjlandsqft+  
                block+
                lot+
                logage+
                yrbuilt+
                borough+
                factor(bldclasscat)+
                factor(taxclasssale))^2,
           brooklyn_2016_2020_final))


#2.2.6.2 - New version of model by adding interaction terms from step 2.2.6.1
brooklyn_2016_2020_final.lm.transform.v2 <- lm(formula = adjprice~
                                              factor(bldclasssalecategory)*grosssqft+
                                              factor(zip)+
                                              logage,
                                            brooklyn_2016_2020_final)
brooklyn_2016_2020_final.lm.transform.v2.summary <- summary(brooklyn_2016_2020_final.lm.transform.v2)
brooklyn_2016_2020_final.lm.transform.v2.summary

RMSE_transform_v2_model <- sqrt(mean(brooklyn_2016_2020_final.lm.transform.v2.summary$residuals^2))
sprintf("Root Mean Square Error(RMSE) for Transformed V2 Model : %s", round(RMSE_transform_v2_model, digits = 4))



#2.3 - Reach a stopping point 

#2.3.1.1 - Applying Box-Cox transformations on strictly positive response variable
boxcox <- boxcox(brooklyn_2016_2020_final.lm.transform.v2, lambda = seq(-0.25, 0.75, by = 0.05), plotit = TRUE)

#find optimal lambda for Box-Cox transformation 
optimal_lambda_boxcox <- boxcox$x[which.max(boxcox$y)]

#Using the Box-Cox method, we see that λ=0.31565 is both in the confidence interval, and is extremely close to the maximum, 
#which suggests a transformation of the form

##(y^λ − 1)/λ = (y^0.31565 − 1)/0.31565
brooklyn_2016_2020_final$boxcoxprice <- ((brooklyn_2016_2020_final$adjprice^optimal_lambda_boxcox - 1) / optimal_lambda_boxcox)


#2.3.1.2 - New version of model by adding Box-Cox transformations from step 2.2.7.1
brooklyn_2016_2020_final.lm.transform.v3 <- lm(formula = boxcoxprice~
                                                 grosssqft+sqrt(grosssqft)+
                                                 factor(zip)+
                                                 factor(decade)*yrbuilt,
                                               brooklyn_2016_2020_final)
brooklyn_2016_2020_final.lm.transform.v3.summary <- summary(brooklyn_2016_2020_final.lm.transform.v3)
brooklyn_2016_2020_final.lm.transform.v3.summary


#2.3.1.3 - un-transform the response variable from step 2.2.7.2
invBoxCox <- function(x, lambda)
  if (lambda == 0) exp(x) else (lambda*x + 1)^(1/lambda)

# un-transform’ your predictions, manually compute RMSE, and compare that to your first model.
brooklyn_2016_2020_final$boxcoxprice_predicted <- predict(brooklyn_2016_2020_final.lm.transform.v3, 
                                                          newdata= brooklyn_2016_2020_final)

brooklyn_2016_2020_final$price_predicted <- floor(invBoxCox(brooklyn_2016_2020_final$boxcoxprice_predicted, optimal_lambda_boxcox))
brooklyn_2016_2020_final$price_residuals <- brooklyn_2016_2020_final$adjprice - brooklyn_2016_2020_final$price_predicted

RMSE_transform_v3_model <- sqrt(mean(brooklyn_2016_2020_final$price_residuals^2))
sprintf("Root Mean Square Error(RMSE) for Transformed V3 Model : %s", round(RMSE_transform_v3_model, digits = 4))
