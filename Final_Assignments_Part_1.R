# title: Final Assignment Part 1
# author: Qingwei Zhang"


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

setwd("C:/Users/QZHAN101/Downloads/play/UCHICAGO/MSCA_31007_ON01_Autumn_2022_Statistical_Analysis/Statistical_Analysis_final_project")


# Step 1: Import and prepare the data for analysis
#1.1 Bring the data into R
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


# define column names
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
#The resulting data set should have roughly 119,000 rows.

#remove empty rows
brooklyn_2016 <- brooklyn_2016[!apply(brooklyn_2016 == "", 1, all),]
brooklyn_2017 <- brooklyn_2017[!apply(brooklyn_2017 == "", 1, all),]
brooklyn_2018 <- brooklyn_2018[!apply(brooklyn_2018 == "", 1, all),]
brooklyn_2019 <- brooklyn_2019[!apply(brooklyn_2019 == "", 1, all),]
brooklyn_2020 <- brooklyn_2020[!apply(brooklyn_2020 == "", 1, all),]

#since we removed blank rows from Brooklyn 2019 & 2020, we reduced 
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

# This will replace the old value with new value.
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
columns_with_hyphen = c(
  'borough',
  'resunits',
  'comunits',
  'totunits',
  'landsqft',
  'grosssqft',
  'taxclasssale'
  )

brooklyn_2016 <- func.df.replace(brooklyn_2016, columns_with_hyphen, '-', '')
brooklyn_2017 <- func.df.replace(brooklyn_2017, columns_with_hyphen, '-', '')
brooklyn_2018 <- func.df.replace(brooklyn_2018, columns_with_hyphen, '-', '')
brooklyn_2019 <- func.df.replace(brooklyn_2019, columns_with_hyphen, '-', '')
brooklyn_2020 <- func.df.replace(brooklyn_2020, columns_with_hyphen, '-', '')

#change data types to Integer for following columns
columns_To_Int <- list(
  'borough',
  'taxclasscurr',
  'block',
  'lot',
  'resunits',
  'comunits',
  'totunits',
  'yrbuilt',
  'taxclasssale',
  'zip'
)

#change data types to numerical for following columns
columns_To_Num <- list(
  'landsqft',
  'grosssqft',
  'price'
)

# start changing data type for all five data sets, 2016 to 2020.
brooklyn_2016 <- func.df.ToInt(brooklyn_2016,columns_To_Int)
brooklyn_2016 <- func.df.ToNum(brooklyn_2016,columns_To_Num)
brooklyn_2016 <- func.df.ToDate(brooklyn_2016,list('date'),format="%m/%d/%Y")
brooklyn_2016 <- brooklyn_2016 %>% filter(!is.na(price))

brooklyn_2017 <- func.df.ToInt(brooklyn_2017,columns_To_Int)
brooklyn_2017 <- func.df.ToNum(brooklyn_2017,columns_To_Num)
brooklyn_2017 <- func.df.ToDate(brooklyn_2017,list('date'),format="%m/%d/%y")
brooklyn_2017 <- brooklyn_2017 %>% filter(!is.na(price))

brooklyn_2018 <- func.df.ToInt(brooklyn_2018,columns_To_Int)
brooklyn_2018 <- func.df.ToNum(brooklyn_2018,columns_To_Num)
brooklyn_2018 <- func.df.ToDate(brooklyn_2018,list('date'),format="%m/%d/%y")
brooklyn_2018 <- brooklyn_2018 %>% filter(!is.na(price))

brooklyn_2019 <- func.df.ToInt(brooklyn_2019,columns_To_Int)
brooklyn_2019 <- func.df.ToNum(brooklyn_2019,columns_To_Num)
brooklyn_2019 <- func.df.ToDate(brooklyn_2019,list('date'),format="%m/%d/%y")
brooklyn_2019 <- brooklyn_2019 %>% filter(!is.na(price))

brooklyn_2020 <- func.df.ToInt(brooklyn_2020,columns_To_Int)
brooklyn_2020 <- func.df.ToNum(brooklyn_2020,columns_To_Num)
brooklyn_2020 <- func.df.ToDate(brooklyn_2020,list('date'),format="%m/%d/%y")
brooklyn_2020 <- brooklyn_2020 %>% filter(!is.na(price))

# to get the total observation number of rows.
total_observations_after_cleanup <- nrow(brooklyn_2016) + 
  nrow(brooklyn_2017) + 
  nrow(brooklyn_2018) + 
  nrow(brooklyn_2019) +
  nrow(brooklyn_2020)

#merge the data frames
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

#filter observations considering purchases of single-family residences
# and single-unit apartments or condos
#Restrict the data to purchases where the building class at the time
# of sale starts with ‘A’(ONE FAMILY DWELLINGS) or ‘R’(CONDOMINIUMS).
brooklyn_2016_2020_final <- brooklyn_2016_2020 %>% 
  filter(str_detect(bldclasssale, "^A") 
         | str_detect(bldclasssale, "^R"))

#the number of total units and the number of residential units are both 1
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(resunits == 1 & totunits == 1)

#additionally restrict the data to observation where gross square
# footage is more than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(grosssqft > 0 & !is.na(grosssqft))

#additionally restrict the data to observation where sale price is 
#non-missing
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(!is.na(price))

#additionally restrict the data to observation where Year Built is 
#more than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(yrbuilt > 0)

#additionally restrict the data to observation where price is 
#less than 100 million. Anything outside of that, I would consider 
#those as outliers
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(price <= 10000000)

#additionally restrict the data to observation where gross sqft is 
#less than 20k. Anything outside of that, I would consider those as outliers
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  filter(grosssqft < 20000)


# Step 2: EDA and feature engineering 

#2.1 Exploratory data analysis 
#2.1.1.1 - Statistics summary
dim(brooklyn_2016_2020_final)
summary(brooklyn_2016_2020_final)


#2.1.1.2 - Check for any NA’s in the dataframe
missmap(
  brooklyn_2016_2020_final,
  col=c('yellow','black'),
  y.at=1,
  y.labels='',legend=TRUE
)
colSums(is.na(brooklyn_2016_2020_final))


#2.1.1.3 - Data Cleansing — Handle missing data
brooklyn_2016_2020_final[["taxclasscurr"]][
  str_detect(brooklyn_2016_2020_final[["bldclasssale"]], "^A") 
  & is.na(brooklyn_2016_2020_final[["taxclasscurr"]])] <- 1

brooklyn_2016_2020_final[["taxclasscurr"]][
  str_detect(brooklyn_2016_2020_final[["bldclasssale"]], "^R") 
  & is.na(brooklyn_2016_2020_final[["taxclasscurr"]])] <- 2

brooklyn_2016_2020_final[["comunits"]][
  is.na(brooklyn_2016_2020_final[["comunits"]])] <- 0


#2.1.1.4 - Correlations
#A positive correlation indicates the extent to which those variables
# increase or decrease in parallel; 
#a negative correlation indicates the extent to which one variable 
#increases as the other decreases.
ggcorr(
  brooklyn_2016_2020_final, 
  label = T, hjust = 1, 
  legend.position="top", 
  layout.exp = 1
)

#The correlation between each independent variable with the target
# variable must not be weak. 
#However, the correlation between two independent variables must not
# be too strong. Multicollinearity occurs 
#when independent variables in a regression model are correlated. 
#This correlation is a problem because 
#independent variables should be independent. If the degree of 
#correlation between variables is high enough, 
#it can cause problems when you fit the model and interpret the results.

#By looking at the correlation coefficient of the independent 
#variables 'taxclasscurr', 'taxclasssale'
#'landsqft', 'lot' with the target variable 'price' 
#'(0.1, 0.1, 0.1, 0.1 respectively) are weak correlations, 
#'therefore we can exclude these two independent variable from our model.


#2.1.1.5 - visualizing the distribution of the target variable 'price'
brooklyn_2016_2020_final %>% 
  ggplot(aes(price)) +
  stat_density() + 
  theme_bw()


#2.1.1.6 - effect of the predictor variables on target variable 'price'
brooklyn_2016_2020_final %>%
  dplyr::select(
    c(price,
      zip,
      neighborhood,
      block,
      grosssqft,
      landsqft,
      yrbuilt,
      bldclasssale,
      taxclasssale
      )) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Price ($1000s)") +
  theme_minimal()


#2.1.3.1 - Additionally restrict the data to observation where 
#price is greater than 0
brooklyn_2016_2020_final <- 
  brooklyn_2016_2020_final %>% filter(price > 0 & !is.na(price))

#2.1.3.2 - Additionally restrict the data to observation where 
#zip is greater than 0
brooklyn_2016_2020_final <- 
  brooklyn_2016_2020_final %>% filter(zip > 0)


#2.1.3.3 - visualizing the distribution of the target variable 'price'
brooklyn_2016_2020_final %>% 
  ggplot(aes(price)) +
  stat_density() + 
  theme_bw()

#2.1.3.4 - Create a histogram of housing prices
ggplot(data=brooklyn_2016_2020_final) + 
  geom_histogram(mapping = aes(price))

ggplot(data=brooklyn_2016_2020_final) +                         
  geom_histogram(mapping = aes(price/100000), 
                 breaks=seq(0, 7, by = 1), col="red", fill="lightblue") + 
  geom_density(mapping = aes(x=price/100000, y = (..count..)))  +   
  labs(title="Housing Prices in Brooklyn, NY (in $100,000)", 
       x="Sale Price of Individual Homes/Condos")   

ggplot(data=brooklyn_2016_2020_final) + 
  geom_point(mapping= aes(x=grosssqft, y=price))

ggplot(data=brooklyn_2016_2020_final) + 
  geom_point(mapping= aes(x=log(grosssqft), y=price))

ggplot(data=brooklyn_2016_2020_final) + 
  geom_point(mapping= aes(x=yrbuilt, y=price))

ggplot(data=brooklyn_2016_2020_final) + 
  geom_point(mapping= aes(x=price, y=bldclasssale))

ggplot(data=brooklyn_2016_2020_final) + 
  geom_point(mapping= aes(x=price, y=zip))

ggplot(data=brooklyn_2016_2020_final) + 
  geom_point(mapping= aes(x=price, y=block))

ggplot(data=brooklyn_2016_2020_final) + 
  geom_point(mapping= aes(x=price, y=neighborhood))


#2.1.3.5 - effect of the predictor variables on target variable 'price'
brooklyn_2016_2020_final %>%
  dplyr::select(c(
    price,
    zip,
    neighborhood,
    block,
    grosssqft,
    landsqft,
    yrbuilt,
    bldclasssale,
    taxclasssale
    )) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Price ($1000s)") +
  theme_minimal()


#2.2 Pre-modeling and feature engineering

#Begin to construct linear models explaining price (or a transformation 
#of price).  Consider your total model degrees of 
#freedom, your adjusted R^2, and your RMSE (root means square error).  
#Also consider whether your models show severe violations 
#of the OLS model assumptions, or merely slight violations of the OLS 
#model assumptions.

#feature engineering

#2.2.1.1 - find the average price of each neighborhood and assign that
# price to price having 0 for those matching neighborhood
unique_neighborhoods <- as.list(unique(brooklyn_2016_2020_final$neighborhood))
unique_zips <- as.list(unique(brooklyn_2016_2020_final$zip))


#2.2.1.2 - Remove duplicates based on columns 
#(neighborhood,bldclasscat,block,zip,resunits,totunits,landsqft,
#grosssqft,yrbuilt,bldclasssale,price)
brooklyn_2016_2020_final <- brooklyn_2016_2020_final[
  !duplicated(brooklyn_2016_2020_final, 
  by=c('neighborhood','bldclasscat','block',
         'zip','resunits','totunits','landsqft',
         'grosssqft','yrbuilt','bldclasssale',
         'price')
  ), ]


#2.2.1.3 - find duplicate rows with same values for column 
#(neighborhood,bldclasscat,block,zip,resunits,totunits,landsqft,
#grosssqft,yrbuilt,bldclasssale)
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  group_by(
    neighborhood,address,bldclasscat,block,zip,resunits,
    totunits,landsqft,grosssqft,yrbuilt,bldclasssale) %>% 
  mutate(duplicate_row = case_when(
    n()>1  ~ 1,
    TRUE  ~ 0
  ))
brooklyn_2016_2020_final <- func.df.ToInt(
  brooklyn_2016_2020_final,list('duplicate_row')
)


#2.2.1.4 - find the average price of each neighborhood and 
#corresponding zip and assign that average price to rows 
#that has the price between 1 and 10000 for those matching 
#neighborhood and zip
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

brooklyn_2016_2020_final <- func.df.adjPrice(
  brooklyn_2016_2020_final, unique_neighborhoods, unique_zips
)

#2.2.1.5 - Set landsqft = grosssqft if it's 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% 
  mutate(adjlandsqft = case_when(
    landsqft <= 0  ~ grosssqft,
    TRUE  ~ landsqft
  ))

brooklyn_2016_2020_final <- 
  func.df.ToNum(brooklyn_2016_2020_final,list('adjlandsqft'))

#2.2.2.1 - extract year from sale date
brooklyn_2016_2020_final$yrsold <- 
  format(brooklyn_2016_2020_final$date,"%Y")

brooklyn_2016_2020_final <- 
  func.df.ToInt(brooklyn_2016_2020_final,list('yrsold'))


#2.2.3.1 - adding decade as new column. Also as the year build 
#increases the house price decreases
brooklyn_2016_2020_final$decade <- 
  10*floor(brooklyn_2016_2020_final$yrbuilt/10)

brooklyn_2016_2020_final$decade[brooklyn_2016_2020_final$decade<1970] <- 1970


#2.2.3.2 - adding yrbuiltbycategory by dividing the year built
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>%
  mutate(yrbuiltbycategory = case_when(
    yrbuilt <= 1900  ~ 0,
    yrbuilt > 1900 & yrbuilt <= 1970  ~ 1,
    yrbuilt > 1970 & yrbuilt <= 2000  ~ 2,
    yrbuilt > 2000  ~ 3
  ))

brooklyn_2016_2020_final <- 
  func.df.ToInt(brooklyn_2016_2020_final,list('yrbuiltbycategory'))

#2.2.3.3 - adding quarter by extracting month from date
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>%
  mutate(quarter = case_when(
    is.element(format(date,"%m"), c("01", "02", "03"))  ~ 1,
    is.element(format(date,"%m"), c("04", "05", "06"))  ~ 2,
    is.element(format(date,"%m"), c("07", "08", "09"))  ~ 3,
    is.element(format(date,"%m"), c("10", "11", "12"))  ~ 4
  ))

brooklyn_2016_2020_final <- func.df.ToInt(brooklyn_2016_2020_final,list('quarter'))


#2.2.4.1 - group all "A5" "A1" "A9" "A4" "A3" "A2" "A0" "A7" "A6" to "A"
#2.2.4.2 - group all "R3" "R2" "R4" "R1" "R6" "RR" to "R"
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>%
  mutate(bldclasssalecategory = case_when(
    str_detect(bldclasssale, "^A")  ~ "0",
    str_detect(bldclasssale, "^R")  ~ "1"
  ))
brooklyn_2016_2020_final <- func.df.ToInt(
  brooklyn_2016_2020_final,list('bldclasssalecategory')
)


#2.2.5.1 - log transformations of predictors
brooklyn_2016_2020_final$logage <- 
  log(brooklyn_2016_2020_final$yrsold-brooklyn_2016_2020_final$yrbuilt+0.1)


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
#The Breusch-Pagan test is used to determine whether or not 
#heteroscedasticity is present in a regression model.

#The test uses the following null and alternative hypotheses:

#Null Hypothesis (H0): Homoscedasticity is present (the residuals 
#are distributed with equal variance)
#Alternative Hypothesis (HA): Heteroscedasticity is present (the 
#residuals are not distributed with equal variance)

#If the p-value of the test is less than some significance level 
#(i.e. α = .05) then we reject the null hypothesis 
#and conclude that heteroscedasticity is present in the regression model.
bptest(brooklyn_2016_2020_final.lm.transform.v1)

#If the residuals become more spread out at higher values in the 
#plot, this is a tell-tale sign that heteroscedasticity is present.
plot(fitted(brooklyn_2016_2020_final.lm.transform.v1), 
     resid(brooklyn_2016_2020_final.lm.transform.v1), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)


#2.2.5.5 - a scale-location plot
ggplot(brooklyn_2016_2020_final.lm.transform.v1, 
  aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  ggtitle("Scale-Location plot : Standardized Residual vs Fitted values")

#2.2.5.6 - normal QQ plot
ggplot(brooklyn_2016_2020_final, 
  aes(sample=brooklyn_2016_2020_final.lm.transform.v1$residuals)) +
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
                                              factor(bldclasssalecategory)*
                                                grosssqft+
                                              factor(zip)+
                                              logage,
                                            brooklyn_2016_2020_final)

brooklyn_2016_2020_final.lm.transform.v2.summary <- 
  summary(brooklyn_2016_2020_final.lm.transform.v2)

brooklyn_2016_2020_final.lm.transform.v2.summary

RMSE_transform_v2_model <- sqrt(
  mean(brooklyn_2016_2020_final.lm.transform.v2.summary$residuals^2)
)

sprintf("Root Mean Square Error(RMSE) for Transformed V2 Model : %s", 
        round(RMSE_transform_v2_model, digits = 4))

# to get property sold in Q3 and Q4 2020.
q3_2020_sold <- filter(brooklyn_2016_2020_final, yrsold == "2020", quarter =="3" ) 

q4_2020_sold <- filter(brooklyn_2016_2020_final, yrsold == "2020", quarter =="4" )  


# average sold price at Q4, USD 1070895.
average_sold_price_q4 <- mean(q4_2020_sold$price)

#  average sold price at Q3, USD 957949.9
average_sold_price_q3 <- mean(q3_2020_sold$price)

# average sold price all property types, quarter 4 over quarter 3.
average_sold_price_change = (average_sold_price_q4 - average_sold_price_q3)/average_sold_price_q3

# 0.1179029
average_sold_price_change


# number of sold properties
# 339 and 573
q3_count <- nrow(q3_2020_sold)
q4_count <- nrow(q4_2020_sold)

# number of sold properties increase rate
properties_sold_increase_rate <- (q4_count - q3_count)/q3_count

# 0.6902655
properties_sold_increase_rate


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

