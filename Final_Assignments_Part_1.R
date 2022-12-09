# Final Assignment Part 1
# Qingwei Zhang"


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
library(GGally) 
library(Amelia)
library(Hmisc)
library(lmtest)
library(MASS)
library(faraway)
library(reshape2)

setwd("C:/Users/QZHAN101/Downloads/play/UCHICAGO/MSCA_31007_ON01_Autumn_2022_Statistical_Analysis/Statistical_Analysis_final_project")


# Step 1: Import and prepare the data for analysis
#1.1 Bring the data into R
data_2016 <- read.csv('2016_brooklyn.csv')
data_2017 <- read.csv('2017_brooklyn.csv')
data_2018 <- read.csv('2018_brooklyn.csv')
data_2019 <- read.csv('2019_brooklyn.csv')
data_2020 <- read.csv('2020_brooklyn.csv')

#removing first 'n' rows from csv files since they are irrelevant.
data_2016 <- tail(data_2016, -4)
data_2017 <- tail(data_2017, -4)
data_2018 <- tail(data_2018, -4)
data_2019 <- tail(data_2019, -4)
data_2020 <- tail(data_2020, -7)

#define column names for each data frames
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
colnames(data_2016) <- colnames
colnames(data_2017) <- colnames
colnames(data_2018) <- colnames
colnames(data_2019) <- colnames
colnames(data_2020) <- colnames

#define rows names for each dataframes
rownames(data_2016) <- 1:nrow(data_2016)
rownames(data_2017) <- 1:nrow(data_2017)
rownames(data_2018) <- 1:nrow(data_2018)
rownames(data_2019) <- 1:nrow(data_2019)
rownames(data_2020) <- 1:nrow(data_2020)


#1.2 Join the data and make it usable for analysis

#remove empty rows
data_2016 <- data_2016[!apply(data_2016 == "", 1, all),]
data_2017 <- data_2017[!apply(data_2017 == "", 1, all),]
data_2018 <- data_2018[!apply(data_2018 == "", 1, all),]
data_2019 <- data_2019[!apply(data_2019 == "", 1, all),]
data_2020 <- data_2020[!apply(data_2020 == "", 1, all),]

#since we removed blank rows from Brooklyn 2019 & 2020, we reduced 
# from 119351 rows to 117151
count_observations <- nrow(data_2016) + 
  nrow(data_2017) + 
  nrow(data_2018) + 
  nrow(data_2019) +
  nrow(data_2020)

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
data_2016 <- func.df.trim(data_2016, colnames)
data_2017 <- func.df.trim(data_2017, colnames)
data_2018 <- func.df.trim(data_2018, colnames)
data_2019 <- func.df.trim(data_2019, colnames)
data_2020 <- func.df.trim(data_2020, colnames)

#replace column value '-' with empty.
columns_with_hyphen = c(
  'borough',
  'resunits',
  'comunits',
  'totunits',
  'landsqft',
  'grosssqft',
  'taxclasssale'
  )

data_2016 <- func.df.replace(data_2016, columns_with_hyphen, '-', '')
data_2017 <- func.df.replace(data_2017, columns_with_hyphen, '-', '')
data_2018 <- func.df.replace(data_2018, columns_with_hyphen, '-', '')
data_2019 <- func.df.replace(data_2019, columns_with_hyphen, '-', '')
data_2020 <- func.df.replace(data_2020, columns_with_hyphen, '-', '')

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
data_2016 <- func.df.ToInt(data_2016,columns_To_Int)
data_2016 <- func.df.ToNum(data_2016,columns_To_Num)
data_2016 <- func.df.ToDate(data_2016,list('date'),format="%m/%d/%Y")
data_2016 <- data_2016 %>% filter(!is.na(price))

data_2017 <- func.df.ToInt(data_2017,columns_To_Int)
data_2017 <- func.df.ToNum(data_2017,columns_To_Num)
data_2017 <- func.df.ToDate(data_2017,list('date'),format="%m/%d/%y")
data_2017 <- data_2017 %>% filter(!is.na(price))

data_2018 <- func.df.ToInt(data_2018,columns_To_Int)
data_2018 <- func.df.ToNum(data_2018,columns_To_Num)
data_2018 <- func.df.ToDate(data_2018,list('date'),format="%m/%d/%y")
data_2018 <- data_2018 %>% filter(!is.na(price))

data_2019 <- func.df.ToInt(data_2019,columns_To_Int)
data_2019 <- func.df.ToNum(data_2019,columns_To_Num)
data_2019 <- func.df.ToDate(data_2019,list('date'),format="%m/%d/%y")
data_2019 <- data_2019 %>% filter(!is.na(price))

data_2020 <- func.df.ToInt(data_2020,columns_To_Int)
data_2020 <- func.df.ToNum(data_2020,columns_To_Num)
data_2020 <- func.df.ToDate(data_2020,list('date'),format="%m/%d/%y")
data_2020 <- data_2020 %>% filter(!is.na(price))

# to get the total observation number of rows.
count_observation_after_processing <- nrow(data_2016) + 
  nrow(data_2017) + 
  nrow(data_2018) + 
  nrow(data_2019) +
  nrow(data_2020)

#merge the data frames
data_2016_2020_list <- list(
  data_2016, 
  data_2017, 
  data_2018, 
  data_2019, 
  data_2020
)

data_2016_2020 <- data_2016_2020_list %>% reduce(full_join)
remove(data_2016_2020_list)


#1.3 Filter the data and make transformations specific to this analysis 
data_2016_2020_clean <- data_2016_2020 %>% 
  filter(str_detect(bldclasssale, "^A") 
         | str_detect(bldclasssale, "^R"))

#the number of total units and the number of residential units are both 1
data_2016_2020_clean <- data_2016_2020_clean %>% 
  filter(resunits == 1 & totunits == 1)

#additionally restrict the data to observation where gross square
# footage is more than 0
data_2016_2020_clean <- data_2016_2020_clean %>% 
  filter(grosssqft > 0 & !is.na(grosssqft))

#additionally restrict the data to observation where sale price is 
#non-missing
data_2016_2020_clean <- data_2016_2020_clean %>% 
  filter(!is.na(price))

#additionally restrict the data to observation where Year Built is 
#more than 0
data_2016_2020_clean <- data_2016_2020_clean %>% 
  filter(yrbuilt > 0)

#additionally restrict the data to observation where price is 
#less than 100 million. Anything outside of that, I would consider 
#those as outliers.
outlier_threhold=10000000

data_2016_2020_clean <- data_2016_2020_clean %>% 
  filter(price <= outlier_threhold)

#additionally restrict the data to observation where gross sqft is 
#less than 20k. Anything outside of that, I would consider those as outliers
grosssqft_threhold=20000

data_2016_2020_clean <- data_2016_2020_clean %>% 
  filter(grosssqft < grosssqft_threhold)


# Step 2: EDA and feature engineering 

#2.1 Exploratory data analysis 

#2.1.1.1 - Statistics summary
dim(data_2016_2020_clean)
summary(data_2016_2020_clean)


#2.1.1.2 - Check for any NA’s in the dataframe
missmap(
  data_2016_2020_clean,
  col=c('yellow','black'),
  y.at=1,
  y.labels='',legend=TRUE
)

colSums(is.na(data_2016_2020_clean))


#2.1.1.3 - Data Cleansing — Handle missing data
data_2016_2020_clean[["taxclasscurr"]][
  str_detect(data_2016_2020_clean[["bldclasssale"]], "^A") 
  & is.na(data_2016_2020_clean[["taxclasscurr"]])] <- 1

data_2016_2020_clean[["taxclasscurr"]][
  str_detect(data_2016_2020_clean[["bldclasssale"]], "^R") 
  & is.na(data_2016_2020_clean[["taxclasscurr"]])] <- 2

data_2016_2020_clean[["comunits"]][
  is.na(data_2016_2020_clean[["comunits"]])] <- 0

# make sure no columns has NA any more.
colSums(is.na(data_2016_2020_clean))


#2.1.1.4 - Correlations

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
#variables 'taxclasscurr', 'taxclasssale', 'landsqft', 'lot' 
#with the target variable 'price' are weak correlations, 
#(0.1, 0.1, 0.1, 0.1 respectively)
#'therefore we can exclude these four independent variable from our model.
ggcorr(
  data_2016_2020_clean, 
  label = T, 
  hjust = 1, 
  legend.position="top", 
  layout.exp = 2
)


#2.1.1.5 - visualizing the distribution of the target variable 'price'
# and draw kernel density estimate.
data_2016_2020_clean %>% 
  ggplot(aes(price)) +
  geom_density() + 
  theme_bw()

#2.1.3.1 - Additionally restrict the data to observation where 
#price is greater than 0
data_2016_2020_clean <- 
  data_2016_2020_clean %>% filter(price > 0 & !is.na(price))

#2.1.3.2 - Additionally restrict the data to observation where 
#zip is greater than 0
data_2016_2020_clean <- 
  data_2016_2020_clean %>% filter(zip > 0)


#2.1.3.3 - visualizing the distribution of the target variable 'price'
data_2016_2020_clean %>% 
  ggplot(aes(price)) +
  geom_density() + 
  theme_bw()

#2.1.3.4 - Create a histogram of housing prices
ggplot(data=data_2016_2020_clean) + 
  geom_histogram(mapping = aes(price))

ggplot(data=data_2016_2020_clean) +                         
  geom_histogram(mapping = aes(price/100000), 
                 breaks=seq(0, 7, by = 1), col="red", fill="lightblue") + 
  geom_density(mapping = aes(x=price/100000, y = (..count..)))  +   
  labs(title="Housing Prices in $100,000", 
       x="Sale Price of Individual Homes/Condos")   

ggplot(data=data_2016_2020_clean) + 
  geom_point(mapping= aes(x=grosssqft, y=price))

ggplot(data=data_2016_2020_clean) + 
  geom_point(mapping= aes(x=log(grosssqft), y=price))

ggplot(data=data_2016_2020_clean) + 
  geom_point(mapping= aes(x=yrbuilt, y=price))

ggplot(data=data_2016_2020_clean) + 
  geom_point(mapping= aes(x=price, y=bldclasssale))

ggplot(data=data_2016_2020_clean) + 
  geom_point(mapping= aes(x=price, y=zip))

ggplot(data=data_2016_2020_clean) + 
  geom_point(mapping= aes(x=price, y=block))

ggplot(data=data_2016_2020_clean) + 
  geom_point(mapping= aes(x=price, y=neighborhood))


#2.1.3.5 - effect of the predictor variables on target variable 'price'
data_2016_2020_clean %>%
  dplyr::select(c(
    price,
    zip,
    # neighborhood,
    # block,
    grosssqft,
    # landsqft,
    yrbuilt,
    bldclasssale,
    # taxclasssale
    )) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Price ($1000s)") +
  theme_gray()


#2.2 Pre-modeling and feature engineering

#2.2.1.1 - find the average price of each neighborhood and assign that
# price to price having 0 for those matching neighborhood. We first
# need to know what are the unique_neighborhoods and unique_neighborhoods.
unique_neighborhoods <- as.list(unique(data_2016_2020_clean$neighborhood))

unique_zips <- as.list(unique(data_2016_2020_clean$zip))

#2.2.1.2 - Remove duplicates based on columns 
#(neighborhood,bldclasscat,block,zip,resunits,totunits,landsqft,
#grosssqft,yrbuilt,bldclasssale,price)

columns_with_duplicates <- c('neighborhood',
  'bldclasscat',
  'block',
  'zip',
  'resunits',
  'totunits',
  'landsqft',
  'grosssqft',
  'yrbuilt',
  'bldclasssale',
  'price'
)

data_2016_2020_clean <- data_2016_2020_clean[
  !duplicated(
    data_2016_2020_clean, by=columns_with_duplicates), ]


#2.2.1.3 - find duplicate rows with same values for column 
#(neighborhood,bldclasscat,block,zip,resunits,totunits,landsqft,
#grosssqft,yrbuilt,bldclasssale)
data_2016_2020_clean <- data_2016_2020_clean %>% 
  group_by(
    neighborhood,
    address,
    bldclasscat,
    block,
    zip,
    resunits,
    totunits,
    landsqft,
    grosssqft,
    yrbuilt,
    bldclasssale
    ) %>% 
  mutate(duplicate_row = case_when(
    n()>1  ~ 1,
    TRUE  ~ 0
  ))

data_2016_2020_clean <- func.df.ToInt(
  data_2016_2020_clean,list('duplicate_row')
)


#2.2.1.4 - find the average price of each neighborhood and 
#corresponding zip. Some houses are sold with less than 3000 USD,
# we assign the average price from their neighborhood and zip to 
# those price-unreasonable houses with those matching neighborhood and zip.
# We create a function to do this.
func.df.adjPrice <- function(df, neighborhoods, zips) {
  df$adjprice = df$price
  colname_price = 'adjprice'
  colname_neighborhood = 'neighborhood'
  colname_zip = 'zip'
  threshold_price=3000
  
  group_by_neighbour_zip <- df %>% 
    filter(is.element(
      neighborhood, 
      unique_neighborhoods) & 
        price > threshold_price) %>%  
    group_by(neighborhood, zip) %>% 
    summarise(mean_price=floor(mean(price)), .groups = 'drop') %>%
    as.data.frame()
  
  for(i in 1:nrow(group_by_neighbour_zip)) {
    row <- group_by_neighbour_zip[i,]
    col_neighborhood_val <- row[,1]
    col_zip_val <- row[,2]
    col_mean_price_val <- row[,3]
    
    df[[colname_price]][df[[colname_price]] > 0 & 
                        df[[colname_price]] <= threshold_price & 
                        df[[colname_neighborhood]] == col_neighborhood_val & 
                        df[[colname_zip]] == col_zip_val] <- col_mean_price_val
  }
  
  return(df)
}

# run the function to fill the adjusted prices to all rows.
data_2016_2020_clean <- func.df.adjPrice(
  data_2016_2020_clean, 
  unique_neighborhoods, 
  unique_zips
)

#2.2.1.5 - Set landsqft equals grosssqft if landsqft less than 0
data_2016_2020_clean <- data_2016_2020_clean %>% 
  mutate(adjlandsqft = case_when(
    landsqft <= 0  ~ grosssqft,
    TRUE  ~ landsqft
  ))

data_2016_2020_clean <- 
  func.df.ToNum(data_2016_2020_clean,list('adjlandsqft'))

#2.2.2.1 - extract year from sale date
data_2016_2020_clean$yrsold <- 
  format(data_2016_2020_clean$date,"%Y")

#2.2.3.2 - convert the column to integer type.
data_2016_2020_clean <- 
  func.df.ToInt(data_2016_2020_clean,list('yrsold'))

#2.2.3.3 - adding quarter by extracting month from date
data_2016_2020_clean <- data_2016_2020_clean %>%
  mutate(quarter = case_when(
    is.element(format(date,"%m"), c("01", "02", "03"))  ~ 1,
    is.element(format(date,"%m"), c("04", "05", "06"))  ~ 2,
    is.element(format(date,"%m"), c("07", "08", "09"))  ~ 3,
    is.element(format(date,"%m"), c("10", "11", "12"))  ~ 4
  ))

#2.2.3.4 - convert the column to integer type.
data_2016_2020_clean <- 
  func.df.ToInt(data_2016_2020_clean,list('quarter'))

#2.2.4.1 - group all "A5" "A1" "A9" "A4" "A3" "A2" "A0" "A7" "A6" to "A"
#2.2.4.2 - group all "R3" "R2" "R4" "R1" "R6" "RR" to "R"
data_2016_2020_clean <- data_2016_2020_clean %>%
  mutate(bldclasssalecategory = case_when(
    str_detect(bldclasssale, "^A")  ~ "0",
    str_detect(bldclasssale, "^R")  ~ "1"
  ))

# convert the column to int.
data_2016_2020_clean <- func.df.ToInt(
  data_2016_2020_clean,list('bldclasssalecategory')
)


#2.2.5.1 - log transformations of predictors
data_2016_2020_clean$log_house_age <- 
  log(data_2016_2020_clean$yrsold - 
        data_2016_2020_clean$yrbuilt +
        0.1
      )

#2.2.6.1 - Let's identify significance level from interaction between variables
summary(lm(formula = adjprice~
             (factor(bldclasssalecategory)+
                factor(zip)+
                grosssqft+
                adjlandsqft+  
                block+
                lot+
                log_house_age+
                yrbuilt+
                borough+
                factor(bldclasscat)+
                factor(taxclasssale))^2,
           data_2016_2020_clean))


#2.3 - Reach a stopping point 
#New version of model by adding interaction terms 
transform.lm <- lm(formula = adjprice~factor(bldclasssalecategory)*
  grosssqft+
  factor(zip)+
  log_house_age,
  
  data_2016_2020_clean
)

transform.lm.summary <- 
  summary(transform.lm)

transform.lm.summary

RMSE_transform_v2_model <- sqrt(
  mean(transform.lm.summary$residuals^2)
)

sprintf("Root Mean Square Error(RMSE) for Transformed V2 Model : %s", 
        round(RMSE_transform_v2_model, digits = 4))

#2.3.0.1 - Test IID assumptions
#Kolmogorov-Smirnov test for normality
hist(transform.lm$residuals)
ks.test(transform.lm$residuals/summary(transform.lm)$sigma, pnorm)

#Breusch-Pagan test for normality heteroscedasticity
bptest(transform.lm)

#If the residuals become more spread out at higher values in the plot, 
#this is a tell-tale sign that heteroscedasticity is present.
plot(fitted(transform.lm), 
     resid(transform.lm), 
     col = "dodgerblue",
     pch = 20, cex = 1.5, 
     xlab = "Fitted", 
     ylab = "Residuals")

abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#2.2.5.5 - a scale-location plot
ggplot(transform.lm, 
       aes(x=.fitted, y=sqrt(abs(.stdresid)))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  ggtitle("Scale-Location plot : Standardized Residual vs Fitted values")

#2.2.5.6 - normal QQ plot
ggplot(data_2016_2020_clean, aes(sample=transform.lm$residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of BC Model")



#  Step 3.
# to get property sold in Q3 and Q4 2020.
q3_2020_sold <- filter(data_2016_2020_clean, 
                       yrsold == "2020", quarter =="3" ) 

q4_2020_sold <- filter(data_2016_2020_clean, 
                       yrsold == "2020", quarter =="4" )  


# average sold price at Q4, USD 1070895.
average_sold_price_q4 <- mean(q4_2020_sold$price)

#  average sold price at Q3, USD 957949.9
average_sold_price_q3 <- mean(q3_2020_sold$price)



# average sold price all property types, quarter 4 over quarter 3.
average_sold_price_change = (
  average_sold_price_q4 - average_sold_price_q3)/average_sold_price_q3

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


# get all types of residential class
residential_class_price_q3 <- filter(q3_2020_sold, str_detect(bldclasssale, "^A"))

residential_class__price_q4 <- filter(q4_2020_sold, str_detect(bldclasssale, "^A"))

q3_mean = mean(residential_class_price_q3$price)

q4_mean = mean(residential_class__price_q4$price)

(q4_mean - q3_mean)/q4_mean


# get all types of condo class
condo_class_q3 <- filter(q3_2020_sold, str_detect(bldclasssale, "^R"))

condo_class_q4 <- filter(q4_2020_sold, str_detect(bldclasssale, "^R"))

q3_mean = mean(condo_class_q3$price)

q4_mean = mean(condo_class_q4$price)

(q4_mean - q3_mean)/q3_mean

