# ---
# title: "MSCA 31007 Statistical Analysis - Final Assignment Part 1"
# author: Prinu Mathew, Qingwei Zhang"
# start_date: "11/25/2022"
# last_revision_date: "12/09/2022"
# ---

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd('C:\\Prinu\\Personal\\Studies\\Masters\\UChicago\\After Admission\\Courses\\Statistical Analysis\\Assignments\\Final Assignment - Part1\\data')

#*******************************Step 1: Import and prepare the data for analysis*******************************#


#1.1 Bring the data into R
#Using R, bring all five datasets into your workspace.  Notice that all five datasets have 21 columns, 
#with similar (but not identical) column names.  Please use the following vector of column names to standardize the data.

#c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement','bldclasscurr','address',
#'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date') 

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
colnames <- c('borough','neighborhood','bldclasscat','taxclasscurr','block','lot','easement',
              'bldclasscurr','address','aptnum','zip','resunits','comunits','totunits','landsqft',
              'grosssqft','yrbuilt','taxclasssale','bldclasssale','price','date')

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
#There are some data cleaning steps and transformations that would be necessary or helpful to almost any analysis.  
#Consider your data carefully, column by column.  Re-format, change data types, pay attention to white space and 
#special characters.  Datasets kept over multiple years are not always created in exactly the same way, so take 
#care that your data is standardized across years.  When you are done, create a new datasets which joins all 
#five yearly datasets.  This step will likely take a large amount of time.  Do not assume you can complete it quickly.  
#The resulting dataset should have roughly 119,000 rows.


#remove empty rows
brooklyn_2016 <- brooklyn_2016[!apply(brooklyn_2016 == "", 1, all),]
brooklyn_2017 <- brooklyn_2017[!apply(brooklyn_2017 == "", 1, all),]
brooklyn_2018 <- brooklyn_2018[!apply(brooklyn_2018 == "", 1, all),]
brooklyn_2019 <- brooklyn_2019[!apply(brooklyn_2019 == "", 1, all),]
brooklyn_2020 <- brooklyn_2020[!apply(brooklyn_2020 == "", 1, all),]

#since we removed blank rows from brooklyn 2019 & 2020, we reduced from 119351 rows to 117151
total_observations <- nrow(brooklyn_2016) + nrow(brooklyn_2017) + nrow(brooklyn_2018) + nrow(brooklyn_2019) + nrow(brooklyn_2020)

#define functions
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
func.df.ToInt <- function(df, colnames) {
  for (colname in colnames) {
    df[[colname]] <- as.integer(df[[colname]])
  }
  
  return(df)
}
func.df.ToNum <- function(df, colnames) {
  for (colname in colnames) {
    df[[colname]] <- str_replace_all(df[[colname]], "[^0-9.]", "")
    df[[colname]] <- suppressWarnings(as.numeric(gsub(",", "", format(df[[colname]], scientific = F))))
  }
  
  return(df)
}
func.df.ToDate <- function(df, colnames, format) {
  for (colname in colnames) {
    date_formatted <- as.POSIXct(df[[colname]], format=format)
    df[[colname]] <- as.Date(date_formatted, format=format)
  }
  
  return(df)
}

#trim leading and trailing white spaces for columns
brooklyn_2016 <- func.df.trim(brooklyn_2016, 
                         list('borough','neighborhood','bldclasscat','taxclasscurr','block','easement','lot','bldclasscurr','address',
                              'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                              'bldclasssale','price','date'))
brooklyn_2017 <- func.df.trim(brooklyn_2017, 
                              list('borough','neighborhood','bldclasscat','taxclasscurr','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))
brooklyn_2018 <- func.df.trim(brooklyn_2018, 
                              list('borough','neighborhood','bldclasscat','taxclasscurr','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))
brooklyn_2019 <- func.df.trim(brooklyn_2019, 
                              list('borough','neighborhood','bldclasscat','taxclasscurr','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))
brooklyn_2020 <- func.df.trim(brooklyn_2020, 
                              list('borough','neighborhood','bldclasscat','taxclasscurr','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))


#check if resunits is '-' for totunit= 1 and bldclasssale= starts with A or R and taxclasscurr= starts with 1 or 2
#brooklyn_2016 %>% filter(str_detect(totunits, "^1") & str_detect(resunits, "^-") & str_detect(comunits, "^-") & 
#                           price > 0 & (str_detect(bldclasssale, "^A") | str_detect(bldclasssale, "^R")) & 
#                           (str_detect(taxclasscurr, "^1") | str_detect(taxclasscurr, "^2")))


#replace column value '-' with empty
brooklyn_2016 <- func.df.replace(brooklyn_2016, 
                              list('borough','resunits','comunits','totunits','landsqft','grosssqft','taxclasssale'), '-', '')
brooklyn_2017 <- func.df.replace(brooklyn_2017, 
                                 list('borough','resunits','comunits','totunits','landsqft','grosssqft','taxclasssale'), '-', '')
brooklyn_2018 <- func.df.replace(brooklyn_2018, 
                                 list('borough','resunits','comunits','totunits','landsqft','grosssqft','taxclasssale'), '-', '')
brooklyn_2019 <- func.df.replace(brooklyn_2019, 
                                 list('borough','resunits','comunits','totunits','landsqft','grosssqft','taxclasssale'), '-', '')
brooklyn_2020 <- func.df.replace(brooklyn_2020, 
                                 list('borough','resunits','comunits','totunits','landsqft','grosssqft','taxclasssale'), '-', '')

#replace column value '0' with empty
#brooklyn_2016 <- func.df.replace(brooklyn_2016,list('yrbuilt'), '0', '')
#brooklyn_2017 <- func.df.replace(brooklyn_2017,list('yrbuilt'), '0', '')
#brooklyn_2018 <- func.df.replace(brooklyn_2018,list('yrbuilt'), '0', '')
#brooklyn_2019 <- func.df.replace(brooklyn_2019,list('yrbuilt'), '0', '')
#brooklyn_2020 <- func.df.replace(brooklyn_2020,list('yrbuilt'), '0', '')

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
total_observations_after_cleanup <- nrow(brooklyn_2016) + nrow(brooklyn_2017) + nrow(brooklyn_2018) + nrow(brooklyn_2019) + nrow(brooklyn_2020)


#merge the dataframes
brooklyn_2016_2020_list <- list(brooklyn_2016, brooklyn_2017, brooklyn_2018, brooklyn_2019, brooklyn_2020)
brooklyn_2016_2020 <- brooklyn_2016_2020_list %>% reduce(full_join)
remove(brooklyn_2016_2020_list)



#1.3 Filter the data and make transformations specific to this analysis 

#For the purposes of this analysis, we will only consider purchases of single-family residences and single-unit apartments 
#or condos.  Restrict the data to purchases where the building class at the time of sale starts with ‘A’ or ‘R’ and where 
#the number of total units and the number of residential units are both 1.  Additionally restrict the data to observations 
#where gross square footage is more than 0 and sale price is non-missing.  The resulting dataset should have roughly 19,000 rows. 


#filter observations considering purchases of single-family residences and single-unit apartments or condos
#Restrict the data to purchases where the building class at the time of sale starts with ‘A’ or ‘R’
brooklyn_2016_2020_final <- brooklyn_2016_2020 %>% filter(str_detect(bldclasssale, "^A") | str_detect(bldclasssale, "^R"))

#the number of total units and the number of residential units are both 1
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(resunits == 1 & totunits == 1)

#additionally restrict the data to observation where gross square footage is more than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(grosssqft > 0 & !is.na(grosssqft))

#additionally restrict the data to observation where sale price is non-missing
#brooklyn_2016_2020_final[["price"]][is.na(brooklyn_2016_2020_final[["price"]])] <- 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(!is.na(price))

#additionally restrict the data to observation where Year Built is more than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(yrbuilt > 0)

#additionally restrict the data to observation where price is less than 10 million. I would consider those as outliers
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(price < 15000000)

#additionally restrict the data to observation where grosssqft is less than 20k. I would consider those as outliers
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(grosssqft < 20000)


#*******************************Step 2: EDA and feature engineering *******************************#
#Your goal will be to use linear regression to explain Brooklyn housing prices within the 2016-2020 window.  
#You will be asked to make predictions for the sale prices within the dataset.  You are encouraged to think of ways 
#to get the most explanatory power out of your current variables. 


#2.1 Exploratory data analysis 

#Consider price as a potential response variable.  Examine how it is distributed, and how it associates with the other variables 
#in your data.  Think about how you would use these other variables to explain price.  Consider whether each variable should 
#enter as a continuous numeric predictor, or as a factor.  Consider transformations of your response variable, transformations 
#of your predictors, or both.  Use this exploratory data analysis to revisit your initial data cleaning steps, which might need revision. 


#2.1.1 - plot and analysis between response and predictor variables
plot(price ~ grosssqft, data = brooklyn_2016_2020_final, xlab = "Gross Sqft", ylab = "Price", pch = 20, cex = 2)
plot(price ~ zip, data = brooklyn_2016_2020_final, xlab = "Zip", ylab = "Price", pch = 20, cex = 2)
plot(price ~ yrbuilt, data = brooklyn_2016_2020_final, xlab = "Year Built", ylab = "Price", pch = 20, cex = 2)


#2.1.2 - Neighborhood consolidation
#https://www.unitedstateszipcodes.org/11223/
#unique((brooklyn_2016_2020_final %>% filter(str_detect(neighborhood, "FLATBUSH-CENTRAL")))$zip)
#unique((brooklyn_2016_2020_final %>% filter(str_detect(zip, "11234")))$neighborhood)

#COBBLE HILL    same as   COBBLE HILL-WEST
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "COBBLE HILL-WEST")] <- "COBBLE HILL"

#Zipcodes in neighborhoods FLATBUSH-CENTRAL, FLATBUSH-LEFFERTS GARDEN, FLATBUSH-EAST, FLATBUSH-NORTH overlap each other.
#So combine all those neighborhoods to one as FLATBUSH
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "FLATBUSH-CENTRAL")] <- "FLATBUSH"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "FLATBUSH-LEFFERTS GARDEN")] <- "FLATBUSH"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "FLATBUSH-EAST")] <- "FLATBUSH"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "FLATBUSH-NORTH")] <- "FLATBUSH"

#Zipcodes in neighborhoods DOWNTOWN-FULTON MALL, DOWNTOWN-FULTON FERRY, DOWNTOWN-METROTECH, BROOKLYN HEIGHTS overlap each other.
#So combine all those neighborhoods to one as DOWNTOWN BROOKLYN
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "DOWNTOWN-FULTON MALL")] <- "DOWNTOWN BROOKLYN"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "DOWNTOWN-FULTON FERRY")] <- "DOWNTOWN BROOKLYN"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "DOWNTOWN-METROTECH")] <- "DOWNTOWN BROOKLYN"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "BROOKLYN HEIGHTS")] <- "DOWNTOWN BROOKLYN"

#Zipcodes in neighborhoods OLD MILL BASIN, MILL BASIN overlap each other.
#So combine all those neighborhoods to one as MILL BASIN
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "OLD MILL BASIN")] <- "MILL BASIN"

#Zipcodes in neighborhoods WILLIAMSBURG-SOUTH, WILLIAMSBURG-NORTH, WILLIAMSBURG-CENTRAL, WILLIAMSBURG-EAST overlap each other.
#So combine all those neighborhoods to one as DOWNTOWN BROOKLYN
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "WILLIAMSBURG-SOUTH")] <- "WILLIAMSBURG"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "WILLIAMSBURG-NORTH")] <- "WILLIAMSBURG"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "WILLIAMSBURG-CENTRAL")] <- "WILLIAMSBURG"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "WILLIAMSBURG-EAST")] <- "WILLIAMSBURG"

#Zipcodes in neighborhoods PARK SLOPE, PARK SLOPE SOUTH overlap each other.
#So combine all those neighborhoods to one as PARK SLOPE SOUTH
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["neighborhood"]], "PARK SLOPE")] <- "PARK SLOPE SOUTH"

#Zipcode 11234 actually belongs to BERGEN BEACH
#So modify all those neighborhoods where zipcode=11234 to BERGEN BEACH.
#Here zipcode in neighborhoods MILL BASIN, FLATLANDS changed to BERGEN BEACH
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11234")] <- "BERGEN BEACH"

#Zipcode 11223 actually belongs to BENSONHURST
#So modify all those neighborhoods where zipcode=11223 to BENSONHURST.
#Here zipcode in neighborhoods OCEAN PARKWAY-NORTH, OCEAN PARKWAY-SOUTH changed to BENSONHURST
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11223")] <- "BENSONHURST"

#Zipcode 11235 actually belongs to SHEEPSHEAD BAY
#So modify all those neighborhoods where zipcode=11235 to SHEEPSHEAD BAY
#Here zipcode in neighborhoods OCEAN PARKWAY-SOUTH,MANHATTAN BEACH,BRIGHTON BEACH changed to SHEEPSHEAD BAY
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11235")] <- "SHEEPSHEAD BAY"

#Zipcode 11221 actually belongs to BUSHWICK
#So modify neighborhood=OCEAN HILL and zipcode=11221 to BUSHWICK
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11221") & 
                                           str_detect(brooklyn_2016_2020_final[["neighborhood"]], "OCEAN HILL")] <- "BUSHWICK"

#Zipcode 11212 actually belongs to BROWNSVILLE
#So modify neighborhood=OCEAN HILL and zipcode=11212 to BROWNSVILLE
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11212") & 
                                             str_detect(brooklyn_2016_2020_final[["neighborhood"]], "OCEAN HILL")] <- "BROWNSVILLE"


#Zipcode 11230 actually belongs to OCEAN PARKWAY-NORTH
#So modify neighborhood=OCEAN HILL and zipcode=11230 to OCEAN PARKWAY-NORTH
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11230") & 
                                             str_detect(brooklyn_2016_2020_final[["neighborhood"]], "OCEAN HILL")] <- "OCEAN PARKWAY-NORTH"


#Zipcode 11201 actually belongs to DOWNTOWN BROOKLYN
#So modify neighborhood=OCEAN HILL & OCEAN PARKWAY-NORTH and zipcode=11201 to DOWNTOWN BROOKLYN
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11201") & 
                                             str_detect(brooklyn_2016_2020_final[["neighborhood"]], "OCEAN HILL")] <- "DOWNTOWN BROOKLYN"
brooklyn_2016_2020_final[["neighborhood"]][str_detect(brooklyn_2016_2020_final[["zip"]], "11201") & 
                                             str_detect(brooklyn_2016_2020_final[["neighborhood"]], "OCEAN PARKWAY-NORTH")] <- "DOWNTOWN BROOKLYN"



#2.1.3.1 - Additionally restrict the data to observation where price is greater than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(price > 0 & !is.na(price))

#2.1.3.2 - Additionally restrict the data to observation where zip is greater than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(zip > 0)

#2.1.4 - check the model summary without any transformation
brooklyn_2016_2020_final.lm.native <- lm(formula = price~
                                           factor(bldclasssale)+
                                           factor(neighborhood)+
                                           factor(zip)+
                                           grosssqft+landsqft+
                                           yrbuilt+taxclasssale,
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


#2.2 Pre-modeling and feature engineering

#Begin to construct linear models explaining price (or a transformation of price).  Consider your total model degrees of 
#freedom, your adjusted R^2, and your RMSE (root means square error).  Also consider whether your models show severe violations 
#of the OLS model assumptions, or merely slight violations of the OLS model assumptions.

#brooklyn_2016_2020_final.lm <- lm(logprice~resunits+totunits+grosssqft+sqrt(grosssqft)+landsqft+sqrt(landsqft)+factor(decade)+logage,brooklyn_2016_2020_final)


#feature engineering

#2.2.1 - find the average price of each neighborhood and assign that price to price having 0 for those matching neighborhood
unique_neighborhoods <- as.list(unique(brooklyn_2016_2020_final$neighborhood))

func.df.adjPrice <- function(df, neighborhoods) {
  colname_price = 'price'
  colname_neighborhood = 'neighborhood'
  for (item in neighborhoods) {
    df_price_temp <- df %>% filter(price > 0 & neighborhood == item)
    df[[colname_price]][df[[colname_price]] == 0 & df[[colname_neighborhood]] == item] <- floor(mean(df_price_temp$price))
  }
  
  return(df)
}
#brooklyn_2016_2020_final <- func.df.adjPrice(brooklyn_2016_2020_final, unique_neighborhoods)


#2.2.2 - extract year from sale date
brooklyn_2016_2020_final$yrsold <- format(brooklyn_2016_2020_final$date,"%Y")
brooklyn_2016_2020_final <- func.df.ToInt(brooklyn_2016_2020_final,list('yrsold'))

#2.2.3 - adding decade as new column. Also as the year build increases the house price decreases
brooklyn_2016_2020_final$decade <- 10*floor(brooklyn_2016_2020_final$yrbuilt/10)
brooklyn_2016_2020_final$decade[brooklyn_2016_2020_final$decade<1950] <- 1950
#aggregate(data = brooklyn_2016_2020_final, yrbuilt ~ decade, function(x) length(unique(x)))

#2.2.4.1 - group all "A5" "A1" "A9" "A4" "A3" "A2" "A0" "A7" "A6" to "A"
#2.2.4.2 - group all "R3" "R2" "R4" "R1" "R6" "RR" to "R"
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>%
  mutate(bldclasssalecategory = case_when(
    str_detect(bldclasssale, "^A")  ~ "A",
    str_detect(bldclasssale, "^R")  ~ "R"
  ))

#log transformations of response
brooklyn_2016_2020_final$logprice <- log(brooklyn_2016_2020_final$price)
#brooklyn_2016_2020_final$logadjprice <- log(brooklyn_2016_2020_final$adjprice)

#log transformations of predictors
brooklyn_2016_2020_final$logage <- log(brooklyn_2016_2020_final$yrsold-brooklyn_2016_2020_final$yrbuilt+0.1)
brooklyn_2016_2020_final$loggrosssqft <- log(brooklyn_2016_2020_final$grosssqft)

#functions of two different variables transformations of predictor variables
brooklyn_2016_2020_final$totsqft <- brooklyn_2016_2020_final$landsqft + brooklyn_2016_2020_final$grosssqft


#2.2.5 - plot and analysis between response and predictor variables
plot(price ~ grosssqft, data = brooklyn_2016_2020_final, xlab = "Gross Sqft", ylab = "Adj Price", pch = 20, cex = 2)
plot(price ~ zip, data = brooklyn_2016_2020_final, xlab = "Zip", ylab = "Adj Price", pch = 20, cex = 2)
plot(price ~ yrbuilt, data = brooklyn_2016_2020_final, xlab = "Year Built", ylab = "Adj Price", pch = 20, cex = 2)
plot(price ~ decade, data = brooklyn_2016_2020_final, xlab = "Decade", ylab = "Adj Price", pch = 20, cex = 2)


#check the model summary after transformations
brooklyn_2016_2020_final.lm.transform <- lm(formula = price~
                                              factor(bldclasssalecategory)+
                                              factor(neighborhood)+
                                              grosssqft+sqrt(grosssqft)+
                                              factor(decade)+
                                              yrbuilt+
                                              logage+
                                              taxclasssale,
                                            brooklyn_2016_2020_final)
brooklyn_2016_2020_final.lm.transform.summary <- summary(brooklyn_2016_2020_final.lm.transform)
brooklyn_2016_2020_final.lm.transform.summary

#TESTING
brooklyn_2016_2020_final.lm.transform <- lm(formula = price~
                                              factor(bldclasssalecategory)+
                                              factor(zip)+
                                              grosssqft+sqrt(grosssqft)+
                                              factor(decade)+
                                              yrbuilt+
                                              logage+
                                              taxclasssale,
                                            brooklyn_2016_2020_final)
brooklyn_2016_2020_final.lm.transform.summary <- summary(brooklyn_2016_2020_final.lm.transform)
brooklyn_2016_2020_final.lm.transform.summary

#get metrics from transformed model summary
brooklyn_2016_2020_final.lm.transform.summary.metric <- data.frame(
  R2 = brooklyn_2016_2020_final.lm.transform.summary$r.squared,
  Adj.R2 = brooklyn_2016_2020_final.lm.transform.summary$adj.r.squared
)

RMSE_transform_model <- sqrt(mean(brooklyn_2016_2020_final.lm.transform.summary$residuals^2))
sprintf("Root Mean Square Error(RMSE) for Transform Model : %s", round(RMSE_transform_model, digits = 4))

