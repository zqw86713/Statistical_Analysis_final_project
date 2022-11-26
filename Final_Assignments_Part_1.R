# ---
# title: "MSCA 31007 Statistical Analysis - Final Assignment Part 1"
# author: Prinu Mathew, Qingwei Zhang"
# start_date: "11/25/2022"
# last_revision_date: "12/09/2022"
# ---

install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)

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
    df[[colname]] <- as.numeric(gsub(",", "", format(df[[colname]], scientific = F)))
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
                         list('borough','neighborhood','bldclasscat','block','easement','lot','bldclasscurr','address',
                              'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                              'bldclasssale','price','date'))
brooklyn_2017 <- func.df.trim(brooklyn_2017, 
                              list('borough','neighborhood','bldclasscat','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))
brooklyn_2018 <- func.df.trim(brooklyn_2018, 
                              list('borough','neighborhood','bldclasscat','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))
brooklyn_2019 <- func.df.trim(brooklyn_2019, 
                              list('borough','neighborhood','bldclasscat','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))
brooklyn_2020 <- func.df.trim(brooklyn_2020, 
                              list('borough','neighborhood','bldclasscat','block','easement','lot','bldclasscurr','address',
                                   'aptnum','zip','resunits','comunits','totunits','landsqft','grosssqft','yrbuilt','taxclasssale',
                                   'bldclasssale','price','date'))


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

#replace column value '-' with zero
brooklyn_2016 <- func.df.replace(brooklyn_2016,list('price'), '-', '0')
brooklyn_2017 <- func.df.replace(brooklyn_2017,list('price'), '-', '0')
brooklyn_2018 <- func.df.replace(brooklyn_2018,list('price'), '-', '0')
brooklyn_2019 <- func.df.replace(brooklyn_2019,list('price'), '-', '0')
brooklyn_2020 <- func.df.replace(brooklyn_2020,list('price'), '-', '0')

#replace column value '0' with empty
brooklyn_2016 <- func.df.replace(brooklyn_2016,list('yrbuilt'), '0', '')
brooklyn_2017 <- func.df.replace(brooklyn_2017,list('yrbuilt'), '0', '')
brooklyn_2018 <- func.df.replace(brooklyn_2018,list('yrbuilt'), '0', '')
brooklyn_2019 <- func.df.replace(brooklyn_2019,list('yrbuilt'), '0', '')
brooklyn_2020 <- func.df.replace(brooklyn_2020,list('yrbuilt'), '0', '')

#change data types for following columns block
brooklyn_2016 <- func.df.ToInt(brooklyn_2016,list('borough','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale'))
brooklyn_2016 <- func.df.ToNum(brooklyn_2016,list('landsqft','grosssqft','price'))
brooklyn_2016 <- func.df.ToDate(brooklyn_2016,list('date'),format="%m/%d/%Y")

brooklyn_2017 <- func.df.ToInt(brooklyn_2017,list('borough','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale'))
brooklyn_2017 <- func.df.ToNum(brooklyn_2017,list('landsqft','grosssqft','price'))
brooklyn_2017 <- func.df.ToDate(brooklyn_2017,list('date'),format="%m/%d/%y")

brooklyn_2018 <- func.df.ToInt(brooklyn_2018,list('borough','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale'))
brooklyn_2018 <- func.df.ToNum(brooklyn_2018,list('landsqft','grosssqft','price'))
brooklyn_2018 <- func.df.ToDate(brooklyn_2018,list('date'),format="%m/%d/%y")

brooklyn_2019 <- func.df.ToInt(brooklyn_2019,list('borough','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale'))
brooklyn_2019 <- func.df.ToNum(brooklyn_2019,list('landsqft','grosssqft','price'))
brooklyn_2019 <- func.df.ToDate(brooklyn_2019,list('date'),format="%m/%d/%y")

brooklyn_2020 <- func.df.ToInt(brooklyn_2020,list('borough','block','lot','resunits','comunits','totunits','yrbuilt','taxclasssale'))
brooklyn_2020 <- func.df.ToNum(brooklyn_2020,list('landsqft','grosssqft','price'))
brooklyn_2020 <- func.df.ToDate(brooklyn_2020,list('date'),format="%m/%d/%y")


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

#Additionally restrict the data to observation where gross square footage is more than 0
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(grosssqft > 0 & !is.na(grosssqft))

#Additionally restrict the data to observation where sale price is non-missing
brooklyn_2016_2020_final <- brooklyn_2016_2020_final %>% filter(!is.na(price))



#*******************************Step 2: EDA and feature engineering *******************************#
#Your goal will be to use linear regression to explain Brooklyn housing prices within the 2016-2020 window.  
#You will be asked to make predictions for the sale prices within the dataset.  You are encouraged to think of ways 
#to get the most explanatory power out of your current variables. 

