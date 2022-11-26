# ---
# title: "MSCA 31007 Statistical Analysis - Final Assignment Part 1"
# author: Prinu Mathew, Qingwei Zhang"
# start_date: "11/25/2022"
# last_revision_date: "12/09/2022"
# ---

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

#removing first 4 rows from csv files
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


#trim leading and trailing white spaces for columns
df.trim <- function(df, colname) {
  #df[colname] <- trimws(df[colname])
  #df[colname] <- trimws(`$`(df , colname))
  df[[colname]] <- trimws(df[[colname]])
}

df.trim(brooklyn_2016, "address")

brooklyn_2016$borough <- trimws(brooklyn_2016$borough)
brooklyn_2016$neighborhood <- trimws(brooklyn_2016$neighborhood)
brooklyn_2016$bldclasscat <- trimws(brooklyn_2016$bldclasscat)
brooklyn_2016$block <- trimws(brooklyn_2016$block)
brooklyn_2016$easement <- trimws(brooklyn_2016$easement)
brooklyn_2016$lot <- trimws(brooklyn_2016$lot)
brooklyn_2016$bldclasscurr <- trimws(brooklyn_2016$bldclasscurr)
brooklyn_2016$address <- trimws(brooklyn_2016$address)
brooklyn_2016$aptnum <- trimws(brooklyn_2016$aptnum)
brooklyn_2016$zip <- trimws(brooklyn_2016$zip)
brooklyn_2016$resunits <- trimws(brooklyn_2016$resunits)
brooklyn_2016$comunits <- trimws(brooklyn_2016$comunits)
brooklyn_2016$totunits <- trimws(brooklyn_2016$totunits)
brooklyn_2016$landsqft <- trimws(brooklyn_2016$landsqft)
brooklyn_2016$grosssqft <- trimws(brooklyn_2016$grosssqft)
brooklyn_2016$yrbuilt <- trimws(brooklyn_2016$yrbuilt)
brooklyn_2016$taxclasssale <- trimws(brooklyn_2016$taxclasssale)
brooklyn_2016$bldclasssale <- trimws(brooklyn_2016$bldclasssale)
brooklyn_2016$price <- trimws(brooklyn_2016$price)
brooklyn_2016$date <- trimws(brooklyn_2016$date)

#replace column value '-' with empty
brooklyn_2016$borough[brooklyn_2016$borough == "-"] <- ''
brooklyn_2016$resunits[brooklyn_2016$resunits == "-"] <- ''
brooklyn_2016$comunits[brooklyn_2016$comunits == "-"] <- ''
brooklyn_2016$totunits[brooklyn_2016$totunits == "-"] <- ''
brooklyn_2016$landsqft[brooklyn_2016$landsqft == "-"] <- ''
brooklyn_2016$grosssqft[brooklyn_2016$grosssqft == "-"] <- ''
brooklyn_2016$taxclasssale[brooklyn_2016$taxclasssale == "-"] <- ''

#replace column value '-' with zero
brooklyn_2016$price[brooklyn_2016$price == "-"] <- '0'

#replace column value '0' with empty
brooklyn_2016$yrbuilt[brooklyn_2016$yrbuilt == "0"] <- ''

#change data types for following columns block
brooklyn_2016$borough <- as.integer(brooklyn_2016$borough)
brooklyn_2016$block <- as.integer(brooklyn_2016$block)
brooklyn_2016$lot <- as.integer(brooklyn_2016$lot)
brooklyn_2016$resunits <- as.integer(brooklyn_2016$resunits)
brooklyn_2016$comunits <- as.integer(brooklyn_2016$comunits)
brooklyn_2016$totunits <- as.integer(brooklyn_2016$totunits)
brooklyn_2016$landsqft <- as.numeric(gsub(",", "", brooklyn_2016$landsqft))
brooklyn_2016$grosssqft <- as.numeric(gsub(",", "", brooklyn_2016$grosssqft))
brooklyn_2016$yrbuilt <- as.integer(brooklyn_2016$yrbuilt)
brooklyn_2016$taxclasssale <- as.integer(brooklyn_2016$taxclasssale)
brooklyn_2016$price <- as.numeric(gsub(",", "", brooklyn_2016$price))
brooklyn_2016$date <- as.Date(brooklyn_2016$date, format="%m/%d/%Y")


