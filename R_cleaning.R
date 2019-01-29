# Before running the code:
#   -- install package "stringr" which is for string processing
#   -- change the setwd() to file path in your system

# @ Zipei Lu @

setwd("C:\\Users\\lzp\\Documents\\UMD\\SPRING19\\ALP\\data")
rawdata <- read.csv("train_forclass.csv",header = T)

is.data.frame(rawdata)
str(rawdata)

# Part 1
# This part aims to do basic cleaning for train_forclass.csv.

# datatype conversion
rawdata <- within(rawdata, 
                  {date <- as.Date(as.character(date),"%Y%m%d")
                  device <- as.character(device)
                  geoNetwork <- as.character(geoNetwork)
                  sessionId <- as.character(sessionId)
                  socialEngagementType <- NULL
                  totals <- as.character(totals)
                  trafficSource <- as.character(trafficSource)
                  visitStartTime <- as.POSIXct(visitStartTime,origin="1970-01-01")})

str(rawdata)

# split column
library(stringr)

# device
rawdata$browser <- str_extract(rawdata$device,"\"browser\": \"[^\"]*\"")
rawdata$browser <- str_sub(rawdata$browser,13,-2)

rawdata$operatingSystem <- str_sub(str_extract(rawdata$device,"\"operatingSystem\": \"[^\"]*\""),21,-2)

rawdata$isMobile <- str_sub(str_extract(rawdata$device,"\"isMobile\": [a-zA-Z ]*"),13)
rawdata$isMobile <- rawdata$isMobile=="true"

# geoNetwork
rawdata$continent <- str_sub(str_extract(rawdata$geoNetwork,"\"continent\": \"[^\"]*\""),15,-2)

rawdata$subContinent <- str_sub(str_extract(rawdata$geoNetwork,"\"subContinent\": \"[^\"]*\""),18,-2)

rawdata$country <- str_sub(str_extract(rawdata$geoNetwork,"\"country\": \"[^\"]*\""),13,-2)

rawdata$region <- str_sub(str_extract(rawdata$geoNetwork,"\"region\": \"[^\"]*\""),12,-2)
rawdata$metro <- str_sub(str_extract(rawdata$geoNetwork,"\"metro\": \"[^\"]*\""),11,-2)
rawdata$city <- str_sub(str_extract(rawdata$geoNetwork,"\"city\": \"[^\"]*\""),10,-2)
rawdata$cityId <- str_sub(str_extract(rawdata$geoNetwork,"\"cityId\": \"[^\"]*\""),12,-2)
rawdata$networkDomain <- str_sub(str_extract(rawdata$geoNetwork,"\"networkDomain\": \"[^\"]*\""),19,-2)

# totals
rawdata$visits <- as.integer(str_sub(str_extract(rawdata$totals,"\"visits\": \"[0-9]*\""),12,-2))
rawdata$hits <- as.integer(str_sub(str_extract(rawdata$totals,"\"hits\": \"[0-9]*\""),10,-2))
rawdata$pageviews <- as.integer(str_sub(str_extract(rawdata$totals,"\"pageviews\": \"[0-9]*\""),15,-2))
rawdata$bounces <- as.integer(str_sub(str_extract(rawdata$totals,"\"bounces\": \"[0-9]*\""),13,-2))
rawdata$newVisits <- as.integer(str_sub(str_extract(rawdata$totals,"\"newVisits\": \"[0-9]*\""),15,-2))
rawdata$transactionRevenue <- str_sub(str_extract(rawdata$totals,"\"transactionRevenue\": \"[0-9]*\""),24,-2)

# trafficSource
rawdata$referralPath <- str_sub(str_extract(rawdata$trafficSource,"\"referralPath\": \"[^\"]*\""),18,-2)
rawdata$campaign <- str_sub(str_extract(rawdata$trafficSource,"\"campaign\": \"[^\"]*\""),14,-2)
rawdata$source <- str_sub(str_extract(rawdata$trafficSource,"\"source\": \"[^\"]*\""),12,-2)
rawdata$medium <- str_sub(str_extract(rawdata$trafficSource,"\"medium\": \"[^\"]*\""),12,-2)
rawdata$keyword <- str_sub(str_extract(rawdata$trafficSource,"\"keyword\": \"[^\"]*\""),13,-2)
rawdata$isTrueDirect <- str_sub(str_extract(rawdata$trafficSource,"\"isTrueDirect\": [a-zA-Z]*"),17,-1)=="true"

# delete old columns
rawdata$device <- NULL
rawdata$geoNetwork <- NULL
rawdata$totals <- NULL
rawdata$trafficSource <- NULL

# generate unique ID
rawdata$completeId <- paste(rawdata$fullVisitorId,rawdata$visitId,sep = "")

str(rawdata)

# output to csv file "cleaned.v1". 
# Note: separator here is tab (not comma).
# "cleaned.v1" contains all missing values without recoding.
write.table(rawdata,file="rawdata_cleaned v1.csv",quote = F,sep = "\t",row.names = F)

# test the cleaned data. Remember to set sep = "\t".
cleaned.v1 <- read.csv("rawdata_cleaned v1.csv",sep = "\t",header = T)
str(cleaned.v1)


# Part 2
# This part deals with missing values.

# "browser"

levels(cleaned.v1$browser)
# first check levels in browser. You will find "(not set)" and "0" in this variable.
# I would recommend coding "(not set)" as NA and keep"0" as we are not sure what it means.

rawdata$browser[rawdata$browser=="(not set)"] <- NA
levels(as.factor(rawdata$browser)) # check if the conversion was successful.

# "operatingSystem"

levels(cleaned.v1$operatingSystem)

rawdata$operatingSystem[rawdata$operatingSystem=="(not set)"] <- NA
levels(as.factor(rawdata$operatingSystem))

# "isMobile"
# seems good...

# "continent"
levels(cleaned.v1$continent)

rawdata$continent[rawdata$continent=="(not set)"] <- NA
levels(as.factor(rawdata$continent))

# "subcontinent"
levels(cleaned.v1$subContinent)

rawdata$subContinent[rawdata$subContinent=="(not set)"] <- NA
levels(as.factor(rawdata$subContinent))

# "country"
levels(cleaned.v1$country)

rawdata$country[rawdata$country=="(not set)"] <- NA

# "region"
levels(cleaned.v1$region)
rawdata$region[rawdata$region=="(not set)"] <- NA

# "city"
levels(cleaned.v1$city)
rawdata$city[rawdata$city=="(not set)"] <- NA

# "cityId"
rawdata$cityId <- NULL

# "networkDomain"
rawdata$networkDomain[rawdata$networkDomain=="(not set)"] <- NA

# "campaign"
levels(cleaned.v1$campaign)
rawdata$campaign[rawdata$campaign=="(not set)"] <- NA

# "source"
levels(cleaned.v1$source)
rawdata$source[rawdata$source=="(not set)"] <- NA

# "medium"
levels(cleaned.v1$medium)
rawdata$medium[rawdata$medium=="(not set)"] <- NA

# "keyword"
levels(cleaned.v1$keyword)
rawdata$keyword[rawdata$keyword=="(not provided)"] <- NA

# "isTrueDirect"
rawdata$isTrueDirect[is.na(rawdata$isTrueDirect)] <- FALSE

# output "cleaned v2"
write.table(rawdata,file="rawdata_cleaned v2.csv",quote = F,sep = "\t",row.names = F)

# test the cleaned data. Remember to set sep = "\t".
cleaned.v2 <- read.csv("rawdata_cleaned v2.csv",sep = "\t",header = T)
str(cleaned.v2)
