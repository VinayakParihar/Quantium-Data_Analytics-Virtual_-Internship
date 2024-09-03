# Solution for Task 2
*This file is a solution for the Task 2 of the Quantium Virtual Internship.*

# Open the libraries

library(data.table)
library(tibble)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)
library(dplyr)


# Load the data set
QVI_data <- read_csv("QVI_data.csv")
data <- QVI_data
setDT(data)

# Set themes for plots
      
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))


# Convert DATE to the format yyyymm
monthYear <- format(as.Date(data$DATE), "%Y%m")
data[, YEARMONTH := monthYear]
data$YEARMONTH <- as.numeric(as.character(data$YEARMONTH))

## Calculating Key Measures and creating new data dtable
The client has selected store numbers 77, 86 and 88 as trial stores and want control stores to be established stores that are operational for the entire observation period. We would want to match trial stores to control stores that are similar to the trial store prior to the trial period of Feb 2019 in terms of :

Monthly overall sales revenue
Monthly number of customers
Monthly number of transactions per customer
Let’s first create the metrics of interest and filter to stores that are present throughout the pre-trial period.

measureOverTime <- data %>%
  group_by(STORE_NBR, YEARMONTH) %>%
  summarise(
    totSales = sum(TOT_SALES),
    nCustomers = n_distinct(LYLTY_CARD_NBR),
    nTxnPerCust = n_distinct(TXN_ID) / n_distinct(LYLTY_CARD_NBR),
    nChipsPerTxn = sum(PROD_QTY) / n_distinct(TXN_ID),
    avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)
  )

# Filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- as.data.table(table(measureOverTime$STORE_NBR))
storesWithFullObs <- storesWithFullObs %>% filter(N==12)
storesWithFullObs<-setNames(storesWithFullObs,c("STORE_NBR","N"))
preTrialMeasures <- measureOverTime %>% filter(YEARMONTH< 201902,STORE_NBR %in% storesWithFullObs$STORE_NBR)

## Create a function to calculate correlation for a measure, looping through each control store.
#For Sales
trialStore_sales <- preTrialMeasures %>% filter(STORE_NBR ==77)
trialStore_sales <- trialStore_sales %>% select(STORE_NBR,YEARMONTH,totSales,nCustomers)
calCorr <- function(preTrialMeasures,trialStore_sales,trialStoreN){
  
  calTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  
  stN <- preTrialMeasures %>% select(STORE_NBR)
  
  for(i in stN$STORE_NBR){
    
    contSt <- preTrialMeasures %>% filter(STORE_NBR==i)
    contSt <- contSt %>% select(totSales)
    
    calMeasure = data.table("Store1" = trialStoreN, "Store2" = i, "corr_measure" = cor(trialStore_sales$totSales,contSt$totSales))
    
    calTable <- rbind(calTable, calMeasure) }
  return(calTable)
}
##For Customers
calculateCorrelation <- function(preTrialMeasures,trialStore_sales,trialStoreN){
  
  calTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  
  stN <- preTrialMeasures %>% select(STORE_NBR)
  
  for(i in stN$STORE_NBR){
    
    contSt <- preTrialMeasures %>% filter(STORE_NBR==i)
    contSt <- contSt %>% select(nCustomers)
    
    calMeasure = data.table("Store1" = trialStoreN, "Store2" = i, "corr_measure" = cor(trialStore_sales$nCustomers,contSt$nCustomers))
    
    calTable <- rbind(calTable, calMeasure) }
  return(calTable)
}

##Apart from correlation, we can also calculate a standardised metric based on the absolute difference between the trial store’s performance and each control store’s performance.
##Let’s write a function for this.

## Create a function to calculate a standardised magnitude distance for a measure, looping through each control store
###Sales
calculateMagnitudeDistance1 <- function(preTrialMeasures,trialStore_sales,trial_storeN){
  calTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(),mag_measure = numeric())
  stN <- preTrialMeasures %>% select(STORE_NBR)
  for(i in stN$STORE_NBR){
    contSt <- preTrialMeasures %>% filter(STORE_NBR==i)
    contSt <- contSt %>% select(totSales)
    calMeasure = data.table("Store1" = trial_storeN, "Store2" = i, "YEARMONTH" = preTrialMeasures$YEARMONTH ,"mag_measure" = abs(trialStore_sales$totSales - contSt$totSales))
    
    calTable <- rbind(calTable,calMeasure) 
    calTable <- unique(calTable)
  }
  return(calTable)
}
###Standardize
standMag1 <- function(magnitude_nSales) {
  minMaxDist <- magnitude_nSales[, .(minDist = min( magnitude_nSales$mag_measure), maxDist = max(magnitude_nSales$mag_measure)), by = c("Store1", "YEARMONTH")]
  distTable <- merge(magnitude_nSales, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (mag_measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(magN_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}
##Customers
calculateMagnitudeDistance2 <- function(preTrialMeasures,trialStore_sales,trial_storeN){
  calTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH = numeric(),mag_measure = numeric())
  stN <- preTrialMeasures %>% select(STORE_NBR)
  for(i in stN$STORE_NBR){
    contSt <- preTrialMeasures %>% filter(STORE_NBR==i)
    contSt <- contSt %>% select(nCustomers)
    calMeasure = data.table("Store1" = trial_storeN, "Store2" = i, "YEARMONTH" = preTrialMeasures$YEARMONTH ,"mag_measure" = abs(trialStore_sales$nCustomers - contSt$nCustomers))
    
    calTable <- rbind(calTable,calMeasure) 
    calTable <- unique(calTable)
  }
  return(calTable)
}
##Standardize
standMag2 <- function(magnitude_nCustomers) {
  minMaxDist <- magnitude_nCustomers[, .(minDist = min( magnitude_nCustomers$mag_measure), maxDist = max(magnitude_nCustomers$mag_measure)), by = c("Store1", "YEARMONTH")]
  distTable <- merge(magnitude_nCustomers, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (mag_measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(magN_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}

Now let’s use the functions to find the control stores! We’ll select control stores based on how similar monthly total sales in dollar amounts and monthly number of customers are to the trial stores. So we will need to use our functions to get four scores, two for each of total sales and total customers

## Use the function you created to calculate correlations against store 77 using total sales and number of customers.
trial_store <- 77
corr_nSales <- calCorr(preTrialMeasures,trialStore_sales,trial_store)
corr_nSales <- unique(corr_nSales)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, trialStore_sales, trial_store )
corr_nCustomers <- unique(corr_nCustomers)

## Use the functions for calculating magnitude
magnitude_nSales <- calculateMagnitudeDistance1(preTrialMeasures, trialStore_sales, trial_store)
magnitude_nSales <- standMag1(magnitude_nSales)
magnitude_nCustomers <‐ calculateMagnitudeDistance2(preTrialMeasures,trialStore_sales, trial_store)
magnitude_nCustomers <- standMag2(magnitude_nCustomers)

We’ll need to combine the all the scores calculated using our function to create a composite score to rank on.

## Let’s take a simple average of the correlation and magnitude scores for each driver. Note that if we consider it more important for the trend of the drivers to be similar, we can increase the weight of the correlation score (a simple average gives a weight of 0.5 to the corr_weight) or if we consider the absolute size of the drivers to be more important, we can lower the weight of the correlation score.

corr_weight <- 0.5
score_nSales <- merge(corr_nSales,magnitude_nSales, by = c("Store1", "Store2"))
score_nSales <- score_nSales %>% mutate(scoreNSales = (score_nSales$corr_measure * corr_weight)+(score_nSales$magN_measure * (1 - corr_weight)))
score_nCustomers <- merge(corr_nCustomers,magnitude_nCustomers, by = c("Store1", "Store2"))
score_nCustomers <- score_nCustomers %>% mutate(scoreNCust = (score_nCustomers$corr_measure * corr_weight)+(score_nCustomers$magN_measure * (1 - corr_weight)))
Now we have a score for each of total number of sales and number of customers. Let’s combine the two via a simple average.

score_Control <- merge(score_nSales,score_nCustomers, by = c("Store1", "Store2"))
score_Control <- score_Control %>% mutate(finalControlScore = (scoreNSales * 0.5) + (scoreNCust * 0.5))
The store with the highest score is then selected as the control store since it is most similar to the trial store.

### Select control stores based on the highest matching store (closest to 1 but not the store itself, i.e. the second ranked highest store)
control_store <- score_Control[order(-finalControlScore),]
control_store <- control_store$Store2




#### Visual checks on trends based on the drivers

measureOverTimeSales <- as.data.table(measureOverTime)
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",ifelse(STORE_NBR == control_store,"Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][YEARMONTH < 201903 , ]

##Visualize
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) + geom_line() + labs(x = "Month of Operation", y = "Total Sales", title = "Total Sales by Month")



control_store <- control_store[2]




