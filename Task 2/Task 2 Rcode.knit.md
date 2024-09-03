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
preTrialMeasures <- measureOverTime %>% filter(YEARMONTH 


