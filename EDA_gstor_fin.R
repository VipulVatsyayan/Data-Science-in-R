
#install.packages('jsonlite')
library(data.table)
library(jsonlite)
#library(readr)
library(tidyverse)
# library(dplyr)
# library(tidyr)
library(magrittr)
library(lubridate)
library(purrr)
library(ggplot2)
library(gridExtra)
#install.packages('countrycode')
library(countrycode)
library(highcharter)
library(ggExtra)


dtrain <- read_csv('train.csv')


#Taking a look at the data.
glimpse(dtrain)

#Converting the date variables.

dtrain$date <- as.Date(as.character(dtrain$date), format = '%Y%m%d')
dtrain$visitStartTime <- as_datetime(dtrain$visitStartTime)


# Convverting the  JSON ( Java format) and then dropping the original Json variables,

tr_device <- paste("[", paste(dtrain$device, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_geoNetwork <- paste("[", paste(dtrain$geoNetwork, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_totals <- paste("[", paste(dtrain$totals, collapse = ","), "]") %>% fromJSON(flatten = T)
tr_trafficSource <- paste("[", paste(dtrain$trafficSource, collapse = ","), "]") %>% fromJSON(flatten = T)

dtrain <- cbind(dtrain, tr_device, tr_geoNetwork, tr_totals, tr_trafficSource) %>%
  as.data.table()

# drop the old json columns
dtrain[, c('device', 'geoNetwork', 'totals', 'trafficSource') := NULL]


# values to convert to NA
na_vals <- c('unknown.unknown', '(not set)', 'not available in demo dataset', 
             '(not provided)', '(none)', '<NA>')

for(col in names(dtrain)) {
  
  set(dtrain, i=which(dtrain[[col]] %in% na_vals), j=col, value=NA)
  
}

# Several of the newly parsed json columns have only 1 unique value, 
# e.g. ‘not available in demo dataset’. These columns are obviously useless, so we drop them here.

# get number of unique values in each column
unique <- sapply(dtrain, function(x) { length(unique(x[!is.na(x)])) })

# subset to == 1
one_val <- names(unique[unique <= 1])

# but keep bounces and newVisits
one_val = setdiff(one_val, c('bounces', 'newVisits'))

# drop columns from dtrain
dtrain[, (one_val) := NULL]

glimpse(dtrain)


# character columns to convert to numeric
num_cols <- c('hits', 'pageviews', 'bounces', 'newVisits',
              'transactionRevenue')

# change columns to numeric
dtrain[, (num_cols) := lapply(.SD, as.numeric), .SDcols=num_cols]

#Divide transactionRevenue by 1,000,000
dtrain[, transactionRevenue := transactionRevenue / 1e+06]
dtrain$transactionRevenue


#Missing  values  in columns

data.table(
  pmiss = sapply(dtrain, function(x) { (sum(is.na(x)) / length(x)) }),
  column = names(dtrain)
) %>%
  ggplot(aes(x = reorder(column, -pmiss), y = pmiss)) +
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(
    title='Missing data by feature',
    x='Feature',
    y='% missing')

#  Checking the range of date.
range(dtrain$date)

range(dtrain$transactionRevenue, na.rm = TRUE)

dtrain %>% 
  ggplot(aes(x = log(transactionRevenue), y = ..density..))+
  geom_histogram(fill='steelblue', na.rm=TRUE, bins=40)+
  geom_density(aes(x=log(transactionRevenue)), fill='orange', color='orange', alpha=0.3, na.rm=TRUE) + 
  labs(
    title = 'Distribution of transaction revenue',
    x = 'Natural log of transaction revenue'
  )

#  Seeems to be normally distributed with a mean of 4
#daily revenue over the time period 


g1 <- dtrain[, .(n = .N), by=date] %>%
  ggplot(aes(x=date, y=n)) + 
  geom_line(color='steelblue') +
  geom_smooth(color='orange') + 
  labs(
    x='',
    y='Visits (000s)',
    title='Daily visits'
  )

g2 <- dtrain[, .(revenue = sum(transactionRevenue, na.rm=TRUE)), by=date] %>%
  ggplot(aes(x=date, y=revenue)) + 
  geom_line(color='steelblue') +
  geom_smooth(color='orange') + 
  labs(
    x='',
    y='Revenue (unit dollars)',
    title='Daily transaction revenue'
  )

grid.arrange(g1, g2, nrow=2)


#revenue by hour of day.

g1 <-
  dtrain[, .(visitHour = hour(visitStartTime))][
    , .(visits = .N), by = visitHour] %>%
  ggplot(aes(x = visitHour, y = visits / 1000)) +
  geom_line(color = 'steelblue', size = 1) +
  geom_point(color = 'steelblue', size = 2) +
  labs(
    x = 'Hour of day',
    y = 'Visits (000s)',
    title = 'Aggregate visits by hour of day (UTC)',
    subtitle = 'August 1, 2016 to August 1, 2017'
    
  )

g2 <-
  dtrain[, .(transactionRevenue, visitHour = hour(visitStartTime))][
    , .(revenue = sum(transactionRevenue, na.rm =
                        T)), by = visitHour] %>%
  ggplot(aes(x = visitHour, y = revenue / 1000)) +
  geom_line(color = 'steelblue', size = 1) +
  geom_point(color = 'steelblue', size = 2) +
  labs(
    x = 'Hour of day',
    y = 'Transaction revenue (000s)',
    title = 'Aggregate revenue by hour of day (UTC)',
    subtitle = 'August 1, 2016 to August 1, 2017'
    
  )

grid.arrange(g1, g2, nrow = 2)