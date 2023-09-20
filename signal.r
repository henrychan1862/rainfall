#download dataset from this url:
#https://www.kaggle.com/datasets/henrychan1862/hong-kong-rainstorm-record-apr1998aug2022
#extract the zipped file and place it inside a folder named "data"

#import packages
library(tidyverse)
library(lubridate)
library(janitor)

#import dataset
signal <- read_csv("data/rainstorm9804_2208.csv")

#=========clean signal===========
glimpse(signal)
summary(signal)

#use clean_names() to get tidier column names
signal <- signal %>% clean_names()

#parse start_date and end_date as date type, duration as period type
signal <- signal %>%
  mutate(
    start_date = dmy(start_date),
    end_date = dmy(end_date)
  )

#mutate interval and duration
signal <- signal %>%
  mutate(
    duration = hm(duration)
  )

#check the final result
glimpse(signal)

#export the clean dataset
write_csv(signal, "data/signal_clean.csv")
