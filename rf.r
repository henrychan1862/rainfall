#download dataset from this url :
###download only Daily Total Rainfall All Year - Hong Kong Observatory###
#https://data.gov.hk/en-data/dataset/hk-hko-rss-daily-total-rainfall
#place it inside a folder named "data"

#import packages
library(tidyverse)
library(lubridate)

#import dataset
rainfall <- read_csv("data/daily_HKO_RF_ALL.csv", skip = 2)


#=========clean rainfall===========
glimpse(rainfall)

#rename the columns
rainfall <- rainfall %>%
  rename(
    "year" = "年/Year",
    "month" = "月/Month",
    "day" = "日/Day",
    "value" = "數值/Value",
    "completeness" = "數據完整性/data Completeness")

#remove NAs
summary(rainfall)
rainfall <- rainfall %>%
  drop_na()

#"Trace" means rainfall less than 0.05mm
rainfall %>%
  count(value)

rainfall %>%
  count(value, sort=TRUE)

#mutate value
rainfall <- rainfall %>%
  mutate(
    value = case_when(value=="Trace"~"0.05",
                      value!="Trace"~value)
  )

#set it as double
rainfall <- rainfall %>%
  mutate(
    value = as.double(value)
  )

#confirm if there is any NAs in value
sum(is.na(rainfall$value))

#add zero when it is a single digit,
#this will help applying lubridate later
rainfall <- rainfall %>%
  mutate(
    month_zero = case_when(month<10~str_c(0, month),
                           month>=10~as.character(month)),
    day_zero = case_when(day<10~str_c(0, day),
                           day>=10~as.character(day)),
    date = str_c(year, month_zero, day_zero)
  )

#combine year, month and day into a single column,
#and select only date and value
rainfall <- rainfall %>%
  mutate(
    date = ymd(date)
  ) %>%
  select(date, value)

#check the final result
glimpse(rainfall)

#export the clean dataset
write_csv(rainfall, "data/rainfall_clean.csv")
