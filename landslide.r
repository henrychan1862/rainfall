#download dataset from this url:
#https://data.gov.hk/en-data/dataset/hk-cedd-csu-cedd-incident/resource/c6b409e1-c4ca-4f81-a515-c6fdc33b4cfd
#extract the zipped file and place it inside a folder named "data"
#reference sheet:
#https://www.geomap.cedd.gov.hk/GEOOpenData/Data/Incident/Incidents_Dict.pdf


#import packages
library(tidyverse)
library(lubridate)

#import dataset
landslide <- read_csv("data/1984 - 2020 landslide data for public_20220315_CSV format.csv")


#=========clean landslide===========
glimpse(landslide)

#remove unrelated columns
landslide <- landslide %>%
  select(-X18, -X19)

#lowercase column case for convenience
names(landslide) <- tolower(names(landslide))

#make long names short
names(landslide)[14:17] <- names(landslide)[14:17] %>%
  str_replace("consec - ", "")

#parse call date and fail date to date format
landslide <- landslide %>%
  mutate(
    calldateProper = dmy(calldate),
    faildateProper = dmy(faildate)
  )

#check for the nas
landslide %>% 
  select(calldateProper, faildateProper, faildate, calldate) %>%
  filter(is.na(calldateProper) | is.na(faildateProper))

#put call dates as missing fail dates, vice versa
landslide <- landslide %>%
  mutate(
    faildateProper = case_when(faildate!="-"~dmy(faildate),
                               faildate=="-"~dmy(calldate)),
    calldateProper = case_when(calldate!="-"~dmy(calldate),
                               calldate=="-"~dmy(faildate)),
  )

#check for the nas again
landslide %>%
  select(calldateProper, faildateProper, faildate, calldate) %>%
  filter(is.na(calldateProper) | is.na(faildateProper))

#landslides occurred in April 2000 may probably come from a single event
landslide %>%
  filter(faildate=="04/2000") %>%
  select(incid, location)

#we can search online to fill in the dates, but we will just exclude them here for convenience
landslide <- landslide %>%
  mutate(
    calldate = calldateProper,
    faildate = faildateProper
  ) %>%
  select(-calldateProper, -faildateProper) %>%
  filter(!(is.na(calldate)|is.na(faildate)))

#confirm that nas are removed
sum(is.na(landslide$calldate))
sum(is.na(landslide$faildate))

#for columns between deathno and roadclosed, 
#set them as double type, and change NAs to 0
landslide <- landslide %>%
  mutate(
    across(.cols = deathno:roadclosed,
           .fns = ~as.double(.x))
  )

landslide <- landslide %>%
  mutate(
    across(.cols = deathno:roadclosed,
           .fns = ~replace_na(data=.x, replace =0)
        )
  )

#for columns ogdincid, slopenum, consec and facaff, 
#put "-" as NAs
landslide <- landslide %>%
  mutate(
    across(.cols = c("ogdincid", "slopenum", "consec", "facaff"),
           .fns = ~na_if(.x, "-")
    )
  )

#however, for consec and facaff column, replace NAs as "Not observed"
#this will make analysis with them easier later
landslide <- landslide %>%
  mutate(
    across(.cols = c("consec", "facaff"),
           .fns = ~replace_na(data=.x, replace ="Not observed")
    )
  )

#confirm that nas are removed
sum(is.na(landslide$consec))
sum(is.na(landslide$consec))

#there are duplicated records in feattype,
#e.g. "Soil Cut" and "Soil cut"
landslide %>%
  count(feattype)

#lowercase them
landslide <- landslide %>%
  mutate(
    feattype = tolower(feattype)
    )

#also, merge all subtypes of retaining wall as "retaining wall"
landslide <- landslide %>%
  mutate(
    feattype = case_when(grepl("retaining", feattype)~"retaining wall",
                         !grepl("retaining", feattype)~feattype
    )
  )

#same for rock cut and soil cut into "soil/rock cut"
landslide <- landslide %>%
  mutate(
    feattype = case_when(grepl("cut", feattype)~"soil/rock cut",
                         !grepl("cut", feattype)~feattype
    )
  )

#check if they are unified
landslide %>%
  count(feattype)

#check the final result
glimpse(landslide)

#export the clean dataset
write_csv(landslide, "data/landslide_clean.csv")
