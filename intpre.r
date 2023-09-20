###BEFORE YOU RUN THIS SCRIPT###
#1. run landslide.r
#2. run rf.r
#3. run signal.r
#these will generate you the cleaned datasets for following analysis

#import packages
library(tidyverse)
library(lubridate)
library(janitor)

#import cleaned datasets
landslide <- read_csv("data/landslide_clean.csv")
rainfall <- read_csv("data/rainfall_clean.csv")
signal <- read_csv("data/signal_clean.csv")

#=====merge landslide and rainfall datasets=====
glimpse(landslide)
glimpse(rainfall)
glimpse(signal)

#duration column in signal is not correctly parsed as Period 
signal <- signal %>%
  mutate(
    duration = as.period(duration)
  )

#filter rainfall record between 1983-09-28 and 2020-12-28
rainfall_83 <- rainfall %>%
  filter(date>="1983-01-01"&date<="2020-12-31")

#get the previous rainfall data of a specific day
previous <- vector(mode="list", length=14)
for (i in c(1:14)) {
  previous[[i]] <- lag(rainfall_83$value, n=i)
}
#set proper names
previous<- as_tibble(previous, .name_repair = "unique")
names(previous) <- str_c("d", c(1:14))
previous <- previous %>%
  mutate(
    date = rainfall_83$date
  )

glimpse(previous)

#integrate previous into rainfall_83
rainfall83_wp <- full_join(rainfall_83, previous, by="date")

#filter part of interest i.e. date of first landslide
rainfall83_wp <- rainfall83_wp %>%
  filter(date>="1983-10-12") %>%
  mutate(
    d0 = value
  ) %>%
  select(date, d0, d1:d14)

#summarise the total rainfall in past 5 days, past 10 days
#and past 14 days. Total will mean day0 + past14
rainfall_sum <- rainfall83_wp %>%
  transmute(
    faildate = date,
    day0 = d0,
    past5 = select(., d1:d5) %>% rowSums(),
    past10 = select(., d1:d10) %>% rowSums(),
    past14 = select(., d1:d14) %>% rowSums(),
    total = day0+past14
  )

glimpse(rainfall_sum)

#merge rainfall_sum with landslide
RFSLS <- right_join(rainfall_sum, landslide, by="faildate")

#=====Q1: pattern of rainfall before landslide=====
#plot PDF
#get the required columns from RFSLS,
#pivot it into longer format first
tdyp14 <- RFSLS %>%
  select(faildate:total) %>%
  pivot_longer(cols = day0:total, names_to = "type", values_to = "rainfall") %>%
  mutate(
    type = fct_reorder(type, rainfall)
    )

ggplot(data=tdyp14) +
  ggtitle("PDF of Landslide against Rainfall") +
  geom_density(aes(x=rainfall, fill=type))+
  xlab("Rainfall (mm)") +
  ylab("Probability density of landslide") +
#  scale_color_brewer(name="", palette="Set2", 
#                     labels=c("Day 0", "Past 14 days", "Total")) +
  theme(legend.position = c(.8,.8))

#ecdf version
ggplot(data=tdyp14, aes(x=rainfall, color=type)) +
  ggtitle("ECDF of Landslide against Rainfall") +
  xlab("Rainfall (mm)") +
  ylab("Proportion of landslide") +
  stat_ecdf(geom = "point")+
  stat_ecdf(geom="step")+
  scale_color_brewer(name="Category", palette="Set2", 
                     labels=c("Day 0", "Past 5 days",
                              "Past 10 days", "Past 14 days", "Total"))+
  theme(legend.position = c(.9,.1))

  
#boxplot as well 
ggplot(data=tdyp14) +
  ggtitle("Boxplot of Cateroies of Rainfall") +
  geom_boxplot(aes(x=type, y=rainfall))+
  ylab("Rainfall (mm)") +
  xlab("Category of rainfall")

#the above plots suggest that previous rainfall is a crucial factor of landslide,
#as there were not rainfall recorded on day0 in many cases
#this is also why most of the landslide occurs in summer, as shown below

#=====Q1: pattern of rainfall and landslide part2======
#EDA across months

#get total cases in each month (all-time)
month_acc <- RFSLS %>%
  select(faildate, total) %>%
  mutate(
    month = month(faildate, abbr = TRUE, label = TRUE)
  ) %>%
  count(month)

month_acc

#get average monthly rainfall between 1983-2020
month_avg <- rainfall_83 %>%
  mutate(
    year = year(date),
    month = month(date, abbr = TRUE, label = TRUE)
  ) %>%
  group_by(month,year) %>%
  summarise(
    total = sum(value)
  ) %>% ungroup()

month_avg <- month_avg %>%
  group_by(month) %>%
  summarise(avg_total = mean(total))

month_avg

#merge both of them
monthy <- full_join(month_avg, month_acc, by="month")

#pivot longer for analysis
monthy_l <- monthy %>%
  mutate(avg_total=avg_total*2) %>%
  pivot_longer(cols=n:avg_total, names_to = "type", values_to = "count")

monthy_l

#distribution of average monthly rainfall and landslide occurrence 
ggplot(data=monthy_l) +
  ggtitle("Distribution of Average Monthly Rainfall 
          \n and Total No. of Landslide (1983-2020)") +
  xlab("Month") +
  geom_col(aes(x=month, y=count, fill=type), position="dodge") +
  scale_y_continuous(name="Total no. of landslide", 
                     sec.axis=sec_axis(name = "Average monthly rainfall (mm)",
                                       trans = ~./2)) +
  scale_fill_manual(name="",values=c("darkblue", "red"),
                    labels=c("Average monthly rainfall (mm)", "Total no. of landslide"))+
  theme(legend.position = "top")

#obviously they show the same pattern, most cases happened in summer, 
#when the amount of rainfall is highest

#=====Q2: rainall against types of slope=====
#count the number of types of slope
RFSLS %>%
  count(feattype, sort=TRUE)

#classified them into man-made, natural or other
RFSLS <- RFSLS %>%
  mutate(
    stype = case_when(
      feattype=="natural hillside"~"natural",
      feattype=="others"~"other",
      feattype!="natural hillside"&feattype!="others"~"man_made"
    )
  )

RFSLS %>%
  count(stype, sort=TRUE)%>%
  summarise(
    stype = stype,
    n=n,
    prop = n/sum(n)
  )

#over 81% is failure of man-made slope
#boxplot with rainfall against stype and feattype
ggplot(data=RFSLS) +
  geom_boxplot(aes(x=feattype, y=total)) +
  ggtitle("Total Rainfall against Features of Slope")+
  xlab("Feature of slope") +
  ylab("Total Rainfall (mm)") +
  coord_flip()

ggplot(data=RFSLS) +
  geom_boxplot(aes(x=stype, y=total)) +
  ggtitle("Total Rainfall against Types of Slope")+
  xlab("Type of slope")+
  ylab("Total Rainfall (mm)")

#to confirm the result that natural slope is more
#precipitation-resistance than man-made slope,
#i.e. man-made slope is more vulnerable,
#summarise the data as below
RFSLS %>%
  group_by(stype) %>%
  select(stype, total) %>%
  summarise(
    mean = mean(total)
  )
#mean of total rainfall to cause landslide on natural slope
#is slightly higer than that of man-made slope,
#our hypothesis may be true (need to perform statistical test)

#do the same thing to feattype
RFSLS %>%
  group_by(feattype) %>%
  select(stype, total) %>%
  summarise(
    n= n(),
    mean = mean(total)
  )
#retaining wall has the least mean=309mm, which align with reality

#=====Q3: rainfall against scale of failure=====
summary(RFSLS$scale)

#variance is too large, due to several major landslide event
#also quite a number of NAs

#compare the consequence of cases that scale is a NA to 
#the rest
RFSLS %>%
  filter(is.na(scale)) %>%
  select(deathno, injuryno, evacuated, roadclosed) %>%
  summary()

RFSLS %>%
  filter(!is.na(scale)) %>%
  select(deathno, injuryno, evacuated, roadclosed) %>%
  summary()

#by comparing the mean of these numbers, it shows the mean is smaller
#when scale is a NA, but this could be once again due to a few 
#major landslide

#for later analysis, I will remove the NAs for convenience 

#jitter plot for scale against rainfall
ggplot(data=RFSLS) +
  ggtitle("Scale of Failure against Total Rainfall") +
  xlab("Total rainfall (mm)") +
  ylab(expression("Scale of failure ("~m^3~")")) +
  geom_jitter(aes(x=total, y=scale), na.rm=TRUE)

#I can hardly observe any pattern hidden in the plot due to the 
#extreme values in scale. So I will be removing them 

#remove extreme values in scale
#15 is the 3rd quantile of scale
no_extsc <- RFSLS %>%
  filter(scale<15)
no_extsc
#boxplot to check the distribution
ggplot(data=no_extsc) +
  ggtitle("Boxplot of Scale of Failure")+
  xlab(expression("Scale of failure ("~m^3~")"))+
  geom_boxplot(aes(scale))
#about 6500 data points which will be appropriate for
#the following analysis

#plot scale against rainfall
no_extsc <- RFSLS %>%
  filter(scale<15)
ggplot(data=no_extsc) +
  ggtitle("Total Rainfall against Scale of Failure")+
  xlab(expression("Scale of failure ("~m^3~")"))+
  ylab("Total rainfall (mm)")+
  geom_jitter(aes(x=scale, y=total), width=0.4, height=5)

#check for the correlation coefficient as well
no_extsc %>%
  summarise(
    pearson = cor(total, scale, method="pearson"), 
    kendall = cor(total, scale, method= "kendall"),
    spearman = cor(total, scale, method="spearman")
  )
#although Spearman's coefficient is slightly positive,
#the jitter plot reveals that 
#there is not any clear correlation

#check for the number of consequences
RFSLS %>%
  count(deathno, sort=TRUE)
RFSLS %>%
  count(injuryno, sort=TRUE)
RFSLS %>%
  count(roadclosed, sort=TRUE)
RFSLS %>%
  count(evacuated, sort=TRUE)
#except for roadclosed, most of the numbers are 0
#with that, it is meaningless to make boxplots comparing their rainfalls

#=====Q4: rainstorm warning signals and landslide occurrence=====
#rainstorm warning signal will be posted when an intensified rainfall
#is taking place. There are 3 kinds of them, black>red>amber.

summary(signal$duration)
#as the maximum duration is less than 24 hours, we can use
#start_date and end_date to merge faildate in RFSLS

#pivot signal into longer format for further manipulation
signal_ref <- signal %>%
  select(start_date, end_date, color, duration) %>%
  pivot_longer(cols=start_date:end_date, names_to="type", 
               values_to = "faildate") %>%
  select(faildate, color, duration)

#keep only the colour of highest level
#as the representing signal in days that have multiple signals
signal_sim <- signal_ref %>%
  mutate(
    level = case_when(color=="Black"~3,
                      color=="Red"~2,
                      color=="Amber"~1)
  ) %>%
  group_by(faildate) %>%
  summarise(
    rep_color = max(level)
  )
signal_sim

#merge RFSLS to signal_sim i.e. keep landslides that have 
#faildates same in signal_sim
SGRFSLS <- right_join(RFSLS, signal_sim, by="faildate")

#change rep_color to string
SGRFSLS <- SGRFSLS %>%
  mutate(rep_color = case_when(rep_color==3~"Black",
                                rep_color==2~"Red",
                                rep_color==1~"Amber"))
SGRFSLS
#boxplot to confirm the difference in rainfall
#among the 3 signals
ggplot(SGRFSLS)+
  ggtitle("Daily Rainfall when Rainstorm \nWarning Signal is in Force") +
  xlab("Representative Colour of RWS") +
  ylab("Daily Rainfall (mm)") +
  geom_boxplot(aes(x=rep_color, y=day0), na.rm=TRUE)

#odds for amber, red and black
#number of no landslide under signal
nols_sg <- SGRFSLS %>%
  filter(is.na(incid)) %>%
  count(rep_color)
nols_sg

#number of landslide under signal
yesls_sg <- SGRFSLS %>%
  filter(!is.na(incid)) %>%
  count(rep_color)
yesls_sg

#merge the two tables
odds_sg <- full_join(yesls_sg, nols_sg, by="rep_color")
odds_sg <- odds_sg %>% 
    rename("Yes_LS"="n.x", 
           "No_LS"="n.y")
odds_sg %>%
  summarise(
    rep_color = rep_color,
    odds = Yes_LS/No_LS,
    prob = odds/ (1+odds)
  )
#the result is aligned with normal perception
#i.e. heavier rainfall has a higher chance to trigger a landslide

#next compute the odds ratio (OR) of landslide when signal 
#is posting to when it is not posting

#since rainstorm signal is first issued after Apr1998
#filter rainfall_83 to match the range,
#and include signal and landslide occurence between 1983-2020
rainfall83_lsg <- rainfall_83 %>%
  filter(date >= ymd("1998-04-01")) %>%
  mutate(
    landslide = case_when(
      date %in% landslide$faildate ~ "Yes",
      !(date %in% landslide$faildate) ~ "No",
    ),
    signal = case_when(
      date %in% signal_sim$faildate ~ "Yes",
      !(date %in% signal_sim$faildate) ~ "No",
    )
  ) %>%
  select(date, landslide, signal, value)

#make a contingency table
ortab <- xtabs(~signal + landslide, data=rainfall83_lsg)
ortab
#compute the OR
OR <- (307/150) / (1483/6371)
OR
#it is concluded that the OR of landslide when a signal is in force
#is around 8.8 times larger than OR of landslide when 
#there is not any signal

#this may suggest that sudden intensive rainfall 
#is more likely to trigger landslides

