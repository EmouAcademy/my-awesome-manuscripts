############### Cleaning and Handling MISSING data #########################
# I. Mining bad data
# a) explore the spikes
# b) check the spikes against the other data whether to keep it or delete it
# c) do iterative process with a) and b) for all data elements

## 1. data cleaning
###  1. UB
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
###  2. DZ
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
###  3. SSh
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
###  4. ZU
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
######### Decision
##################################################################
# II. Remove the bad data
# a) replace with NA
# b) replace with Median or use Ratio = pm2.5/pm10
# c) replace with Mean
# III. Data gap filling, carefully choosing the correct strategy
# a) fill the data based on the seasonal/ daily variations/ and consider trend
# b) fill the data with the median/mean or with the some relations
# c) Search for the suitable... method
#############################################################################
library(tidyverse)
library(ggplot2)
library(VIM)
library(readr)

#set current directory
setwd("/Users/munkhtsetseg/WORK/Research/my-awesome-manuscripts/Data")

#read in CSV file


df <- read.csv("Preprocessing data.csv")

#Data$Station.name <- as.factor(Data$Station.name)
#view(Data)

## And remove duplicates
df1 <- df %>%
  distinct()


##### Converting types, renaming.
df1 <-  df1 |>
  mutate(pm2_swap=PM10, pm10_swap=PM2, ratio = pm2_swap/pm10_swap)

glimpse(df1)
df1$Date <- as.Date(df1$Date)
df1$Station.name <- as.factor(df1$Station.name) 





df1 |>
  dplyr::select(Station.name, pm2_swap) |>
  spineMiss()



df1 |>
  dplyr::select(Station.name, pm10_swap) |>
  spineMiss()


##### 1. Remove spikes
### spikes to NA, PM10 must be greater than PM2, and PM must be > 0
## a. data range constrain 0-7
df_spike_up <- df1 |>
  mutate(pm10_miss = replace(pm10_swap, pm10_swap > 7, NA), 
         pm2_miss = replace(pm2_swap, pm2_swap > 7 , NA))
plot(df_spike_up$pm10_miss, df_spike_up$pm2_miss)

df_spike_uplow <- df_spike_up |>
  mutate(pm10 = replace(pm10_miss, pm10_miss ==0, NA), 
         pm2.5 = replace(pm2_miss, pm2_miss ==0, NA))
plot(df_spike_uplow$pm10, df_spike_uplow$pm2.5)

## b. It is suggested that pm10 > pm2.5 value. 
####  But, this rule somehow cannot seen, particularly when the PMs low.
####. Maybe it is sensor accuracy. So:
### b-01 When pm10 =<0.001; if pm2.5>pm10*3.5 ===> pm2.5 <- NA
### b-02 When pm10 >0.5 and pm2.5 < 0.03 ===> pm2.5 <- NA (Note: It is detected with the site)
df_spike_ratio01 <- df_spike_uplow |>
  mutate(pm2.5 = replace(pm2.5, pm2.5 > pm10*3.5 | pm10>0.5 & pm2.5<0.03, NA))
### b-03 When pm10 >0.2 and pm2.5 < 0.01 ===> pm2.5 <- NA (Note: It is detected with UB)
df_spike7_0 <- df_spike_ratio01 |>
  mutate(pm2.5 = replace(pm2.5, pm10>0.2 & pm2.5<0.01, NA))
plot(df_spike7_0$pm10, df_spike7_0$pm2.5)
abline(a = 0, b = 1)



###  1. UB
####     1. detect the 0
####     2. is it temporal or continuous for some time //
####     3. is it error or ... 
####     4. remove or keep
df_spike7_0 |>
  ggplot(aes(pm10, pm2.5, color=Station.name)) +
  geom_point() 

data <- df_spike7_0 |>
  mutate(id = "Gobi") 

data <- data |>
  mutate(id = replace(id, Station.name == "UB", "Urban") )

########### RESULTS
######### 3.1. Two distinct spatial variations of PM10 and PM2.5 concentrations
########## UB: urban
########## Gobi: dust
######### 3.2. Temporal variations of PM10 and PM2.5 concentrations
######### 3.3. Meteorological effects on variations of PM10 and PM2.5 concentrations

########### RESULTS
######### 3.1. Two distinct spatial variations of PM10 and PM2.5 concentrations
########## UB: urban
########## Gobi: dust
######### 3.2. Temporal variations of PM10 and PM2.5 concentrations
######### 3.3. Meteorological effects on variations of PM10 and PM2.5 concentrations

data |>
  filter(Station.name == "Dalanzadgad") |>
  ggplot(aes(pm10, pm2.5, size = WS, colour = Month)) +
  geom_point()

data |>
  filter(Station.name == "Zamynuud") |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Month, group = interaction(Year,Month))) +
  geom_point()

df_spike7_0 |>
  filter(Station.name == "Sainshand") |>
  ggplot(aes(Date, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Year)
####     2. is it temporal or continuous for some time 
### 2014, PM2.5 > PM10 for September ::: June-August has no data: Small variations:; is difficult; Usually scattered
### 2015, PM2.5 > PM10 for April ::: January-March has no data: Is able to remove
###       PM2.5 > PM10 for October : Should remove???! Small value as 2014 case.
### 2016, PM2.5 = 0 Хавар намарт шуурганы нөлөө их бж maybe. 0 болон 0.001 холимог бна
### 2017, PM2.5 = 0 constant small value for February. error to be removed

## UB site: by year

### 2014,
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2014) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

df_spike7_0 |>
  filter(Station.name == "UB", Year == 2014,  Month==9) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

### 2015,
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2015) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

df_spike7_0 |>
  filter(Station.name == "UB", Year == 2015,  Month==10) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

#### 2016. Хавар намарт шуурганы нөлөө их бж. 0 болон 0.001 холимог бна
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2016) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

error2 <- df_spike7_0 |>
  filter(Station.name == "UB", Year == 2016,  Month==3 | Month==4 | Month==5 | Month ==11 | Month==12) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point()+ 
  facet_wrap(~Month)

#### 2017. 
df_spike7_0 |>
  filter(Station.name == "UB", Year == 2017) |>
  ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)
#### 2017. February... strange, better to remove.
 df_spike7_0 |>
  filter(Station.name == "UB", Year == 2017,  Month==2 ) |>
  ggplot(aes(Date, pm2.5, size = WS, color=Visibility)) +
  geom_point() +
  facet_wrap(~Month)

### Decision: technical or equipment error. pm2_miss==0 needs to be changed as NA.

 
#### Sainshand
 
 df_spike7_0 |>
   filter(Station.name == "Sainshand") |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Year)


 #### 2017. 
 df_spike7_0 |>
   filter(Station.name == "Sainshand", Year == 2011) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 #### 2017. February... strange, better to remove.
 df_spike7_0 |>
   filter(Station.name == "Sainshand", Year == 2017,  Month==2 ) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 

 #### Dalanzadgad
 
 df_spike7_0 |>
   filter(Station.name == "Dalanzadgad") |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Year)
 
 
 #### 2017. 
 df_spike7_0 |>
   filter(Station.name == "Dalanzadgad", Year == 2010) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 #### 2017. February... strange, better to remove.
 df_spike7_0 |>
   filter(Station.name == "Sainshand", Year == 2012,  Month==6 ) |>
   ggplot(aes(pm10, pm2.5_1, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 
 
 ### When pm10 =<0.001; if pm2.5>pm10 ===> pm2.5 <- NA
 df_spike7_0 <- df_spike7_0 |>
   mutate(pm2.5_1 = replace(pm2.5, pm10 <0.01 & pm2.5 > pm10, NA))
 
 #### Zamynuud
 
 df_spike7_0 |>
   filter(Station.name == "Zamynuud") |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Year)
 
 
 #### 2017. 
 df_spike7_0 |>
   filter(Station.name == "Zamynuud", Year == 2011) |>
   ggplot(aes(pm10, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 #### 2017. February... strange, better to remove.
 df_spike7_0 |>
   filter(Station.name == "Zamynuud", Year == 2011,  Month==2 | Month==3  ) |>
   ggplot(aes(Date, pm2.5, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 
 
 



###### 
 
 library(forecast)
 library(imputeTS)
 # imp2.5 <- na_kalman(df_spike7_0$pm2.5, maxgap = 15)
 imp10interpol <- na_interpolation(df_spike7_0$pm10, option = "spline",  maxgap = 3)
 #imp10seas <- na_seasplit(df_spike7_0$pm10, find_frequency=TRUE, maxgap = 3)
 ggplot_na_distribution(df_spike7_0$pm10)
 ggplot_na_distribution(imp10interpol)
 #ggplot_na_distribution(imp10)
 #ggplot_na_gapsize(imp10)

 
df_fill <-  
   na_interpolation(df_spike7_0$pm10, option = "spline",  maxgap = 3) |>
   ggplot(aes(Date, pm10, size = WS, color=Visibility)) +
   geom_point() +
   facet_wrap(~Month)
 
df_fill <- df_spike7_0 |>
  filter(Station.name == "Zamynuud") |>
  mutate(pm25 = replace(pm2.5, NA, na_interpolation(df_spike7_0$pm2.5, option = "spline",  maxgap = 3)))
plot(df_spike7_0$pm10, df_spike7_0$pm2.5)
abline(a = 0, b = 1)