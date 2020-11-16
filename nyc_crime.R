#load libraries

library(tidyverse)
library(sparklyr)

# Read Data from internet

df_arrests_hist <-read_csv("https://data.cityofnewyork.us/api/views/8h9b-rp9u/rows.csv?accessType=DOWNLOAD")


df_arrests_ytd <- read_csv("https://data.cityofnewyork.us/api/views/uip8-fykc/rows.csv?accessType=DOWNLOAD")

df_shooting <- read_csv("https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD")


# Exploratory Data Analysis

glimpse(df_arrests_hist)
glimpse(df_arrests_ytd)
glimpse(df_shooting)

summary(df_arrests_hist)
summary(df_arrests_ytd)
summary(df_shooting)
