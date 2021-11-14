library(tidyverse)
library(carat)
library(ggplot2)
library(lubridate)
library(scales)
library(corrplot)


###############
# Task 1: Q1  #
###############

# load data from data folder as data frames from Countires.csv file
df_countries <- read_csv("data/Countries.csv")
df_covid19 <- read_csv("data/COVID19.csv")
df_recovered <- read_csv("data/Recovered.csv")
df_tests <- read_csv("data/Tests.csv")

###############
# Task 1: Q2  #
###############

df_recovered <- df_recovered %>%
  # Create long version of the dataset using gather function
  gather(key = "Date", value = "Recovered", -"Country.Region")
head(df_recovered)

###############
# Task 1: Q3  #
###############

# Assign new column names
names(df_covid19) <- c("Code", "Country", "Continet", "Date", "NewCases", "NewDeaths")
names(df_covid19)

names(df_tests) <- c("Code", "Date", "NewTests")
names(df_tests)

names(df_countries) <- c("Code","Country","Population","GDP","GDPCapita")
names(df_countries)

names(df_recovered) <- c("Country","Date","Recovered")
names(df_recovered)

###############
# Task 1: Q3  #
###############
str(df_covid19)
str(df_recovered)

# Convert the date field from string to date
df_recovered$Date <- as.Date(df_recovered$Date, format = "%Y.%m.%d")
str(df_recovered)

###############
# Task 1: Q5  #
###############

head(df_covid19)
head(df_recovered)

df_master <- merge(x = df_covid19, y = df_recovered, by = c("Country", "Date"), all.x = TRUE)
View(df_master)

# Inner join - df & tests

df_master <- merge(x = df_master, y = df_tests, by = c("Code", "Date"), all.x = TRUE)
View(df_master)

# Inner join - df & countries dataset

# Remove Country field from the countries dataset to avoid duplicates in the merged data frame
df_countries <- df_countries %>%
  select(-Country)

df_master <- merge( x = df_master, y = df_countries, by = "Code", all.x = TRUE)
View(df_master)

summary(df_master)

###############
# Task 1: Q6  #
###############

# Find alll NA values in Recovered and NewTests fields and assign new value as 0
df_master <- df_master %>%
  mutate(Recovered = if_else(is.na(Recovered), 0, Recovered)) %>%
  mutate(NewTests = if_else(is.na(NewTests), 0, NewTests))

View(df_master)

###############
# Task 1: Q7  #
###############

months(df_master$Date)
week(df_master$Date)

df_master <- df_master %>%
  mutate(Month = month(Date)) %>%
  mutate(Week = week(Date))
head(df_master)





