library(tidyverse)
# library(carat)
library(caret)
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
head(df_master)

# Inner join - df & tests

df_master <- merge(x = df_master, y = df_tests, by = c("Code", "Date"), all.x = TRUE)
head(df_master)

# Inner join - df & countries dataset

# Remove Country field from the countries dataset to avoid duplicates in the merged data frame
df_countries <- df_countries %>%
  select(-Country)

df_master <- merge( x = df_master, y = df_countries, by = "Code", all.x = TRUE)
head(df_master)

summary(df_master)

###############
# Task 1: Q6  #
###############

# Find alll NA values in Recovered and NewTests fields and assign new value as 0
df_master <- df_master %>%
  mutate(Recovered = if_else(is.na(Recovered), 0, Recovered)) %>%
  mutate(NewTests = if_else(is.na(NewTests), 0, NewTests))

head(df_master)

###############
# Task 1: Q7  #
###############

months(df_master$Date)
week(df_master$Date)

df_master <- df_master %>%
  mutate(Month = month(Date)) %>%
  mutate(Week = week(Date))
head(df_master)


###############
# Task 2: Q1  #
###############

df_master <- df_master %>%
  group_by(Country) %>% # Group the dataset by country
  arrange(Date) %>% # Arrange the data by date from low to high
  mutate(CumCases = cumsum(NewCases)) %>%  # Calculate cumulative sum for Cases
  mutate(CumDeaths = cumsum(NewDeaths)) %>% # Calculate cumulative sum for Dealths
  mutate(CumRecovered = cumsum(Recovered)) %>% # Calculate cumulative sum for Recovered
  mutate(CumTests = cumsum(NewTests)) # Calculate cumulative sum for Tests

# Check the functionalities by filtering a country
df_master %>%
  filter(Country == "Bangladesh") %>%
  arrange(desc(Date))


###############
# Task 2: Q2  #
###############

df_master <- df_master %>%
  mutate(Active = CumCases - (CumRecovered + CumDeaths)) %>% # Calculate the number of active cases
  mutate(FatalityRate = CumDeaths / CumCases) # Calculate the number of Fatality Rate
head(df_master)

# Check the functionalities by filtering a country
head(df_master %>%
  filter(Country == "Australia") %>%
  arrange(desc(Date)))

###############
# Task 2: Q3  #
###############

df_master <- df_master %>%
  mutate(Cases_1M_Pop = (CumCases / Population) * 1000000) %>% # Calculate cases per million population
  mutate(Deaths_1M_Pop = (CumDeaths / Population) * 1000000) %>%
  mutate(Recovered_1M_Pop = (CumRecovered / Population) * 1000000) %>%
  mutate(Test_1M_Pop = (CumTests / Population) * 1000000)

###############
# Task 2: Q4  #
###############

df_master %>%
  group_by(Date) %>%
  summarise(TotalDeath = sum(NewDeaths)) %>%
  arrange(desc(TotalDeath)) %>%
  top_n(1) # Get top one row


###############
# Task 2: Q5  #
###############

# Prepare the dataset for the graph

# Get global total by date
df_cumulative <- df_master %>%
  group_by(Date) %>%
  summarise_at(vars(CumCases, CumDeaths, CumRecovered, CumTests), sum)

names(df_cumulative) <- c("Date", "Infected Cases", "Deaths", "Recovered", "Tests")

df_cumulative <- gather(data = df_cumulative, key = "Indicators", value = "Count", -Date)
df_cumulative

ggplot(data = df_cumulative, aes(x = Date, y = Count, color = Indicators)) +
  geom_line() +
  scale_y_log10(label = comma) +
  labs(title = "COVID-19 Cases, Deaths, Recovery and Tests Globally",
       x = "Time")


###############
# Task 2: Q6  #
###############

lastDay_data <- df_master %>%
  filter(Date == "2020-05-05")


###############
# Task 2: Q7  #
###############

lastDay_data %>%
  group_by(Continet) %>%
  summarise_at(vars(CumCases, CumDeaths, CumRecovered, CumTests), sum)

###############
# Task 2: Q8  #
###############

top10activeW <- lastDay_data %>%
  arrange(desc(Active)) %>%
  head(n = 10)
top10activeW

top10casesW <- lastDay_data %>%
  arrange(desc(CumCases)) %>%
  head(n = 10)
top10casesW


top10fatalityW <- lastDay_data %>%
  arrange(desc(FatalityRate)) %>%
  head(n = 10)
top10fatalityW

top10testMW <- lastDay_data %>%
  arrange(desc(Test_1M_Pop)) %>%
  head(n = 10)
top10testMW

###############
# Task 2: Q9 #
###############

top10casesCountries <- top10casesW$Country
top10casesCountries

df_master %>%
  filter(Country %in% top10casesCountries) %>%
  ggplot(aes(x = Date, y = CumCases, color = Country)) +
  geom_line() +
  scale_y_log10(label = comma) +
  labs(y = "Total Cases",
       x = "Day")

###############
# Task 2: Q10 #
###############

top10activeCountries <- top10activeW$Country
top10activeCountries

df_master %>%
  filter(Country %in% top10casesCountries) %>%
  ggplot(aes(x = Date, y = NewCases, color = Country)) +
  geom_line() +
  facet_wrap(~Country) +
  scale_y_log10(label = comma) +
  labs(y = "Total Cases",
       x = "Day")


###############
# Task 2: Q11 #
###############

top10testMW %>%
  select(Date, Country, CumCases, CumTests, Test_1M_Pop) %>%
  rename(`Total number of infected Cases` = CumCases, `Total test so far` = CumTests, `Total test per million of the population` = Test_1M_Pop) %>%
  gather(key = "Indicators", value = "Value", `Total number of infected Cases`:`Total test per million of the population`) %>%
  ggplot(aes(x = Indicators, y = Value, fill = Indicators)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  facet_wrap(~Country, scales = "free") +
  theme(axis.text.x = element_blank())

###############
# Task 2: Q12 #
###############

lastDay_data %>%
  group_by(Continet) %>%
  summarise_at(vars(CumCases, CumDeaths, CumRecovered, CumTests), sum) %>%
  gather(key = "Indicators", value = "Count", -Continet) %>%
  ggplot(aes(x = Indicators, y = Count, fill = Continet)) +
  geom_col() +
  scale_y_log10()


###############
# Task 3: Q1  #
###############

cor_data <- subset(lastDay_data, select = c(CumCases, CumTests, Population, GDP, GDPCapita) )
cor_data


###############
# Task 3: Q2  #
###############

cor_matrix <- cor(cor_data)
cor_matrix

corrplot(cor_matrix)
corrplot(cor_matrix, method = "number")


###############
# Task 3: Q3  #
###############

# Distribution of cumulative cases without tranformation

cor_data %>%
  ggplot(aes(x = CumCases)) +
  geom_histogram()

# Distribution of cumulative cases with tranformation
cor_data %>%
  ggplot(aes(x = log(CumCases))) +
  geom_histogram()

###############
# Task 3: Q4  #
###############

boxplot.stats(cor_data$CumCases)$out

###############
# Task 3: Q5  #
###############

# Divide the cor_data into training and testing data
dt <- createDataPartition(cor_data$CumCases, p = 0.65, list = FALSE)
train<-cor_data[dt,]
test<-cor_data[-dt,]

train
test

###############
# Task 3: Q6  #
###############

# Fit the model
lm_model_GDP_CUM <- lm(CumCases ~ GDP, data = train)

# evaluate model on the test data
summary(lm_model_GDP_CUM)
plot(lm_model_GDP_CUM)

# predicting
test$PreditedCases <- predict(lm_model_GDP_CUM, test)
# print the root mean squared error
rmse(test$CumCases, test$PreditedCases)


###############
# Task 3: Q7  #
###############

lmModel2 <- lm(CumCases ~ . , data = train)
# Validating Regression Coefficients and Models
summary(lmModel2)
plot(lmModel2)


