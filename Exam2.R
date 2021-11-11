options(stringsAsFactors = FALSE)
# Recommended packages
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(modelr)
library(purrr)

# Data 
load('census_city_2009.Rda')
load('census_city_2014.Rda')
load('census_city_2019.Rda')
load('example_df.Rda')

# Setup

# Update Column name

colnames(city_2009) <- c("state","place","city","population")
colnames(city_2014) <- c("state","place","city","population")
colnames(city_2019) <- c("state","place","city","population","household_income","housing_units","population_in_house","GEOID","latitude","longitude")

#Combine city data frame 
city <- city_2009 %>% select(city) %>% inner_join(city_2014, by="city") %>% select(city) %>% inner_join(city_2019, by="city")

# Filter
city <- city %>% filter(grepl(" city,",city), population >=25000)


load('city_models.Rda')
cities <- names(models)
city_2029 <- data.frame()
for(i in seq_along(cities)){
  city_temp <- data.frame(city = cities[i], year = 2029)
  city_temp <- city_temp %>% add_predictions(models[[i]],var = 'population')
  city_2029 <- rbind(city_2029, city_temp)
}
colnames(city_2029)[3] <- "population_2029"
city <- city %>% inner_join(city_2029, by='city')

# New calculation column
city <- city %>% mutate(homeless=population - population_in_house,
                        homeless_rate = homeless/population,
                        expected_growth = population_2029-population,
                        expected_growth_rate = expected_growth/population,
                        expected_homeless = homeless_rate * population_2029,
                        expected_homeless_increase = expected_homeless-homeless,
                        household_size = population_in_house/housing_units)


# 3 cities have the most homeless in 2019?
highest_number_homeless <- city %>% arrange(desc(homeless))

