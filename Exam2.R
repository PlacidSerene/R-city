options(stringsAsFactors = FALSE)
# Recommended packages
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(modelr)
library(purrr)
library(gridExtra)


# Data 
load('census_city_2009.Rda')
load('census_city_2014.Rda')
load('census_city_2019.Rda')
# load('example_df.Rda')

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
highest_number_homeless <- city %>% arrange(desc(homeless)) %>% slice(1:3) %>% select(city,homeless)
highest_rate_homeless <- city %>% arrange(desc(homeless_rate)) %>% slice(1:3) %>% select(city,homeless_rate)


# 4. What are the fastest growing cities? Show 3 plots combined with grid arrange.

## New method
top_city <- city %>% mutate(top='Others') %>% arrange(desc(expected_growth))
plot1 <- ggplot(top_city[1:10,], aes(x=city, y=expected_growth)) +geom_bar(stat='identity') + coord_flip()
top_city[1:10,]$top <- 'Top 10 Growth'
top_city <- top_city %>% arrange(desc(expected_growth_rate))
plot2 <- ggplot(top_city[1:10,], aes(x=city, y=expected_growth_rate)) +geom_bar(stat='identity') + coord_flip()
top_city[1:10,]$top <- 'Top 10 Growth Rate'
top_city$top <- factor(top_city$top, levels = c('Top 10 Growth','Top 10 Growth Rate','Others'))
plot3 <- ggplot(top_city, aes(x=expected_growth, y=expected_growth_rate, color=top)) + geom_point() + scale_colour_manual(values=c('red','green','gray'))

grid.arrange(plot1,plot2,plot3, ncol=1)
# 5. Do growing cities have higher homeless rates?
# Show a scatterplot between the rate of growth and the rate of homeless in 2019.
plot4 <- ggplot(top_city, aes(x=homeless_rate, y=expected_growth_rate)) +geom_point()
