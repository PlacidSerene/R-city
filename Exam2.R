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
plot4 <- ggplot(top_city, aes(x=expected_growth_rate, y=homeless_rate)) +geom_point()
test <-  cor(top_city$homeless_rate, top_city$expected_growth_rate)

correlation <- top_city %>% keep(is.numeric) %>% select(-year) %>% pivot_longer(names_to='key', values_to ='value', -homeless_rate) %>% group_by(key) %>% summarize(cor=cor(value,homeless_rate,use='complete.obs'))%>% 
  mutate(Rank = dense_rank(1-abs(cor))) %>% arrange(Rank) 
# slight relationship
# 1 or 2 other features: since others relate to homeless, I would pick household_income and household_size. They have a negative cor meaning that cities which have small household size and less income will likely to have higher rate of homeless.





#6. What cities have the highest expected increase in homelessness and how could they eliminate homelessness?
homeless_increase <- city %>% arrange(desc(expected_homeless_increase)) %>% slice(1:10)
top_city <- top_city %>% mutate(build=ceiling(expected_homeless/household_size))
top_city <- top_city %>% mutate(share=(population_2029/housing_units)-household_size)
top_city <- top_city %>% mutate(top_homeless='Others') %>% arrange(desc(expected_homeless_increase))
top_city[1:100,]$top_homeless <- 'Top Homeless Increase'
top_city$top_homeless <- factor(top_city$top_homeless, levels = c('Top Homeless Increase','Others'))
plot5 <- ggplot(top_city, aes(x=longitude,y=latitude,color=top_homeless)) + geom_point() + borders('state') + scale_colour_manual(values=c('red','green'))
#Average for all cities
house_and_share <- data.frame(avg_build=mean(top_city$build), avg_share=mean(top_city$share))

more_house <- top_city %>% arrange(desc(build)) %>% slice(1:10)
more_share <- top_city %>% arrange(desc(share)) %>% slice(1:10)