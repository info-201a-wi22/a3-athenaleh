# set working directory 
setwd("~/Documents/Info201Code/a3-athenaleh/source")

# library
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(maps)
library(reshape2)
library(patchwork)

# csv file
incarceration_dataset <- read.csv("incarceration_trends.csv")

# summary - 5 values 
# California has the highest jail population in 2018
highest_jail_pop <- incarceration_dataset %>%
  # give year 2018
  filter(year == "2018") %>%
  # want state value 
  group_by(state) %>%
  # summarize will add up all the total population in jail (of all counties)
  summarize(total_pop_in_jail = sum(total_pop_15to64)) %>%
  # filter to give us the highest value of that total population in jail 
  filter(total_pop_in_jail == max(total_pop_in_jail)) %>%
  # gives us the state with the highest total population in jail
  pull(state) 

# = sets value, == is this value true 

# Population of each race in California in 2018
races_jail_pop <- incarceration_dataset %>%
  # give year 2018
  filter(year == "2018") %>%
  # want state value 
  group_by(state) %>%
  # sum of each race data // used summarize to get rid of other data & keep this one
  summarize(total_pop_in_jail = sum(total_pop_15to64), 
            total_aapi_pop = sum(aapi_pop_15to64),
            total_black_pop = sum(black_pop_15to64),
            total_latinx_pop = sum(latinx_pop_15to64),
            total_native_pop = sum(native_pop_15to64),
            total_white_pop = sum(white_pop_15to64)) %>%
  # select only the ones that we want (we already did that above so the line
  # below is not necessary // keeping this line in case we change it to mutate)
  # select(state, total_pop_in_jail, total_aapi_pop, total_black_pop, total_latinx_pop,
         #total_native_pop, total_white_pop)
  filter(state == "CA") 

# pull number of total population 
CA_total_pop <- races_jail_pop %>%
  pull(total_pop_in_jail)

# pull number of asian american / pacific islander population
CA_aapi_total <- races_jail_pop %>%
  pull(total_aapi_pop)

# pull number of black population
CA_black_pop <- races_jail_pop %>%
  pull(total_black_pop)

# pull number of latinx population
CA_latinx_pop <- races_jail_pop %>%
  pull(total_latinx_pop)

# pull number of native population 
CA_native_pop <- races_jail_pop %>%
  pull(total_native_pop)

# pull number of white population
CA_white_pop <- races_jail_pop %>%
  pull(total_white_pop)

# chart that shows trends over time in California
CA_over_time <- incarceration_dataset %>% 
  # want data in California
  filter(state == "CA") %>%
  # group by year because we want to combine all data into that year
  group_by(year) %>%
  # total population per year (since we used group_by)
  summarize(total_aapi_pop = sum(aapi_pop_15to64),
            total_black_pop = sum(black_pop_15to64),
            total_latinx_pop = sum(latinx_pop_15to64),
            total_native_pop = sum(native_pop_15to64),
            total_white_pop = sum(white_pop_15to64)) %>%
  # omit NA numbers
  na.omit() %>%
  # change dataset around for ggplot 
  melt(id.vars = c("year"))
  
plot_1 <- ggplot(CA_over_time, aes(x = year, y = value, color = variable)) +
  geom_smooth() + 
  labs(title = "Total Incarcerated Population by Races in CA", subtitle = "Measured per person", 
       x = "Year", y = "Population", color = "Races", 
       caption = "Source: Vera Institute of Justice") + 
  scale_color_manual(labels = c("Asian American / Pacific Islander", "Black", 
                                "Latinx", "Native American", "White"),
                     values = c("#264653", "#006d77", "#83c5be", "#9c6644", 
                                "#e29578")) +
  theme_minimal()
  


# chart that compares two variables to one another
# jail rated capacity vs. total population in jail in CA 
total_pop_in_jail_CA <- incarceration_dataset %>%
  filter(year == "2018" & state == "CA") %>% # filter year and state
  # select rows that I want to use 
  select(jail_rated_capacity, black_pop_15to64, latinx_pop_15to64, 
         native_pop_15to64, white_pop_15to64, aapi_pop_15to64) %>%
  # rename so it makes sense in the legend of the chart
  rename("Black" = black_pop_15to64, "Latinx" = latinx_pop_15to64, 
         "Native" = native_pop_15to64, "White" = white_pop_15to64, 
         "Asian American / Pacific Islander" = aapi_pop_15to64) %>%
  # change dataset around for ggplot 
  melt(id.vars = c("jail_rated_capacity")) %>%
  # rename variable to Races to change the legend name in ggplot
  rename("Races" = variable)

plot_2 <- ggplot(total_pop_in_jail_CA, mapping = aes(x = value, y = jail_rated_capacity, 
       color = Races)) + 
  labs(title = "Races in Jail Rated Capacity in California", 
       x = "Population in Jail", y = "Jail Rated Capacity", 
       caption = "Source: Vera Institute of Justice") + 
  geom_point()

# map of CA county in 2018
US_county_map <- map_data("county") %>%
  unite(col="polyname", c('region', 'subregion'), sep = ",") %>%
  left_join(county.fips, by = "polyname")

CA_county <- incarceration_dataset %>%
  filter(year == "2018" & state == "CA") %>%
  select(year, fips, county_name, total_jail_pop) 

CA_county_jail_pop <- left_join(CA_county, US_county_map, by = "fips")

plot_3 <- ggplot(data = CA_county_jail_pop) + 
  geom_polygon(mapping = aes(x = long, y = lat, 
                             group = group, fill = total_jail_pop)) +
  labs(title = "California County Jail Population", 
       caption = "Source: Vera Institute of Justice", 
       fill = "Total Jail Population") +
  coord_map() + theme_void() 

