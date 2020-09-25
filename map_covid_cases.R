#### Data & Donuts Sept. 25, 2020 ####
# By Emily Javan 
# Note: ending a line with 4 '#' signs will create headers in document outline
# The code below will walk you through plotting data from the NYT for COVID-19 cases/deaths in each US county.

#### IMPORT LIBRARIES ####
library(tidyverse) # needed to do %>% pipes
library(usmap) # where county polygons come from
library(ggplot2) # how we plot the county level data

#### GET DATA ####
# NYT case data provides cumulative cases through time, but a county is only added once a case is detected
cty_case_data = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
typeof(cty_case_data$fips) # Even though the fips code is a number it is coded as a character string, because 0's matter
length(unique(cty_case_data$fips)) # On Aug. 31, 2020 I counted 3204 counties
max_date = as.character(max(cty_case_data$date[!is.na(cty_case_data$date)])) # determine the most recent date in column
cty_case_data$date = as.character(cty_case_data$date) # convert all dates from double to character


# Census provided county population data up to 2019, but a lot of other stats as well
cty_pop_data = read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
cty_pop_data$fips = paste0(cty_pop_data$STATE, cty_pop_data$COUNTY)

# Usmap package has polygons for US counties
us_counties=us_map(regions = "counties")
typeof(us_counties$fips) # fips is also a character string, so we can join both data frames
length(unique(us_counties$fips)) # 3142 total counties in US

      # Why does the number of counties appear so much higher in NYT data? ####

#### MERGE DATAFRAMES ####
# We'll plot the most recent data on our map, but any date in the correct format will work e.g. "2020-05-04"
most_recent_case_data = subset(cty_case_data, date==max_date) 

# Join data frames and turn all missing counties cases/deaths to 0
all_counties_cases = us_counties %>% 
  as_tibble() %>% 
  distinct(x, y, group, fips, full, county) %>% # get the fips, state name, and county name from us_counties
  left_join(most_recent_case_data %>% # we'll merge select columns from the two data frames together by the fips code
              select(fips, date, cases, deaths), by = "fips" ) %>%
  left_join(cty_pop_data %>%
              select(fips, POPESTIMATE2019), by = "fips") %>%
  mutate(cases = ifelse(is.na(cases), 0, cases), # make NA=0, otherwise do not change
         deaths = ifelse(is.na(deaths), 0, deaths),
         date = ifelse(is.na(date), max_date, date)) %>% # previous double to character conversion used for this action
  rename(state = full,
         pop19 = POPESTIMATE2019)
length(unique(all_counties_cases$fips)) # quick check we have as many counties as we expected

# Additional metric to normalize our case data by the 2019 population
all_counties_cases$cases_per_100 = round(all_counties_cases$cases/all_counties_cases$pop19*100, 0)

#### PLOT MAPS ####
# Map of US cases at county level
all_counties_cases %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group = group, fill = cases), color = "black", size = 0.1) +
  scale_fill_gradient(low = "gainsboro", high = "dark red", name = paste0("Cases \n", max_date) )+
  scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))

# cases per 100 people
all_counties_cases %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group = group, fill = cases_per_100), color = "black", size = 0.1) +
  scale_fill_gradient(low = "gainsboro", high = "dark red", name = paste0("Cases per 100 \n", max_date) )+
  scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))

# cases discrete
breaks = quantile(all_counties_cases$cases) # seperate the data into 4 quantiles/bins
names(breaks) = NULL # remove names, so we only have numbers
all_counties_cases$bin_cases = cut(all_counties_cases$cases, breaks=breaks, # cut relabels data with bin range
                              include.lowest=TRUE, right=FALSE)
all_counties_cases %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group = group, fill = bin_cases), color = "black", size = 0.1) +
  scale_fill_manual(values = c("gainsboro", "darkgoldenrod1", "darkorange3", "indianred4"), 
                    name = paste0("Cases \n", max_date) )+
  scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))

# cases per 100 people discrete
max(all_counties_cases$cases_per_100)
bin_breaks = c(0, 1, 2, 8, 15) 
labs = c("0", "1", "2", "8-15")
all_counties_cases$bins = cut(all_counties_cases$cases_per_100, breaks=bin_breaks, 
           include.lowest=TRUE, right=FALSE, labels = labs)
all_counties_cases %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(group = group, fill = bins), color = "black", size = 0.1) +
  scale_fill_manual(values = c("gainsboro", "darkgoldenrod1", "darkorange3", "indianred4"), 
                    name = paste0("Cases per 100 \n", max_date) )+
  scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.title=element_text(size=8),
        legend.text=element_text(size=6))

#### AT HOME ####
# Use the code above to make a discrete map of death data at county or state level




