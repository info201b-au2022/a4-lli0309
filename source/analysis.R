library(tidyverse)
library(ggplot2)
library(maps)
library(mapproj)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
#The function finds the average daily number of black people held in jail in 1990. 
average_black <- function() {
  incarceration_trend <- data.frame(get_data())
  trend_1990 <- filter(incarceration_trend, year == "1990")
  avg <- round(mean(trend_1990$black_pop_15to64, na.rm = TRUE), 2)
  return(avg)
}

#The function finds the maximum daily number of black people held in jail in 1990.
highest_pop <- function() {
  incarceration_trend <- data.frame(get_data())
  trend_1990 <- filter(incarceration_trend, year == "1990")
  maximum <- max(trend_1990$black_pop_15to64, na.rm = TRUE)
  return(maximum)
}

#The function finds the minimum daily number of black people held in jail in 1990.
lowest <- function() {
  incarceration_trend <- data.frame(get_data())
  trend_1990 <- filter(incarceration_trend, year == "1990")
  minimum <- min(trend_1990$black_pop_15to64, na.rm = TRUE)
  return(minimum)
}

#The function finds the daily maximum change of black people held in jail over the last 10 years.
change <- function() {
  incarceration_trend <- data.frame(get_data())
  trend_1990 <- filter(incarceration_trend, year == "1990")
  trend_1980 <- filter(incarceration_trend, year == "1980")
  maximum_1990 <- max(trend_1990$black_jail_pop, na.rm = TRUE)
  maximum_1980 <- max(trend_1980$black_jail_pop, na.rm = TRUE)
  changes <- maximum_1990 - maximum_1980
  return(changes)
}

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function finds the sum of total county population in each year 
get_year_jail_pop <- function() {
  incarceration_trend <- get_data()
  df <- incarceration_trend %>%
    group_by(year) %>%
    summarize(total = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(year, total)
return(df)   
}

# This function graphs the jail population from 1970 to 2018.
options(scipen=999)
plot_jail_pop_for_us <- function()  {
p <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total)) +
labs(title = "Increase of Jail Population in U.S. (1970 - 2018)", 
     x = "Year", 
     y = "Total Jail Population",
     caption = "Figure 1: Increase of Jail Population in U.S.(1970 - 2018). Shows how the jail population has increased throughout U.S. history.")
  return(p) 
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
#This function takes states as a parameter and finds the total jail population from each state and year.
get_jail_pop_by_states <- function(states) {
  incarceration_trend <- get_data()
  df <- incarceration_trend %>%
    group_by(year, state) %>%
    filter(state %in% states) %>%
    summarize(total = sum(total_jail_pop, na.rm = TRUE), .groups = 'drop') %>%
    select(year, state, total)
return(df)
}

#This function graphs the line chart that shows the growth of prison population by state.
plot_jail_pop_by_states <- function(states) {
  p2 <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total, color = state)) +
labs(title = "Growth of Prision Population by State in U.S. (1970 - 2018)",
  x = "Year",
  y = "Total Jail Population",
  caption = "Figure 2: Growth of Prision Population of WA, CA, NY, and FL in U.S. (1970 - 2018).")
return (p2)
}

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# See Canvas
#This function finds the black jail population from different urbanicities.
black_pop_by_urbanicity <- function(urbanicities) {
  incarceration_trend <- get_data()
  df <- incarceration_trend %>%
    group_by(year, urbanicity) %>%
    filter(urbanicity == urbanicities) %>%
    summarize(total = sum(black_jail_pop, na.rm = TRUE), .groups = 'drop') %>%
    select(year, total, urbanicity)
return(df)
}

#This function finds the white jail population from different urbanicities.
white_pop_by_urbanicity <- function(choice) {
  incarceration_trend <- get_data()
  df <- incarceration_trend %>%
    group_by(year, urbanicity) %>%
    filter(urbanicity == choice) %>%
    summarize(total = sum(white_jail_pop, na.rm = TRUE), .groups = 'drop') %>%
    select(year, total, urbanicity)
  return(df)
}

#This function graphs the line chart that shows the growth of prison population by race and urbanicity.
plot_jail_pop_by_urbanicity <- function(urbanicities, choice, choice2, urbanicities2) {
  p3 <- ggplot() +
    geom_line(data = black_pop_by_urbanicity(urbanicities), mapping = aes(x = year, y = total, color = "Urban Black Jail Population")) + 
    geom_line(data = white_pop_by_urbanicity(choice), mapping = aes(x = year, y = total, color = "Urban White Jail Population")) +
    geom_line(data = white_pop_by_urbanicity(choice2), mapping = aes(x = year, y = total, color = "Rural White Jail Population")) +
    geom_line(data = black_pop_by_urbanicity(urbanicities2), mapping = aes(x = year, y = total, color = "Rural Black Jail Population")) +
    labs(title = "Growth of Prision Population by Race and Urbanicity in U.S. (1970 - 2018)",
         x = "Year",
         y = "Total Jail Population",
         caption = "Figure 3: Growth of Prision Population by Race and Urbanicity in U.S. (1970 - 2018).")
  return (p3)
}


#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
#This function joins the dataset with map_data for county and filters the data for CA.
county_shapes  <- function() {
  incarceration_trend <- get_data()
  shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname") %>%
  left_join(incarceration_trend, by = "fips") %>%
  filter(state == "CA")
  return(shapes)
}

#This function finds the total population of black people in jail.
black_pop_in_CA <- function() {
  df <- county_shapes() %>%
    group_by(county_name) %>%
    summarize(total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE), .groups = 'drop') %>%
    left_join(county_shapes(), by = "county_name")
  return(df)
}

#This function removes background and grid lines of the graph.
black_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

#This function plots the graph of black population in jail of counties.
options(scipen=999)
create_map <- function() {
  p4 <- ggplot(data = black_pop_in_CA()) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = total_black_jail_pop),
      color = "gray", size = 0.3
    ) +
    coord_map() +
    scale_fill_continuous(limits = c(0, max(black_pop_in_CA()$total_black_jail_pop)), na.value = "white", low = "blue", high = "green") +
    black_theme +
    labs(title = "Total Black Population in Jail of Counties in California",
         caption = "Figure 4: Total Black Population in Jail of Counties in California. (1970 - 2018)")
  return(p4)
}
create_map()

# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 

