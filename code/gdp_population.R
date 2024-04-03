## Setup Section

#Loading the tidyverse package
library(tidyverse)
library(ggplot2)

###################################################
## Day 1 work: Intro to R and Plotting
###################################################

# Read in data in csv file
gapminder_1997 <- read_csv("data/gapminder_1997.csv")
#gapminder_1997 <- read_csv(file="data/gapminder_1997.csv")

#cat_name <- "abcde"
#sum(5,6)
#?round()
#round(3.1415)
#round(3.1415,3)
#round(3.1415,digit = 3)

# Using ggplot to make a plot
ggplot(data = gapminder_1997) +
  aes( x = gdpPercap,
       y = lifeExp,
       color = continent,
       size = pop/1000000)+
  labs(x = "GDP Per Capita",
       y = "Life Expectancy",
       title = "Do people in wealthy countries live longer?",
       size = "Population (in millions)")+
  geom_point() + 
  scale_color_brewer(palette = "Set3")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()

ggsave("figures/gdpVsLife.png")
# List all palettes
RColorBrewer::display.brewer.all()

# Map continent variable to the shape of the point

# Load in full gapminder data set
gapminder_data <- read_csv("data/gapminder_data.csv")
dim(gapminder_data)
head(gapminder_data)
glimpse(gapminder_data)

# Plot sphagette ?
ggplot(data = gapminder_data)+
  aes(x = year,y = lifeExp, color = continent,
      group = country) +
  geom_line(alpha = 0.5)+
  theme_classic()

# Plot lifeExp versus pop
ggplot(data = gapminder_data)+
  aes(x = year, y = pop, color = continent, group = country)+
  geom_line()

# box plot of distribution of lice expectancies per continent
ggplot(data = gapminder_data)+
  aes(x = continent, y = lifeExp)+
  geom_boxplot()

# Violin plot 
# Note order matters in ggplot!
ggplot(data = gapminder_data)+
  aes(x = continent, y = lifeExp)+
  geom_violin()+
  geom_jitter(width = 0.15, alpha = 0.2)
  #geom_point()+
  
###################################################
## Day 2 Work: Data Manipulation and cleaning 
###################################################

gapminder_data <- read_csv("data/gapminder_data.csv")
glimpse(gapminder_data)
View(gapminder_data)

#pipes
# %>%
# means "and then do this thing"

# option 1 without pipes
summarize(gapminder_data, avgLifeExp = mean(lifeExp))
summarize(gapminder_data, maxLifeExp = max(lifeExp))

# option 2 
gapminder_data %>% summarize(avgLifeExp = mean(lifeExp))

# save the summary table
gapminder_data_summary <- gapminder_data %>% summarize(avgLifeExp = mean(lifeExp))


gapminder_data %>% filter(year == 1962) %>% summarize(avgLifeExp07 = mean(lifeExp))

# find the earliest year in the data set using summarize() and min()
gapminder_data %>% summarise(startpoint = min(year))
# filter the data to 1952 and find ave GDP per capita
gapminder_data %>% filter(year == min(year)) %>% 
  summarise(agvMinGDP = mean(gdpPercap))

# calculate life expectancy by year
gapminder_data %>% group_by(year) %>% summarize(avg = mean(lifeExp))
gapminder_data %>% group_by(continent) %>% 
  summarize(avg = mean(lifeExp), 
            min = min(lifeExp), 
            max = max(lifeExp))

# adding columns to data set using mutate
gapminder_data %>% mutate(gdp = gdpPercap*pop)
# use mutate to create a col for popInMillions
gapminder_data %>% mutate(popInMillions = pop/1e6)

# filter to select rows
# select to select cols
gapminder_data %>% select(pop, year)
gapminder_data %>% select(-continent) # cols without continent
gapminder_data %>% select(country, continent, year, lifeExp)
gapminder_data %>% select(year, starts_with("c"))
gapminder_data %>% select(year, ends_with("p"))

gapminder_data %>% select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# save a dataframe that contains only the Americas in 2007
gapminder_data_2007 <- gapminder_data %>% filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

# cleaning messy data
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region_number","country","year","series",
                       "value","footnotes","source"))
glimpse(co2_emissions_dirty)
# select the country, year, series, and value columns
co2_emissions_dirty %>% select(country, year, series, value) %>%
  print(n = 50)

co2_emissions_dirty %>% select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                  "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
           pivot_wider(names_from = series, values_from = value)

# filter only data from 2005 and drop year column
co2_emission <- co2_emissions_dirty %>% select(country, year, series, value) %>%
  filter(year == 2005) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  select(-year)

# previous we created gapminder for 2007
# now we have co2 emission for 2005
inner_join(gapminder_data, co2_emission, by = "country")
anti_join(gapminder_data, co2_emission, by = "country")

co2_emission <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region_number","country","year","series",
                       "value","footnotes","source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))

anti_join(gapminder_data, co2_emission, by="country")

# address Puerto Rico
gapminder_data <- gapminder_data %>% mutate(country = recode(country, "Puerto Rico" = "United States"))

gapminder_co2 <- inner_join(gapminder_data, co2_emission, by = "country")
glimpse(gapminder_co2)

gapminder_co2 %>% group_by(continent) %>% summarise(avgLifeExp = mean(lifeExp))

gapminder_co2 %>% filter(continent == "Americas" & year == 2007) %>%
  mutate(region = ifelse(country == "United States" | country == "Canada" | country == "Mexico",
                         "north", "south"))

# I want the Americas in 2007
gapminder_co2 <- gapminder_co2 %>% filter(continent == "Americas" & year == 2007)

# write out new clean data set as csv
write_csv(gapminder_co2,"data/gapminder_co2.csv")
