#Loading the tidyverse package
library(tidyverse)

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
  
