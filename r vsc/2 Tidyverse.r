if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(bruceR)
pacman::p_load(here)

library(gapminder)
library(dplyr)

# data wrangling
gapminder %>%
  filter(year == 2007, country == "China") %>%
  arrange(desc(gdpPercap)) %>% #increasing order
  mutate(pop = pop / 1000000) %>% # create new variables
  mutate(gdp = gdpPercap * pop) %>%
  summarise(meanlifeExp = mean(lifeExp),
            totalPop = sum(pop),
            medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap)) # summarizing into one row

# group_by verb
gapminder %>%
  filter(continent == "Asian") %>%
  group_by(year) %>% # summarize based on the year
  group_by(year, continent) %>%
  summarize(meanlifeExp = mean(lifeExp)) # got one row for each year

# visualizing with ggplot2
# 1 vatiable assignment
gapminder_2007 <- gapminder %>%
  filter(year == 2007)

# 2 visualizing with ggplot2 ggplot(x,aes) + geom + log
library(ggplot2)
ggplot(gapminder_2007,
       aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() + # scatter plot
  geom_line() + # line plot
  geom_col() + # 条形图 📊
  geom_histogram(binwidth = 5, bins = 50) + # 直方图 distribution
  geom_boxplot() + # 箱线图
  scale_x_log10() + # x on log10 scale
  facet_wrap(~ continent) # faceting = split the plot by continent

# 3 summarize the data by year
by_year <- gapminder %>%
  group_by(year) %>%
  summarise(totalPop = sum(pop), meanLifeExp = mean(lifeExp))

ggplot(by_year, aes(x = year, y = totalPop, color = continent)) +
  geom_point() +
  expand_limits(y = 0) + # start y-axis at zero
  ggtitle()