# Imported libraries
library(tidyverse)

# load and inspect the data
yearly <- read_csv("data/yearly_deaths_by_clinic.csv")
monthly <- read_csv("data/monthly_deaths.csv")
yearly
monthly

# add a new column with the proportions
yearly <- yearly %>%
  mutate(proportion_deaths_yearly = deaths / births)

monthly <- monthly %>%
  mutate(proportion_deaths_monthly = deaths / births)

# make a line plot for each data frame
ggplot(yearly, aes(x = year, y = proportion_deaths_yearly, color = clinic)) +
  geom_line()
ggplot(monthly, aes(x = date, y = proportion_deaths_monthly)) +
  geom_line() +
  labs(x = "Year", y = "Proportion Deaths")

# visualize the thresshold
threshold <- as.Date("1847-06-01")
monthly <- monthly %>%
  mutate(handwashing_started = ifelse(date >= threshold, TRUE, FALSE))
# mutate(handwashing_started = date >= handwashing_start)

ggplot(monthly, aes(x = date, y = proportion_deaths_monthly,
                    color = handwashing_started)) + geom_line()

# calculate the mean proportion of deaths
monthly_summary <- monthly %>%
  group_by(handwashing_started) %>%
  summarise(mean_proportion_deaths =
              mean(proportion_deaths_monthly))

monthly_summary