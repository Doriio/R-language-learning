# 安装包
install.packages("dplyr")
install.packages("tidyverse")

glimpse(counties) # 查看每个行列的前几个值

# select/arrange/filter/mutate verb
counties_selected <- counties %>%
  select(state, county, population, umemployment) %>%
  arrange(desc(population)) %>%
  filter(state == "New York", umemployment < 6) %>%
  mutate(umemployed_population = population * umemployment / 100) %>%
  mutate(state, county, population,
         proportion_men = men / population, .keep = "none")

# count/group_by/summarize/ungroup verb
counties %>%
  count(state, wt = population, sort = TRUE) %>% # desc
  group_by(state) %>%
  summarize(total_population = sum(population)) %>% # one result
  ungroup()

# slice_min / slice_max verb
# return largest observation in each group
counties_selected %>%
  group_by(state) %>%
  slice_max(population, n = 1)

# select / tranform data
# ?select_helpers
# select for subsetting and moving
# relocate keeps all columns, best for moving only
counties %>%
  rename(umemployment_rate = umemployment) %>%
  relocate(region, .before = state, .after = last_col()) %>%
  select(state, county, drive:work_at_home, contains("work"),
         starts_with("income"),
         -census_id, # remove some variables
         umemployment_rate = umemployment) %>% # rename
  arrange(drive)

# Case Study: The babynames Dataset
# filter for multiple  names: %in%
babynames_multiple <- babynames %>%
  filter(name %in% c("Amy", "Christopher")) %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total)

# window functions
v <- c(1, 3, 6, 14)
lag(v)
v - lag(v)

babynames_fraction %>%
  filter(name == "Matthew") %>%
  arrange(year) %>%
  mutate(difference = fraction - lag(fraction)) %>%
  arrange(desc(difference))

# changes within every name
babynames_fraction %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(difference = fraction - lag(fraction)) %>%
  ungroup() %>%
  arrange(desc(difference))