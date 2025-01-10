# Load necessary packages
library(readr)
library(dplyr)
library(ggplot2)

# Load the dataset
data <- read_csv("stack_overflow_data.csv")

# Start coding here
# Use as many cells as you like!

# identify the r trend
r_over_time <- data %>%
  mutate(fraction = num_questions / year_total * 100) %>%
  filter(tag == "r")

# find the fraction of r questions in 2020
percentage <- data %>%
  mutate(fraction = num_questions / year_total * 100) %>%
  filter(tag == "r", year == 2020) %>%
  select(fraction)
r_percentage <- as.numeric(percentage)

# calculate the five most asked-about tags between 2016-2020
highest_tags <- data %>%
  filter(year >= 2015) %>%
  group_by(tag) %>%
  summarise(sum = sum(num_questions)) %>%
  arrange(desc(sum)) %>%
  slice_head(n = 5) %>% # 提取前五行
  pull(tag) %>% # 提取 'tag' 列并转换为vector
  as.character() # 转换为字符向量

# find the tag whose function has increased the most over time
ratio_data <- data %>%
  group_by(year) %>%
  mutate(total = sum(year_total)) %>% # 一年的总问题数
  ungroup() %>%
  mutate(fra = num_questions / total) # 每个tag的百分比

highest_ratio_tag <- ratio_data %>%
  group_by(tag) %>%
  arrange(year) %>%
  mutate(ratio_increase = fra / lag(fra)) %>% # 今年 / 前一年 环比增长
  ungroup() %>%
  slice_max(ratio_increase, n = 1) %>%
  pull(tag) %>%
  as.character()
