# 01 data type constraints : character integer numeric logical factor date

# 简单查看一下数据的两种方法
sales <- read.csv("sales.csv")
head(sales)

library(dplyr)
glimpse(sales) # return data type

# 检查数据类型
is.numeric(sales$revenue) # TRUE or FALSE
is.character()
is.logical()
is.factor()
is.Date()

library(assertive)
assert_is_numeric(sales$revenue) # error
assert_is_numeric(sales$quantity) # return nothing
assert_all_are_not_na(accounts_clean$cust_id)

class(sales$revenue)

# 如何移除字符串中的逗号，之后再转为数值
library(stringr)
revenue_trimmed <- str_remove(sales$revenue, ", ") # str_remove funtion
as.numeric(revenue_trimmed) # 转为数值
sales %>%
  mutate(revenue_usd = as.numeric(str_remove(sales$revenue, ","))) %>%
  mutate(revenue_usd = revenue_trimmed)

# factor to numeric
as.numeric(as.character(product_type))

# 02 range constraints
# histogram to find out of range values
breaks <- c(min(movies$avg_rating), 0, 5, max(movies$avg_rating))
ggplot(movies, aes(avg_rating)) +
  geom_histogram(breaks = breaks) # 横坐标刻度

# assertive to find out of range values
library(assertive)
assert_all_are_in_closed_range(movies$avg_rating, lower = 0, upper = 5) # failures

# 异常值处理方式
# 删除行 filter function
movies %>%
  filter(avg_rating >= 0, avg_rating <= 5)
# 处理为缺失值NA dplyr::replace
movies %>%
  mutate(rating_miss = # 处理之后的变量名
           replace(avg_rating, avg_rating > 5, NA))
# 用极值代替


movies %>%
  mutate(rating_const = # 处理之后的变量名
           replace(avg_rating, avg_rating > 5, 5))
# 用均值及其他代替

# 03 date range constraints
# check
assert_all_are_in_past(movies$date_recorded) # failures

library(lubridate)
movies %>%
  filter(date_recorded > today()) # lubridate::today got today date

# remove
library(lubridate)
movies %>%
  filter(date_recorded <= today())

# uniqueness constraints for duplicated data
# find full duplicates duplicated func
duplicated(cerdit_scores) # logical TRUE for duplicates
sum(duplicated(credit_scores)) # 计算重复项总和 2

filter(credit_scores, duplicated(credit_scores))

# drop full duplicates distinct func
credit_scores_unique <- distinct(credit_scores) # remove full duplicate
sum(duplicated(credit_scores)) # 0

# partial duplicate
# find and rename
dup_ids <- credit_scores %>%
  count(first_name, last_name) %>% # 查看每一行的数量
  filter(n > 1) # 大于1即为重复项

# check and drop
credit_scores %>%
  filter(firs_name %in% dup_ids$first_name, last_name %in% dup_ids$last_name) %>%
  distinct(first_name, last_name, .keep_all = TRUE) # 保留所有列

# 利用数值筛选部分重复项
credit_scores %>%
  group_by(first_name, last_name) %>%
  mutate(mean_credit_score = mean(credit_score)) %>%
  distinct(first_name, last_name, .keep_all = TRUE) %>% # remove duplicates
  select(-credit_score) # remove old collumn

# 分类和文本数据清洗
# category as factor data type
# check levels of factors
levels(tshirt_size) #  S M  L XL

# use semi_join or anti_join to handle
# remove invalid blood types
study_data %>%
  anti_join(blood_types, by = "blood_type") # in study not in blood
# remove non-menbers
study_data %>%
  semi_join(blood_types, by = "blood_type") # in study and in blood

# check 分类情况
library(stringr)
animals %>%
  count(type, sort = TRUE)  %>% # 检查每个种类的数量
  # 分类的大小写问题
  mutate(type_lower = str_to_lower(type)) %>%
  mutate(type_lower = str_to_upper(type)) %>%
  # 分类的空格问题
  mutate(type_trimmed = str_trim(type_lower)) %>% # 移除开头和结尾的空格

  # 新建分类变量
  other_categories <- c("amphibian", "fish", "bug", "invertebrate", "reptile")
library(forcats)
animals %>%
  mutate(type_collapsed = fct_collapse(type_trimmed, other_categories)) %>%
  count(type_collapsed) %>%
  mutate(type_collapsed = fct_collapse(type, other_categories)) # type是分类变量的变量名

# clean text data
# format inconsistency
# information inconsistency
# invalid data

# 数据查找1 detect
str_detect(customers$credit_card, "-") # 检查这一列是否有-
# 数据查找2 filter
customers %>%
  filter(str_length(customers$credit_card) == 16)

# 修改法1 替换
customers %>%
  filter(str_detect(customers$credit_card, "-")) %>% # 返回TRUE(含有-)的情况
  mutate(credit_card_spaces = str_replace_all(credit_card, "-", " ")) # 带空格的数值

# 修改法2 移除
credit_card_clean <- customers$credit_card %>%
  str_remove_all("-") %>%
  str_remove_all(" ")
customers %>%
  mutate(credit_card = credit_card_clean)

sfo_survey %>%
  filter(str_detect(phone, fixed("(")) | str_detect(phone, fixed(")")))
# use fixed when detect for ()
# regular expressions

# 03 advanced data problems
ggplot(nyc_temps, aes(x = date, y = temp)) +
  geom_point()
# find 3 outliers
nyc_temps %>%
  mutate(temp_c = ifelse(temp > 50, (temp - 32) * 5 / 9, temp))

# tranform date
library(lubridate)
parse_date_time(nyc_temps$date,
                orders = c("%Y-%m-%d", "%m/%d/%y", "%B %d, %Y"))

# cross field validation
credit_cards %>%
  select(dining_cb:total_cb) %>%
  mutate(theoretical_total = dining_cb + groceries_cb + gas_cb) %>%
  filter(theoretical_total != total_cb) %>%
  select(dining_cb:theoretical_total)

date_difference <- as.Date("2025-09-04") %--% today() # 今天和open date的差异
floor(as.numeric(date_difference, "year")) # 以年为单位 + 去掉小数

credit_cards %>%
  mutate(theoretical_age = floor(as.numeric(date_opened %-% today(), "year"))) %>%
  filter(theoretical_age != acct_age)

# missing value
is.na(airquality) # TRUE if na
sum(is.na(airquality)) # total number of na

# visualizing missing value
install.packages("visdat")
library(visdat)
vis_miss(airquality) # black = na

# 判断有臭氧和没有臭氧层之间温度是否存在差异
airquality %>%
  mutate(miss_ozone = is.na(Ozone)) %>%
  group_by(miss_ozone) %>%
  summarise(across(everything(), median, na.rm = TRUE))

airquality %>%
  arrange(Temp) %>%
  vis_miss()

# treat na with mean
airquality %>%
  filter(!is.na(Ozone), !is.na(Solar.R)) %>%
  mutate(ozone_filled = ifelse(ia.na(Ozone), mean(Ozone, na.rm = TRUE), Ozone))

# 04 match string and record links
# min edit distance
install.packages("stringdist")
library(stringdist)

stringdist("baboon", "typhoon", method = "dl")
stringdist("baboon", "typhoon", method = "jaccard")
stringdist("baboon", "typhoon", method = "lcs")

install.packages("fuzzyjoin")
library(fuzzyjoin) # join based on edit dist

stringdist_left_join(survey, cities, by = "city", method = "dl", max_dist = 1)

# record linkage
# 配对
install.packages("reclin")
library(reclin)

pair_blocking(df_A, df_B)
pair_blocking(df_A, df_B, blocking_var = "state") %>%
  compare_pairs(by = "name", default_comparator = lcs()) %>%
  compare_pairs(by = c("name", "zip"), default_comparator = lcs())

# scoring and linking
score_simsum()
score_problink()
select_n_to_m()
link()

sum(dates, na = FALSE)
replace_na(replace = list(value = 5))
separate(date, c("mpnth", "day", "year"), "-")
mutate(type_trimmed = str_trim(type_lower)) # 移除开头和结尾的空格
