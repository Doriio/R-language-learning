# categorical data
# 查看分类变量水平
levels(comics$align)
# 创建列联表
tab_cnt <- table(comics$id, comics$align)
# 可视化 条形图
library(ggplot2)
ggplot(comics, aes(x = id, fill = align)) +
  geom_bar(position = "fill") +
  ylab("proportion")

ggplot(comics, aes(x = align, fill = id)) +
  geom_bar(position = "fill") +
  ylab("proportion")
# 计数和比率
prop.table(tab_cnt, 1) # row = 1; coloum =2
sum(prop.table(tab_cnt))
# simple bar chat and faceting
ggplot(comics, aes(x = id)) +
  geom_bar() +
  facet_wrap(~ align) # broken down by align

# numerical data
# dotplot
ggplot(cars, aes(x = weight)) +
  geom_dotplot(dotsize = 0.4)
# faceted histogram
ggplot(cars, aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_wrap(~ pickup) # 以分类变量分面

# density plot
cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(cars2, aes(x = hwy_mpg)) +
  geom_density(bw = 5)

# boxplot
ggplot(data, aes(x = 1, y = weight)) +
  geom_boxplot() +
  coord_flip()

# one variable
str(cars)
cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(cars2, aes(x = hwy_mpg)) +
  geom_histogram(binwidth = 5)

# multi vars visulization
ggplot(cars, aes(x = msrp)) +
  geom_density() +
  facet_grid(pickup ~ rear_weel, labeller = label_both)
table(cars$rear_wheel, cars$pickup)

# 集中趋势度量 group_by + summarise
x <- head(round(life$expectancy), 11)
mean(x)
median(x)
table(x)

life <- life %>%
  mutate(west_cosat = state %in% c("canifornia", "oregon", "washington"))

life %>%
  group_by(west_cosat) %>%
  summarise(mean(expectancy), median(expectancy))

# 变异趋势度量 var sd
var(x)
sd(x)
summary(x) # min q1 median mean q3
IQR(x) # q3 - q1
diff(range(x)) # 极差

# shape and transformation
ggplot(life, aes(x = income, fill = west_cosat)) +
  geom_density(alpha = .3)

# sqrt / log transform
ggplot(life, aes(x = log(income), fill = west_cosat)) +
  geom_density(alpha = .3)

# outliers filter + arrange
life <- life %>%
  mutate(is_outlier = income > 75000)
life %>%
  filter(is_outlier) %>%
  arrange(desc(income))

# plot without outliers
life %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = income, fill = west_cosat)) +
  geom_density(alpha = .3)

# case study
# build histogram but facet
ggplot(data, aes(x = var1)) +
  geom_histogram() +
  facet_wrap(~var2)

# Load packages
library(dplyr)
library(ggplot2)
library(openintro)

# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarize(median(num_char),
            IQR(num_char))

# Create plot
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()



# practice 01
# Create side-by-side bar chart of gender by alignment
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar(position = "dodge") # 条形图side by side

# Create side-by-side bar chart of alignment by gender
ggplot(comics, aes(x = gender, fill = align)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90)) # 横轴字体竖排

# Plot of gender by align
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar()
  
# Plot proportion of gender, conditional on align
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar(position = "fill") +
  ylab("proportion")

# Reorder the levels of align
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))

# practice 02
# Filter cars with 4, 6, 8 cylinders
common_cyl <- filter(cars, ncyl %in% c(4, 6, 8))

# Create box plots of city mpg by ncyl
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Create overlaid density plots for same data
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)

# Create hist of horsepwr
cars %>%
  ggplot(aes(horsepwr)) +
  geom_histogram() +
  ggtitle("title 1")

# Create hist of horsepwr for affordable cars
cars %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(horsepwr)) +
  geom_histogram() +
  xlim(c(90, 550)) +
  ggtitle("title 2")

# practice 03
# Compute groupwise measures of spread
gapminder
gap2007 %>%
  group_by(continent) %>%
  summarize(sd(lifeExp),
            IQR(lifeExp),
            n())

# Generate overlaid density plots
gap2007 %>%
  ggplot(aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3)