# summary statistics
# 1 data type: numeric(continuous+discrete) + categorical(nominal+ordinal)
mean(msleep$column)
median(msleep$column)
msleep %>% count(sleep_total, sort = TRUE)

# varicace
# 离均差 / 方差 / 标准差 / 平均差
dists <- msleep$sleep_total - mean(msleep$sleep_total)
squared_dists <- (dists) ^2
sum_sq_dists <- sum(squared_dists)
variance <- sum_sq_dists / 82
var(msleep$sleep_total)
sd(msleep$sleep_total)
mean(abs(dists))

# quartiles / quantiles 四分位数 / 百分位数
quantile(msleep$sleep_total)
quantile(msleep$sleep_total, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
quantile(msleep$sleep_total, probs = seq(0, 1, 0.2))

# 四分位差 （q3 - q1）/2
q3 <- quantile(msleep$sleep_total, 0.75)
q1 <- quantile(msleep$sleep_total, 0.25)
iqr <- q3 - q1

# outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# random number and probalibility
set.seed(5)
sales_counts %>%
  sample_n(1)
sample_n(2, replace = TRUE) # sample with replcaement = independent
sample(sales_team, 5, replace = TRUE)

# discrete uniform distribution
expected_value <- mean(die$n)
mean(rolls_1000$n)
rolls_10 <- die %>%
  sample_n(10, replace = TRUE)
ggplot(rolls_10, aes(n)) +
  geom_histogram(bins = 6)

# continuous distribution
punif(7, min = 0, max = 12, lower.tail = FALSE) # p(wait_time <= 7)
# Generate 1000 wait times between 0 and 30 mins, save in time column
wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30))

# the binomial distribution
# a single flip 1 = head 0 = tail
rbinom(1, 1, 0.5)
rbinom(8, 1, 0.5)
rbinom(1, 8, 0.5)
rbinom(10, 3, 0.5)
rbinom(10, 3, 0.25) # trials number of dice prob
dbinom(7, 10, 0.5, lower.tail = FALSE) # head trials prob of head

# normal distribution 68 95 99.7
pnorm(154, mean = 161, sd = 7, lower.tail = FALSE) # 154以下的占比
qnorm(0.9, mean = 161, sd = 7) # 已知概率求数据
rnorm(10, mean = 161, sd = 7) # generate 10 random heights

# central limit theorem
die <- c(1, 2, 3, 4, 5, 6)
sample_of_5 <- sample(die, 5, replace = TRUE) # give 5 rolls
mean(sample_of_5)
# repeat it for 10 times
sample_means <- replicate(10, sample(die, 5, replace = TRUE) %>% mean())
# sampling distribution 样本统计量的抽样分布

# poisson distribution
# lambda = mean(events per time interval)
dpois(5, lambda = 8) # equal
ppois(5, lambda = 8) # less than
ppois(5, lambda = 8, lower.tail = FALSE) # greater than
rpois(10, lambda = 8) # sampling from poisson distribution

# exponential distribution 指数分布 poisson事件间隔时间的概率
pexp(1, rate = 0.5, lower.tail = FALSE) # less than, rate = 1/lambda
# t-distribution
# log-normal distribution

# correlation
# 相关系数 correlation coefficient
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # linear model 趋势线

cor(df$x, df$y, use = "pairwise.complete.obs") # pearson ignore na
# 当数据不对称时，取log() / sqrt() / 1/x
msleep %>%
  mutate(log_bodywt = log(bodywt)) %>%
  ggplot(aes(log_bodywtbody, awake)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# experimental design
# randomized controlled trial