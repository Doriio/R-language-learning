median()
rank(-gold_medals, na.last = "keep", ties.method = "min")
# na.last = "keep",不处理缺失值，na.last = TRUE,后排序缺失值，na.last = FALSE,先排序缺失值。

# Your functions, from previous steps
toss_coin <- function() {
  coin_sides <- c("head", "tail")
  sample(coin_sides, 1)
}
# Call your function
toss_coin()

# Update the function to return n coin tosses
toss_coin <- function(n_flips) {
  coin_sides <- c("head", "tail")
  sample(coin_sides, size = n_flips, replace = TRUE)
}
# Generate 10 coin tosses
toss_coin(10)

# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1 - p_head)
  # Modify the sampling to be weighted
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}
# Generate 10 coin tosses
toss_coin(10, 0.8)

# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# glm(formula, family, data, weights, subset, na.action)
# 	•	formula：表示模型的公式，例如 y ~ x1 + x2，定义了因变量（y）和自变量（x1, x2）的关系。
# 	•	family：指定广义线性模型的类型，即数据的分布类型和连接函数。例如 binomial 对应逻辑回归，poisson 对应泊松回归。
# 	•	data：用于指定包含因变量和自变量的data.frame 数据集。
# 	•	weights：可选参数，用于指定观察值的权重。
# 	•	subset：可选参数，表示应从数据中选择的子集。
# 	•	na.action：指定如何处理缺失值。

# quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
# 	•	x：需要计算分位数的数值向量或数据集。
# 	•	probs：指定分位点的概率，范围在 0 到 1 之间，表示需要计算的分位数。例如，probs = 0.5 表示计算中位数，probs = c(0.25, 0.5, 0.75) 表示计算四分位数。
# 	•	默认值是 seq(0, 1, 0.25)，即 0%、25%、50%、75%、100% 的分位数（四分位数）。
# 	•	na.rm：是否移除缺失值（NA），默认为 FALSE，即不移除。
# 	•	names：如果为 TRUE，则返回结果带有分位数的标签。
# 	•	type：计算分位数的方法，共有 9 种不同的方法，默认是 type = 7
install.packages("assertive")
library(assertive)
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  # Check if any values of x are non-positive
  if (any(is_non_positive(x), na.rm = TRUE)) {
    # Throw an error
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}
# See what happens when you pass it negative numbers
calc_harmonic_mean(std_and_poor500$pe_ratio - 20)
# Error: x contains non-positive values, so the harmonic mean makes no sense.

# return func
simple_sum <- function(x) {
  if (any(x)) {
    return(NA)
  }
  total <- 0
  for (value in x) {
    total <- total + value
  }
  total
}

# Define a pipeable plot fn with data and formula args
pipeable_plot <- function(data, formula) {
  # Call plot() with the formula interface
  plot(formula, data) # 根据提供的公式和数据集绘制图形
  # Invisibly return the input dataset
  invisible(data) # 函数返回 data，但是 invisible() 的作用是让返回值不显示在控制台。
  # 这样既保持了管道操作的连贯性，又不让中间结果打断流程。
}

# Draw the scatter plot of dist vs. speed again
plt_dist_vs_speed <- cars %>%
  pipeable_plot(dist ~ speed)# 绘制散点图

# return cars
plt_dist_vs_speed

# return multi values
session <- function() {
  list(
    r_version = R.version.string,
    operating_system = Sys.info()[c("sysname", "release")],
    loade_pkgs = loadedNamespaces()
  )
}
session()

# 多重赋值
install.packages("zeallot")
library(zeallot)
c(vrsn, os, pkgs) %<-% session()

# 利用属性多重赋值
month_no <- setNames(1:12, month.abb)
attributes(month_no) # name of month
attr(month_no, "names") # 检索特定属性
attr(month_no, "names")  <- month.name # 给属性赋值

install.packages("broom")
library(broom)
# 使用 mtcars 数据集进行线性回归
model <- lm(mpg ~ wt + hp, data = mtcars)
# glance 模型整体概览 level = model
glance(model)
# 使用 tidy() 提取模型系数 level = coefficient
tidy(model)
# augment 获得预测值和残差 level = rediduals
augment(model)

# From previous step
groom_model <- function(model) {
  list(
    model = glance(model),
    coefficients = tidy(model),
    observations = augment(model)
  )
}
# Call groom_model on model, assigning to 3 variables
c(mdl, cff, obs) %<-% groom_model(model)
# See these individual variables
mdl ; cff ; obs # nolint

# environment
# 一个变量是否存在
datacamp_lst <- list(
  name = "datacamp",
  website = 1
)
# Convert the list to an environment
datacamp_env <- list2env(datacamp_lst)
# List the structure of each variable
ls.str(rsa_lst)
# Find the parent environment of rsa_env
parent <- parent.env(datacamp_env)
# Print its name
environmentName(parent)
search()

founding_year <- 2013
exists("founding_year", envir = datacamp_env)
# Does population exist in rsa_env, ignoring inheritance?
exists("population", envir = rsa_env, inherits = FALSE)

install.packages("magrittr")
library(magrittr)
# case study
x %>% multiply_by(y)
x %>% raise_to_power(y)
x %>% extract(y)

# visualizing grain yields
ggplot(dataset, aes(x, y)) +
  geom_line(aes(group = group)) +
  geom_smooth() +
  facet_wrap(vars(facet))

# modeling grain yield to predict
lm(
    response_var ~ explanatory_var1 + explanatory_var2
    data = database
)

install.packages("nlme")
library(mgcv)
gam(
    response_var ~ s(explanatory_var1) + explanatory_var2
    data = dataset
)

predict_this <- data.frame(
    explanatory_var1 = c("some", "values"),
    explanatory_var2 = c("some", "values")
)
predicted_response <- predict(model, predict_this, type = "response")
predict_this %>%
  mutate(predicted_responses = predicted_responses)

Map(food, var)
Map(vectors, mean)
Map(vectors, measures_of_center)
x <- rep(TRUE, 50)
rep(8, 88)
df$y <- 6:10
x[2] = 5