install.packages("ematools")
library(ematools)

# 使用 calc_sample_size 函数计算样本量
result <- calc_sample_size(
  ICC = 0.05,
  power = 0.9,
  alpha = 0.05,
  effect_size = 0.,
  observations = 20
)

# 输出结果
print(result)