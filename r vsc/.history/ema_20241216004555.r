install.packages("EMAtools")
library(EMAtools)

# 使用 calc_sample_size 函数计算样本量
result <- calc_sample_size(
  ICC = 0.05,
  power = 0.9,
  alpha = 0.05,
  effect_size = 0.5,
  observations = 21
)

# 输出结果
print(result)

R.version.string