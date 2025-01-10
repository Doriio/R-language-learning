install.packages("ematools")
library(ematools)

calc_sample_size(
  IC = C,           # 组内相关系数 (Intraclass Correlation Coefficient)
  power,         # 统计检验的功效（通常为 0.8）
  alpha,         # 显著性水平（通常为 0.05）
  effect_size,   # 效应大小 (Cohen's d)
  observations   # 每个被试的观测次数
)