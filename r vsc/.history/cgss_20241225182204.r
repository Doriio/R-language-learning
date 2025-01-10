#####PACKAGES#####
install.packages("haven")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("tidyr")
install.packages("psych")
install.packages("lavaan")
install.packages("mirt")
install.packages("tidyverse")
install.packages('difR')
install.packages("lordif")

library(haven)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(psych)
library(lavaan)
library(mirt)
library(tidyverse)
library(difR)
library(lordif)


#####IMPORTING DATASETS#####
# 读取.sav文件
data <- read_sav("/Users/a/Desktop/NJU/1 课程/高级测量/assignment23/CGSS2010.sav")
# 保留列名为 l2501 到 l2515 的这15列
data <- data[, c("id", "l2501", "l2502", "l2503", "l2504", "l2505",
                 "l2506", "l2507", "l2508", "l2509", "l2510",
                 "l2511", "l2512", "l2513", "l2514", "l2515")]
#####DATA SCREENING#####
# 定义转换函数
convert_to_text <- function(x) {
  # 检查是否是 NA
  if (is.na(x)) return(NA_character_)
  # 检查并返回对应的文本
  if (x == 1) return("完全不同意")
  if (x == 2) return("比较不同意")
  if (x == 3) return("无所谓")
  if (x == 4) return("比较同意")
  if (x == 5) return("完全同意")
  if (x == 8) return("无法选择")
  if (x == -3) return("拒绝回答")
  if (x == -2) return("不知道")
  if (x == -1) return("不适用")
  # 默认返回 NA
  return(NA_character_)
}
# 使用 dplyr 的 mutate(across()) 批量转换列
data <- data %>%
  mutate(across(l2501:l2515, ~sapply(., convert_to_text)))
# 定义要检查的值
excluded_values <- c("缺失值", "无法选择", "拒绝回答", "不知道", "不适用", NA_character_)
# 计算每行中符合条件的值的数量，考虑 NA
count_excluded_values <- apply(data, 1, function(row) {
  sum(row %in% excluded_values | is.na(row), na.rm = TRUE)
})
# 找出不符合要求的行：符合条件数量大于5的行
rows_to_remove <- data[count_excluded_values > 5, ]
# 查看不符合要求的行 head(rows_to_remove)
# 删除符合条件的行，保留符合条件数量小于等于5的行
data_cleaned <- data[count_excluded_values <= 5, ]
# 查看删除后的数据
head(data_cleaned)
# 查看原始数据行数
original_row_count <- nrow(data)
# 查看删除后的数据行数
cleaned_row_count <- nrow(data_cleaned)
# 打印结果
cat("原始数据行数:", original_row_count, "\n")
cat("删除后的数据行数:", cleaned_row_count, "\n")
# 将数据框保存为 CSV 文件 3046
write.csv(data_cleaned, "data_cleaned.csv")


##### 赋分 #####
# 定义正向和反向计分的列
positive_items <- c("l2501", "l2503", "l2505", "l2507", "l2509", "l2511", "l2513", "l2515")
reverse_items <- c("l2502", "l2504", "l2506", "l2508", "l2510", "l2512", "l2514")

# 创建一个映射表，将文本转化为数字
response_map <- c(
  "完全不同意" = 1,
  "比较不同意" = 2,
  "无所谓" = 3,
  "比较同意" = 4,
  "完全同意" = 5,
  "无法选择" = 8,
  "拒绝回答" = -3,
  "不知道" = -2,
  "不适用" = -1
)

# 定义赋分函数
assign_scores <- function(x, item_type) {
  # 检查该值是否在映射表中
  if (!x %in% names(response_map)) {
    return(NA)  # 如果不是有效值，返回 NA
  }
  # 将文本转换为数字
  x_numeric <- response_map[as.character(x)]
  # 如果是特殊值（8, -3, -2, -1），则返回 NA，后续用均值填充
  if (x_numeric %in% c(8, -3, -2, -1)) {
    return(NA)
  }
  # 根据列类型进行赋分
  if (item_type == "positive") {
    return(x_numeric)  # 正向计分：1 -> 1, 2 -> 2, ..., 5 -> 5
  } else if (item_type == "reverse") {
    return(6 - x_numeric)  # 反向计分：1 -> 5, 2 -> 4, ..., 5 -> 1
  } else {
    return(NA)
  }
}

# 1. 先赋分
data_with_scores <- data_cleaned %>%
  mutate(across(all_of(c(positive_items, reverse_items)), ~ {
    # 确定该列是正向还是反向计分
    if (cur_column() %in% positive_items) {
      item_type <- "positive"
    } else if (cur_column() %in% reverse_items) {
      item_type <- "reverse"
    } else {
      item_type <- NA  # 其他列不处理
    }
    # 应用赋分函数
    sapply(., assign_scores, item_type = item_type)
  }))

# 2. 计算每列的均值（不包括 NA）
column_means <- data_with_scores %>%
  summarise(across(all_of(c(positive_items, reverse_items)), ~ mean(.x, na.rm = TRUE)))

# 3. 用均值填充 NA
data_filled <- data_with_scores %>%
  mutate(across(all_of(c(positive_items, reverse_items)), ~ {
    # 获取当前列名
    col_name <- cur_column()
    # 获取该列的均值
    mean_val <- column_means[[col_name]]
    # 用均值替换 NA
    ifelse(is.na(.), mean_val, .)
  }))

# 查看处理后的数据
head(data_filled)
write.csv(data_filled, "data_filled.csv")


#####descriptive#####
# 将 data_filled 转换为长格式
long_data <- data_filled %>%
  pivot_longer(
    cols = -id,        # 选择所有列
    names_to = "Question",      # 新列名为 "Question"
    values_to = "Score"         # 新列名为 "Score"
  )
# 过滤分数为整数且在1到5之间的记录
long_data_filtered <- long_data %>%
  filter(Score %in% 1:5 & Score %% 1 == 0)
# 计算每个分数的频数和占比
score_summary <- long_data_filtered %>%
  group_by(Question, Score) %>%
  summarise(Frequency = n(), .groups = "drop") %>%  # 计算频数
  group_by(Question) %>%
  mutate(Proportion = round((Frequency / sum(Frequency)) * 100, 1)) %>%  # 计算占比，保留一位小数
  ungroup() %>%
  arrange(Question, Score)  # 按Question和Score排序
# 查看描述性分析结果
print(score_summary)
View(score_summary)
# 转换成表格
write.csv(score_summary, "score_summary.csv", row.names = FALSE)


##### EFA #####
# 提取条目
data_efa <- data_filled[, c("l2501", "l2502", "l2503", "l2504", "l2505",
                            "l2506", "l2507", "l2508", "l2509", "l2510",
                            "l2511", "l2512", "l2513", "l2514", "l2515")]
# KMO检验
kmo_result <- KMO(data_efa)
print(kmo_result)
# 巴特利特球形检验
bartlett_result <- cortest.bartlett(cor(data_efa), n = nrow(data_filled))
print(bartlett_result)

# 计算多项式相关矩阵
polychoric_corr <- polychoric(data_efa)$rho
# 进行第一次探索性因子分析，使用多项式相关矩阵
efa_result <- fa(polychoric_corr, nfactors = 1, fm = "pa", rotate = "varimax")
print(efa_result)
# 查看因子载荷量
loadings <- efa_result$loadings[, 1]  # 提取第一个因子的载荷量
# 打印载荷量
print(loadings)
# 识别载荷量小于0.4的项目（绝对值）
low_loading_items <- names(loadings)[abs(loadings) < 0.4]
cat("需要删除的低载荷量项目：", paste(low_loading_items, collapse = ", "), "\n")

# 第二次EFA
data_efa2 <- data_filled[, c("l2503", "l2505", "l2507", "l2509", "l2511", "l2513", "l2515")]
# 进行并行分析
fa_parallel <- fa.parallel(data_efa2, fa = "pc", n.iter = 100, show.legend = FALSE, main = "Parallel Analysis")
# 计算多项式相关矩阵
polychoric_corr2 <- polychoric(data_efa2)$rho
# 进行第一次探索性因子分析，使用多项式相关矩阵
efa_result2 <- fa(polychoric_corr2, nfactors = 1, fm = "pa", rotate = "varimax")
print(efa_result2)


##### IRT #####
# 模型比较
# 使用Graded Response Model拟合无约束模型
model_unconstrained <- mirt(data_efa2, model = 1, itemtype = "graded")
summary(model_unconstrained)
# 使用rash Model拟合受限模型
model_constrained <- mirt(data_efa2, model = 1, itemtype = "Rasch")
summary(model_constrained)
# 进行似然比检验（Likelihood Ratio Test）
lrt_result <- anova(model_constrained, model_unconstrained)
print(lrt_result)
# 获取模型的对数似然值
logLik_constrained <- logLik(model_constrained)
logLik_unconstrained <- logLik(model_unconstrained)
# 显示对数似然值
cat("Log Likelihood (Constrained Model):", logLik_constrained, "\n")
cat("Log Likelihood (Unconstrained Model):", logLik_unconstrained, "\n")
# 计算似然比
likelihood_ratio <- -2 * (logLik_constrained - logLik_unconstrained)
# 显示似然比
cat("Likelihood Ratio:", likelihood_ratio, "\n")
# 自由度（df）为模型之间的参数数量差异
df <- 6
# 计算p值
p_value <- 1 - pchisq(likelihood_ratio, df)
# 显示p值
cat("p-value:", p_value, "\n")

# 获取项目参数估计（难度和区分度）
coef(model_unconstrained, IRTpars = TRUE)
# 你的绘图代码
plot(model_unconstrained, type = "trace", item = 1)  # 示例绘制第一个条目的 CRC
plot(model_unconstrained, type = "info", item = "l2507")  # 示例绘制第一个条目的 ICC


##### CFA #####
# 描述性分析
data <- read_sav("/Users/a/Desktop/NJU/1 课程/高级测量/assignment23/CGSS2010.sav")
data1 <- data[, c("id", "s5", "a2", "a3a", "a7a")]
data1 <- data1 %>%
  select(id, region = s5, gender = a2, age = a3a, education = a7a) %>%
  mutate(age = 2010 - age) 
# 合并数据集，只保留两者都有的行
data_combined <- inner_join(data_filled, data1, by = "id")
# 查看合并后数据的前几行以确认合并正确
head(data_combined)
write.csv(data_combined, "data_combined.csv")

# 计算region的分类频率
region_counts <- data_combined %>%
  count(region) %>%
  mutate(percentage = n / sum(n) * 100)
# 计算性别比例
gender_ratio <- data_combined %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)
# 计算教育程度比例
education_ratio <- data_combined %>%
  count(education) %>%
  mutate(percentage = n / sum(n) * 100)
# 打印结果
print(region_counts)
print(gender_ratio)
print(education_ratio)
# 年龄的描述性统计
age_stats <- data_combined %>%
  summarise(
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    variance_age = var(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  )
# 打印结果
print(age_stats)

# 多组
data_cfa <- data_combined[, c("l2503", "l2505", "l2507", "l2509", "l2511", "l2513", "l2515", "region", "gender", "age", "education")]
# 转换 gender为因子
data_cfa$gender <- as.factor(data_cfa$gender)
model <- '
  F1 =~ l2503 + l2505 + l2507 + l2509 + l2511 + l2513 + l2515
'
# 拟合模型
fit <- cfa(model, data=data_cfa)
summary(fit, fit.measures=TRUE)
# 性别分组的 MGCFA
fit_gender <- cfa(model, data=data_cfa, group="gender")
summary(fit_gender, fit.measures=TRUE)
# 配置恒等性
configural_fit <- cfa(model, data=data_cfa, group="gender")
# 度量恒等性
metric_fit <- update(configural_fit, group.equal="loadings")
# 标量恒等性
scalar_fit <- update(metric_fit, group.equal=c("loadings", "intercepts"))
# 严格恒等性
strict_fit <- update(scalar_fit, group.equal=c("loadings", "intercepts", "residuals"))
# 比较所有模型的拟合度
anova(configural_fit, metric_fit, scalar_fit, strict_fit)


##### DIF DTF #####
# 提取题目数据和性别分组变量
data_dif_origin <- data_combined[, c("l2503", "l2505", "l2507", "l2509", "l2511", "l2513", "l2515")]
data_dif <- data_combined[, c("l2503", "l2505", "l2507", "l2509", "l2511", "l2513", "l2515", "gender")]
# 确保 gender 是因子
data_dif$gender <- as.factor(data_dif$gender)

library(mirt)
# 创建多组模型
model_dif <- mirt::multipleGroup(data_dif_origin, 1, itemtype = "graded", group = data_dif$gender)
# 检测难度 ("d") 和区分度 ("a") 的 DIF
DIFresults_all <- mirt::DIF(model_dif, which.par = c("a1", "d1", "d2", "d3", "d4"))
print(DIFresults_all)
# 绘制第 9 题（l2509）的项目特征曲线
plot(model_dif, type = "trace", which.items = 4, facet_items = TRUE)
# 绘制第 11 题（l2511）的项目特征曲线
plot(model_dif, type = "trace", which.items = 5, facet_items = TRUE)
# 绘制第 13 题（l2513）的项目特征曲线
plot(model_dif, type = "trace", which.items = 6, facet_items = TRUE)

# 提取模型参数
coef_model <- coef(model_dif, simplify = TRUE)
# 提取两组参数
group1_params <- coef_model[[1]]$items  # 第一组 (group 1)
group2_params <- coef_model[[2]]$items  # 第二组 (group 2)
# 计算区分度参数 (a1) 的差异
dif_a1 <- group2_params[, "a1"] - group1_params[, "a1"]
# 计算每个难度参数 (d1, d2, d3, d4) 的差异
dif_d1 <- group2_params[, "d1"] - group1_params[, "d1"]
dif_d2 <- group2_params[, "d2"] - group1_params[, "d2"]
dif_d3 <- group2_params[, "d3"] - group1_params[, "d3"]
dif_d4 <- group2_params[, "d4"] - group1_params[, "d4"]
# 打印具体 DIF 差异
dif_results <- data.frame(
  Item = rownames(group1_params),
  DIF_a1 = dif_a1,
  DIF_d1 = dif_d1,
  DIF_d2 = dif_d2,
  DIF_d3 = dif_d3,
  DIF_d4 = dif_d4
)
print(dif_results)


# 将性别变量转换为数值
data_dif$gender_numeric <- as.numeric(data_dif$gender)
# 检查题目数据是否为数值型矩阵
data_dif_origin <- as.matrix(data_dif_origin)

# 使用 lordif 进行 DIF 分析
lordif_results <- lordif(
  data_dif_origin,       # 题目矩阵
  data_dif$gender_numeric  # 分组变量
)
# 查看 DIF 分析结果
summary(lordif_results)

# 查看 DTF 结果
dtf_results <- lordif_results$DTF
# 手动计算 DTF
total_dif <- sum(lordif_results$stats$R2, na.rm = TRUE)  # 假设 stats$R2 包含每个条目的 DIF 值
print(total_dif)

# 提取 DIF 指标
dif_stats <- lordif_results$stats
# 查看主要列，例如：pseudo R2 和卡方值
print(dif_stats[, c("item", "chi12", "pseudo12.McFadden", "pseudo12.Nagelkerke")])
# 总 DIF 计算：使用 McFadden R2
total_dif_mcfadden <- sum(dif_stats$pseudo12.McFadden, na.rm = TRUE)

# 总 DIF 计算：使用 Nagelkerke R2
total_dif_nagelkerke <- sum(dif_stats$pseudo12.Nagelkerke, na.rm = TRUE)

# 打印结果
print(total_dif_mcfadden)
print(total_dif_nagelkerke)