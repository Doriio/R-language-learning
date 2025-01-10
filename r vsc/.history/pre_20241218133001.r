#####PACKAGES#####
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")
install.packages("ggplot2")
install.packages("tidyr")
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
#####IMPORTING DATASETS#####
# wd "/Users/a/R-language-learning/r vsc"
#importing dataset
rawdata <- readxl::read_excel("/Users/a/Desktop/NJU/1 课程/高级测量/measure/ATOP.xlsx", sheet = 1)


#####DATA SCREENING#####
# 删除指定列（2, 4:7）查看结果 head(data)
data <- rawdata %>%
  dplyr::select(-c(2, 4:7))
# 对列重命名
colnames(data) <- c("ID", "Time","1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Attention", "11", "12", "13", "14", "15", "16", "17", "18","19", "20", "D1", "D2", "D3", "D4", "D5", "D6", "Gender", "Age", "Height", "Weight") # nolint

data <- data %>%
# 处理 Time 列，去掉秒，转换为数值
  mutate(Time = as.numeric(gsub("秒", "", Time))) %>%
# 处理 Age 列，去掉岁，转换为数值
  mutate(Age = as.numeric(gsub("岁", "", Age))) %>%
# 处理 Height 列，去掉c m，转换为数值
  mutate(
    # 删除非数字字符，只保留数字和小数点
    Height = gsub("[^0-9.]", "", Height),
    # 处理异常值 1630 -> 163
    Height = ifelse(Height == "1630", "163", Height),
    Height = ifelse(Height == "1.6", "160", Height),
    # 转换为数值型
    Height = as.numeric(Height)) %>%
# 处理 Weight 列，替换 "秘密" 为 NA，删除 "kg" 并转换为数值
  mutate(
    Weight = gsub("[^0-9.]", "", Weight),  # 删除所有非数字和小数点的字符
    Weight = as.numeric(Weight)) %>% # 转换为数值型
# 转换 ID 为数值型
  mutate(ID = as.numeric(ID)) %>%

# ATOP liket映射
  mutate(
    # 假设 `1` 到 `20` 和 `Attention` 都是响应列
    across(c(`1`:`20`, Attention), ~ case_when(
      . == "完全不同意" ~ -3,
      . == "大部分不同意" ~ -2,
      . == "有些不同意" ~ -1,
      . == "有些同意" ~ 1,
      . == "大部分同意" ~ 2,
      . == "完全同意" ~ 3,
      TRUE ~ NA_real_  # 处理未知或无效响应
    )),
    # 反向计分的列：2, 3, 4, 5, 6, 10, 11, 12, 14, 15, 16, 19, 20
    across(c(`2`, `3`, `4`, `5`, `6`, `10`, `11`, `12`, `14`, `15`, `16`, `19`, `20`), ~ -.)
  ) %>%

# DBS liket映射
  mutate(
    across(c(D1, D2, D3, D4, D5, D6), ~ case_when(
      . == "完全不同意" ~ 6,
      . == "大部分不同意" ~ 5,
      . == "有些不同意" ~ 4,
      . == "有些同意" ~ 3,
      . == "大部分同意" ~ 2,
      . == "完全同意" ~ 1,
      TRUE ~ NA_real_  # 对未知或无效响应设置为 NA
    ))
  ) %>%

# 性别 映射
  mutate(
    Gender = case_when(
      Gender == "男" ~ 1,
      Gender == "女" ~ 0,
      TRUE ~ NA_integer_  # 对未知或无效响应设置为 NA
    )
  ) %>%
# 增加ATOP总分列：1到20列相加再加60
  mutate(ATOPtotal = rowSums(select(., c("1":"20")), na.rm = TRUE) + 60) %>%
# 增加DBS总分列：D1-D6相加
  mutate(DBStotal = rowSums(select(., c("D1", "D2", "D3", "D4", "D5", "D6")), na.rm = TRUE)) %>%
  

# 删除作答时间<60 >1000的被试
  filter(Time >= 60 & Time <= 1000) %>%
# 删除17以下或30岁以上的被试
  filter(Age >= 17 & Age <= 30) %>%
# 删除 Weight 列为 NA 的行
  filter(!is.na(Weight)) %>%
# 剔除Attention列不通过的被试
  filter(Attention == 1)


#####DATA OVERVIEW#####
# 以表格形式查看数据
View(data)
# 数据概览
summary(data)

#####样本随机分组为样本1和样本2#####
# 设置随机种子，确保结果可复现
set.seed(123)
# 随机生成分组标签
data$sample <- sample(rep(c("Sample1", "Sample2"), each = 133))
# 检查分组结果
table(data$sample)  # 确保每组都是133个
# 分割数据
sample1 <- data %>% filter(sample == "Sample1") # 项目分析和探索性因素分析
sample2 <- data %>% filter(sample == "Sample2") # 验证性因素分析  总样本用于校标关联效度和内部一致性信度
# 查看分组数据
View(sample1)
View(sample2)


#####项目分析：样本1#####
# 1 筛针对样本1，计算ATOP总分，并排序
sample1 <- sample1 %>% arrange(desc(ATOPtotal))

# 2 取前27%和后27%被试，分为高分组和低分组
# 计算分组的样本数 (前27% 和 后27%)
n <- nrow(sample1)
n_27 <- round(n * 0.27)  # 27%的样本数
# 创建高分组和低分组
highgroup <- sample1[1:n_27, ]  # 前27%为高分组
lowgroup <- sample1[(n - n_27 + 1):n, ]  # 后27%为低分组
# 检查分组情况
print(dim(highgroup))
print(dim(lowgroup))

# 3 进行独立样本t检验，计算每个条目的临界比（CR）
# 提取条目列 (假设题目列是1-20)
item_columns <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
                  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")
# 进行独立样本t检验，计算CR和p值
t_results <- data.frame(Item = item_columns, CR = NA, p_value = NA)
for (i in item_columns) {
  t_test <- t.test(highgroup[[i]], lowgroup[[i]]) # 高分组 vs 低分组
  t_results$CR[t_results$Item == i] <- abs(t_test$statistic)  # 取t统计量的绝对值
  t_results$p_value[t_results$Item == i] <- t_test$p.value # p值
}
# 查看检验结果
print(t_results)
# 剔除出p值 > 0.05 的条目
non_significant_items <- t_results %>% filter(p_value > 0.05)
print(non_significant_items)

# 4 计算Pearson相关，确认每个条目与量表总分的相关关系
# 创建一个数据框存储相关结果
cor_results <- data.frame(Item = item_columns, Correlation = NA, p_value = NA)
# 计算每个条目与量表总分的相关系数
for (i in item_columns) {
  cor_test <- cor.test(sample1[[i]], sample1$ATOPtotal, method = "pearson")  # 用 ATOPtotal
  cor_results$Correlation[cor_results$Item == i] <- cor_test$estimate        # 相关系数
  cor_results$p_value[cor_results$Item == i] <- cor_test$p.value             # p值
}
# 查看结果
print(cor_results)
# 剔除出 p值 > 0.05 的条目
non_significant_cor <- cor_results %>% filter(p_value > 0.05)
print(non_significant_cor)

# 5 初步项目分析：难度分析
# 基于样本1计算每个条目的难度（平均分）
difficulty_scores <- colMeans(sample1[, item_columns], na.rm = TRUE)
# 查看难度结果
print(difficulty_scores)
# 筛选难度小于0.3的项目
low_difficulty_items <- names(difficulty_scores[difficulty_scores < 0.3])
print(low_difficulty_items)

# 6 初步项目分析：区分度分析
# 创建一个数据框存储区分度结果
discrimination_results <- data.frame(Item = item_columns, Discrimination = NA)
# 计算每个条目的区分度
for (i in item_columns) {
  discrimination_results$Discrimination[discrimination_results$Item == i] <- 
    mean(highgroup[[i]], na.rm = TRUE) - mean(lowgroup[[i]], na.rm = TRUE)
}
# 查看区分度结果
print(discrimination_results)
# 筛选出区分度 < 0.2 的条目
low_discrimination_items <- discrimination_results %>% filter(Discrimination < 0.2)
print(low_discrimination_items)  # 查看区分度较低的条目

#####校标工具DBS信度#####
# 提取 D1 到 D6 列
items <- data[, c("D1", "D2", "D3", "D4", "D5", "D6")]
# 计算 Cronbach's Alpha
alpha_result <- psych::alpha(items)
print(alpha_result)


#####项目分析#####
# 计算难度分析：每个题项的平均得分
difficulty_scores <- colMeans(data[, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")], na.rm = TRUE)
print(difficulty_scores)

# 计算区分度：每个题项与总分的相关性
discrimination_scores <- sapply(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"), function(x) {
  total_minus_item <- rowSums(data[, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")], na.rm = TRUE) - data[, x]
  cor(data[, x], total_minus_item, use = "complete.obs")
})
print(discrimination_scores)


#####DATA OVERVIEW#####


# 探索性因素分析
efa_result <- fa(data[, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")], nfactors = 3, rotate = "varimax")
print(efa_result)  # 打印EFA结果
# 可视化EFA
# 提取因子载荷矩阵
loadings_matrix <- efa_result$loadings
# 将载荷矩阵转换为数据框，并确保 NA 值正确填充
loadings_df <- as.data.frame(unclass(loadings_matrix))  # 确保矩阵变为标准数据框
# 添加题目编号作为新列
loadings_df$item <- rownames(loadings_matrix)
# 查看数据框结构
print(head(loadings_df))
# 将宽格式转换为长格式
loadings_long <- pivot_longer(loadings_df, 
                              cols = -item, 
                              names_to = "factor", 
                              values_to = "loading")
# 检查转换后的数据
print(head(loadings_long))
# 绘制因子载荷图
ggplot(loadings_long, aes(x = item, y = loading, fill = abs(loading) > 0.3)) +
    geom_col() +
    facet_wrap(~factor) +
    theme_minimal() +
    labs(title = "Factor Loadings", x = "Item", y = "Loading") +
    scale_fill_manual(values = c("grey", "blue"), name = "Significant Loading")


