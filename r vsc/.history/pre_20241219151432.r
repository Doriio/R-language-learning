#####PACKAGES#####
install.packages("readxl")
install.packages("dplyr")
install.packages("psych")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("MVN")
install.packages("lavaan")
install.packages("semPlot")
install.packages("openxlsx")
install.packages("mirt")
install.packages("semTools")
install.packages("GPArotation")

library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(tidyr)
library(MVN)
library(lavaan)
library(semPlot)
library(openxlsx)
library(mirt)
library(semTools)
library(GPArotation)


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
    Weight = as.numeric(Weight), # 转换为数值型
    Weight = ifelse(Weight >= 100, Weight / 2, Weight)) %>% # 修改超过 100 的值
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
# 新增BMI及index一列
  mutate(
    BMI = Weight / ( (Height / 100)^2 ),  # 计算 BMI，注意身高单位转换
    index = case_when(
      BMI < 18.5 ~ "偏瘦",              # BMI 小于 18.5
      BMI >= 18.5 & BMI < 24 ~ "正常",  # BMI 在 18.5 和 24 之间
      BMI >= 24 & BMI < 28 ~ "超重",    # BMI 在 24 和 28 之间
      BMI >= 28 ~ "肥胖",               # BMI 大于或等于 28
      TRUE ~ NA_character_              # 其他情况，设置为 NA
    )
  ) %>%
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
# 导出为 Excel 文件 write.xlsx(data, file = "data_cleaned.xlsx", rowNames = FALSE)


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
# write.xlsx(data, file = "sample1.xlsx", rowNames = FALSE)
# write.xlsx(data, file = "sample2.xlsx", rowNames = FALSE)

# Shapiro-Wilk 检验
shapiro_data <- shapiro.test(data$Weight)
shapiro_sample1 <- shapiro.test(sample1$Weight)
shapiro_sample2 <- shapiro.test(sample2$Weight)
# 输出结果表明不服从正态分布
print(shapiro_data)
print(shapiro_sample1)
print(shapiro_sample2)
# 正态分布可视化
par(mfrow = c(2, 2))  # 创建2x2网格
hist(data$Weight, main = "Histogram of Data Weight", col = "lightblue")
qqnorm(data$Weight); qqline(data$Weight)

hist(sample1$Weight, main = "Histogram of Sample1 Weight", col = "lightgreen")
qqnorm(sample1$Weight); qqline(sample1$Weight)

hist(sample2$Weight, main = "Histogram of Sample2 Weight", col = "lightcoral")
qqnorm(sample2$Weight); qqline(sample2$Weight)


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


#####探索性因素分析#####
# 1 检验前提：KMO 和 Bartlett 检验
# 提取样本1的条目数据
item_data <- sample1[, c("1", "2", "3", "4", "5", "6", "7", "9", "10", "12", "14", "15", "16", "17", "18", "19", "20")]
# KMO 检验
kmo_result <- KMO(item_data)
print(kmo_result)  # KMO值和MSA指标
# Bartlett 球形检验
bartlett_result <- cortest.bartlett(cor(item_data, use = "pairwise.complete.obs"), n = nrow(item_data))
print(bartlett_result)  # 查看p值

# 2 EFA：主成分分析法 + 方差最大正交旋转
# 碎石图确定因子数量
# 运行平行分析并提取特征值
fa_parallel <- fa.parallel(item_data, fa = "pc", n.iter = 100, show.legend = FALSE)
# 提取实际特征值和随机特征值
actual_values <- fa_parallel$pc.values  # 实际特征值
simulated_values <- fa_parallel$pc.sim  # 随机特征值
# 准备碎石图的数据框
scree_data <- data.frame(
  Component = 1:length(actual_values),
  Actual = actual_values,
  Simulated = simulated_values
)
# 使用 ggplot 绘制碎石图
ggplot(scree_data, aes(x = Component)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1.2) +  # 实际特征值
  geom_line(aes(y = Simulated, color = "Simulated"), linetype = "dashed", size = 1) +  # 随机特征值
  geom_point(aes(y = Actual), size = 2) +  # 实际特征值的点
  geom_point(aes(y = Simulated), size = 2) +  # 随机特征值的点
  labs(title = "Scree Plot with Parallel Analysis",
       x = "Component Number",
       y = "Eigenvalue",
       color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Simulated" = "red")) +  # 自定义颜色
  theme_minimal(base_size = 14) +  # 使用简约主题
  theme(legend.position = "top",    # 调整图例位置
        plot.title = element_text(hjust = 0.5))  # 标题居中

# 进行探索性因素分析
efa_result <- principal(item_data, nfactors = 4, rotate = "varimax", scores = TRUE)
print(efa_result)  # 打印因子载荷矩阵
print(efa_result$loadings)  # 打印因子载荷矩阵
# 3剔除不符合要求的选项
# 剔除条目4，进一步检查双重负荷条目
item_data_filtered <- item_data %>% select(-c("2", "3", "4", "6", "17"))

fa_parallel <- fa.parallel(item_data, fa = "pc", n.iter = 100, show.legend = FALSE)

# 重新运行EFA，提取3个因子
efa_result_filtered <- principal(item_data_filtered, nfactors = 3, rotate = "varimax", scores = TRUE)
# 查看新的因子载荷矩阵
print(efa_result_filtered$loadings)
# 可视化因子结构
fa.diagram(efa_result_filtered)


#####验证性因素分析#####
# 重命名 sample2 数据框的列名
colnames(sample2)[3:22] <- paste0("Q", 1:20)
# 检查列名
print(colnames(sample2))

# 提取剔除问题条目后的数据
item_data_clean <- sample2[, c("Q1", "Q5", "Q7", "Q9", "Q10", "Q12", "Q14", "Q15", "Q16", "Q18", "Q19", "Q20")]

# 2. 定义模型
cfa_model <- '
  Factor1 =~ Q14 + Q15 + Q16 + Q10
  Factor2 =~ Q1 + Q9
  Factor3 =~ Q5 + Q18 + Q20
'
# 3. 运行 CFA
cfa_result <- cfa(cfa_model, data = item_data_clean, estimator = "ML")
# 查看拟合结果
summary(cfa_result, fit.measures = TRUE, standardized = TRUE)
# 4. 绘制路径图
semPaths(cfa_result, 
         what = "std", 
         layout = "tree", 
         style = "lisrel", 
         residuals = TRUE, 
         edge.label.cex = 1.2, # 路径系数字体大小
         nCharNodes = 10,      # 限制因子节点的名称长度
         curvePivot = TRUE,    # 使曲线美观
         title = TRUE)

# 检查潜变量协方差
lavInspect(cfa_result, "cov.lv")

# 2. 定义模型
cfa_model <- '
  Factor1 =~ Q15 + Q16 + Q10
  Factor2 =~ Q1 + Q9
  Factor3 =~ Q5 + Q20
'
# 3. 运行 CFA
cfa_result <- cfa(cfa_model, data = item_data_clean, estimator = "ML")
# 查看拟合结果
summary(cfa_result, fit.measures = TRUE, standardized = TRUE)
# 4. 绘制路径图
semPaths(cfa_result, 
         what = "std", 
         layout = "tree", 
         style = "lisrel", 
         residuals = TRUE, 
         edge.label.cex = 1.2, # 路径系数字体大小
         nCharNodes = 10,      # 限制因子节点的名称长度
         curvePivot = TRUE,    # 使曲线美观
         title = TRUE)

# 检查潜变量协方差
lavInspect(cfa_result, "cov.lv")

# 删除 Q14 和 Q18 后的模型已达较为合理的拟合程度，三个因子的结构也清晰可见。
# 可以将此结果作为量表模型的最终结构，或在后续研究中进一步优化。


#####校标关联效度#####
# 1 计算DBS信度
# 提取 D1 到 D6 列
items <- data[, c("D1", "D2", "D3", "D4", "D5", "D6")]
# 计算 Cronbach's Alpha
alpha_result <- psych::alpha(items)
print(alpha_result)

# 2 计算两个维度得分
# 计算 ATOP 的两个维度总分（假设 Factor1 和 Factor2 维度已经分好）
data$Factor1total <- rowSums(data[, c("15", "16", "10")], na.rm = TRUE)  # 因子1的题目
data$Factor2total <- rowSums(data[, c("1", "9")], na.rm = TRUE)  # 因子2的题目
data$Factor3total <- rowSums(data[, c("5", "20")], na.rm = TRUE)  # 因子3的题目

# 校标关联效度计算：ATOP 总分与 DBS 总分
cor_atop_dbs <- cor.test(data$ATOPtotal, data$DBStotal, method = "pearson")
print(cor_atop_dbs)

# 校标关联效度计算：Factor1 与 DBS 总分
cor_factor1_dbs <- cor.test(data$Factor1total, data$DBStotal, method = "pearson")
print(cor_factor1_dbs)

# 校标关联效度计算：Factor2 与 DBS 总分
cor_factor2_dbs <- cor.test(data$Factor2total, data$DBStotal, method = "pearson")
print(cor_factor2_dbs)

# 校标关联效度计算：Factor3 与 DBS 总分
cor_factor3_dbs <- cor.test(data$Factor3total, data$DBStotal, method = "pearson")
print(cor_factor3_dbs)

# 将相关性结果整理为表格
cor_results <- data.frame(
  Comparison = c("ATOPtotal vs DBStotal", "Factor1total vs DBStotal", "Factor2total vs DBStotal", "Factor3total vs DBStotal"),
  Correlation = c(cor_atop_dbs$estimate, cor_factor1_dbs$estimate, cor_factor2_dbs$estimate, cor_factor3_dbs$estimate),
  p_value = c(cor_atop_dbs$p.value, cor_factor1_dbs$p.value, cor_factor2_dbs$p.value, cor_factor3_dbs$p.value)
)
print(cor_results)

# 可视化
library(ggplot2)
# 散点图：ATOPtotal vs DBStotal
ggplot(data, aes(x = ATOPtotal, y = DBStotal)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "ATOPtotal vs DBStotal",
       x = "ATOP Total Score",
       y = "DBS Total Score") +
  theme_minimal()

# 散点图：Factor1total vs DBStotal
ggplot(data, aes(x = Factor1total, y = DBStotal)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Factor1total vs DBStotal",
       x = "Factor1 Total Score",
       y = "DBS Total Score") +
  theme_minimal()

# 散点图：Factor2total vs DBStotal
ggplot(data, aes(x = Factor2total, y = DBStotal)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Factor2total vs DBStotal",
       x = "Factor2 Total Score",
       y = "DBS Total Score") +
  theme_minimal()

# 散点图：Factor3total vs DBStotal
ggplot(data, aes(x = Factor3total, y = DBStotal)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Factor3total vs DBStotal",
       x = "Factor3 Total Score",
       y = "DBS Total Score") +
  theme_minimal()


#####信度检验#####
# ATOP量表
total_items <- data[, c("1", "5", "9", "10", "15", "16", "20")]

# 计算总量表的 Cronbach's α 系数
total_alpha <- psych::alpha(total_items)
print(total_alpha)

# Factor1的alpha
factor1_items <- data[, c("10", "15", "16")]
# 计算 Factor1 的 Cronbach's α 系数
factor1_alpha <- psych::alpha(factor1_items)
print(factor1_alpha)

# Factor2 的alpha
factor2_items <- data[, c("1", "9")]
# 计算 Factor2 的 Cronbach's α 系数
factor2_alpha <- psych::alpha(factor2_items)
print(factor2_alpha)

# Factor3 的alpha
factor3_items <- data[, c("5", "20")]
# 计算 Factor3 的 Cronbach's α 系数
factor3_alpha <- psych::alpha(factor3_items)
print(factor3_alpha)

# 整理结果为表格
reliability_results <- data.frame(
  Scale = c("Total Scale", "Factor1", "Factor2", ""),
  Cronbach_Alpha = c(total_alpha$total$raw_alpha, 
                     factor1_alpha$total$raw_alpha, 
                     factor2_alpha$total$raw_alpha)
)
print(reliability_results)

# BMI与量表得分的关系
# 计算 BMI 与量表得分的 Pearson 相关
cor_bmi_atop <- cor.test(data$ATOPtotal, data$BMI, method = "pearson")
cor_bmi_factor1 <- cor.test(data$Factor1total, data$BMI, method = "pearson")
cor_bmi_factor2 <- cor.test(data$Factor2total, data$BMI, method = "pearson")

# 打印结果
print(cor_bmi_atop)
print(cor_bmi_factor1)
print(cor_bmi_factor2)

# 如果 ANOVA 不满足正态性假设，改用 Kruskal-Wallis 检验
kruskal_atop <- kruskal.test(ATOPtotal ~ index, data = data)
kruskal_factor1 <- kruskal.test(Factor1total ~ index, data = data)
kruskal_factor2 <- kruskal.test(Factor2total ~ index, data = data)

# 打印 Kruskal-Wallis 结果
print(kruskal_atop)
print(kruskal_factor1)
print(kruskal_factor2)


#####项目反应理论#####
# 提取 20 个题目作答数据
item_data <- data[, c("1", "2", "3", "4", "5", "6", "7", "9", "10",
                      "12", "14", "15", "16", "17", "18", "19", "20")]

# 确保题目作答数据是数值型
item_data <- as.data.frame(lapply(item_data, as.integer))

# 使用 GPCM 模型进行参数估计
gpcm_model <- mirt(item_data, 1, itemtype = 'gpcm')  # 单维度模型

# 提取项目参数
item_parameters <- coef(gpcm_model, IRTpars = TRUE, simplify = TRUE)
print(item_parameters)

# 提取区分度参数
discrimination <- item_parameters$items[, "a"]
print(discrimination)

# 提取难度参数
difficulty <- item_parameters$items[, c("b1", "b2", "b3", "b4", "b5")]
print(difficulty)

low_discrimination <- which(discrimination < 0.35)
print(low_discrimination)
# 删除低区分度题目
filtered_items <- item_parameters$items[-low_discrimination, ]
print(filtered_items)

# 筛选难度参数范围异常的题目
abnormal_difficulty <- which(apply(difficulty, 1, function(x) any(x > 5 | x < -5, na.rm = TRUE)))
print(abnormal_difficulty)

# 删除难度范围异常的题目
filtered_items <- item_parameters$items[-abnormal_difficulty, ]
print(filtered_items)

# 区分度分布
ggplot(data = data.frame(Item = rownames(item_parameters$items), a = discrimination),
       aes(x = Item, y = a)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Item Discrimination", x = "Item", y = "Discrimination (a)") +
  theme_minimal()

# 难度参数分布
difficulty_df <- data.frame(Item = rep(rownames(difficulty), each = ncol(difficulty)),
                            Difficulty = as.vector(t(difficulty)),
                            Threshold = rep(colnames(difficulty), times = nrow(difficulty)))

ggplot(difficulty_df, aes(x = Threshold, y = Difficulty, group = Item, color = Item)) +
  geom_line() +
  geom_point() +
  labs(title = "Item Difficulty Parameters", x = "Threshold", y = "Difficulty") +
  theme_minimal()

# 绘制 ICC
plot(gpcm_model, type = "trace", items = 1:5)  # 示例绘制前 5 个题目
# 总信息曲线
plot(gpcm_model, type = "info")
# 单个题目的信息曲线
plot(gpcm_model, type = "info", items = 1:5)  # 示例绘制前5个题目


#####ESEM#####
# 提取样本1的条目数据
item_data <- data[, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20")]

# 碎石图和并行分析
fa.parallel(item_data, fa = "fa", n.iter = 100, show.legend = TRUE, main = "Scree Plot")