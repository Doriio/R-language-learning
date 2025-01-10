#####PACKAGES#####
install.packages("haven")  # 安装haven包
install.packages("dplyr")
install.packages("ggplot2")
library(haven)             # 加载haven包
library(dplyr)
library(ggplot2)


#####IMPORTING DATASETS#####
# 读取.sav文件
data <- read_sav("/Users/a/Desktop/NJU/1 课程/高级测量/assignment23/CGSS2010.sav")
# 保留列名为 l2501 到 l2515 的这15列
data <- data[, c("l2501", "l2502", "l2503", "l2504", "l2505", 
                             "l2506", "l2507", "l2508", "l2509", "l2510", 
                             "l2511", "l2512", "l2513", "l2514", "l2515")]
# 将数据框保存为 CSV 文件 write.csv(data, "data_sample.csv")


#####DATA SCREENING#####
# 定义转换函数
convert_to_text <- function(x) {
  if (x == 1) return("完全不同意")
  if (x == 2) return("比较不同意")
  if (x == 3) return("无所谓")
  if (x == 4) return("比较同意")
  if (x == 5) return("完全同意")
  if (x == 8) return("无法选择")
  if (x == -3) return("拒绝回答")
  if (x == -2) return("不知道")
  if (x == -1) return("不适用")
  return(NA)  # 如果没有匹配的值，则返回 NA
}
# 使用 dplyr 的 mutate(across()) 批量转换列
data <- data %>%
  mutate(across(l2501:l2515, ~sapply(., convert_to_text)))
# 定义要检查的值
excluded_values <- c("缺失值", "无法选择", "拒绝回答", "不知道", "不适用")
# 计算每行中符合条件的值的数量，考虑 NA
count_excluded_values <- apply(data, 1, function(row) {
  sum(row %in% excluded_values | is.na(row), na.rm = TRUE)
})
# 删除符合条件的行，保留符合条件数量小于等于5的行
data_cleaned <- data[count_excluded_values <= 5, ]
# 查看删除后的数据
head(data_cleaned)
# 将数据框保存为 CSV 文件 
write.csv(data_cleaned, "data_sample.csv")



# 假设数据框为 data_cleaned
# 正向计分的列：l2501, l2503, l2505, l2507, l2509, l2511, l2513, l2515
positive_items <- c("l2501", "l2503", "l2505", "l2507", "l2509", "l2511", "l2513", "l2515")
# 反向计分的列：l2502, l2504, l2506, l2508, l2510, l2512, l2514
reverse_items <- c("l2502", "l2504", "l2506", "l2508", "l2510", "l2512", "l2514")

# 创建一个映射表，将文本转化为数字
response_map <- c(
  "非常同意" = 5,
  "比较同意" = 4,
  "无所谓" = 3,
  "比较不同意" = 2,
  "完全不同意" = 1,
  "无法选择" = 8,
  "拒绝回答" = -3,
  "不知道" = -2,
  "不适用" = -1
)

# 赋分函数：处理每一题
assign_scores <- function(x, item_type) {
  # 将文本转换为数字
  x <- response_map[as.character(x)]  # 使用映射表进行转换
  # 检查是否转换成功
  if (is.na(x)) {
    return(NA)  # 如果转换失败，返回 NA
  }
  # 根据列类型进行赋分
  if (item_type == "positive") {
    return(6 - x)  # 正向计分：5 -> 1, 4 -> 2, 3 -> 3, 2 -> 4, 1 -> 5
  } else if (item_type == "reverse") {
    return(x)  # 反向计分：1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4, 5 -> 5
  } else {
    return(NA)
  }
}

# 给正向计分和反向计分题赋分
for (col in names(data_cleaned)) {
  if (col %in% positive_items) {
    # 对正向计分的题目赋分
    data_cleaned[[col]] <- sapply(data_cleaned[[col]], assign_scores, item_type = "positive")
  } else if (col %in% reverse_items) {
    # 对反向计分的题目赋分
    data_cleaned[[col]] <- sapply(data_cleaned[[col]], assign_scores, item_type = "reverse")
  }
}

# 对每列进行特殊值填补（8, -1, -2, -3）和缺失值填补，用列的均值填补
for (col in names(data_cleaned)) {
  # 计算该列的均值，忽略缺失值
  col_mean <- mean(data_cleaned[[col]], na.rm = TRUE)
  
  # 用该列的均值填补特殊值（8, -1, -2, -3）和缺失值
  data_cleaned[[col]][data_cleaned[[col]] %in% c(8, -1, -2, -3)] <- col_mean
  data_cleaned[[col]][is.na(data_cleaned[[col]])] <- col_mean
}

# 查看赋分和填补后的数据
head(data_cleaned)
# 检查是否有缺失值
sum(is.na(data_cleaned))
# 将数据框保存为 CSV 文件 
write.csv(data_cleaned, "data_cleaned.csv")


#####descriptive#####
# # 创建一个函数来计算每一列的得分占比
# calculate_score_proportion <- function(column_data) {
#   # 计算每个得分的频率
#   score_table <- table(column_data)
#   # 计算每个得分的占比 (除以总数)
#   score_proportion <- score_table / sum(score_table) * 100  # 转换为百分比
#   return(score_proportion)
# }
# # 用于存储每一题的得分占比
# score_proportions <- list()
# # 计算每一列的得分占比
# for (col in names(data_cleaned)) {
#   score_proportions[[col]] <- calculate_score_proportion(data_cleaned[[col]])
# }
# # 查看结果：得分占比
# score_proportions
# # 创建一个数据框来保存每列得分的占比
# score_proportions_df <- data.frame()
# # 将每一列的得分占比转换为数据框格式，便于绘图
# for (col in names(data_cleaned)) {
#   # 计算每一列的得分占比
#   score_table <- table(data_cleaned[[col]])
#   score_proportion <- score_table / sum(score_table) * 100
#   # 将结果添加到数据框中
#   temp_df <- data.frame(
#     Question = rep(col, length(score_proportion)),
#     Score = as.numeric(names(score_proportion)),
#     Proportion = as.numeric(score_proportion)
#   )
#   score_proportions_df <- rbind(score_proportions_df, temp_df)
# }
# # 绘制条形图
# ggplot(score_proportions_df, aes(x = factor(Score), y = Proportion, fill = Question)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Score Proportions for Each Question", 
#        x = "Score", y = "Proportion (%)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 创建一个数据框来保存每列得分的频数和占比
score_proportions_df <- data.frame()
# 遍历每一列
for (col in names(data_cleaned)) {
  # 计算每一列的得分频率，确保只计算得分 1 到 5
  score_table <- table(data_cleaned[[col]], useNA = "ifany")
  # 过滤只保留得分 1 到 5，其他值不计算
  score_table_filtered <- score_table[names(score_table) %in% c("1", "2", "3", "4", "5")]
  # 确保每个得分都在最终结果中，即使得分没有出现
  full_score_table <- data.frame(Score = 1:5, Frequency = rep(0, 5))
  score_table_df <- data.frame(Score = as.numeric(names(score_table_filtered)), Frequency = as.numeric(score_table_filtered))
  # 合并两者，确保每个得分都有记录
  score_table_merged <- merge(full_score_table, score_table_df, by = "Score", all.x = TRUE)
  score_table_merged$Frequency[is.na(score_table_merged$Frequency)] <- 0  # 将NA值转为0
  # 计算占比
  score_table_merged$Proportion <- round(score_table_merged$Frequency / sum(score_table_merged$Frequency) * 100, 1)
  # 将频数和占比一起保存到数据框中
  temp_df <- data.frame(
    Question = rep(col, length(score_table_merged$Proportion)),
    Score = score_table_merged$Score,
    Frequency = score_table_merged$Frequency,
    Proportion = score_table_merged$Proportion
  )
  score_proportions_df <- rbind(score_proportions_df, temp_df)
}

# 显示数据框
print(score_proportions_df)

# 绘制条形图
ggplot(score_proportions_df, aes(x = factor(Score), y = Proportion, fill = Question)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Score Proportions for Each Question (1-5)", 
       x = "Score", y = "Proportion (%)") +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +  # 确保X轴只有1-5
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))