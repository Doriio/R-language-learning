#####PACKAGES#####
install.packages("haven")  # 安装haven包
install.packages("dplyr")
library(haven)             # 加载haven包
library(dplyr)

#####IMPORTING DATASETS#####
# 读取.sav文件
data <- read_sav("/Users/a/Desktop/NJU/1 课程/高级测量/assignment23/CGSS2010.sav")
# 保留列名为 l2501 到 l2515 的这15列
data <- data[, c("l2501", "l2502", "l2503", "l2504", "l2505", 
                             "l2506", "l2507", "l2508", "l2509", "l2510", 
                             "l2511", "l2512", "l2513", "l2514", "l2515")]
# 将数据框保存为 CSV 文件 write.csv(data, "data_sample.csv")


#####DATA SCREENING#####
# 定义一个函数来进行数字到文字的转换
convert_to_text <- function(x) {
  if (is.na(x)) {
    return("缺失值")  # 处理缺失值
  } else if (x == 1) {
    return("完全不同意")
  } else if (x == 2) {
    return("比较不同意")
  } else if (x == 3) {
    return("无所谓")
  } else if (x == 4) {
    return("比较同意")
  } else if (x == 5) {
    return("完全同意")
  } else if (x == 8) {
    return("无法选择")
  } else if (x == -3) {
    return("拒绝回答")
  } else if (x == -2) {
    return("不知道")
  } else if (x == -1) {
    return("不适用")
  } else {
    return(NA)  # 其他值处理为缺失值
  }
}
# 使用 dplyr 的 mutate(across()) 来批量转换列
data <- data %>%
  mutate(across(l2501:l2515, ~sapply(., convert_to_text)))
# 定义要检查的值
excluded_values <- c("缺失值", "无法选择", "拒绝回答", "不知道", "不适用")
# 计算每行中符合条件的值的数量
count_excluded_values <- apply(data, 1, function(row) {
  sum(row %in% excluded_values, na.rm = TRUE)
})
# 查看符合条件的行
rows_to_show <- data[count_excluded_values > 5, ]
print(rows_to_show)
# 删除符合条件的行
data_cleaned <- data[count_excluded_values <= 5, ]
# 查看删除后的数据
head(data_cleaned)

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

# 对每列进行缺失值填补，用列的均值填补
for (col in names(data_cleaned)) {
  # 计算该列的均值，忽略缺失值
  col_mean <- mean(data_cleaned[[col]], na.rm = TRUE)
  
  # 用该列的均值填补缺失值
  data_cleaned[[col]][is.na(data_cleaned[[col]])] <- col_mean
}

# 查看赋分后的数据
head(data_cleaned)
sum(is.na(data_cleaned))