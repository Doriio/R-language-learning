#####PACKAGES#####
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)



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
    Height = ifelse(Height == "1630", "163", Height),
    # 转换为数值型
    Height = as.numeric(Height)) %>%

# 处理 Weight 列，替换 "秘密" 为 NA，删除 "kg" 并转换为数值
  mutate(
    Weight = gsub("[^0-9.]", "", Weight),  # 删除所有非数字和小数点的字符
    Weight = as.numeric(Weight)) %>% # 转换为数值型

# 转换 ID 为数值型
  mutate(ID = as.numeric(ID)) %>%

# 删除作答时间<60 >1000的被试
  filter(Time >= 60 & Time <= 1000) %>%
# 删除17以下或30岁以上的被试
  filter(Age >= 17 & Age <= 30) %>%
# 删除 Weight 列为 NA 的行
  filter(!is.na(Weight))

# 定义 reverse_score 函数
reverse_score <- function(x) {
  case_when(
    x == -3 ~ 3,
    x == -2 ~ 2,
    x == -1 ~ 1,
    x == 1 ~ -1,
    x == 2 ~ -2,
    x == 3 ~ -3,
    TRUE ~ x  # 其他值不变
  )
}
# 处理数据
data <- data %>%
  mutate(
    # 映射 Attention 列
    Attention = case_when(
      Attention == "完全不同意" ~ -3,
      Attention == "大部分不同意" ~ -2,
      Attention == "有些不同意" ~ -1,
      Attention == "有些同意" ~ 1,
      Attention == "大部分同意" ~ 2,
      Attention == "完全同意" ~ 3,
      TRUE ~ NA_real_  # 如果是其他无效值，设置为 NA
    ),
    
    # 将字符型列 1-20 转换为数值型并反向计分
    across(`1`:`20`, ~ case_when(
      . %in% c("完全不同意", "大部分不同意", "有些不同意", "有些同意", "大部分同意", "完全同意") ~
        reverse_score(as.numeric(factor(., levels = c("完全不同意", "大部分不同意", "有些不同意", "有些同意", "大部分同意", "完全同意")))),
      TRUE ~ .  # 其他列保持原样
    ))
  ) 





View(data)
str(data)  # 查看数据框结构


# 数据的摘要信息
summary(data)