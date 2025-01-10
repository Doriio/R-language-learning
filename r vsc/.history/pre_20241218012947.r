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
    Height = ifelse(Height == "1.6", "160", Height),
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
  filter(!is.na(Weight)) %>%
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
    # 反向计分的列：例如，2, 3, 4, 5, 6, 10, 11, 12, 14, 15, 16, 19, 20
    across(c(`2`, `3`, `4`, `5`, `6`, `10`, `11`, `12`, `14`, `15`, `16`, `19`, `20`), ~ -.)
  ) %>%
# 剔除Attention列不通过的被试
  filter(Attention == 1) %>%
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
    ))





# 以表格形式查看数据

# 查看数据的摘要信息
summary(data)




#####IMPORTING OVERVIEW#####
# 以表格形式查看数据

# 查看数据摘要信息