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
# 删除所有非数字字符（包括单位和空格）
  mutate(
    Height = ifelse(Weight == "秘密", NA, Weight),   # 将 "秘密" 替换为 NA
    Weight = gsub("[^0-9.]", "", Weight),  # 删除所有非数字和小数点的字符
    Weight = as.numeric(Weight)) %>% # 转换为数值型
# 处理 Weight 列，替换 "秘密" 为 NA，删除 "kg" 并转换为数值
  mutate(
    Weight = ifelse(Weight == "秘密", NA, Weight),   # 将 "秘密" 替换为 NA
    Weight = gsub("[^0-9.]", "", Weight),  # 删除所有非数字和小数点的字符
    Weight = as.numeric(Weight)) %>% # 转换为数值型
  mutate(ID = as.numeric(ID), # 转换 ID 为数值型
    Height = as.numeric(Height)) %>% # 转换 Height 为数值型
  
# 删除作答时间<60 >1000的被试
  filter(Time >= 60 & Time <= 1000) %>%
# 删除17以下或30岁以上的被试
  filter(Age >= 17 & Age <= 30)

View(data)
str(data)  # 查看数据框结构

# 正向计分

# 反向计分

# 
# 以表格形式查看数据

# 数据的摘要信息
summary(data)