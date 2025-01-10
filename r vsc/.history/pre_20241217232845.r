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
# 删除作答时间<60 >1000的被试
data <- data %>%
  mutate(Time = as.numeric(gsub("秒", "", Time))) %>%
  filter(Time >= 60 & Time <= 1000) %>%
# 删除注意检查未果的被试
  filter(Attention == 1) %>%

  filter(Age >= 17 & Age <= 30)
# 删除17以下或30岁以上的被试

# 删除体重保密的被试

# 正向计分

# 反向计分

# 
# 以表格形式查看数据
View(data)
# 数据的摘要信息
summary(data)