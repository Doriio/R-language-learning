#####PACKAGES#####
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)



#####IMPORTING DATASETS#####
# wd "/Users/a/R-language-learning/r vsc"
#importing dataset
data <- readxl::read_excel("/Users/a/Desktop/NJU/1 课程/高级测量/measure/ATOP.xlsx", sheet = 1)

#####DATA SCREENING#####
# 删除指定列（2:6）查看结果 head(data)
data <- data %>%
  select(-c(2, 4:7))
data <- dplyr::select(data, -c(2, 4:7))
# 对列重命名
colnames(data) <- c("ID", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Attention", "11", "12", "13", "14", "15", "16", "17", "18","19", "20", "D1", "D2", "D3", "D4", "D5", "D6", "Gender", "Age", "Height", "Weight")
# 删除作答时间过长的被试

# 删除注意检查未果的被试

# 删除17以下或30岁以上的被试

# 删除体重保密的被试

# 正向计分

# 反向计分

# 
# 以表格形式查看数据
View(data)
# 数据的摘要信息
summary(data)