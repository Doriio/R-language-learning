#####PACKAGES#####
install.packages("readxl")
library(readxl)
library(dplyr)



#####IMPORTING DATASETS#####
# wd "/Users/a/R-language-learning/r vsc"
#importing dataset
data <- readxl::read_excel("/Users/a/Desktop/NJU/1 课程/高级测量/measure/ATOP.xlsx", sheet = 1)

#####DATA SCREENING#####
# 删除指定列（2:6）查看结果 head(data)
data <- dplyr::select(data, -c(2:6))
# 对列重命名
colnames(data) <- c("ID", "Total", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "attention", "11", "12", "13", "14", "15", "16", "17", "18",
"19", "20", "D1", "D2", "D3", "D4", "D5")
