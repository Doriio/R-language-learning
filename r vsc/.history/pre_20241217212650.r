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
colnames(data) <- c("ID", "Total", "1")
