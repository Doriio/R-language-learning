#####PACKAGES#####
install.packages("readxl")
library(readxl)
library(dplyr)



#####IMPORTING DATASETS#####
# wd "/Users/a/R-language-learning/r vsc"
#importing dataset
data <- read_excel("/Users/a/Desktop/NJU/1 课程/高级测量/measure/ATOP.xlsx", sheet = 1)

#####DATA SCREENING#####
# 删除指定列（删除 "列名1" 和 "列名2"）
# 或者按列的索引删除（如删除第2和第3列）
data <- data %>% select(-c(2, 3))

# 查看结果
head(data)