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
# 查看数据的列名
# 查看数据的前几行
View(data)
# 查看数据框的类型
class(data)
# 检查数据中是否有缺失值
sum(is.na(data))