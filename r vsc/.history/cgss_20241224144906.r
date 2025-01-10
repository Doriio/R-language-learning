#####PACKAGES#####
install.packages("haven")  # 安装haven包
library(haven)             # 加载haven包


#####IMPORTING DATASETS#####
# 读取.sav文件
data <- read_sav("/Users/a/Desktop/NJU/1 课程/高级测量/assignment23/CGSS2010.sav")
# 查看数据的前几行
View(data)
# 查看数据的列名
names(data)