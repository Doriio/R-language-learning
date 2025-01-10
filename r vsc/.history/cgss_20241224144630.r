install.packages("haven")  # 安装haven包
library(haven)             # 加载haven包


# 读取.sav文件
data <- read_sav("your_data_file.sav")

# 查看数据的前几行
head(data)