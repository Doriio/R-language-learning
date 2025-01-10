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
# 将数据框保存为 CSV 文件 write.csv(data, "data_sample.csv")

# 然后用 Excel 或其他表格工具打开

#####DATA SCREENING#####
# 定义一个函数来进行数字到文字的转换
convert_to_text <- function(x) {
  if (x == 1) {
    return("完全不同意")
  } else if (x == 2) {
    return("比较不同意")
  } else if (x == 3) {
    return("中立")
  } else if (x == 4) {
    return("同意")
  } else if (x == 5) {
    return("完全同意")
  } else if (x == 8) {
    return("无法选择")
  } else if (x == -3) {
    return("拒绝回答")
  } else if (x == -2) {
    return("不知道")
  } else if (x == -1) {
    return("不适用")
  } else {
    return(NA)  # 其他值处理为缺失值
  }
}

# 使用 dplyr 的 mutate() 函数应用转换
data <- data %>%
  mutate(VariableName_text = sapply(VariableName, convert_to_text))

# 查看结果
head(data)