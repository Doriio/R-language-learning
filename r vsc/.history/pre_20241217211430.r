#####PACKAGES#####
install.packages("readxl")  # 如果尚未安装
library(readxl)




#####IMPORTING DATASETS#####
setwd("~/Desktop")
#importing T0 dataset
T0 <- read.csv("/Users/evageiger/Desktop/T0_HD_osf.csv")
#importing EMA dataset
EMA = read.csv("/Users/evageiger/Desktop/EMA_HD_osf.csv")