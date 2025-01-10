#####PACKAGES#####
install.packages("readxl")
library(readxl)




#####IMPORTING DATASETS#####
setwd("~/Desktop")
#importing dataset
T0 <- read.csv("/Users/a/Desktop/测量/问卷.xlsx")
data <- read_excel("/Users/a/Desktop/NJU/1 课程/高级测量/measure/ATOP.xlsx", sheet = 1)
