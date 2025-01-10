#####PACKAGES#####
install.packages("readxl")
library(readxl)




#####IMPORTING DATASETS#####
setwd("~/Desktop")
#importing dataset
T0 <- read.csv("/Users/a/Desktop/NJU/1 课程/高级测量/measure/ATOP.xlsx")
data <- read_excel("your_file.xlsx", sheet = 1)
