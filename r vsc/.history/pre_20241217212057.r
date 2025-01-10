#####PACKAGES#####
install.packages("readxl")
library(readxl)




#####IMPORTING DATASETS#####
# wd "/Users/a/R-language-learning/r vsc"
#importing dataset
data <- read_excel("/Users/a/Desktop/NJU/1 课程/高级测量/measure/ATOP.xlsx", sheet = 1)

#####DATA SCREENING#####
# delete