#####PACKAGES#####
install.packages("readxl")
library(readxl)




#####IMPORTING DATASETS#####
setwd("~/Desktop")
#importing dataset
T0 <- read.csv("/Users/a/Desktop/测量/问卷.xlsx")
data <- read_excel("your_file.xlsx", sheet = 1)
