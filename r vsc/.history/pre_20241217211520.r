#####PACKAGES#####
install.packages("readxl")
library(readxl)




#####IMPORTING DATASETS#####
setwd("~/Desktop")
#importing dataset
T0 <- read.csv("/Users/evageiger/Desktop/T0_HD_osf.csv")
data <- read_excel("your_file.xlsx", sheet = 1)
