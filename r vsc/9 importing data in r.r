# flat file 逗号分隔
pools <- read.csv("swimming_pools.csv")
str(pools)
read.table("swimming_pools.csv", header = TRUE, sep = ",")

# in folder or home directory
# 确保路径在不同操作系统上的兼容性
path1 <- file.path("~", "Desktop", "毕业论文数据&文章-熊紫珂2023", "研究数据-熊紫珂20230404", "研究1数据", "IER-S1-sum.xlsx")
path2 <- file.path("/Users/a/Desktop/NJU/1 课程/高级测量/作业一/yangp59.csv")
path1 # 转化为绝对路径
path2
path1_full <- normalizePath(path1)
path1_full
read.csv(path2, header = TRUE)

# read.delim 制表符分隔
read.delim("states.txt")
read.table("states.txt", header = TRUE, sep = "\t")

# read.table 斜杠分隔 表格形式的文件
read.table("states2.txt", # 路径
           header = TRUE, # 文件的第一行包含变量名称
           sep = "/",
           sep = "\t",
           col.names = c("type", "calories", "sodium"),
           colClasses = c("character", "numeric", "logical", "NULL")) # 数据的分隔方式

# readr packages
install.packages("readr")
library(readr)

read_csv("states.csv") # read.csv
read_tsv("states.txt") # read.delim dataframe
read_delim("states.txt", delim = "/", # = read.table
           col_names = FALSE, # 自动生成列名
           col_types = "ccdd", # character and double
           col_types = list(fac, int, int),
           skip = 2, n_max = 3) # 从第三行开始读数据，最多读三行

# common practice
properties <- c("area", "temp", "size", "storage", "method",
                "texture", "flavor", "moistness")
potatoes <- read_tsv("potatoes.txt", col_names = properties)
head(potatoes)

# data.table
install.packages("data.table")
library(data.table)

fread("states.csv", select = c(6, 8))

# import excel data
install.packages("readxl")
library(readxl)

dir()
excel_sheets(path2) # 有几张工作表
read_excel(path2, sheet = 1, # 工作表的具体内容
           col_names = TRUE, # false 自动命名
           col_types = NULL) # 不告诉r数据类型

read_excel(path2, sheet = 1, # 工作表的具体内容
           col_names = TRUE, # false 自动命名
           col_types = NULL, # 不告诉r数据类型
           col_types = c("text", "blank"), # ignore
           skip = 0)

pop_list <- lapply(excel_sheets("urbanpop.xlsx"), # 返回 urbanpop.xlsx 文件中所有工作表的名称
                   read_excel, # 每个工作表需要进行这个function
                   path = "urbanpop.xlsx")
urbanpop_sel[1, ]

# reading sheets
install.packages("XLConnect")
library(XLConnect)

book <- loadWorkbook("cities.xlsx")
str(book)
getSheets(book) # = excel_sheets("cities.xlsx")
readWorksheet(book, sheet = "year_2000",
              startRow = 3,
              endRow = 4,
              startCol = 2,
              header = FALSE) # 变量名不指定

# practice
my_book <- loadWorkbook("urbanpop.xlsx")
urbanpop_sel <- readWorksheet(my_book, sheet = 2, startCol = 3, endCol = 5)
countries <- readWorksheet(my_book, sheet = 2, startCol = 1, endCol = 1)
selection <- cbind(countries, urbanpop_sel)

# adapting sheets
createSheet(book, name = "year_2010")
writeWorksheet(book, pop_2010, sheet = "year_2010")
saveWorkbook(book, file = "cities2.xlsx")

renameSheet(book, "year_1990", "Y1990") # old name - new name
saveWorkbook(book, file = "cities3.xlsx")

removeSheet(book, sheet = "Y2010")
saveWorkbook(book, file = "cities4.xlsx")