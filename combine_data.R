#### Author Comment Part
# modified on 2016-12-20

#### File Descriptiong Part
# 代码目的：用于比较私募排排网提供的基金收益率信息

#### Library Quoting Part
rm(list = ls())

library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)

#### Source files inputting Part

source("D:\\MyR\\jijin\\input_and_preprocess_data.R")



#### Function Definition Part

CombineMonthDataInOneYear <- function(arg.year = 2016, arg.month = 2) {
        
        # 2.read csv files to get data
        #
        # Args:
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回ls_value列表中的保存指定时间的csv数据的数据框元素的名称
        

        ls_value <- InputData("simujijin",
                              arg.year,
                              arg.month)
        
        for(i in 1:length(arg.month)){
                
                month.index <- GetCSVMonthName(arg.year, arg.month[i])
                
                if (i == 1){
                        
                        result <- ls_value[[month.index]]
                }else{
                        result <- cbind(result, ls_value[[month.index]][,2])
                }
                
                
        }
        
        matrix.result <- t(result)
        
        fund.names <- matrix.result[1,]
        
        matrix.result <- matrix.result[-1,]
        
        matrix.result <- array(as.numeric(matrix.result), dim = dim(matrix.result))
        
        colnames(matrix.result) <- fund.names
        
        matrix.result <- (matrix.result / 100) + 1
        
        if(arg.month[1] == 2){

                matrix.result <- rbind(rep(1,dim(matrix.result)[2]),
                                       matrix.result)                
        }

        
        # matrix.result <- ts(matrix.result, start = c(2016, 12), frequency = 12)
        
        matrix.result <- diff(log(matrix.result))
        
        return(matrix.result)
}




######Execution Part
setwd("D:\\MyR\\jijin_modelAnalysis")

# numeric_Specied_Year <- 2017
# numeric_Specied_Month <- 2:13  ## change here every time!

matrix.result.2017 <- CombineMonthDataInOneYear(2017, 2:13)

matrix.result.2017 <- ts(matrix.result.2017, start = c(2017, 1), frequency = 12)

# ls_value <- InputData("simujijin",
#                       numeric_Specied_Year,
#                       numeric_Specied_Month)
# 
# for(i in 1:length(numeric_Specied_Month)){
# 
#         month.index <- GetCSVMonthName(numeric_Specied_Year, numeric_Specied_Month[i])
# 
#         if (i == 1){
# 
#                 result2017 <- ls_value[[month.index]]
#         }else{
#                 result2017 <- cbind(result2017, ls_value[[month.index]][,2])
#         }
# 
# 
# }
# 
# matrix.result.2017 <- t(result2017)
# 
# fund.names.2017 <- matrix.result.2017[1,]
# 
# matrix.result.2017 <- matrix.result.2017[-1,]
# 
# matrix.result.2017 <- array(as.numeric(matrix.result.2017), dim = dim(matrix.result.2017))
# 
# colnames(matrix.result.2017) <- fund.names.2017
# 
# matrix.result.2017 <- (matrix.result.2017 / 100) + 1
# 
# matrix.result.2017 <- rbind(rep(1,dim(matrix.result.2017)[2]),
#                             matrix.result.2017)
# 
# matrix.result.2017 <- ts(matrix.result.2017, start = c(2016, 12), frequency = 12)
# 
# matrix.result.2017 <- diff(log(matrix.result.2017))

# 
matrix.result.2017[,colnames(matrix.result.2017) == "景林稳健"]
# 
# # matrix.result.2017[,colnames(matrix.result.2017) == "景林丰收"]
# 
# matrix.result.2017[,colnames(matrix.result.2017) == "赤子之心价值"]



# write.csv(result2017, "result2017.csv")

##############################

# numeric_Specied_Year <- 2016
# numeric_Specied_Month <- 2:13  ## change here every time!

matrix.result.2016 <- CombineMonthDataInOneYear(2016, 2:13)

matrix.result.2016 <- ts(matrix.result.2016, start = c(2016, 1), frequency = 12)

# 
# ls_value <- InputData("simujijin",
#                       numeric_Specied_Year,
#                       numeric_Specied_Month)
# 
# for(i in 1:length(numeric_Specied_Month)){
# 
#         month.index <- GetCSVMonthName(numeric_Specied_Year, numeric_Specied_Month[i])
# 
#         if (i == 1){
# 
#                 result2016 <- ls_value[[month.index]]
#         }else{
#                 result2016 <- cbind(result2016, ls_value[[month.index]][,2])
#         }
# 
# 
# }
# 
# matrix.result.2016 <- t(result2016)
# 
# fund.names.2016 <- matrix.result.2016[1,]
# 
# matrix.result.2016 <- matrix.result.2016[-1,]
# 
# matrix.result.2016 <- array(as.numeric(matrix.result.2016), dim = dim(matrix.result.2016))
# 
# colnames(matrix.result.2016) <- fund.names.2016
# 
# matrix.result.2016 <- (matrix.result.2016 / 100) + 1
# 
# matrix.result.2016 <- rbind(rep(1,dim(matrix.result.2016)[2]),
#                             matrix.result.2016)
# 
# matrix.result.2016 <- ts(matrix.result.2016, start = c(2015, 12), frequency = 12)
# 
# matrix.result.2016 <- diff(log(matrix.result.2016))

# 
# matrix.result.2016[,colnames(matrix.result.2016) == "景林稳健"]

# matrix.result.2016[,colnames(matrix.result.2016) == "赤子之心价值"]
# 
# # 
# # write.csv(result2016, "result2016.csv")
# 


##############################

# numeric_Specied_Year <- 2015
# numeric_Specied_Month <- 10:13  ## change here every time!

matrix.result.2015 <- CombineMonthDataInOneYear(2015, 10:13)

matrix.result.2015 <- ts(matrix.result.2015, start = c(2015, 10), frequency = 12)

 
# ls_value <- InputData("simujijin",
#                       numeric_Specied_Year,
#                       numeric_Specied_Month)
# 
# for(i in 1:length(numeric_Specied_Month)){
# 
#         month.index <- GetCSVMonthName(numeric_Specied_Year, numeric_Specied_Month[i])
# 
#         if (i == 1){
# 
#                 result2015 <- ls_value[[month.index]]
#         }else{
#                 result2015 <- cbind(result2015, ls_value[[month.index]][,2])
#         }
# 
# 
# }
# 
# matrix.result.2015 <- t(result2015)
# 
# fund.names.2015 <- matrix.result.2015[1,]
# 
# matrix.result.2015 <- matrix.result.2015[-1,]
# 
# matrix.result.2015 <- array(as.numeric(matrix.result.2015), dim = dim(matrix.result.2015))
# 
# colnames(matrix.result.2015) <- fund.names.2015
# 
# matrix.result.2015 <- (matrix.result.2015 / 100) + 1
# 
# 
# #### different from 2016&2017 !!!!!!
# 
# # matrix.result.2015 <- rbind(rep(1,dim(matrix.result.2015)[2]), 
# #                             matrix.result.2015)
# 
# matrix.result.2015 <- ts(matrix.result.2015, start = c(2015, 9), frequency = 12)
# 
# matrix.result.2015 <- diff(log(matrix.result.2015))
# 
# 
matrix.result.2015[,colnames(matrix.result.2015) == "景林稳健"]
# # # # # 
matrix.result.2015[,colnames(matrix.result.2015) == "赤子之心价值"]

# 
# write.csv(result2015, "result2015.csv")



##################################################################
fund.name <- intersect(intersect(colnames(matrix.result.2015),
                                 colnames(matrix.result.2016)),
                       colnames(matrix.result.2017))
# 
# matrix.result.2015 <- matrix.result.2015[,colnames(matrix.result.2015) %in% fund.name]
# 
# matrix.result.2016 <- matrix.result.2016[,colnames(matrix.result.2016) %in% fund.name]
# 
# matrix.result.2017 <- matrix.result.2017[,colnames(matrix.result.2017) %in% fund.name]
# 
# matrix.result <- rbind(matrix.result.2015, matrix.result.2016, matrix.result.2017)
# 
# # matrix.result <- ts(matrix.result, start = c(2015, 10), frequency = 12)
# 
# start.month <- as.Date("2015-10-1")
# end.month <- as.Date("2017-12-1")
# 
# month.label <- as.character(seq(start.month, end.month, "month"))
# row.names(matrix.result) <- substr(month.label,1,7)
# 
# # matrix.result[,colnames(matrix.result) == "景林稳健"]
# 
# # matrix.result[,colnames(matrix.result) == "景林丰收"]
# 
# matrix.result[,colnames(matrix.result) == "赤子之心价值"]
# 
# # write.csv(matrix.result, "result.csv")
