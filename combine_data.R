#### Author Comment Part
# modified on 2018-1-19

#### File Descriptiong Part
# 代码目的：基于私募排排网提供的基金收益率信息，返回交集基金在指定年中每一月的对数收益率组成的时序数据

#### Library Quoting Part
rm(list = ls())

library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)

#### Source files inputting Part

source("D:\\MyR\\jijin\\input_and_preprocess_data.R")



#### Function Definition Part

CombineMonthDataInOneYear <- function(arg.year = 2016, arg.month = 2) {
        
        # 1.read csv files to get data
        # 2.combine monthly data into one data set
        # 3.compute monthly log return
        
        #
        # Args:
        #   arg.year: 绘画针对的年份，以单向量形式输入
        #   arg.month: 绘画针对的月份范围，以向量形式输入
        #
        # Returns:
        #   返回交集基金在指定年中每一月的对数收益率组成的时序数据

                
        # 1.read csv files to get data
        ls_value <- InputData("simujijin",
                              arg.year,
                              arg.month)

        # 2.combine monthly data into one data set
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
        
        # 3.compute monthly log return
        matrix.result <- (matrix.result / 100) + 1
  
        ### if the data begins from Jan, 1 is set as the start value      
        if(arg.month[1] == 2){

                matrix.result <- rbind(rep(1,dim(matrix.result)[2]),
                                       matrix.result)                
        }


        matrix.result <- diff(log(matrix.result))
        
        return(matrix.result)
}




######Execution Part
setwd("D:\\MyR\\jijin_modelAnalysis")


matrix.result.2017 <- CombineMonthDataInOneYear(2017, 2:13)

matrix.result.2017 <- ts(matrix.result.2017, start = c(2017, 1), frequency = 12)

# 
# matrix.result.2017[,colnames(matrix.result.2017) == "景林稳健"]
# 
# # matrix.result.2017[,colnames(matrix.result.2017) == "景林丰收"]
# 
# matrix.result.2017[,colnames(matrix.result.2017) == "赤子之心价值"]


##############################

matrix.result.2016 <- CombineMonthDataInOneYear(2016, 2:13)

matrix.result.2016 <- ts(matrix.result.2016, start = c(2016, 1), frequency = 12)


# 
# matrix.result.2016[,colnames(matrix.result.2016) == "景林稳健"]

# matrix.result.2016[,colnames(matrix.result.2016) == "赤子之心价值"]
# 


##############################


matrix.result.2015 <- CombineMonthDataInOneYear(2015, 10:13)

matrix.result.2015 <- ts(matrix.result.2015, start = c(2015, 10), frequency = 12)

# 
# 
# matrix.result.2015[,colnames(matrix.result.2015) == "景林稳健"]
# # # # # # 
# matrix.result.2015[,colnames(matrix.result.2015) == "赤子之心价值"]




##################################################################
fund.name <- intersect(intersect(colnames(matrix.result.2015),
                                 colnames(matrix.result.2016)),
                       colnames(matrix.result.2017))
# 
matrix.result.2015 <- matrix.result.2015[,colnames(matrix.result.2015) %in% fund.name]

matrix.result.2016 <- matrix.result.2016[,colnames(matrix.result.2016) %in% fund.name]

matrix.result.2017 <- matrix.result.2017[,colnames(matrix.result.2017) %in% fund.name]

matrix.result <- rbind(matrix.result.2015, matrix.result.2016, matrix.result.2017)

start.month <- as.Date("2015-10-1")
end.month <- as.Date("2017-12-1")

month.label <- as.character(seq(start.month, end.month, "month"))
row.names(matrix.result) <- substr(month.label,1,7)
# 
# # matrix.result[,colnames(matrix.result) == "景林稳健"]
# 
# # matrix.result[,colnames(matrix.result) == "景林丰收"]
# 
# matrix.result[,colnames(matrix.result) == "赤子之心价值"]
# 
# # write.csv(matrix.result, "result.csv")
