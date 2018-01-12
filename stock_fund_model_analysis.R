#### Author Comment Part
# modified on 2016-12-20

#### File Descriptiong Part
# 代码目的：用于比较私募排排网提供的基金收益率信息

#### Library Quoting Part
rm(list = ls())

library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)

#### Source files inputting Part
setwd("D:\\MyR\\jijin_modelAnalysis")

source("input_data_for_model_analysis.R")


#### Function Definition Part




######Execution Part

numeric_Specied_Year <- 2017
numeric_Specied_Month <- 2:13  ## change here every time!

ls_value <- InputData("simujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

for(i in 1:length(numeric_Specied_Month)){

        month.index <- GetCSVMonthName(numeric_Specied_Year, numeric_Specied_Month[i])

        if (i == 1){

                result2017 <- ls_value[[month.index]]
        }else{
                result2017 <- cbind(result2017, ls_value[[month.index]][,2])
        }


}

matrix.result.2017 <- t(result2017)

fund.names.2017 <- matrix.result.2017[1,]

matrix.result.2017 <- matrix.result.2017[-1,]

matrix.result.2017 <- array(as.numeric(matrix.result.2017), dim = dim(matrix.result.2017))

colnames(matrix.result.2017) <- fund.names.2017

matrix.result.2017 <- (matrix.result.2017 / 100) + 1

matrix.result.2017 <- rbind(rep(1,dim(matrix.result.2017)[2]),
                            matrix.result.2017)

matrix.result.2017 <- ts(matrix.result.2017, start = c(2016, 12), frequency = 12)

matrix.result.2017 <- diff(log(matrix.result.2017))

# 
matrix.result.2017[,colnames(matrix.result.2017) == "景林稳健"]
# # 
matrix.result.2017[,colnames(matrix.result.2017) == "赤子之心价值"]



# write.csv(result2017, "result2017.csv")

##############################

numeric_Specied_Year <- 2016
numeric_Specied_Month <- 2:13  ## change here every time!

ls_value <- InputData("simujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

for(i in 1:length(numeric_Specied_Month)){

        month.index <- GetCSVMonthName(numeric_Specied_Year, numeric_Specied_Month[i])

        if (i == 1){

                result2016 <- ls_value[[month.index]]
        }else{
                result2016 <- cbind(result2016, ls_value[[month.index]][,2])
        }


}

matrix.result.2016 <- t(result2016)

fund.names.2016 <- matrix.result.2016[1,]

matrix.result.2016 <- matrix.result.2016[-1,]

matrix.result.2016 <- array(as.numeric(matrix.result.2016), dim = dim(matrix.result.2016))

colnames(matrix.result.2016) <- fund.names.2016

matrix.result.2016 <- (matrix.result.2016 / 100) + 1

matrix.result.2016 <- rbind(rep(1,dim(matrix.result.2016)[2]),
                            matrix.result.2016)

matrix.result.2016 <- ts(matrix.result.2016, start = c(2015, 12), frequency = 12)

matrix.result.2016 <- diff(log(matrix.result.2016))

# 
matrix.result.2016[,colnames(matrix.result.2016) == "景林稳健"]
# # 
matrix.result.2016[,colnames(matrix.result.2016) == "赤子之心价值"]
# 
# # 
# # write.csv(result2016, "result2016.csv")
# 


##############################

numeric_Specied_Year <- 2015
numeric_Specied_Month <- 10:13  ## change here every time!

ls_value <- InputData("simujijin",
                      numeric_Specied_Year,
                      numeric_Specied_Month)

for(i in 1:length(numeric_Specied_Month)){

        month.index <- GetCSVMonthName(numeric_Specied_Year, numeric_Specied_Month[i])

        if (i == 1){

                result2015 <- ls_value[[month.index]]
        }else{
                result2015 <- cbind(result2015, ls_value[[month.index]][,2])
        }


}

matrix.result.2015 <- t(result2015)

fund.names.2015 <- matrix.result.2015[1,]

matrix.result.2015 <- matrix.result.2015[-1,]

matrix.result.2015 <- array(as.numeric(matrix.result.2015), dim = dim(matrix.result.2015))

colnames(matrix.result.2015) <- fund.names.2015

matrix.result.2015 <- (matrix.result.2015 / 100) + 1


#### different from 2016&2017 !!!!!!

# matrix.result.2015 <- rbind(rep(1,dim(matrix.result.2015)[2]), 
#                             matrix.result.2015)

matrix.result.2015 <- ts(matrix.result.2015, start = c(2015, 9), frequency = 12)

matrix.result.2015 <- diff(log(matrix.result.2015))


matrix.result.2015[,colnames(matrix.result.2015) == "景林稳健"]
# # # 
matrix.result.2015[,colnames(matrix.result.2015) == "赤子之心价值"]

# 
# write.csv(result2015, "result2015.csv")



##################################################################
fund.name <- intersect(intersect(fund.names.2015,fund.names.2016),fund.names.2017)

matrix.result.2015 <- matrix.result.2015[,colnames(matrix.result.2015) %in% fund.name]

matrix.result.2016 <- matrix.result.2016[,colnames(matrix.result.2016) %in% fund.name]

matrix.result.2017 <- matrix.result.2017[,colnames(matrix.result.2017) %in% fund.name]

matrix.result <- rbind(matrix.result.2015, matrix.result.2016, matrix.result.2017)

matrix.result <- ts(matrix.result, start = c(2015, 10), frequency = 12)

matrix.result[,colnames(matrix.result) == "景林稳健"]
# # # 
matrix.result[,colnames(matrix.result) == "赤子之心价值"]

write.csv(matrix.result, "result.csv")
