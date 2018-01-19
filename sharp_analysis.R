#### Author Comment Part
# modified on 2018-1-19

#### File Descriptiong Part
# 代码目的：基于已经生成的基金对数收益率信息，计算夏普指数以及总收益

#### Library Quoting Part
rm(list = ls())

library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)
library(sca)

#### Source files inputting Part



#### Function Definition Part




######Execution Part
setwd("D:\\MyR\\jijin_modelAnalysis")

fund.return <- read.csv("result.csv", stringsAsFactors = FALSE)
row.names(fund.return) <- fund.return[,1]
fund.return <- fund.return[,-1]

huobililv <- read.csv("huobililv.csv", stringsAsFactors = FALSE)
month.label <- huobililv[,1]
huobililv <- huobililv[,2] 
names(huobililv) <- month.label

sharp.result <- apply(fund.return, 2, function(x){mean(x - log(1 + huobililv) / 12) / sd(x)})

sum.result <- apply(fund.return, 2, sum)
sum.result <- (exp(sum.result) - 1)

df.sharp.result <- data.frame(sharp = round(sharp.result,digits = 2),
                              sum = percent(sum.result,d = 2, sep = ""))


df.sharp.result <- df.sharp.result[order(df.sharp.result$sharp, decreasing = TRUE),]

colnames(df.sharp.result) <- c("夏普比例\n(2015-10 ~ 2017-12)", "总收益率\n(2015-10 ~ 2017-12)")

# write.csv(df.sharp.result, "df.sharp.result.csv")
