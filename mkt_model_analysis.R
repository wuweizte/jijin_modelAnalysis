#### Author Comment Part
# modified on 2018-1-19

#### File Descriptiong Part
# 代码目的：用于比较私募排排网提供的基金收益率信息

#### Library Quoting Part
rm(list = ls())

library(RColorBrewer)
library(dplyr, warn.conflicts = FALSE)
library(sca)
library(portes)
library(forecast)

#### Source files inputting Part


#### Function Definition Part




######Execution Part
setwd("D:\\MyR\\jijin_modelAnalysis")


fund.return <- read.csv("result.csv", stringsAsFactors = FALSE)
# row.names(fund.return) <- fund.return[,1]
fund.return <- ts(fund.return[,-1], start = c(2015,10), frequency = 12)

window(fund.return[,colnames(fund.return) == "赤子之心价值"], start = c(2017,1) )

huobililv <- read.csv("huobililv.csv", stringsAsFactors = FALSE)
month.label <- huobililv[,1]
huobililv <- huobililv[,2] 
names(huobililv) <- month.label

mkt.index <- read.csv("mkt_index.csv", stringsAsFactors = FALSE)
mkt.index <- ts(mkt.index[,-1], start = c(2015,9), frequency = 12)

mkt.return <- diff(log(mkt.index))
row.names(mkt.return) <- row.names(fund.return)

net.fund.return <- fund.return - log(1 + huobililv) / 12
net.mkt.return <- mkt.return - log(1 + huobililv) / 12

net.mkt.return <- cbind(rep(1,nrow(net.mkt.return)), net.mkt.return)

############################# statility test

ndiffs(net.mkt.return[, 2])
ndiffs(net.mkt.return[, 3])

# apply(net.fund.return, 2, ndiffs)

############################# serial test

BoxPierce(net.mkt.return[, -1], lags = seq(5,27,5))

# BoxPierce(net.fund.return, lags = seq(5,27,5))



############################ pure market model
solve.result <- qr.solve(net.mkt.return, net.fund.return)
alpha <- solve.result[1,]
beta.hushen300 <- solve.result[2,]
beta.zz500 <- solve.result[3,]

# names(alpha) <- colnames(net.fund.return)

df.alpha.result <- data.frame(alpha = alpha,
                              beta.hushen300 = beta.hushen300,
                              beta.zz500 = beta.zz500)

row.names(df.alpha.result) <- colnames(net.fund.return)


df.alpha.result <- df.alpha.result[order(df.alpha.result$alpha, decreasing = TRUE),]
df.alpha.result[row.names(df.alpha.result) == "赤子之心价值",]


############################ pure market model in the last 12 months
solve.result.last <- qr.solve(tail(net.mkt.return,12), tail(net.fund.return, 12))
alpha.last <- solve.result.last[1,]
beta.hushen300.last <- solve.result.last[2,]
beta.zz500.last <- solve.result.last[3,]

df.alpha.result.last <- data.frame(alpha = alpha.last,
                              beta.hushen300 = beta.hushen300.last,
                              beta.zz500 = beta.zz500.last)

row.names(df.alpha.result.last) <- colnames(net.fund.return)


df.alpha.result.last <- df.alpha.result.last[order(df.alpha.result.last$alpha, decreasing = TRUE),]
df.alpha.result.last[row.names(df.alpha.result.last) == "赤子之心价值",]


############################ TM model in the last 12 months
net.mkt.return.TM <- cbind(net.mkt.return, net.mkt.return[,2]^2, net.mkt.return[,3]^2)

solve.result.TM <- qr.solve(tail(net.mkt.return.TM,12), tail(net.fund.return, 12))
alpha.TM <- solve.result.TM[1,]
beta.hushen300.TM <- solve.result.TM[2,]
beta.zz500.TM <- solve.result.TM[3,]

df.alpha.result.TM <- data.frame(alpha = alpha.TM,
                                   beta.hushen300 = beta.hushen300.TM,
                                   beta.zz500 = beta.zz500.TM)

row.names(df.alpha.result.TM) <- colnames(net.fund.return)


df.alpha.result.TM <- df.alpha.result.TM[order(df.alpha.result.TM$alpha, decreasing = TRUE),]
df.alpha.result.TM[row.names(df.alpha.result.TM) == "赤子之心价值",]


