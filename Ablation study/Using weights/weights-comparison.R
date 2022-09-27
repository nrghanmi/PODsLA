#library(rpart)
#library(dplyr)
#library(caret) 
library(ggplot2)
library(reshape2)

rm(list=ls())


setwd("/Users/Nusaibah-Mac/Desktop/PhD dissertation/Number of PODs/paper of number and location of PODs using PW-AP/GitHub/Results/Excel/")

pop_cov_PW_AP <- readxl::read_excel(
  path = "pop_cov_PODsLA_ONE.xlsx",
)

pop_cov_PW_AP_Max <- readxl::read_excel(
  path = "pop_cov_PW-AP_Max.xlsx",
)

pop_cov_PW_AP_Avg <- readxl::read_excel(
  path = "pop_cov_PW-AP_Avg.xlsx",
)



x  <- seq(1, 10, 1)

y1 <- pop_cov_PW_AP 
y2 <- pop_cov_PW_AP_Max
y3 <- pop_cov_PW_AP_Avg

df <- data.frame(x, y1, y2,y3) #,y5)
colnames(df) <- c("x","PW-AP","PW-AP-Max ", "PW-AP-Avg") 

df2 <- melt(data = df, id.vars = "x")

png("/Users/Nusaibah-Mac/Desktop/PhD dissertation/Number of PODs/paper of number and location of PODs using PW-AP/GitHub/Results/Figures/PODsLA-POP-ONE-Weights.png", units="in", width=10, height=10, res=150)

g<-ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line() +  theme(text = element_text(size = 30))  
g <- g + ylab("% of population with access ") + xlab("Miles")
g$labels$colour ="Methods"
g

dev.off()

