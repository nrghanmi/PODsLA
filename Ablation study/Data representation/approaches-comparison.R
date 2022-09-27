library(ggplot2)
library(reshape2)

rm(list=ls())

setwd("/Results/Excel/")

#-----please select the required state----

pop_cov_SPODsLA<- readxl::read_excel(
  path = "pop_cov_S-PODsLA_ONE.xlsx",
 # path = "pop_cov_S-PODsLA_TWO.xlsx",
 # path = "pop_cov_S-PODsLA_THREE.xlsx",
)

pop_cov_PODsLA <- readxl::read_excel(
 path = "pop_cov_PODsLA_ONE.xlsx",
 # path = "pop_cov_PODsLA_TWO.xlsx",
 # path = "pop_cov_PODsLA_THREE.xlsx",
  
)


x  <- seq(1, 10, 1)
y1 <- pop_cov_PODsLA 
y2 <- pop_cov_SPODsLA

df <- data.frame(x, y1, y2)
colnames(df) <- c("x","PODsLA","S-PODsLA") 

df2 <- melt(data = df, id.vars = "x")

setwd("/Results/Figures/")

#-----please select the required state----
png("S-PODsLA-POP-ONE.png", units="in", width=10, height=10, res=150)
#png("S-PODsLA-POP-TWO.png", units="in", width=10, height=10, res=150)
#png("S-PODsLA-POP-THREE.png", units="in", width=10, height=10, res=150)


g<-ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line() +  theme(text = element_text(size = 30))  
g <- g + ylab("% of population with access ") + xlab("Miles")
g$labels$colour ="Methods"
g

dev.off()

