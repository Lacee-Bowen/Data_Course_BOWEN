library(tidyverse)


dframe <-  read.csv("../../Data/landdata-states.csv")
dim(dframe)
summary(dframe)
hist(dframe$Land.Value)

## Q5
## these do the same thing
dframe$Home.Value
dframe[, 4]
## puts the data in one column
dframe[, 4, drop=FALSE]
dframe[, ("Home.Value"), drop=FALSE]

## Q7
## need to include as.factor
dev.off()
plot(x=dframe$region,y=dframe$Land.Value)
plot(x=dframe$Year,y=dframe$Land.Value, col=as.factor(dframe$region))
plot(x=as.factor(dframe$region),y=dframe$Land.Value)


?read.csv
## rest of the assignment
getwd()
df <- read.table("../../Data/ITS_mapping.csv", header= TRUE, sep="\t")
png(filename = "./silly_boxplot.png")
plot(x=(as.factor(df$Ecosystem)), y=df$Lat, xlab= "Ecosystem", ylab= "Lat", cex.lab=1, cex.axis-0.5, cex.main=1, cex.sub=1)
dev.off()

?as.factor








