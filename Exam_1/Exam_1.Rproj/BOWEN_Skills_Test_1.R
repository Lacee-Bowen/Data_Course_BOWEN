library(tidyverse)

# Q1
read.csv("./DNA_Conc_by_Extraction_Date.csv")
Dframe <- read.csv("./DNA_Conc_by_Extraction_Date.csv")
summary(Dframe)
hist(Dframe$DNA_Concentration_Katy, xlab= "DNA Conc", main = "Katy Histogram")
hist(Dframe$DNA_Concentration_Ben, xlab= "DNA Conc", main = "Ben Histogram")

# Q2 and Q3
jpeg(filename = "./BOWEN_Plot1.jpeg")
plot(x=(as.factor(Dframe$Year_Collected)), y = Dframe$DNA_Concentration_Katy, xlab = "YEAR", ylab= "DNA Concentration", main= "Katy's Extraction")
dev.off()
jpeg(filename = "./BOWEN_Plot2.jpeg")
plot(x=(as.factor(Dframe$Year_Collected)), y = Dframe$DNA_Concentration_Ben, xlab = "YEAR", ylab="DNA Concentration", main= "Ben's Extraction")
dev.off()

# Q4
new_vector <- Dframe$DNA_Concentration_Ben / Dframe$DNA_Concentration_Katy
Dframe$DNA_Concentration_Ben / Dframe$DNA_Concentration_Katy
minimum_dif <- min(new_vector)
index_dif <- which(minimumdif == new_vector)
lowest_performance <- Dframe$Year_Collected[index_dif]

# Q5
subset_Downstairs <- filter(Dframe,Lab == "Downstairs")
plot(x = as.POSIXct(subset_Downstairs$Date_Collected), y = subset_Downstairs$DNA_Concentration_Ben, xlab= "Date_Collected", ylab= "DNA_Concentration_Ben")







