library(tidyverse)
library(zoo)
library(ggplot2)

landdata_df <- read.csv("./landdata-states.csv")

## Q1 of of the exam. Copy the exact graph fig1.png and save it
Region <- landdata_df$region
options(scipen = 999)
ggplot(landdata_df, aes(x=Year, y=Land.Value, color=Region))+
  geom_smooth()+
  labs(y="Land Value (USD)")
ggsave("BOWEN_Fig_1.jpg")



## Q2 write code to show which states are in the NA region
# the "NA region" is for places that are not assigned a region
landdata_df_NA <- landdata_df[is.na(landdata_df$region),]



## Q3 make data set "unicef-u5mr.csv" tidy
unicef_df <- read.csv("./unicef-u5mr.csv")
unicef_df2 <- pivot_longer(unicef_df, cols = starts_with("U5MR."),
                           names_to = "Year", values_to = "Mortality_Rate",
                           names_prefix = "U5MR.")
## above combined U5MR. groups and created a new column "year"
            


## Q4 Re-create the graph shown in fig2.png
unicef_df2$Year <- as.numeric(unicef_df2$Year)
ggplot(unicef_df2, aes(x=Year, y=Mortality_Rate, color=Continent))+
  theme_minimal()+ 
  geom_point(size=2)+
  labs(y="MortalityRate")
ggsave("BOWEN_Fig_2.jpg")



## Q4 (part 2) Re-create the graph shown in fig3.png
# saving the mean mortality rate to a new df shown below
unicef_df2_withoutNA_Mean <- unicef_df2_withoutNA %>%
  group_by(Continent, Year) %>%
  summarise(Mean_Mortality_Rate = mean(Mortality_Rate))
# recreating the plot with thick lines
ggplot(unicef_df2_withoutNA_Mean, aes(x=Year, y= Mean_Mortality_Rate, color=Continent))+
  geom_line(aes(group=Continent, color=Continent), size = 2)+
  theme_minimal()+
  labs(y="Mean Mortality Rate (deaths per 1000 live births)")
ggsave("BOWEN_Fig_3.jpg")



## Q5 recreate graph in fig4.png
ggplot(unicef_df2_withoutNA, aes(x= Year, y= Mortality_Rate/1000))+
  geom_point(color = "blue", size = .1)+
  facet_wrap(~Region)+
  theme_minimal()+
  labs(y="Mortality Rate")
ggsave("BOWEN_Fig_4.jpg")













