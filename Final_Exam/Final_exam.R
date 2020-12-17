library(tidyverse)
library(ggplot2)

# Load the landdata-states.csv file into R
df <- read.csv("../Exam_2/landdata-states.csv")
names(df)

## Q1 of of the exam. 
# Re-create the graph shown in "fig1.png"
Region <- df$region
options(scipen = 999)
ggplot(df, aes(x=Year, y=Land.Value, color=Region))+
  geom_smooth()+
  labs(y="Land Value (USD)")
# Exporting the graph
ggsave("BOWEN_Fig_1.jpg")



## Q2 write code to show which states are in the NA region
# the "NA region" is for places that are not assigned a region
df_NA <- df[is.na(df$region),]



## Q3 make data set "unicef-u5mr.csv" tidy
#The rest of the test uses another data set. Load and tidy it up
unicef_df <- read.csv("../Exam_2/unicef-u5mr.csv")
unicef_df2 <- pivot_longer(unicef_df, cols = starts_with("U5MR."),
                           names_to = "Year", values_to = "Mortality_Rate",
                           names_prefix = "U5MR.")
## above combined U5MR. groups and created a new column "year"



## Q4 Re-create the graph shown in fig2.png
# below I changed "year" to numeric
# save the new graph using ggsave
unicef_df2$Year <- as.numeric(unicef_df2$Year)
ggplot(unicef_df2, aes(x=Year, y=Mortality_Rate, color=Continent))+
  theme_minimal()+ 
  geom_point(size=2)+
  labs(y="MortalityRate")
ggsave("BOWEN_Fig_2.jpg")



## Q4 (part 2) Re-create the graph shown in fig3.png
# below filters out values that are "NA"
unicef_df2_withoutNA <- unicef_df2 %>% 
  filter(`Mortality_Rate` != "NA")

# saving the mean mortality rate to a new df is shown below
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
  labs(y="Mortality Rate")+
  theme_bw()+
  theme(panel.border = element_blank())
# I still am struggling with finding how to turn the gray-filled boxes to white...
ggsave("BOWEN_Fig_4.jpg")

