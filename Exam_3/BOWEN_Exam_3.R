# loading as many libraries as I can think of...
library(readr)
library(tidyverse)
library(modelr)
library(GGally)
library(lindia)
library(skimr)
library(patchwork)
library(caret)
library(ggplot2)
library(dplyr)
library(MASS)
library(broom)


# below are the two csv files to read in for the exam
FacultySalaries_1995 <- read_csv("FacultySalaries_1995.csv")
Juniper_Oils <- read_csv("Juniper_Oils.csv")


##Task I, recreating fig1.png
# renaming the text for the x axis
FacultySalaries_1995 <- FacultySalaries_1995 %>% 
  rename(Full = AvgFullProfSalary, Assoc = AvgAssocProfSalary, Assist = AvgAssistProfSalary)

# creating one column which all of the following are in 
combine_columns <- c("Full","Assoc","Assist")
FacultySalaries_1995_long <- pivot_longer(FacultySalaries_1995, cols = combine_columns,
                                          names_to = "Rank", 
                        values_to = "Salary", names_prefix = "Avg")

#getting rid of VIIB
without_VIIB <- FacultySalaries_1995_long[-c(3463:3465), ]

# the facet wrapped graph
p1 <- ggplot(without_VIIB, aes(x=Rank, y=Salary, fill = Rank))+
  geom_boxplot()+
  labs(x = "Rank")+
  facet_wrap(~Tier)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1
ggsave(filename = "BOWEN_Fig_1.jpg", p1)



## Task II Anova test
# below are three models testing the influence of state, tier, and rank on salary

Anova <- aov(Salary ~ State + Tier + Rank, data = FacultySalaries_1995_long)
Anova_summary <- summary(Anova)
Anova_summary

capture.output(Anova_summary, file = "./Salary_ANOVA_Summary.txt")


## Task III cleaning Juniper Oils data using pivot_longer
combine_columns2 <- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")
Juniper_Oils_long <- pivot_longer(Juniper_Oils, combine_columns2, names_to = "Chemicals", values_to = "Concentration")


## Task IV duplicating fig2.png
p2 <- ggplot(Juniper_Oils_long, aes(x= YearsSinceBurn, y= Concentration))+
  geom_smooth()+
  facet_wrap(~Chemicals, scales = "free")+
  theme_minimal()
p2
ggsave(filename = "BOWEN_Fig_2.jpg", p2)



## Task V 
# below is a model for which chemicals show concentrations that are significantly (significant, as in P < 0.05) affected by "Years Since Burn"
mod4 <- glm(data = Juniper_Oils_long, formula = Concentration ~ YearsSinceBurn + Chemicals)
summary(mod4)
# below is a new data frame from mod4
tidy(mod4)

# get variables with p value less than 0.05
mod4_significant <- tidy(mod4)
mod4_significant
filter(mod4_significant, p.value < 0.05)

# this is the data frame only showing chemicals with significant p values
sig_pvalue <- filter(mod4_significant, p.value < 0.05)











