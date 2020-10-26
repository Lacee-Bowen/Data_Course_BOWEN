library(modelr)
library(broom)
library(tidyverse)
library(fitdistrplus)
library(MASS)

#### Q1 - load the data
mushroom_growth <- read.csv("../../Data/mushroom_growth.csv")




#### Q2 create several plots exploring relationships of data
ggplot(mushroom_growth, aes(x= Light, y=GrowthRate, color = Species))+
  geom_point(size = .5)

ggplot(mushroom_growth, aes(x= Humidity, y=GrowthRate, color = Species))+
  geom_point(size = .5)

ggplot(mushroom_growth, aes(x=Species, y=GrowthRate, color = Humidity))+
  geom_point(size = .5)

ggplot(mushroom_growth, aes(x=Nitrogen, y=GrowthRate, color = Species))+
  geom_point(size = .5)




#### Q3 defines models that use lm() and aov() to explain the dependent variable
# p1-p5 are random models I was trying with glm
# stepAIC(p1) was used to generate codes to use
p1 <- glm(data=mushroom_growth, formula = GrowthRate ~ Light * Nitrogen * Humidity * Temperature * Species)
p1

p2 <- glm(data = mushroom_growth, formula = GrowthRate ~ Light + Nitrogen)
p2

p3 <- glm(data = mushroom_growth, formula = GrowthRate ~ Nitrogen * Light)
p3

p4 <- glm(data = mushroom_growth, formula = GrowthRate ~ Light * Nitrogen * Humidity * Temperature)
p4

ggplot(p4, aes(x=Light, y=GrowthRate, color = as.factor(Species)))+
  geom_point()

p5 <- glm(formula = GrowthRate ~ Light + Nitrogen + Humidity + Temperature + 
            Light:Nitrogen + Light:Humidity + Humidity:Temperature, data = mushroom_growth)
p5
summary(p5)

# p6 is a model from "stepAIC(p1)" using glm
p6 <- glm(formula = GrowthRate ~ Light + Nitrogen + Humidity + Temperature + 
            Species + Light:Nitrogen + Light:Humidity + Nitrogen:Humidity + 
            Light:Temperature + Humidity:Temperature + Light:Species + 
            Humidity:Species + Temperature:Species + Light:Nitrogen:Humidity + 
            Light:Humidity:Temperature + Light:Humidity:Species + Light:Temperature:Species + 
            Humidity:Temperature:Species + Light:Humidity:Temperature:Species, 
          data = mushroom_growth)
p6
# below is p6 plot with prediction points added in (x=species, y= growthrate) with glm
p6_with_predictions <- add_predictions(mushroom_growth, p6) %>% 
  ggplot(aes(x=Species, color = as.factor(Light)))+
  geom_point(aes(y=GrowthRate))+
  geom_point(aes(y= pred), color = "black", size = .5)+
  facet_wrap(~ Light)
p6_with_predictions
summary(p6)

# mod1 is a model for another code from "stepAIC(p1)" 
mod1 <- glm(data= mushroom_growth,formula = GrowthRate ~ Light + Nitrogen + Humidity + Temperature + Light:Nitrogen +
              Light:Humidity + Humidity:Temperature)
#below is a plot for mod1 with predictions (x=Light, y=Growth rate)
mod1_with_predictions <- add_predictions(mushroom_growth, mod1)%>%
  ggplot(aes(x=Light, color=Species))+
  geom_point(aes(y=GrowthRate))+
  geom_point(aes(y=pred), color = "black", size=.5)+
  facet_wrap(~ Species)
mod1_with_predictions


# mod2 is the code for another model
mod2 <- glm(formula = GrowthRate ~ Light + Nitrogen + Humidity + Temperature + 
      Species + Light:Nitrogen + Light:Humidity + Nitrogen:Humidity + 
      Light:Temperature + Humidity:Temperature + Light:Species + 
      Humidity:Species + Temperature:Species + Light:Nitrogen:Humidity + 
      Light:Humidity:Temperature + Light:Humidity:Species + Light:Temperature:Species + 
      Humidity:Temperature:Species + Light:Humidity:Temperature:Species, 
    data = mushroom_growth)
# below is the plot for mod2 with predictions (x=)
mod2_with_predictions <- add_predictions(mushroom_growth, mod2)%>%
  ggplot(aes(x=Nitrogen, color= as.factor(Species)))+
  geom_point(aes(y=GrowthRate))+
  geom_point(aes(y=pred), color = "black", size=.5)
mod2_with_predictions #not a very helpful plot...


# below is using aov
mod3 = aov(GrowthRate ~ Species, data = mushroom_growth)
mod3
summary(mod3) # P value of >.05 makes this significantly different



#### Q4 calculate the mean sq
mean(p1$residuals^2)
mean(p2$residuals^2)
mean(p3$residuals^2)
mean(p4$residuals^2)
mean(p5$residuals^2)
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)


#### Q5 best model given the above models
# p1 model had the lowest mean square value with 2332.378
p1 <- glm(data=mushroom_growth, formula = GrowthRate ~ Light * Nitrogen * Humidity * Temperature * Species)




#### Q6 add predictions 
p1_with_predictions <- mushroom_growth %>% add_predictions(p1)




#### Q7 plot the predictions with the real data
p1_with_predictions <- add_predictions(mushroom_growth, p1) %>% 
  ggplot(aes(x=Light, color = as.factor(Species)))+
  geom_point(aes(y=GrowthRate))+
  geom_point(aes(y=pred), color = "black", size=.5)
p1_with_predictions








