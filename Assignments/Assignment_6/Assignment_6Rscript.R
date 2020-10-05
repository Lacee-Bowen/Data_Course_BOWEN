data(mtcars)
str(mtcars)

autotrans <- mtcars$am %in% "0"
autosub <- mtcars[autotrans,]

write.csv(autosub, "automatic_mtcars.csv")

library(tidyverse)
library(ggplot2)

# assignment 6 first plot MPG vs horsepower scatterplot
png(filename = "mpg_vs_hp_auto.png")
ggplot(autosub, aes(x=hp, y=mpg))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Effect of Horsepower on Miles Per Gallon", 
       x= "Horse power", y= "MPG", subtitle = "Automatic Transmission Vehicles")+
  theme_bw()
dev.off()

# assignment 6 second plot MPG vs weight scatterplot

tiff(filename = "mpg_vs_wt_auto.tiff")
ggplot(autosub, aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Effect of Vehicle Weight on Miles Per Gallon",
       subtitle = "Automatic Transmission Vehicles",
       x = "Car weight (lbs in thousands)", y="MPG")+
  theme_light()
dev.off()

# assignemtnt 6 subsets to only cars with displacements less than or equal to 200 cu.in.
data(mtcars)

displace <- filter(mtcars, disp<= 200)
write.csv(displace, "mtcars_max200_displ.csv")

# assignment 6 problems 10 and 11 find the max hp for the 3 data frames
data(mtcars)
maxhp_origional <- max(mtcars$hp)
maxhp_automatic <- max(autosub$hp)
maxhp_max200 <- max(displace$hp)

write.table(c(maxhp_origional, maxhp_automatic, maxhp_max200), "hp_maximums.txt")


# problem 12 use patchwork to combine the three graphs in to one image
library(patchwork)

y1<-ggplot(mtcars, aes(x=wt, y=(mpg), color=as.factor(cyl)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Effect of Vehicle Weight on MPG", subtitle = "from mtcars dataset", x= "Weight (lbs in thousands)", y= "MPG")

y2 <- ggplot(mtcars, aes(x = cyl, y=mpg, fill=as.factor(cyl), color=as.factor(cyl)))+
  geom_violin()+
  labs(title = "Distributions of MPG per cylinders in Vehicle", subtitle= "from mtcars dataset", x="Cylinders", y= "MPG")

y3 <- ggplot(mtcars, aes(x= hp, y=mpg, color=as.factor(cyl)))+
  geom_point()+
  geom_smooth(method= "lm")+
  labs(title= "Effect of Horsepower on Miles Per Gallon", subtitle = "from mtcars dataset", x= "Horsepower", y="MPG")

png(filename = "combined_mtcars_plot.png")
y1 + y2 +y3
dev.off()





