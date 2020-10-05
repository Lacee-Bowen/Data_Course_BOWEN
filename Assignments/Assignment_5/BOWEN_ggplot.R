data("iris")
library(tidyverse)


# below is plot one on the assignment
png(filename = "iris_fig1.png")
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color = Species)) +
  geom_point(aes(color=Species)) +
  geom_smooth(method = "lm", size=1) +
  labs(title = "Sepal length vs petal length", subtitle = "for three iris species") +
  theme_minimal()
dev.off()


# below is plot two for the assignment
png(filename = "iris_fig2.png")
ggplot(iris, aes(x=Petal.Width, fill=Species))+
  geom_density(alpha=0.5)+
  theme_minimal()+
  labs(title= "Distribution of Petal Widths", subtitle = "for three iris species", x ="Petal Width")
dev.off()


# below is plot three
png(filename = "iris_fig3.png")
ggplot(iris, aes(x=Species, y=c(Petal.Width/Sepal.Width), fill=Species))+
  geom_boxplot()+
  labs(title = "Sepal- to Petal-Width Ratio", subtitle = "for three iris species", x= "Species", y="Ratio of Sepal Width to Petal Width")+
  theme_minimal()
dev.off()

# below is for plot four
png(filename = "iris_fig4.png")
iris$`inames` <- rownames(iris) # create new column for species names
iris$length_norm <- round((iris$Sepal.Length - mean(iris$Sepal.Length))/sd(iris$Sepal.Length), 3) # compute normalized Lengths
iris <- iris[order(iris$length_norm), ] #sort
iris$`inames` <- factor(iris$`inames`, levels = iris$`inames`)
ggplot(iris, aes(x=`inames`, y=length_norm, label=length_norm)) +
  #ggplot(iris, aes(x=Sepal.Length, y=Sepal.Length -mean(Sepal.Length))) +
  geom_bar(stat='identity', aes(fill=Species), width = .5, alpha=1) +
  labs(subtitle="Sepal length deviance from the mean of all observations", y="Deviance from the Mean")+
  coord_flip() +
  #theme(axis.title.y=element_blank()
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank())
dev.off()




