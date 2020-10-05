library(tidyverse)


fig1 <- ggplot(iris, aes(x= Sepal.Length, y= Petal.Length)) +
  geom_point(aes(color=Species), size=1) +
  geom_smooth(method="lm", aes(color = Species)) + 
  coord_cartesian(xlim=c(4,8), ylim=c(0,7)) +
  labs(title="Sepal length vs petal Length", subtitle = "for three iris species", y="Petal Length", x="Sepal Length")

names(iris)

fig1

png(filename = "./iris_fig1.png")
fig1
dev.off()

fig2 <- ggplot(iris,aes(x=Petal.Width, fill=Species)) +
  geom_density(alpha=.5) +
  labs(title="Distribution of Petal Widths", x="Petal Width", y="Density", subtitle="for three iris, species")

fig2

png(filename = "./iris_fig2.png")
fig2
dev.off()

fig3 <- ggplot(iris,aes(x=Species,y=c(Petal.Width/Sepal.Width))) +
  geom_boxplot(aes(fill=Species)) +
  labs(x="Species", y="Ratio of Petal Width to Sepal Width", title = "Petal- to Sepal-Width Ration", 
       subtitle = "for three iris species")
fig3

png(filename = "./iris_fig3.png")
fig3
dev.off()



data("iris")
iris$Species <- rownames(iris)
iris$Sepal.Length_z <- round((iris$Sepal.Length - mean(iris$Sepal.Length))/sd(iris$Sepal.Length), 3)
iris$Species_type <- ifelse(iris$Sepal.Length <0, "above", "below")
iris2 <- iris[order(iris$Species), ]


ggplot(iris, aes(x=Species, y=Sepal.Length_z, label=Sepal.Length_z)) +
  geom_bar(stat = "identity", aes(fill=Species_type), width=.5) +
  scale_fill_manual(name="Species",
                    lables =c("Setosa", "Versicolor")
                    values =c("ab")) +
  coord_flip()

?scale_fill_manual
