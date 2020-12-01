library(tidyverse)
library(patchwork)


data("mtcars")
str(mtcars)
?mtcars
mtcars
Aut <- mtcars$am == "0"
Auto <- mtcars[Aut,]
Auto
write.csv(Auto, file = "./automatic_mtcars.csv")

HP <- ggplot(Auto,aes(x=hp,y=mpg)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Effect of Horsepower on MPG", x="Horsepower", y="Miles Per Gallon")
HP


png(filename = "./mpg_vs_hp_auto.png")
ggplot(Auto,aes(x=hp,y=mpg)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Effect of Horsepower on MPG", x="Horsepower", y="Miles Per Gallon")
dev.off()


Weight <- ggplot(Auto,aes(x=wt,y=mpg)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Effect of Weight on MPG", x="Weight (1000lbs)", y="Miles Per Gallon")
Weight

tiff(filename = "./../Assignment_5/mpg_vs_wt_auto.tiff")
Weight
dev.off()

Dis <- mtcars$disp <= "200"
Dis
view(mtcars)
Disp <- mtcars[Dis,]
Disp

write.csv(Disp, file = "./mtcars_max200_displ.csv")

?arrange
?select

arrange(mtcars, hp) %>% tail(1) ; arrange(Auto, hp) %>% tail(1) ; arrange(Disp, hp) %>% tail(1)

max(mtcars$hp)
max(Auto$hp)
max(Disp$hp)


write.csv("./hp_maximums.txt")
max(mtcars$hp)
max(Auto$hp)
max(Disp$hp)
dev.off()

sink("./hp_maximums.txt")
cat("In order, original, automatic, max200")
cat("\n")
max(mtcars$hp)
max(Auto$hp)
max(Disp$hp)
sink(NULL)

CYLL <- ggplot(mtcars, aes(x=CL, y=mpg, fill=factor(cyl))) +
  geom_violin() +
  labs(x="Cylinders",y="Miles Per Gallon",fill="Cylinders",title = "Distribution of MPG")
CYLL

HPALL <- ggplot(mtcars,aes(x=hp,y=mpg,color=factor(cyl))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Effect of Horsepower on MPG", x="Horsepower", y="Miles Per Gallon", 
       color="Cylinder")
HPALL

WeightALL <- ggplot(mtcars,aes(x=wt,y=mpg,color=factor(cyl))) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Effect of Weight on MPG", x="Weight (1000lbs)", y="Miles Per Gallon",
       color="Cylinder")
WeightALL

png(filename = "./combined_mtcars_plot.png")
WeightALL + HPALL + CYLL
dev.off()

