library(tidyverse)



data("mtcars")
str(mtcars)
?mtcars
mtcars
Aut <- mtcars$am == "0"
Auto <- mtcars[Aut,]
Auto
write.csv(Auto, file = "./automatic_mtcars.csv")



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

?tiff
tiff(filename = "./../Assignment_5/mpg_vs_wt_auto.tiff")
Weight
dev.off()

Dis <- mtcars$disp <= "200"
Dis
view(mtcars)
Disp <- mtcars[Dis,]
Disp

write.csv(Disp, file = "./mtcars_max200_displ.csv")


