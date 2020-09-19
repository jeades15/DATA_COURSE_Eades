library(tidyverse)


data("iris")
iris
SepLen <- c("Sepal.Length")
SepWid <- c("Sepal.Width")
iris[1:5,SepWid]
rm SepLen
?remove
rm(SepLen)
rm(SepWid)
iris[c(1:5,5:10),2]
vec <- 1:10
vec > 2
vec >= 2
vec >= 7
vec < 4
vec == 5

11 %in% vec

setosa_rows <- iris$Species == "setosa"
iris[setosa_rows,]

virginica_rows <- iris$Species == "virginica"
iris[virginica_rows,]

iris$Species == "setosa"|iris$Species == "virginica"
set_ver <- iris$Species %in% c("setosa", "virginica")
iris[set_ver,]
Yes


?filter()
filter(iris, Species == "virginica")
filter(iris, Species != "versicolor")
filter(iris, Species %in% c("setosa", "virginica"))
filter(iris, Species == "setosa" | Species == "virginica")


data("mtcars")
mtcars
glimpse(mtcars)

# subset mtcars so that we only have rows that match:
# hp > 100 and disp < 150
# using filter() function from tidyverse

filter(mtcars, disp < 150 & hp > 100)

# Homework is to create many random subsets of mtcars

df <- read.csv("./Data/Fake_grade_data.csv")
filter(df, Final_Project < 100)
names (df)
