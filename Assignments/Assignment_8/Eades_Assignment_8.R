library(tidyverse)
library(modelr)
library(broom)
library(fitdistrplus)

df <- read_csv("./../../Data/mushroom_growth.csv")
view(df)
# 2

ggplot(df,aes(x=Light,y=GrowthRate)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal()

ggplot(df,aes(x=Nitrogen,y=GrowthRate)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal()

ggplot(df,aes(x=Temperature,y=GrowthRate)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal()

# 3

mod1 <- lm(data = df, formula = GrowthRate ~ Light + Species)
summary(mod1)


mod3 <- aov(data = df, formula = GrowthRate ~ Nitrogen + Species)
summary(mod3)

# 4

mean(mod1$residuals^2)
mean(mod3$residuals^2)

# 5

# The first model is better

# 6

df2 <- df %>% 
  add_predictions(mod1) 

df1 <- data.frame(Light = c(15,25,30))

mod2 <- lm(data = df, formula = GrowthRate ~ Light)
pred <- predict(mod2, newdata = df1)
view(pred)

combined <- data.frame(Light = df1$Light,
                       pred = pred)

df$PredictionType <- "Real"
combined$PredictionType <- "Hypothetical"

last1 <- full_join(df2,combined)
view(last1)

# 7

ggplot(last1,aes(x=Light,y=pred,color=PredictionType)) +
  geom_point(size=2) +
  geom_point(aes(y=GrowthRate),color="Black",size=1) +
  theme_minimal()

# I changed the size to be able to see the ones that overlap.

