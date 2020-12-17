# I will be studying the relationship, if any, between batting average and defensive performance in baseball.
# My question: If a player's batting average decreases, does their defensive performance also decrease?
# In other words, is there a correlation between batting performance and defensive performance

# I will use the MLB archival records to test a 10 or 20 year period with not only different teams
# but also different positions

library(tidyverse)
library(modelr)
library(broom)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)
library(ggplot2)
library(patchwork)



F20 <- read_csv("./Data/Fielding_Real/2020_Fielding_Data.csv")
F19 <- read_csv("./Data/Fielding_Real/2019_Fielding_Data.csv")
F18 <- read_csv("./Data/Fielding_Real/2018_Fielding_Data.csv")
F17 <- read_csv("./Data/Fielding_Real/2017_Fielding_Data.csv")
F16 <- read_csv("./Data/Fielding_Real/2016_Fielding_Data.csv")
F15 <- read_csv("./Data/Fielding_Real/2015_Fielding_Data.csv")
F14 <- read_csv("./Data/Fielding_Real/2014_Fielding_Data.csv")
F13 <- read_csv("./Data/Fielding_Real/2013_Fielding_Data.csv")
F12 <- read_csv("./Data/Fielding_Real/2012_Fielding_Data.csv")
F11 <- read_csv("./Data/Fielding_Real/2011_Fielding_Data.csv")
F10 <- read_csv("./Data/Fielding_Real/2010_Fielding_Data.csv")


Field <- rbind(F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20)

write_csv(Field, "./Data/Field_Total.csv")

FieldG <- Field$G > 80
FieldGames <- Field[FieldG,]
write_csv(FieldGames, "./Data/Field_Games.csv")


B10 <- read_csv("./Data/Batting_Real/2010_Batting_Data.csv")
B11 <- read_csv("./Data/Batting_Real/2011_Batting_Data.csv")
B12 <- read_csv("./Data/Batting_Real/2012_Batting_Data.csv")
B13 <- read_csv("./Data/Batting_Real/2013_Batting_Data.csv")
B14 <- read_csv("./Data/Batting_Real/2014_Batting_Data.csv")
B15 <- read_csv("./Data/Batting_Real/2015_Batting_Data.csv")
B16 <- read_csv("./Data/Batting_Real/2016_Batting_Data.csv")
B17 <- read_csv("./Data/Batting_Real/2017_Batting_Data.csv")
B18 <- read_csv("./Data/Batting_Real/2018_Batting_Data.csv")
B19 <- read_csv("./Data/Batting_Real/2019_Batting_Data.csv")
B20 <- read_csv("./Data/Batting_Real/2020_Batting_Data.csv")

Bat <- rbind(B10,B11,B12,B13,B14,B15,B16,B17,B18,B19,B20)

write_csv(Bat, "./Data/Batting_Total.csv")


BatG <- Bat$AB >= 80 & Bat$G > 90
BatGames <- Bat[BatG,]
write_csv(BatGames, "./Data/Batting_Games.csv")

# In order to game the most accurate results, I need to include only players who have
# played enough games to impact their data. I chose 15 games which still gives me a 
# huge data frame to work with


ggplot(FieldG, aes(x=E, y=Fldpct)) +
  geom_point() +
  geom_smooth(method="lm")

mod1 <- glm(data = FieldG,
            formula = Fldpct ~ E)
summary(mod1)

ggplot(FieldG, aes(x=factor(Pos),y=Fldpct)) +
  geom_boxplot()


FB10 <- merge(F10,B10,by="User")
FB11 <- merge(F11,B11,by="User")
FB12 <- merge(F12,B12,by="User")
FB13 <- merge(F13,B13,by="User")
FB14 <- merge(F14,B14,by="User")
FB15 <- merge(F15,B15,by="User")
FB16 <- merge(F16,B16,by="User")
FB17 <- merge(F17,B17,by="User")
FB18 <- merge(F18,B18,by="User")
FB19 <- merge(F19,B19,by="User")
FB20 <- merge(F20,B20,by="User")

F13 %>% pivot_longer()
?mutate


ggplot(FB10, aes(x=Fldpct,y=BA)) +
  geom_point()
All <- merge(Field,Bat, by="User")
AllGames <- merge(FieldGames,BatGames,by="User")
Allyear <- merge(FieldGames,BatGames,by=c("User","Year"))

write_csv(Allyear, "./Data/Years_Combined.csv")


ggplot(Allyear, aes(x=Fldpct,y=BA)) +
  geom_point() +
  facet_wrap(~Year) +
  geom_smooth(method = "lm")

mod1 <- glm(data = Allyear,
            formula = Fldpct ~ BA)
summary(mod1)

mod2 <- aov(data = Allyear,
            formula = BA ~ Fldpct)
summary(mod2)

mod3 <- aov(data = FB10,
            formula = Fldpct ~ E + Ch)
summary(mod3)

mod4 <- glm(data = FB10,
            formula = Fldpct ~ Ch)
summary(mod4)

ggplot(FB10, aes(x=Ch, y=Fldpct)) +
  geom_point() +
  geom_smooth(method="lm")

mod5 <- glm(data = FB10,
            formula = BA ~ Fldpct)
summary(mod5)

mod6 <- glm(data = FB10,
            formula = Fldpct ~ BA)
summary(mod6)

p1 <- ggplot(Allyear, aes(x=factor(Year), y=Fldpct)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Year",y="Fielding Percentage")

p2 <- ggplot(Allyear, aes(x=factor(Year), y=BA)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Year",y="Batting Average")

grid.arrange(p1,p2)


x <- 1
avg.BA <- c()

for (i in unique(Allyear$Year)) {
  AVG <- mean(Allyear[Allyear$Year == i,"BA"])
  avg.BA[x] <- AVG
  x <- x + 1
}

avg.BA

z <- 1
avg.Fld <- c()

for(i in unique(Allyear$Year)) {
  GVA <- mean(Allyear[Allyear$Year == i, "Fldpct"])
  avg.Fld[z] <- GVA
  z <- z+1
}

avg.Fld

df1 <- data.frame(Year=unique(Allyear$Year),
           AvgBA=avg.BA,
           AvgFldpct=avg.Fld)

write_csv(df1, "./Data/Year_Averages.csv")

g1 <- ggplot(df1, aes(x=factor(Year),y=AvgFldpct)) +
  geom_point() +
  theme_minimal() +
  labs(x="Year", y="Avg Field%")

g2 <- ggplot(df1, aes(x=factor(Year),y=AvgBA)) +
  geom_point() +
  theme_minimal() +
  labs(x="Year", y="Avg Batting Avg")


z <- 1
avg.Fld <- c()

for(i in unique(Allyear$Year)) {
  GVA <- mean(Allyear[Allyear$Year == i, "Fldpct"])
  avg.Fld[z] <- GVA
  z <- z+1
}

avg.Fld


t <- 1
avg.avg <- c()

for(i in unique(Allyear$User)) {
  HTR <- mean(Allyear[Allyear$User == i, "Fldpct"])
  avg.avg[t] <- HTR
  t <- t +1
}

avg.avg

y <- 1
hmng <- c()

for (i in unique(Allyear$User)) {
  HRT <- mean(Allyear[Allyear$User == i, "BA"])
  hmng[y] <- HRT
  y <- y + 1
}

df2 <- data.frame(User=unique(Allyear$User),
           AvgFldpct = avg.avg,
           AvgBA = hmng)


Allyear %>% arrange(User)
Allyear %>% group_by(Tm.x) %>% 
  summarise()

