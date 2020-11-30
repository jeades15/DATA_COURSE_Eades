library(tidyverse)
library(modelr)
library(janitor)
library(broom)

FS <- read_csv("./../Data/FacultySalaries_1995.csv")

#1 
# Full, Associate, Assist, All
Full <- FS %>% select(c(1:4,9:17),contains("AvgFull")) %>% 
  pivot_longer(ends_with("FullProfSalary"),
               names_to="Rank",values_to="Salary",
               names_prefix="AvgFull") %>% 
  mutate(ProfType="Full")

Associate <- FS %>% select(c(1:4,9:17),contains("AvgAssoc")) %>% 
  pivot_longer(ends_with("AssocProfSalary"),
               names_to="Rank",values_to="Salary",
               names_prefix="AvgAssoc") %>% 
  mutate(ProfType="Associate")

Assistant <- FS %>% select(c(1:4,9:17),contains("AvgAssist")) %>% 
  pivot_longer(ends_with("AssistProfSalary"),
               names_to="Rank",values_to="Salary",
               names_prefix="AvgAssist") %>% 
  mutate(ProfType="Assistant")

All <- FS %>% select(c(1:4,9:17),contains(c("All"))) %>% 
  pivot_longer(cols = ends_with("ryAll"),
               names_to="Rank",values_to="Salary",
               names_prefix="AvgProf") %>% 
  mutate(ProfType="All")

full2 <- rbind(Full,Associate,Assistant,All)
full3 <- rbind(Full,Associate,Assistant)
view(full3)
full4 <- full3 %>% filter(Tier==c("I","IIA","IIB"))

p1<- ggplot(full4, aes(x=ProfType,y=Salary,fill=ProfType)) +
  geom_boxplot() +
  facet_wrap(~Tier) +
  theme_minimal() +
  labs(x="Rank",fill="Rank")
p1

jpeg("./Eades_Fig_1.jpg")
p1
dev.off()

?geom_boxplot
?stat_boxplot


str(Full)
str(FS)
rlang::last_error()
?pivot_longer

view(Full)
view(Associate)
view(Assistant)
view(All)

names(Full)
names(Associate)
names(Assistant)
names(All)

view(full2)

#2 

str(full2)
mod1 <- aov(data = full2,
            formula = Salary ~ State + Tier + ProfType)
summary(mod1)
TukeyHSD(mod1) %>% plot()

sink("./Salary_ANOVA_Summary.txt")
summary(mod1)
sink(NULL)

#3 

df3 <- read_csv("./../Data/Juniper_Oils.csv")
view(df3)

df4 <-df3 %>% 
  pivot_longer(contains(c("alpha-pinene","para-cymene","alpha-terpineol",
  "cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene",
  "beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol",
  "beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol",
  "cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")),
  names_to="Chemical_ID",
  values_to="Concentration")

view(df4)

#4 

fig2 <- ggplot(df4,aes(x=YearsSinceBurn,y=Concentration)) +
  geom_smooth() +
  facet_wrap(~Chemical_ID,scales = "free_y") +
  theme_minimal()
fig2

png(filename = "./Eades_Fig2.png")
fig2
dev.off()

# 5

mod2 <- glm(data = df4,
            formula = Concentration ~ Chemical_ID + YearsSinceBurn)
summary(mod2)

mod2t <- mod2 %>% tidy()
view(mod2t)


