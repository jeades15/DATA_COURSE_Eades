library(tidyverse)
library(ggimage)

lds <- read_csv("landdata-states.csv")

#1 

options(scipen = 999)

p1 <- ggplot(lds,aes(x=Year,y=Land.Value)) + 
  geom_smooth(aes(color=region)) +
  labs(x="Year", y="Land Value (USD)", color= "Region") +
  theme_minimal()

p1

jpeg(filename = "EADES_Fig_1.jpg")
p1
dev.off()
            
#2
# The region "NA" is a missing value for the "State" DC because DC is not a state

NAstates <- lds %>% 
  filter(is.na(region)) %>% 
  summarise(State)
view(NAstates)

#3

uni <- read_csv("unicef-u5mr.csv")

un2 <- pivot_longer(uni,2:67, names_to = "Year", names_prefix = "U5MR.")
un2

un3 <- un2 %>% 
  mutate(MortalityRate = round(value, 1))
un3
# 4

fig2 <- ggplot(un3,aes(x=as.numeric(Year),y=MortalityRate)) +
  geom_jitter(aes(color=Continent)) +
  scale_x_discrete(breaks=seq(1950,2015, by=10)) +
  scale_x_continuous(minor = seq(1950,2020,20), breaks = seq(1960,2000,20)) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color="gray"),
        panel.grid.major = element_line(c(1960,1980,2000), color = "gray", size=.3, linetype = "solid"),
        panel.grid.minor = element_line(c(1950,1970,1990,2010), color = "gray", size = .2, linetype = "solid"),
        axis.ticks = element_blank()) +
  labs(x="Year")
fig2

jpeg(filename = "EADES_Fig_2.jpg")
fig2
dev.off()

# 4 part 2

un4 <- un3 %>% 
  na.omit()



un5 <- un4 %>% 
  group_by(Year,Continent) %>% 
  summarise(Mean_Mort = mean(MortalityRate))

view(un5)
fig3 <- ggplot(un5, aes(x=as.numeric(Year),y=as.numeric(Mean_Mort))) +
  geom_line(aes(color=Continent),size=2) +
  scale_x_continuous(minor = seq(1950,2020,20), breaks = seq(1960,2000,20)) +
  labs(x="Year",y="Mean Mortality Rate (deaths per 1000 live births)") +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color="gray"),
        panel.grid.major = element_line(c(1960,1980,2000), color = "gray", size=.3, linetype = "solid"),
        panel.grid.minor = element_line(c(1950,1970,1990,2010), color = "gray", size = .2, linetype = "solid"),
        axis.ticks = element_blank())

fig3

jpeg(filename = "EADES_Fig_3.jpg")
fig3
dev.off()

# 5

fig4 <- ggplot(un3, aes(x=as.numeric(Year), y=MortalityRate/1000)) +
  geom_point(color="blue", size=.2) +
  labs(x="Year", y="Mortality Rate") +
  facet_wrap(~Region) +
  scale_x_continuous(minor_breaks = seq(1950,2010,20), breaks = seq(1960,2000,20)) +
  theme(panel.grid = element_line(color="gray"),
        panel.background = element_blank(),
        strip.background = element_rect(fill="white", color = "black"),
        panel.grid.major = element_line(c(1960,1980,2000), color = "gray", size=.3, linetype = "solid"),
        panel.grid.minor = element_line(c(1950,1970,1990,2010), color = "gray", size = .2, linetype = "solid"),
        axis.ticks = element_blank())
  
fig4

jpeg(filename = "EADES_Fig_4.jpg")
fig4
dev.off()

