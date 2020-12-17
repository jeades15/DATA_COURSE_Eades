# This is Exam 4 which is a redo of Exam 1

library(tidyverse)

fil <- read.csv("./../BIOL3100_Exams/Exam_1/DNA_Conc_by_Extraction_Date.csv")

summary(fil)

yrs <- fil$Year_Collected
Katy <- fil$DNA_Concentration_Katy
Ben <- fil$DNA_Concentration_Ben

plot(x=Katy, y=yrsf)
class(yrs)
class(Katy)
yrsf <- as.factor(yrs)
class(yrsf)

# 1

plot(x=yrs, y=Katy)
hist(Katy, main = "DNA Concentration by Katy v. Frequency", xlab = "DNA Concentration")
hist(Ben, main = "DNA Concentration by Ben v. Frequency", xlab = "DNA Concentration")


# 2 and # 3
jpeg(filename = "./EADES_Plot1.jpeg")
plot(x = yrsf, y = Katy, main = "Katy's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()


jpeg(filename = "./EADES_Plot2.jpeg")
plot(x = yrsf, y = Ben, main = "Ben's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()


# 4

summary(fil$DNA_Concentration_Katy)
summary(fil$DNA_Concentration_Ben)

min.diff <- min(fil$DNA_Concentration_Ben - fil$DNA_Concentration_Katy)
min.row <- which(fil$DNA_Concentration_Ben - fil$DNA_Concentration_Katy == min.diff)
min.row

fil[min.row,]$Year_Collected


# 5

dwnn <- fil$Lab == "Downstairs"
dwn <- fil[dwnn,]

as.Date(dwn$Date_Collected)


jpeg(filename = "./Ben_DNA_over_time.jpg")
plot(x = as.Date(dwn$Date_Collected), y = dwn$DNA_Concentration_Ben, xlab = "Date_Collected", ylab = 
       "DNA_Concentration_Ben")
dev.off()


# 6

fil2 <- fil %>% 
  group_by(Year_Collected) %>% 
  summarise(BenMean=mean(DNA_Concentration_Ben))
fil2



# Bonus Question showing what year has the maximum value and what that value is
fil2 %>% 
  arrange(desc(BenMean)) %>% 
  head(1)


# Saving the new data frame

write.csv(fil2, "./Ben_Average_Conc.csv")
