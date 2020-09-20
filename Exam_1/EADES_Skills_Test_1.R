fil <- read.csv("DNA_Conc_by_Extraction_Date.csv")

summary(fil)

yrs <- fil$Year_Collected
Katy <- fil$DNA_Concentration_Katy
Ben <- fil$DNA_Concentration_Ben

plot(x=Katy, y=yrsf)
class(yrs)
class(Katy)
yrsf <- as.factor(yrs)
class(yrsf)

plot(x=yrs, y=Katy)
hist(Katy, main = "DNA Concentration by Katy v. Frequency", xlab = "DNA Concentration")
hist(Ben, main = "DNA Concentration by Ben v. Frequency", xlab = "DNA Concentration")

class(Katy)
class(yrs)


jpeg(filename = "EADES_Plot1.jpeg")
plot(x = yrsf, y = Katy, main = "Katy's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()


jpeg(filename = "./EADES_Plot2.jpeg")
plot(x = yrsf, y = Ben, main = "Ben's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()

summary(fil)



