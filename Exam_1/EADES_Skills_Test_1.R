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

# 1

plot(x=yrs, y=Katy)
hist(Katy, main = "DNA Concentration by Katy v. Frequency", xlab = "DNA Concentration")
hist(Ben, main = "DNA Concentration by Ben v. Frequency", xlab = "DNA Concentration")

class(Katy)
class(yrs)

# 2 and # 3
jpeg(filename = "EADES_Plot1.jpeg")
plot(x = yrsf, y = Katy, main = "Katy's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()


jpeg(filename = "./EADES_Plot2.jpeg")
plot(x = yrsf, y = Ben, main = "Ben's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()

summary(fil)

summary(yrs)

# 4

Yefa <- as.factor(fil$Year_Collected)

for (i in levels(Yefa)) {
  print(mean(fil[Yefa == i, "DNA_Concentration_Ben"]))
  
}

col1 = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2010", "2011", "2012")
col2 = c("0.4844271", "0.5428083", "0.5651776", "0.4378957", "0.7310512", "1.283824", "1.219825", "1.463386", "1.184985", "1.183822", "1.120605", "1.092156")



for(i in levels(Yefa)) {
  print(mean(fil[Yefa == i, "DNA_Concentration_Katy"]))
}

col3 = c("0.1457559", "0.07652045", "0.1208625", "0.081075", "0.1991471", "0.492375", "0.4452727", "0.556", 
         "0.4622353", "0.4792941", "0.44875", "0.3835")
col4 = c(".3386712", ".46628785", ".4443151", ".3568207", ".5319041", ".791449", ".7745523", ".907386", ".7227497", ".7046179", ".671855", ".708656")

dft <- data.frame(YEARS = col1, Ben_avg = col2, Katy_avg = col3, Diff = col4)
min(dft$Diff)
order(dft$Diff, decreasing = FALSE)
# With the first row having the smallest difference and the column of years beginning with 2000, this shows 
# that the year 2000 had the smallest difference between Ben and Katy.


# 5

dwnn <- DNA_Conc_by_Extraction_Date$Lab == "Downstairs"
dwn <- DNA_Conc_by_Extraction_Date[dwnn,]
jpeg(filename = "./Ben_DNA_over_time.jpg")
plot(x = dwn$Date_Collected, y = dwn$DNA_Concentration_Ben, xlab = "Date_Collected", ylab = 
       "DNA_Concentration_Ben")
dev.off()


# 6

Yefa <- as.factor(fil$Year_Collected)
for (i in levels(Yefa)) {
  print(mean(fil[Yefa == i, "DNA_Concentration_Ben"]))
  
}


Ben_year_avg <- read.table(header = TRUE, text ="
                           YEAR DNA_Conc_Ben_avg
                           2000 .4844271
                           2001 .5428083
                           2002 .5651776
                           2003 .4378957
                           2004 .7310512
                           2005 1.283824
                           2006 1.219825
                           2007 1.463386
                           2008 1.184985
                           2010 1.183822
                           2011 1.120605
                           2012 1.092156")

max(Ben_year_avg$DNA_Conc_Ben_avg)

