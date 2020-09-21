dft <- read.delim("../../Data/ITS_mapping.csv")
dft
sdft <- read.csv2("../../Data/ITS_mapping.csv")

sdft
?split.data.frame

summary(dft)


dft


?read.csv
dsft <- read.csv("../../Data/ITS_mapping.csv", header = FALSE, sep = ".",quote = "\"",
                 dec = ".")
summary(dsft)
head(dsft$V1)

dsft <- read.delim2("../../Data/ITS_mapping.csv")

Eco <- as.factor(dft$Ecosystem)
class(Eco)
Latt <- dft$Lat
class(Latt)

Eco
Latt
dsft
dev.off()
plot(x = Eco, y = Latt)

boxplot(Latt ~ Eco, data = dft)
class(dsft$Ecosystem)
class(dsft$Lat)

?png
png(filename = "./silly_boxplot.png")
dev.off()

