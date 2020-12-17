df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


n <- 2000
output <- c()
for (i in Allyear$Year) {
  mean(Allyear$BA)
  n=n+1
}
output


means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
str(output)
#>  num [1:138] 0.912 0.205 2.584 -0.789 0.588 ...

unique(Allyear$Year)

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

data.frame(Year=unique(Allyear$Year),
           AvgBA=avg.BA,
           AVGFldpct=avg.Fld)

Allyear %>% filter(Year == 2010)

p <- 1
Question <- c()
for(i in unique(Allyear$Year)){
  QUE <- glm(data = Allyear, formula = BA ~ Fldpct)
  Question[p] <- QUE
  p <- p+1
}

mood3 <- aov(data = FB10,formula = BA ~ Fldpct)
summary(mood3)


T10 <- All %>% 
  filter(Rk.x %in% c("1","2","3","4","5","6","7","8","9","10"))

BB10 <- Bat %>% 
  filter(Rk %in% c("1","2","3","4","5","6","7","8","9","10"))

BB10 %>% 
  select(Rk,Year,User,BA,HR)

FF10 <- Field %>% 
  filter(Rk %in% c("1","2","3","4","5","6","7","8","9","10"))

FF10 %>% 
  select(Rk,Year,User,Fldpct,E) %>% 
  mutate()

merg <- merge(BB10,FF10,by="Rk")

dfm <- data_frame(List=1:110,
           BUser=BB10$User,
           FUser=FF10$User,
           BA=BB10$BA,
           Fldpct=FF10$Fldpct)

BatFil <- Bat %>% 
  filter(Bat$G > 130) %>% 
  arrange(desc(BA)) %>% 
  select(Rk,User,BA,HR)
           
BBUse <- BatFil %>% head(110)

FieldFil <- Field %>% 
  filter(Field$G > 90) %>% 
  arrange(desc(Fldpct)) %>% 
  select(Rk,User,Fldpct,E)

FFUse <- FieldFil %>% head(110)

df12 <- data_frame(List=1:110,
           Fldpct = FFUse$Fldpct,
           BA = BBUse$BA,
           E = FFUse$E,
           HR = BBUse$HR)
