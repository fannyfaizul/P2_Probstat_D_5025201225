#1

#X adalah kadar saturasi oksigen sebelum  melakukan aktivitas
X<-c(78, 75, 67, 77, 70, 72, 28, 74, 77)
#Y adalah kadar saturasi oksigen sesudah melakukan aktivitas
Y<-c(100, 95, 70, 90, 90, 90, 89, 90, 100)

#1a
difff <- c(22, 20, 3, 13, 20, 18, 11, 16, 23)
sd(difff)
#1b
meanX <- mean(X)
meanY <- mean(Y)

sdX <- sd(X)
sdY <- sd(Y)

variansX <- sdX ^ 2
variansY <- sdY ^ 2

abs(meanX - meanY) / sqrt((variansX/9) + (variansY/9))
#1c
t.test(X, Y)

#soal 2
soal2 <- list("mean" = 23500, "mean_hipo" = 20000, "sd" = 3900, "n" = 100)
tsum.test(mean.x = soal2$mean, n.x = 100, sd(soal2$sd), mu = soal2$mean_hipo)

zScore = (soal2$mean - soal2$mean_hipo) / (soal2$sd / sqrt(soal2$n))
pnorm(-abs(zScore))

#soal 3
bandung <- list("saham" = 19, "mean" = 3.64, "sd" = 1.67)
bali <- list("saham" = 27, "mean" = 2.79, "sd" = 1.32)

tsum.test(
  n.x = bandung$saham,
  n.y = bali$saham,
  mean.x = bandung$mean,
  mean.y = bali$mean,
  s.x = bandung$sd,
  s.y = bali$sd,
  var.equal = TRUE,
  alternative = "two.sided",
)

qt(p = 0.05, df = 2, lower.tail = FALSE)


#soal4
soal4 <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"),header = TRUE, check.names = TRUE)
byGroup <- split(soal4, soal4$Group)
group1 <- byGroup$`1`
group2 <- byGroup$`2`
group3 <- byGroup$`3`

hist(group1$Length, xlim = c(16, 20))
hist(group2$Length, xlim = c(16, 20))
hist(group3$Length, xlim = c(16, 20))

bartlett.test(soal4$Length, soal4$Group)


model1 <- lm(soal4$Length~soal4$Group)
summary(model1)

av <- aov(Length ~ factor(Group), data = soal4)
TukeyHSD(av)

ggplot(soal4, mapping = aes(x = Group, y = Length, group = 1)) +  geom_boxplot()

#soal 5

soal5 <- read_csv("D:\Kuliah\Semester 4\Probabilistic and Statistic\praktikum\modul2\P2_Probstat_D_5025201225\GTL.csv")
qplot(Temp, Light, data = soal5) + facet_wrap(~Glass)


av <- aov(Light ~ factor(Glass)*factor(Temp), data = soal5)
summary.aov(av)

group_by(soal5, Glass, Temp)%>% 
  summarize(
    mean = mean(Light),
    standar_deviasi = sd(Light)
  )

hsd <- TukeyHSD(av)
hsd

multcompLetters4(av, hsd)
