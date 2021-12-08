#Rename File to NYC.csv
View(NYC)
#Histogram of Column Deaths
Deaths <- as.numeric(NYC$Deaths)
hist(Deaths,xlab="Number of Deaths",ylim = c(0,1000), xlim = c(0,7050), breaks = 30)

#Histogram of Column Death Rate
Death_rate<- as.numeric(NYC$`Death Rate`)
hist(Death_rate, xlab = "Death Rate",  breaks=25, col = "antiquewhite")

#Barplot of Column Sex
barplot(table(NYC$Sex), xlab = "Sex (F = Female, M = Male)", ylab = "Frequency", main = "Sex", col = "skyblue")

#Barplot of Column Race Ethnicity
barplot(table(NYC$`Race Ethnicity`), xlab = "Race Ethnicity", ylab = "Frequency", main = "Race Ethnicity", col = "yellow")
