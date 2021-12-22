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

#Changing the values of "F" and "M" to "Female" and "Male" in the Sex Column for better graph.
NYC[NYC == "F"] <- "Female"
NYC[NYC == "M"] <- "Male"

#Calculating mean of Deaths 
Deaths <- as.numeric(NYC$Deaths)
mean(Deaths, trim = 0, na.rm = TRUE)

#Calculating median of Deaths 
Deaths <- as.numeric(NYC$Deaths)
median(Deaths, trim = 0, na.rm = TRUE)

#Calculating Variance of Deaths 
Deaths <- as.numeric(NYC$Deaths)
var(Deaths, na.rm = TRUE)

#Calculating Standard Deviation of Deaths 
Deaths <- as.numeric(NYC$Deaths)
sd(Deaths, na.rm = TRUE)

#Calculating mean of Age Adjusted Death Rate
Age_Death_rate<- as.numeric(NYC$`Age Adjusted Death Rate`)
mean(Age_Death_rate, trim = 0, na.rm = TRUE)

#Calculating median of Age Adjusted Death Rate 
Age_Death_rate<- as.numeric(NYC$`Age Adjusted Death Rate`)
median(Age_Death_rate, trim = 0, na.rm = TRUE)

#Calculating Variance of Age Adjusted Death Rate 
Age_Death_rate<- as.numeric(NYC$`Age Adjusted Death Rate`)
var(Age_Death_rate, na.rm = TRUE)

#Calculating Standard Deviation of Age Adjusted Death Rate 
Age_Death_rate<- as.numeric(NYC$`Age Adjusted Death Rate`)
sd(Age_Death_rate, na.rm = TRUE)

#Calculating correlation between Deaths Column and Age Adjusted Death Rate
Deaths <- as.numeric(NYC$Deaths)
Age_Death_rate<- as.numeric(NYC$`Age Adjusted Death Rate`)
cor(Deaths,Age_Death_rate, use = "complete.obs")

#Calculating 95% confidence interval for Deaths column
Deaths <- as.numeric(NYC$Deaths)
ls(t.test(Deaths))
t.test(Deaths)$"conf.int"

#Calculating 95% confidence interval for Age Adjusted Death Rate
Age_Death_rate<- as.numeric(NYC$`Age Adjusted Death Rate`)
ls(t.test(Age_Death_rate))
t.test(Age_Death_rate)$"conf.int"

#Linear Regression
Deaths <- as.numeric(NYC$Deaths)
Age_Death_rate<- as.numeric(NYC$`Death Rate`)
fit1<- lm(Deaths~Age_Death_rate, NYC)
abline(fit1, col="red")

#Computing R-squared
model <- lm(Deaths~Age_Death_rate, NYC, data=data.frame(NYC))
summary(model) #Tells the R-squared value.

#Plotting the Histogram of the Residuals
library(ggplot2)
ggplot(data = NYC, aes(x = Death_rate)) +
  +     geom_histogram(bins=30, fill = 'steelblue', color = 'black') +
  +     labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

