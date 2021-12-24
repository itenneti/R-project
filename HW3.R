#Problem 1
crime <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%201%20Data%20Sets/CH01PR28.txt")
crime

#Problem 1a
crimereg <- lm(crime$V2 ~ crime$V1)
crimereg

#Problem 1b and Problem 1g
anova(crimereg)

#Problem 1c
sse <- sum((fitted(crimereg) - crime$V2)^2)
sse

crimesum <- summary(crimereg)
crimesum

mse <- mean(crimesum$residuals^2)
mse

#Problem 1d
sigma(crimereg)

#Problem 1e
summary(crimereg)

#Problem 1f
cor(crime$V1, crime$V2)

#Problem 1h
crimeplot <- plot(crime$V2, crime$V1, xlab = 'crime rates', ylab = 'percent of high school graduates', 
     main = 'Problem (h)', sub = 'Inu Tenneti')
crimeplot
abline(lm(crime$V1 ~ crime$V2))

#Problem 1i
crimeresid <- crimereg$residuals
plot(crime$V1, crimeresid, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Problem(i)', sub = 'Inu Tenneti')

#Problem 1j
library("lmtest")
bptest(crimereg, studentize = FALSE)

#Problem 1l
plot(crime$V1, crimeresid, abline(line <- lm(crimeresid ~ crime$V1)), xlab = 'Fitted Values', ylab = 'Residuals', main = 'Problem(l)', sub = 'Inu Tenneti')

#Problem 1m
dwtest(crimereg)

#Problem 1n
lowerbound <- quantile(crime$V1, 0.025)
upperbound <- quantile(crime$V1, 0.975)
crimeoutlier <- which(crime$V1 < lowerbound | crime$V1 > upperbound)
crimeoutlier

crimeout <- crime[crimeoutlier, ]
crimeout

boxplot(crime$V1, xlab = 'High School Graudate', ylab = 'Crime Rates', main = 'Problem(n)', sub = 'Inu Tenneti')
mtext(paste('Outliers present: ', paste(crimeout$V1, collapse = ",")))

#Problem 1o
library('ggplot2')
crimeHist <- lm(crime$V2 ~ crime$V1, data= crime)
ggplot(data = crime, aes(x = crimeHist$residuals)) +
  geom_histogram(bins = 15, fill = 'steelblue', color = 'black', aes(y = ..density..)) +
  labs(title = 'Problem (o)', x = 'Residuals', y = 'Frequency', subtitle = 'Inu Tenneti')

#Problem 1p
crimeresid <- crimereg$residuals
crimeresid
qqnorm(crimeresid, xlab = 'Residuals', ylab =  'Frequency', main = 'Problem (p)
Inu Tenneti')

#Problem 1q
shapiro.test(crimeresid)

#Problem 2
explosives <- read_excel("Documents/explosive.xlsx")
head(explosives)
explosives

#Problem 2a
explosivesplot <- plot(explosives$`Area of Wires`, explosives$`Deflection of Galvonometer`, xlab = 'Area of Wires (1/100,000 in)', ylab = 'Deflection of Galvonometer (mm)', main = 'Problem (a)', sub = 'Inu Tenneti')
explosivesplot

#Problem 2b
explosivesreg <- lm(explosives$`Deflection of Galvonometer` ~ explosives$`Area of Wires`)
explosivesreg

#Problem 2c
explosiveresid <- explosivesreg$residuals
plot(explosives$`Area of Wires`, explosiveresid, xlab = 'Fitted Values', ylab = 'Residuals', main = 'Problem(c)', sub = 'Inu Tenneti')

#Problem 2d
bptest(explosivesreg, studentize = FALSE)

#Problem 2g
plot(explosives$`Area of Wires`, explosiveresid, abline(line <- lm(explosiveresid ~ explosives$`Area of Wires`)), xlab = 'Fitted Values', ylab = 'Residuals', main = 'Problem(g)', sub = 'Inu Tenneti')

#Problem 2h
dwtest(explosivesreg)

#Problem 2i
lb <- quantile(explosives$`Area of Wires`, 0.025)
ub <- quantile(explosives$`Area of Wires`, 0.975)
expoutlier <- which(explosives$`Area of Wires` < lb | explosives$`Area of Wires`> ub)
expoutlier

expout <- explosives[expoutlier, ]
expout

boxplot(explosives$`Area of Wires`, xlab = 'Area of Wires', ylab = 'Deflection of Galvonometer', main = 'Problem(i)', sub = 'Inu Tenneti')
mtext(paste('Outliers present: ', paste(explosives$`Area of Wires`, collapse = ",")))

#Problem 2j
explosiveHist <- lm(explosives$`Deflection of Galvonometer` ~ explosives$`Area of Wires`, data= explosives)
ggplot(data = explosives, aes(x = explosiveHist$residuals)) +
  geom_histogram(bins = 15, fill = 'steelblue', color = 'black', aes(y = ..density..)) +
  labs(title = 'Problem (j)', x = 'Residuals', y = 'Frequency', subtitle = 'Inu Tenneti')

#Problem 2k
explosiveresid <- explosivesreg$residuals
explosiveresid
qqnorm(explosiveresid, xlab = 'Residuals', ylab =  'Frequency', main = 'Problem (k)
Inu Tenneti')

#Problem 2l
shapiro.test(explosiveresid)

#Problem 2m
anova(explosivesreg)

