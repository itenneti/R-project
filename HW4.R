#Problem 1
property <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR18.txt")
property


rentalrates <- property$V1
rentalrates

age <- property$V2
age

OperatingExpAndTax <- property$V3
OperatingExpAndTax

VacancyRates <- property$V4
VacancyRates

TotalSqFt <- property$V5
TotalSqFt

pairs(~age+OperatingExpAndTax+VacancyRates+TotalSqFt+rentalrates,data = property,
      main="Scatterplot Matrix
      Inu Tenneti")

#Problem 1 number 2
cor(age, OperatingExpAndTax)
cor(age, VacancyRates)
cor(age, TotalSqFt)
cor(age, rentalrates)
cor(OperatingExpAndTax, VacancyRates)
cor(OperatingExpAndTax, TotalSqFt)
cor(OperatingExpAndTax, rentalrates)
cor(VacancyRates, TotalSqFt)
cor(VacancyRates, rentalrates)
cor(TotalSqFt, rentalrates)

#Problem 1 number 3
propertyreg <- lm(rentalrates ~ age + OperatingExpAndTax + VacancyRates + TotalSqFt, data = property)
summary(propertyreg)

propertyreg1 <- lm(rentalrates ~ age + OperatingExpAndTax )
summary(propertyreg1)
anova(propertyreg1, propertyreg)

propertyreg2 <- lm(rentalrates ~ age + OperatingExpAndTax + TotalSqFt)
summary(propertyreg2)
anova(propertyreg2, propertyreg)

propertyreg3 <- lm(rentalrates ~ age + OperatingExpAndTax + TotalSqFt)
summary(propertyreg3)

propertyreg3

#Problem 1 number 4
confint(propertyreg, 'age', level = 0.90)
confint(propertyreg, 'OperatingExpAndTax', level = 0.90)
confint(propertyreg, 'VacancyRates', level = 0.90)
confint(propertyreg, 'TotalSqFt', level = 0.90)

confint(propertyreg, 'age', level = (1 - 0.1/5))
confint(propertyreg, 'OperatingExpAndTax', level = (1 - 0.1/5))
confint(propertyreg, 'VacancyRates', level = (1 - 0.1/5))
confint(propertyreg, 'TotalSqFt', level = (1 - 0.1/5))

new <- data.frame(age1 = c(5,6,14,12), opexp = c(8.25, 8.5, 11.5, 10.25), sqft = c(250000, 270000, 300000, 310000))
predict(propertyreg, new, interval = 'confidence', level = (1-0.05/4), se.fit = TRUE)

new2 <- data.frame(cbind(age2 = c(4,6,12), opexp2 = c(10,11.5,12.5), sqft = c(80000, 12000, 340000)))

#Problem 1 number 5
residual <- resid(propertyreg)
plot(rentalrates, residual, main = 'Residuals vs. Fitted Values', ylab = 'Residuals', xlab = 'Fitted Values', sub = 'Inu Tenneti')

qqnorm(residual, main = 'QQ Plot', sub = 'Inu Tenneti')
qqline(residual)

boxplot(residual, main = 'Box Plot of Residuals', sub = 'Inu Tenneti')

hist(residual, main = 'Histogram of Residuals', sub = 'Inu Tenneti')
lines(density(residual))

library(lmtest)
bptest(propertyreg, studentize = FALSE)

plot(residual, ylab = 'Residuals', main = 'Index Plot', sub = 'Inu Tenneti')
abline(h = 0)

shapiro.test(residual)

dwtest(propertyreg)

lower <- quantile(rentalrates, 0.025)
upper <- quantile(rentalrates, 0.975)
outliers <- which(rentalrates < lower | rentalrates > upper)
outliers

dfnooutlier <- data[-c(3,6, 62, 65), ]
dfnooutlier

nooutlier <- lm(dfnooutlier$rentalrates ~ dfnooutlier$age + dfnooutlier$vacancyrates + dfnooutlier$TotalSqFt)
summary(nooutlier)


#Problem 2
#PART A AND PART B
library(readr)
examscore <- read.table(file = "Downloads/Final Exam Score.txt", header = TRUE)
examscore

examscore$QuizAve <- examscore$QuizAve * 10
examscore$CompAve <- examscore$CompAve * 10
View(examscore)

summary(examscore)

cor(examscore)
median(examscore$CompAve)
cor(examscore$CompAve, examscore$FinalExam)

examreg <- lm(examscore$FinalExam ~ examscore$QuizAve + examscore$CompAve + examscore$MidTerm)
examreg

matrix(examscore)

summary(examreg)

residuals(examreg)

summary(examreg)

examreg1 <- lm(examscore$FinalExam ~ examscore$CompAve + examscore$MidTerm)
examreg1

summary(examreg)
anova(examreg1, examreg)

examreg2 <- lm(examscore$FinalExam ~ examscore$QuizAve)
confint(examreg2, level = 0.95)

mean1 <- mean(examscore$QuizAve)
mean2 <- mean(examscore$CompAve)
mean3 <- mean(examscore$MidTerm)

examreg2 <- lm(examscore$FinalExam ~ examscore$MidTerm)
anova(examreg2, examreg)