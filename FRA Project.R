library(readxl)
library(mice)
library(ggplot2)
library(ggcorrplot)
library(ellipse)
library(RColorBrewer)
library(nFactors)
library(psych)
library(lattice)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(data.table)
library(ROCR)
library(ineq)
library(StatMeasures)
library(htmlwidgets)
library(DataExplorer)
library(corrplot)
library(partykit)
library(dplyr)
library(purrr)
library(InformationValue)
library(car)
library(ROCR)
library(MASS)
library(e1071)
library(class)
library(caret)
library(DMwR)
library(ipred)

setwd("C:/Users/Samrat/Documents/R/Directories/")
getwd()

data = read_excel("raw-data.xlsx")

summary(data)
str(data)
dim(data)
names(data)

colnames(data) = make.names(colnames(data))
attach(data)


sum(is.na(data))


imputed.data = mice(data[,-c(1,22,42,43,44,45,46,47,48,52)], method = "pmm")

summary(imputed.data)

complete.data = complete(imputed.data,1)
summary(complete.data)

new.data = complete.data[,-c(4,16)]

summary(new.data)

boxplot(new.data)


qnt = quantile(new.data[,1], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,1], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,1])
new.data[,1][new.data[,1] < (qnt[1] - H)] = caps[1]
new.data[,1][new.data[,1] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,2], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,2], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,2])
new.data[,2][new.data[,2] < (qnt[1] - H)] = caps[1]
new.data[,2][new.data[,2] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,3], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,3], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,3])
new.data[,3][new.data[,3] < (qnt[1] - H)] = caps[1]
new.data[,3][new.data[,3] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,4], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,4], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,4])
new.data[,4][new.data[,4] < (qnt[1] - H)] = caps[1]
new.data[,4][new.data[,4] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,5], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,5], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,5])
new.data[,5][new.data[,5] < (qnt[1] - H)] = caps[1]
new.data[,5][new.data[,5] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,6], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,6], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,6])
new.data[,6][new.data[,6] < (qnt[1] - H)] = caps[1]
new.data[,6][new.data[,6] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,7], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,7], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,7])
new.data[,7][new.data[,7] < (qnt[1] - H)] = caps[1]
new.data[,7][new.data[,7] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,8], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,8], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,8])
new.data[,8][new.data[,8] < (qnt[1] - H)] = caps[1]
new.data[,8][new.data[,8] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,9], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,9], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,9])
new.data[,9][new.data[,9] < (qnt[1] - H)] = caps[1]
new.data[,9][new.data[,9] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,10], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,10], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,10])
new.data[,10][new.data[,10] < (qnt[1] - H)] = caps[1]
new.data[,10][new.data[,10] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,11], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,11], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,11])
new.data[,11][new.data[,11] < (qnt[1] - H)] = caps[1]
new.data[,11][new.data[,11] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,12], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,12], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,12])
new.data[,12][new.data[,12] < (qnt[1] - H)] = caps[1]
new.data[,12][new.data[,12] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,13], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,13], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,13])
new.data[,13][new.data[,13] < (qnt[1] - H)] = caps[1]
new.data[,13][new.data[,13] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,14], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,14], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,14])
new.data[,14][new.data[,14] < (qnt[1] - H)] = caps[1]
new.data[,14][new.data[,14] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,15], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,15], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,15])
new.data[,15][new.data[,15] < (qnt[1] - H)] = caps[1]
new.data[,15][new.data[,15] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,16], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,16], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,16])
new.data[,16][new.data[,16] < (qnt[1] - H)] = caps[1]
new.data[,16][new.data[,16] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,17], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,17], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,17])
new.data[,17][new.data[,17] < (qnt[1] - H)] = caps[1]
new.data[,17][new.data[,17] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,18], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,18], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,18])
new.data[,18][new.data[,18] < (qnt[1] - H)] = caps[1]
new.data[,18][new.data[,18] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,19], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,19], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,19])
new.data[,19][new.data[,19] < (qnt[1] - H)] = caps[1]
new.data[,19][new.data[,19] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,20], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,20], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,20])
new.data[,20][new.data[,20] < (qnt[1] - H)] = caps[1]
new.data[,20][new.data[,20] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,21], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,21], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,21])
new.data[,21][new.data[,21] < (qnt[1] - H)] = caps[1]
new.data[,21][new.data[,21] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,22], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,22], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,22])
new.data[,22][new.data[,22] < (qnt[1] - H)] = caps[1]
new.data[,22][new.data[,22] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,23], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,23], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,23])
new.data[,23][new.data[,23] < (qnt[1] - H)] = caps[1]
new.data[,23][new.data[,23] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,24], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,24], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,24])
new.data[,24][new.data[,24] < (qnt[1] - H)] = caps[1]
new.data[,24][new.data[,24] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,25], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,25], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,25])
new.data[,25][new.data[,25] < (qnt[1] - H)] = caps[1]
new.data[,25][new.data[,25] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,26], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,26], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,26])
new.data[,26][new.data[,26] < (qnt[1] - H)] = caps[1]
new.data[,26][new.data[,26] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,27], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,27], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,27])
new.data[,27][new.data[,27] < (qnt[1] - H)] = caps[1]
new.data[,27][new.data[,27] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,28], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,28], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,28])
new.data[,28][new.data[,28] < (qnt[1] - H)] = caps[1]
new.data[,28][new.data[,28] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,29], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,29], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,29])
new.data[,29][new.data[,29] < (qnt[1] - H)] = caps[1]
new.data[,29][new.data[,29] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,30], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,30], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,30])
new.data[,30][new.data[,30] < (qnt[1] - H)] = caps[1]
new.data[,30][new.data[,30] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,31], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,31], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,31])
new.data[,31][new.data[,31] < (qnt[1] - H)] = caps[1]
new.data[,31][new.data[,31] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,32], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,32], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,32])
new.data[,32][new.data[,32] < (qnt[1] - H)] = caps[1]
new.data[,32][new.data[,32] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,33], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,33], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,33])
new.data[,33][new.data[,33] < (qnt[1] - H)] = caps[1]
new.data[,33][new.data[,33] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,34], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,34], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,34])
new.data[,34][new.data[,34] < (qnt[1] - H)] = caps[1]
new.data[,34][new.data[,34] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,35], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,35], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,35])
new.data[,35][new.data[,35] < (qnt[1] - H)] = caps[1]
new.data[,35][new.data[,35] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,36], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,36], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,36])
new.data[,36][new.data[,36] < (qnt[1] - H)] = caps[1]
new.data[,36][new.data[,36] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,37], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,37], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,37])
new.data[,37][new.data[,37] < (qnt[1] - H)] = caps[1]
new.data[,37][new.data[,37] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,38], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,38], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,38])
new.data[,38][new.data[,38] < (qnt[1] - H)] = caps[1]
new.data[,38][new.data[,38] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,39], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,39], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,39])
new.data[,39][new.data[,39] < (qnt[1] - H)] = caps[1]
new.data[,39][new.data[,39] > (qnt[2] + H)] = caps[2]

qnt = quantile(new.data[,40], probs = c(.25, .75),na.rm = T)
caps = quantile(new.data[,40], probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(new.data[,40])
new.data[,40][new.data[,40] < (qnt[1] - H)] = caps[1]
new.data[,40][new.data[,40] > (qnt[2] + H)] = caps[2]

summary(new.data)

boxplot(new.data)
names(new.data)

new.data$Default = ifelse(new.data$Networth.Next.Year > 0,0,1)
new.data$Default = as.factor(new.data$Default)


summary(new.data$Default)
plot(new.data$Default)

final.data = new.data[,c(10,11,12,13,14,20,25,26,27,33,34,35,36,37,38,39,41)]
summary(final.data)
str(final.data)
names(final.data)

corr.matrix = round(cor(final.data[,-17]),3) 
corr.matrix

ggcorrplot(corr.matrix, type = "lower", ggtheme = ggplot2::theme_gray,
           show.legend = TRUE, show.diag = TRUE, colors = c("cyan","white","sky blue"),
           lab = TRUE)

my_colors = brewer.pal(7, "Blues")
my_colors = colorRampPalette(my_colors)(100)
plotcorr(corr.matrix , col=my_colors[corr.matrix*50+50] , mar=c(1,1,1,1), )


test.model = glm(final.data$Default ~ PBDITA.as...of.total.income + PBT.as...of.total.income  + PAT.as...of.total.income + Cash.profit.as...of.total.income + PAT.as...of.net.worth + Current.liabilities...provisions + TOL.TNW+ Total.term.liabilities...tangible.net.worth + Contingent.liabilities...Net.worth.... + Quick.ratio..times. +  Current.ratio..times. + Debt.to.equity.ratio..times. + Cash.to.current.liabilities..times. + Cash.to.average.cost.of.sales.per.day + EPS + Adjusted.EPS, family = binomial)
vif(test.model)

final.data = final.data[,-c(2,3,4,5,7,8,9,10,13,16)]


plot(final.data$PBDITA.as...of.total.income)
glm(data = final.data, Default~ PBDITA.as...of.total.income, family = binomial)
summary(glm(data = final.data, Default~ PBDITA.as...of.total.income , family = binomial))

plot(final.data$Current.ratio..times.)
glm(data = final.data, Default ~ Current.ratio..times., family = binomial)
summary(glm(data = final.data, Default~ Current.ratio..times. , family = binomial))


plot(final.data$Debt.to.equity.ratio..times.)
glm(data = final.data, Default~ Debt.to.equity.ratio..times. , family = binomial)
summary(glm(data = final.data, Default~ Debt.to.equity.ratio..times. , family = binomial))

plot(final.data$EPS)
glm(data = final.data, Default~ EPS , family = binomial)
summary(glm(data = final.data, Default~ EPS , family = binomial))

plot(Default,final.data$PBDITA.as...of.total.income)
plot(Default,final.data$Current.ratio..times.)
plot(Default,final.data$Debt.to.equity.ratio..times.)
plot(Default,final.data$EPS)


summary(glm(data = final.data, Default ~ PBDITA.as...of.total.income + Current.ratio..times. + Debt.to.equity.ratio..times. + EPS , family = binomial))

model1 = glm(data = final.data, Default ~ PBDITA.as...of.total.income + Current.ratio..times. + Debt.to.equity.ratio..times. + EPS , family = binomial)


prediction = ifelse(model1$fitted.values > 0.055,1,0)

table(model1$y,prediction)


test.data = read_excel("validation_data.xlsx")

summary(test.data)
str(test.data)
names(test.data)

colnames(test.data) = make.names(colnames(test.data))

imputed.data2 = mice(test.data[,-c(1,2,22,42,43,44,45,46,47,48,52)], method = "pmm")

summary(imputed.data2)

complete.data2 = complete(imputed.data2,1)
summary(complete.data2)
names(complete.data2)

new.test.data = complete.data2[,c(10,35,36,39)]
summary(new.test.data)


prediction1 = predict(model1, newdata = new.test.data)


cmLR = table(test.data$Default...1, prediction1 > 0.1)
cmLR
sum(diag(cmLR))/sum(cmLR)

set.seed(1000)
balanced.data = SMOTE(Default ~.,perc.over = 500 , final.data , k = 5, perc.under = 300)
table(balanced.data$Default)

1458/(1458+10935)

model2 = glm(data = balanced.data, Default ~ PBDITA.as...of.total.income + Current.ratio..times. + Debt.to.equity.ratio..times. + EPS , family = binomial)
summary(model2)

prediction = ifelse(model2$fitted.values > 0.13,1,0)
table(model2$y,prediction)


prediction2 = predict(model2, newdata = new.test.data)
cmLR = table(test.data$Default...1, prediction2 > 0.1)
cmLR
sum(diag(cmLR))/sum(cmLR)

new.test.data$Probability.of.Default = predict(model1, newdata = new.test.data)
new.test.data$Decile.groups = decile(vector = new.test.data$Probability.of.Default, decreasing = TRUE )
new.test.data$Default = test.data$Default...1
new.test.data$Default.Prediction = prediction1 > 0.1


output.data = new.test.data[order(new.test.data$Probability.of.Default),]

View(output.data)

write.csv(output.data, file = "FRA.output.csv")

