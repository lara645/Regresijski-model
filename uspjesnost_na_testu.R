library(car)
library(lmtest)
library(MASS)
library(faraway)
library(qpcR)
library(gvlma)
library(caret)
library(leaps)
library(tidyverse)
library("RColorBrewer")

setwd("C:/Users/LARA/Desktop/mva-seminar")
baza <- read.csv('CASchools.csv', header = TRUE, sep = ",")
baza<-baza[-c(1:5,8,15)]
str(baza)
baza[] <- lapply(baza, function(x) as.numeric(x))
#baza <- na.omit(baza)
detach(baza)
attach(baza)
baza$omjer <- students/teachers
baza <- baza[-2]
baza$income<-income*1000
summary(students)
summary(omjer)
summary(lunch)
summary(expenditure)
summary(computer)
summary(income)
summary(students)
summary(english)
summary(read)

### Analiza varijabli

# read (ovisna)
summary(read)
sd(read)
par(mfrow=c(1,2))
boxplot(read,col=c("azure3"), ylab="Èitanje")
hist(read,prob=T, col = c("azure3"),main="",xlab="Èitanje",ylab = "Relativna frekvencija", ylim = c(0,0.025))
lines(density(read),col=c("red"))
legend("topleft", c("Uzoraèka funkcija gustoæe"),lty=c(1), lwd=c(2.5,2.5),col=c("red"),cex=0.95,
       box.lty=0)

# lunch
summary(lunch)
sd(lunch)
boxplot(lunch,col=c("azure3"), ylab="Školski obrok")
hist(lunch, col = c("azure3"),main="",prob=T,xlab="Školski obrok",ylab = "Relativna frekvencija")
lines(density(lunch),col=c("red"))
legend("topright", c("Uzoraèka funkcija gustoæe"),lty=c(1), lwd=c(2.5,2.5),col=c("red"),cex=0.95,
       box.lty=0)

cor.test(read,lunch)
cor.test(read,lunch, method = "spearman")
cor.test(read,lunch, method = "kendall") 
cor.test(read,lunch, alternative = "less")
cor.test(read,lunch, method = "spearman",alternative = "less")
cor.test(read,lunch, method = "kendall",alternative = "less")

model<-lm(read~lunch)
par(mfrow=c(1,1))
plot(read~lunch,xlab="Školski obrok",ylab="Èitanje", cex = 1, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
     font.labels = 2)
abline(model, col="red")
summary(model) #Adjusted R-squared:  0.7718


# income
summary(income)
sd(income)
boxplot(income,col=c("azure3"), ylab="Prihodi")
hist(income, col = c("azure3"),main="",prob=T,xlab="Prihodi",ylab = "Relativna frekvencija")
lines(density(income),col=c("red"))
legend("topright", c("Uzoraèka funkcija gustoæe"),lty=c(1), lwd=c(2.5,2.5),col=c("red"),cex=0.95,
       box.lty=0)

cor.test(read, income)
cor.test(read,income, method = "spearman")
cor.test(read,income, method = "kendall")
cor.test(read,income, alternative = "greater")
cor.test(read,income, method = "spearman",alternative = "greater")
cor.test(read,income, method = "kendall",alternative = "greater")

model<-lm(read~income)
plot(read~income, ylab="Èitanje", xlab = "Prihodi", cex = 1, pch = 19, col = adjustcolor(4, .6), cex.labels = 2, 
     font.labels = 2)
lines(lowess(read~income),col=c("red"))
abline(model)
summary(model) #Adjusted R-squared: 0.4857
crPlots(model)

model<-lm(read~I(log(income)))
plot(read~log(income),xlab="Prihodi",ylab="Èitanje", cex = 1, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
     font.labels = 2)
lines(lowess(read~log(income)),col=c("red"))
abline(model)
summary(model) #Adjusted R-squared: 0.5538 


# english
summary(english)
sd(english)
boxplot(english,col=c("azure3"), ylab="Engleski")
hist(english, col = c("azure3"),main="",xlab="Engleski",prob=T,ylab = "Relativna frekvencija",ylim = c(0,0.06))
lines(density(english),col=c("red"))
legend("topright", c("Uzoraèka fukcija gustoæe"),lty=c(1), lwd=c(2.5,2.5),col=c("red"),cex=0.95,
       box.lty=0)

cor.test(read,english)
cor.test(read,english, method = "spearman")
cor.test(read,english, method = "kendall")
cor.test(read,english, alternative = "less")
cor.test(read,english, method = "spearman",alternative = "less")
cor.test(read,english, method = "kendall",alternative = "less")

model<-lm(read~english)
plot(read~english, ylab="Èitanje", xlab="Engleski", cex = 1, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
     font.labels = 2)
abline(model) 
summary(model) 


## Modeliranje

baza <- baza[c("read","students", "lunch", "computer","expenditure","english","omjer","income")]
colnames(baza) <- c("Èitanje","Studenti", "Školski obrok", "Raèunala","Izdaci","Engleski","Omjer","Prihodi")

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.1, font = 4)
}

pairs(baza, panel = panel.smooth,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 0.9, 
      font.labels = 2, lower.panel = panel.cor)

colnames(baza) <- c("read","students", "lunch", "computer","expenditure","english","omjer","income")

baza$lincome <- log(income)
baza <- baza[-8]
detach(baza)
attach(baza)


#puni model
model<-lm(read~.,data = baza)
summary(model)
vif(model)

X<-model.matrix(model)[,-1]    
izbor=leaps(X,read,method='adjr2') 
max(izbor$adjr2) 
which(izbor$adjr2==max(izbor$adjr2)) 
najbolji.model.adjr2<-izbor$which[which(izbor$adjr2==max(izbor$adjr2)),] 
najbolji.model.adjr2
X
#najbolji:
model1<-lm(read~.-omjer,data = baza)
vif(model1) #students i computer preko 5
ncvTest(model1)
summary(model1) #students i computer nisu stat. znac. prediktori

linearHypothesis(model1, c("students=0","computer=0"))
#0.1418>0.05
model1<-lm(read~.-omjer-computer-students,data = baza)
summary(model1) #Adjusted R-squared:  0.8308
ncvTest(model1)
ncvTest(model1, ~ lunch+lincome+english+expenditure) #odbacujemo homoskedasticnost
gqtest(read ~ lunch+lincome+english+expenditure, point=0.5)
bptest(read ~ lunch+lincome+english+expenditure) 
ks.test(rstandard(model1),'pnorm',0,1)
shapiro.test(rstandard(model1)) #odbacujemo normalnost

boxcox(model1, plotit=T, lambda=seq(-5,5,by=0.1)) #0 unutra
baza$lread<-log(read)
detach(baza)
attach(baza)
model2 <- lm(lread~lunch+lincome+english+expenditure)
summary(model2) #Adjusted R-squared:  0.8319 
ncvTest(model2)
ncvTest(model2,~lunch+lincome+english+expenditure)
ks.test(rstandard(model2),'pnorm',0,1)
shapiro.test(rstandard(model2)) #odbacujemo normalnost
baza <- baza[-10]

forw<-step(lm(read~1),list(upper=~students+lunch+computer+expenditure+lincome+english+omjer),direction='both')
#najbolji:
model3 <- lm(read ~ lunch + expenditure + english + lincome + students)
ncvTest(model3)
summary(model3) #students nije stat znac, Adjusted R-squared:  0.8315
#pa se dobije isti model kao model1

model1 <- lm(read~lincome+english+expenditure+lunch) 
anova(model1) 
avPlots(model1, main = "")
cor(baza,read) #read ima najmanju korelaciju s expenditure od svih prediktora

# konacni model
model4 <- lm(read~lunch+english+lincome)
vif(model4)
summary(model4) #Adjusted R-squared:  0.8184
ncvTest(model4) #p = 0.82896
ncvTest(model4, ~ lunch+lincome+english) #p = 0.11111
gqtest(read ~ lunch+lincome+english, point=0.5) #p-value = 0.3676
bptest(read ~ lunch+lincome+english) #p-value = 0.2196
ks.test(rstandard(model4),'pnorm',0,1) #p-value = 0.8212
shapiro.test(rstandard(model4)) #p-value = 0.06222, ne odbacujemo normalnost
AIC(model4) #3002.443

r<-rstandard(model4) #standardizirani reziduali
f<-fitted(model4)
plot(f,r, xlab="Teorijske vrijednosti", ylab="Standardizirani reziduali", cex = 1, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
     font.labels = 2)

qqPlot(rstandard(model4), main="QQ Plot standardiziranih reziduala",xlab="Kvantili normalne distribucije", ylab="Standardizirani reziduali",col="red",)

#### Stršeæe vrijednosti
m<-model4
leverage<-hatvalues(m)
par(mfrow=c(1,2))
plot(leverage,ylab="leverage vrijednosti", main='',cex = 1, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
     font.labels = 2)
boxplot(leverage,col="azure3")
summary(leverage)
which(leverage>2*mean(leverage))
length(which(leverage>2*mean(leverage))) #33
outlierTest(m)
plot(rstudent(m), ylab="Studentizirani reziduali",main='',cex = 1, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
     font.labels = 2)
boxplot(rstudent(m),col="azure3")
which(rstudent(m)==max(abs(rstudent(m))))
baza[367,]

####utjecajna mjerenja####
dffits(m)
plot(dffits(m))
boxplot(dffits(m))
summary(dffits(m))
2*sqrt((dim(baza)[2])/dim(baza)[1])
length(which( abs(dffits(m))>2*sqrt((dim(baza)[2])/dim(baza)[1]))) #6

cooks.distance(m)
plot(cooks.distance(m),ylab='Cookova udaljenost',cex = 1, pch = 19, col = adjustcolor(4, .4), cex.labels = 2, 
     font.labels = 2)
boxplot(cooks.distance(m),col="azure3")
qf(0.5,dim(baza)[2],dim(baza)[1]-dim(baza)[2]) 
which(cooks.distance(m)>qf(0.5,dim(baza)[2],dim(baza)[1]-dim(baza)[2]))
which( cooks.distance(m)==max(cooks.distance(m)))
baza[6,]
summary(m)
confint(m)
