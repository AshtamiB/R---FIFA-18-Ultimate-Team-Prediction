# install packages - do this one time
# install.packages("data.table")
# install.packages("corrplot")
# install.packages("ggplot2")
# install.packages("ggplot")
# install.packages("gcookbook")
# install.packages("caret")
# install.packages("hexbin")
# install.packages("leaps")
# install.packages("plyr")
# install.packages("plotly")
# install.packages("waffle")
# install.packages("dummies")
# install.packages("caTools")
# install.packages("wesanderson")
# install.packages("visreg")
# install.packages("car")
# install.packages("leaps")
# install.packages("MASS")

#Loading dataset
FIFA <- read.csv("C:/Users/Ashtami/Documents/Predictive Analytics/Assignment/FIFA18.csv", stringsAsFactors = F)
Fifa_data <- data.frame(FIFA) 
colnames(Fifa_data)[colnames(Fifa_data)=="ï..Name"] <- "name"
colnames(Fifa_data)[colnames(Fifa_data)=="overall"] <- "target_overall"

Fifa_data$name <- as.factor(Fifa_data$name)
Fifa_data$intl_rep <- as.factor(Fifa_data$intl_rep)
Fifa_data$position <- as.factor(Fifa_data$position)
Fifa_data$pref_foot <- as.factor(Fifa_data$pref_foot)

Fifa_data_categorical <- Fifa_data[c(1,2,3,7,14)]
Fifa_data_numeric <- Fifa_data[c(2,4,5,6,8,9,10,11,12,13)]


#1
summary(Fifa_data)

#2
pairs(~ Fifa_data$target_overall + Fifa_data$age + Fifa_data$height + Fifa_data$weight + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality, Fifa_data)

par(mfrow=c(5,2))
hist(Fifa_data$target_overall,freq=FALSE)
hist(Fifa_data$age,freq=FALSE)
hist(Fifa_data$height,freq=FALSE)
hist(Fifa_data$weight,freq=FALSE)
hist(Fifa_data$pace,freq=FALSE)
hist(Fifa_data$dribbling,freq=FALSE)
hist(Fifa_data$shooting,freq=FALSE)
hist(Fifa_data$passing,freq=FALSE)
hist(Fifa_data$defending,freq=FALSE)
hist(Fifa_data$physicality,freq=FALSE)


#3
round(cor(Fifa_data[,c(2,4,5,6,8,9,10,11,12,13)],use = "complete.obs"),3)

#4
boxplot(Fifa_data$target_overall~Fifa_data$position)
boxplot(Fifa_data$target_overall~Fifa_data$intl_rep)
boxplot(Fifa_data$target_overall~Fifa_data$pref_foot)

#5
fitnull <- lm(Fifa_data$target_overall~ 1 ,data=Fifa_data)
# fitfull <- lm(Fifa_data$targe_overall~ Fifa_data$age + Fifa_data$height + Fifa_data$weight + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality,data=Fifa_data)
fitfull <- lm(Fifa_data$target_overall~ Fifa_data$position + Fifa_data$age + Fifa_data$height + Fifa_data$intl_rep +  Fifa_data$weight + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality + Fifa_data$pref_foot,data=Fifa_data)
summary(fitfull)
model2 <- step(fitnull, scope=list(lower=fitnull, upper=fitfull),
     direction="forward")

summary(model2)

require(ggplot2)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
ggPredict(model2,interactive = TRUE)

#6
lev = hat(model.matrix(model1))
plot(lev)
Fifa_data[lev > 0.2]

library(car)
scatterplotMatrix(~ Fifa_data$target_overall + Fifa_data$age + Fifa_data$height + Fifa_data$weight + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality | Fifa_data$position + Fifa_data$intl_rep , data=Fifa_data , plot.points = FALSE, smooth=FALSE)

scatterplotMatrix(~ Fifa_data$age + Fifa_data$target_overall| Fifa_data$position, plot.points = FALSE, smooth=FALSE)
scatterplotMatrix(~ Fifa_data$age + Fifa_data$target_overall| Fifa_data$intl_rep, plot.points = FALSE, smooth=FALSE)

scatterplotMatrix(~ Fifa_data$target_overall + Fifa_data$height| Fifa_data$intl_rep, plot.points = FALSE, smooth=FALSE)

scatter <- lm(Fifa_data$target_overall~ Fifa_data$position + Fifa_data$age + Fifa_data$height + Fifa_data$intl_rep +  Fifa_data$weight + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality + Fifa_data$pref_foot + Fifa_data$age*Fifa_data$position+ Fifa_data$age*Fifa_data$intl_rep + Fifa_data$height*Fifa_data$position+ Fifa_data$height*Fifa_data$intl_rep + Fifa_data$weight*Fifa_data$position+ Fifa_data$weight*Fifa_data$intl_rep + Fifa_data$pace*Fifa_data$position + Fifa_data$pace*Fifa_data$intl_rep+ Fifa_data$dribbling*Fifa_data$position+ Fifa_data$dribbling*Fifa_data$intl_rep+ Fifa_data$shooting*Fifa_data$position+ Fifa_data$shooting*Fifa_data$intl_rep + Fifa_data$passing*Fifa_data$position+ Fifa_data$passing*Fifa_data$intl_rep + Fifa_data$defending*Fifa_data$position+ Fifa_data$defending*Fifa_data$intl_rep + Fifa_data$physicality*Fifa_data$position+ Fifa_data$physicality*Fifa_data$intl_rep,data=Fifa_data)
summary(scatter)

#removing non-significant terms to handle extreme collinearity
Scatter2 <- lm(Fifa_data$target_overall~ Fifa_data$position + Fifa_data$age + Fifa_data$height + Fifa_data$weight +  Fifa_data$intl_rep + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality + Fifa_data$age*Fifa_data$position+ Fifa_data$age*Fifa_data$intl_rep + Fifa_data$height*Fifa_data$position+ Fifa_data$height*Fifa_data$intl_rep + Fifa_data$weight*Fifa_data$position+ Fifa_data$weight*Fifa_data$intl_rep + Fifa_data$pace*Fifa_data$position + Fifa_data$pace*Fifa_data$intl_rep+ Fifa_data$dribbling*Fifa_data$position+ Fifa_data$dribbling*Fifa_data$intl_rep+ Fifa_data$shooting*Fifa_data$position+ Fifa_data$shooting*Fifa_data$intl_rep + Fifa_data$passing*Fifa_data$position+ Fifa_data$passing*Fifa_data$intl_rep + Fifa_data$defending*Fifa_data$position+ Fifa_data$defending*Fifa_data$intl_rep + Fifa_data$physicality*Fifa_data$position+ Fifa_data$physicality*Fifa_data$intl_rep,data=Fifa_data)
summary.aov(scatter2)
summary(Scatter2)

vif(Scatter2)

#removing weight and rep*defending, rep*physicality
Scatter3 <- lm(Fifa_data$target_overall~ Fifa_data$position + Fifa_data$age + Fifa_data$height +  Fifa_data$intl_rep + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality + Fifa_data$age*Fifa_data$position+ Fifa_data$age*Fifa_data$intl_rep + Fifa_data$height*Fifa_data$position+ Fifa_data$height*Fifa_data$intl_rep + Fifa_data$weight*Fifa_data$position+ Fifa_data$weight*Fifa_data$intl_rep + Fifa_data$pace*Fifa_data$position + Fifa_data$pace*Fifa_data$intl_rep+ Fifa_data$dribbling*Fifa_data$position+ Fifa_data$dribbling*Fifa_data$intl_rep+ Fifa_data$shooting*Fifa_data$position+ Fifa_data$shooting*Fifa_data$intl_rep + Fifa_data$passing*Fifa_data$position+ Fifa_data$passing*Fifa_data$intl_rep + Fifa_data$defending*Fifa_data$position + Fifa_data$physicality*Fifa_data$position,data=Fifa_data)
# scatter3 <- lm(Fifa_data$target_overall~ Fifa_data$position + Fifa_data$age + Fifa_data$height + Fifa_data$intl_rep + Fifa_data$pace + Fifa_data$dribbling + Fifa_data$shooting + Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality + Fifa_data$age*Fifa_data$position+ Fifa_data$age*Fifa_data$intl_rep + Fifa_data$height*Fifa_data$position+ Fifa_data$height*Fifa_data$intl_rep + Fifa_data$weight*Fifa_data$position+ Fifa_data$weight*Fifa_data$intl_rep + Fifa_data$pace*Fifa_data$position + Fifa_data$pace*Fifa_data$intl_rep+ Fifa_data$dribbling*Fifa_data$position+ Fifa_data$dribbling*Fifa_data$intl_rep+ Fifa_data$shooting*Fifa_data$position+ Fifa_data$shooting*Fifa_data$intl_rep + Fifa_data$passing*Fifa_data$position+ Fifa_data$passing*Fifa_data$intl_rep + Fifa_data$defending*Fifa_data$position+ Fifa_data$physicality*Fifa_data$position,data=Fifa_data)
summary.aov(scatter3)
summary(Scatter3)

vif(Scatter3)

#8
#removing age, height, pace, shooting
Scatter4 <- lm(Fifa_data$target_overall~ Fifa_data$position +  Fifa_data$intl_rep + Fifa_data$dribbling+ Fifa_data$passing + Fifa_data$defending + Fifa_data$physicality + Fifa_data$age*Fifa_data$position+ Fifa_data$age*Fifa_data$intl_rep + Fifa_data$height*Fifa_data$position+ Fifa_data$height*Fifa_data$intl_rep + Fifa_data$weight*Fifa_data$position+ Fifa_data$weight*Fifa_data$intl_rep + Fifa_data$pace*Fifa_data$position + Fifa_data$pace*Fifa_data$intl_rep+ Fifa_data$dribbling*Fifa_data$position+ Fifa_data$dribbling*Fifa_data$intl_rep+ Fifa_data$shooting*Fifa_data$position+ Fifa_data$shooting*Fifa_data$intl_rep + Fifa_data$passing*Fifa_data$position+ Fifa_data$passing*Fifa_data$intl_rep + Fifa_data$defending*Fifa_data$position + Fifa_data$physicality*Fifa_data$position,data=Fifa_data)
summary.aov(Scatter4)
summary(Scatter4)
vif(Scatter4)


#9
lev <- hat(model.matrix(Scatter4))
plot(lev)
Fifa_data[lev > 0.9, ]
summary(Fifa_data)

cook = cooks.distance(Scatter4)
plot(cook,ylab="Cooks distances")
points(1310,cook[1310],col= 'red')
points(2654,cook[2654],col= 'red')
which(cook>0.25)
plot(cook,ylab="Cooks distances")
points(2498,cook[2498],col='red')


Fifa_data[cook > 0.25,]

summary(Fifa_data)
#Removing outlier and saving new dataset without that point 2498
Fifa_data2 <- Fifa_data[-2498,]

#regressing again on new data set
Scatter5 <- lm(Fifa_data2$target_overall~ Fifa_data2$position +  Fifa_data2$intl_rep + Fifa_data2$dribbling+ Fifa_data2$passing + Fifa_data2$defending + Fifa_data2$physicality + Fifa_data2$age*Fifa_data2$position+ Fifa_data2$age*Fifa_data2$intl_rep + Fifa_data2$height*Fifa_data2$position+ Fifa_data2$height*Fifa_data2$intl_rep + Fifa_data2$weight*Fifa_data2$position+ Fifa_data2$weight*Fifa_data2$intl_rep + Fifa_data2$pace*Fifa_data2$position + Fifa_data2$pace*Fifa_data2$intl_rep+ Fifa_data2$dribbling*Fifa_data2$position+ Fifa_data2$dribbling*Fifa_data2$intl_rep+ Fifa_data2$shooting*Fifa_data2$position+ Fifa_data2$shooting*Fifa_data2$intl_rep + Fifa_data2$passing*Fifa_data2$position+ Fifa_data2$passing*Fifa_data2$intl_rep + Fifa_data2$defending*Fifa_data2$position + Fifa_data2$physicality*Fifa_data2$position,data=Fifa_data)
summary.aov(Scatter5)
summary(Scatter5)
vif(Scatter5)
#REMOVING INTERACTION rep*passing from model as its having vif close to 10
Scatter5 <- lm(Fifa_data2$target_overall~ Fifa_data2$position +  Fifa_data2$intl_rep + Fifa_data2$dribbling+ Fifa_data2$passing + Fifa_data2$defending + Fifa_data2$physicality + Fifa_data2$age*Fifa_data2$position+ Fifa_data2$age*Fifa_data2$intl_rep + Fifa_data2$height*Fifa_data2$position+ Fifa_data2$height*Fifa_data2$intl_rep + Fifa_data2$weight*Fifa_data2$position+ Fifa_data2$weight*Fifa_data2$intl_rep + Fifa_data2$pace*Fifa_data2$position + Fifa_data2$pace*Fifa_data2$intl_rep+ Fifa_data2$dribbling*Fifa_data2$position+ Fifa_data2$dribbling*Fifa_data2$intl_rep+ Fifa_data2$shooting*Fifa_data2$position+ Fifa_data2$shooting*Fifa_data2$intl_rep + Fifa_data2$passing*Fifa_data2$position + Fifa_data2$defending*Fifa_data2$position + Fifa_data2$physicality*Fifa_data2$position,data=Fifa_data)

#Plotting residual plot against variables
par(mfrow=c(1,1))

plot(Fifa_data2$target_overall, Scatter5$res)
plot(Fifa_data2$dribbling, Scatter5$res)
plot(Fifa_data2$passing, Scatter5$res)
plot(Fifa_data2$defending, Scatter5$res)
plot(Fifa_data2$physicality, Scatter5$res)


#Plot of fitted values vs residual

plot(Scatter5$fitted, Scatter5$res)

#Normal QQ plot
plot(Scatter5)

#Studentized residuals
r = rstudent(Scatter5)
plot(Fifa_data2$target_overall, r)
plot(Fifa_data2$dribbling, r)
plot(Fifa_data2$passing, r)
plot(Fifa_data2$defending, r)
plot(Fifa_data2$physicality, r)

plot(Scatter5$fitted, r)

#Evidence of non-constant variance. Clear funnel shape in the residuals.Check the normality assumption
boxplot(r)
hist(r,freq=FALSE)
lines(density(r))

#Allot of outliers particularly for the positive values indicating that the prediction ^ Yi is underestimating the true price.
qqnorm(r)

qqnorm(rnorm(length(Fifa_data2$target_overall),mean(Fifa_data2$target_overall),sd(Fifa_data2$target_overall)))

#10
summary(Scatter5)


#11
t.test(Fifa_data2$target_overall~ Fifa_data2$intl_rep)

#12
summary.aov(Scatter5)

#13
summary(Scatter5)
AIC(model1)
AIC(model2)
AIC(Scatter4, Scatter5)
