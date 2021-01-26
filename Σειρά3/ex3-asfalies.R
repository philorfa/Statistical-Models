library(data.table)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)

rm(list = ls())

asfalies <- fread("asfalies.txt")

model <- glm(y ~ factor(cartype)+agecat+district+offset(log(n)), data = asfalies,family=poisson)
summary(model)
step(model,method="backward", test="Chisq")

mod1<-glm(y ~ factor(cartype)+district+offset(log(n)), data = asfalies,family=poisson)

mod2<-glm(y ~ factor(cartype)+agecat+offset(log(n)), data = asfalies,family=poisson)

mod3<-glm(y ~ agecat+district+offset(log(n)), data = asfalies,family=poisson)


summary(mod1)
anova(mod1,model,test="Chisq")
AIC(mod1)

summary(mod2)
anova(mod2,model,test="Chisq")
AIC(mod2)

summary(mod3)
anova(mod3,model,test="Chisq")
AIC(mod3)

1-pchisq(model$deviance,model$df.residual)

confint.default(model)
exp(confint.default(model))

r.pears<-residuals(model,type="pearson")
r.dev<-residuals(model)

qqnorm(r.pears)
qqline(r.pears)

plot(fitted.values(model),r.dev,xlab='fitted values', ylab='Deviance residuals')
abline(h=0)
plot(cooks.distance(model), pch=19)
plot(hatvalues(model), pch=19)
plot(rstudent(model), pch=19)
x_dt <- seq(- 5, 5, by = 0.01)

# Applying the dt() function
y_dt <- dt(x_dt, df = 27)

# Plotting 
plot(y_dt, type = "l", main = "t-distribution density function example", las=1)
qt(c(.025, .975), df=27)
