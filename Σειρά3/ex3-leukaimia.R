library(data.table)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
library(asbio)
library(SciViews)
library(hnp)
library(pROC)
rm(list = ls())

leukaimia <- fread("leukaemia.txt")
mouse<-fread("mouse-trials.txt")
attach(mouse)
sf <- cbind(mouse$Events, mouse$Trials-Events)

model<- glm(response ~ age+smear+infiltrate+index+blasts+temperature, data = leukaimia,family=binomial)

model1<- glm(response ~ age+smear+infiltrate+log(index)+blasts+temperature, data = leukaimia,family=binomial)

model2<- glm(response ~ log(age)+smear+infiltrate+index+blasts+temperature, data = leukaimia,family=binomial)

model3<- glm(response ~ age+log(smear)+infiltrate+index+blasts+temperature, data = leukaimia,family=binomial)

model4<- glm(response ~ age+smear+log(infiltrate)+index+blasts+temperature, data = leukaimia,family=binomial)

model5<- glm(response ~ age+smear+infiltrate+index+blasts+log(temperature), data = leukaimia,family=binomial)

model6<- glm(response ~ age^2+smear+infiltrate+index+blasts+temperature, data = leukaimia,family=binomial)



anova(model1,mod, test="Chisq")
AIC(model1)
anova(model2,mod, test="Chisq")
AIC(model2)
anova(model3,mod, test="Chisq")
AIC(model2)
anova(model4,mod, test="Chisq")
AIC(model2)
anova(model5,mod, test="Chisq")
AIC(model2)
anova(model6,mod, test="Chisq")
AIC(model2)
step(model,method="backward", test="Chisq")

mo<-glm(formula = response ~ age + infiltrate + index + temperature, family = binomial, data = leukaimia)

summary(mo)
1-pchisq(model$deviance,model$df.residual)
1-pchisq(mo$deviance,mo$df.residual)
residuals(model,type="partial")

partial.resid.plot(model, smooth.span = 0.8)
leukaimia$blasts1=leukaimia$blasts+0.1
leukaimia$temperature1=1/leukaimia$temperature
model<- glm(response ~ age+log(blasts1)+smear+infiltrate+index+temperature, data = leukaimia,family=binomial)
model1<- glm(response ~ age+blasts+smear+infiltrate+index+temperature, data = leukaimia,family=binomial)
summary(model)
crPlots(model)
hnp(rstandard(model1), pch=19)
plot(cooks.distance(model1), pch=19)
plot(hatvalues(model1), pch=19)
plot(rstudent(model1), pch=19)
qt(c(.025, .975), df=45)
confint.default(model)
exp(confint.default(model))
roc(leukaimia$response, fitted.values(model), smooth=TRUE, plot=TRUE) 
