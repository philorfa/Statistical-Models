# Title     : Statistical-Modelling 2
# Created by: philip
# Created on: 16/11/20

library(data.table)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)


cars <- fread("../Statistical-Modeling/vehicles.txt")
n_cars <- cars[,2:12]

model <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data = cars)

correlations <- cor(n_cars)
corrplot(correlations,type="lower")
mdl_vif <- vif(model)
par(mfrow=c(2,2))
plot(model,pch = 19)

plot(hatvalues(model),pch=19)
plot(cooks.distance(model),pch=19)
plot(dffits(model),pch=19)
plot(dfbetas(model),pch=19)

step(model,direction="backward",test="F")
r_deleted <- rstudent(model)
r_standard <- rstandard(model)

min.model <- lm(cars$mpg ~ 1)
fwd.model <- step(min.model, direction='forward',
                  scope=(~ cars$cyl + cars$disp + cars$hp + cars$drat +cars$wt +cars$qsec + cars$vs + cars$am + cars$gear + cars$carb),
                  test="F")

plot()
qqplot(r_deleted,pch=19)
qqline(r_deleted)