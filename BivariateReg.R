# Bivariate regression

data(trees)
trees[1:5,]

hist(trees$Height)
hist(trees$Girth)

plot(trees$Girth, trees$Height)
abline(lm(trees$Height ~ trees$Girth), col="red")

# Linear regression model
regression <- lm(Height ~ Girth, data = trees)
regression
summary(regression)

# Confidence intervals
confint(regression)

predict(regression)
predict(regression, interval = "prediction")
# fit=prediction, lwr=could be as low as, upr=could be as high as

# Regression diagnostics
lm.influence(regression)
influence.measures(regression)
?influence.measures
