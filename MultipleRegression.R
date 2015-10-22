data(USJudgeRatings)
USJudgeRatings[1:5,]

regression <- lm(RTEN ~ CONT + INTG + DMNR + DILG + CFMG + DECI + PREP + FAMI + ORAL + WRIT + PHYS,
                 data = USJudgeRatings)

regression
summary(regression)
