# Install dependencies
# install.packages("googlesheets")

### Import Google Sheet
setwd("~/Documents/QSProjects")
data <- read.csv("Habit Tracker - Personal - DailyHabits.csv")


### Normalize data

# Get ride of extra columns/rows
data <- my.sheet.df
data <- my.sheet.df[1:51,2:4]

# Turn Y's in to 1's and N's into 0's
data$Exercise[data$Exercise == "Y"] <- 1
data$Exercise[data$Exercise == "N"] <- 0

data$Meditate[data$Meditate == "Y"] <- 1
data$Meditate[data$Meditate == "N"] <- 0

data$Sleep.8.Hours[data$Sleep.8.Hours == "Y"] <- 1
data$Sleep.8.Hours[data$Sleep.8.Hours == "N"] <- 0

# Convert char columns to numeric columns
for(i in 1:3) {
  data[,i] <- as.numeric(unlist(data[,i]))
}


### Create Logistic Regression Model

the.model <- glm(formula=Exercise ~ Meditate + Sleep.8.Hours,
                 data=data,
                 family=binomial)


### Test the model

# Make an example to predict
new.data1 <- data.frame(Meditate=0, Sleep.8.Hours=0)
new.data2 <- data.frame(Meditate=0, Sleep.8.Hours=1)
new.data3 <- data.frame(Meditate=1, Sleep.8.Hours=0)
new.data4 <- data.frame(Meditate=1, Sleep.8.Hours=1)

# Predict the outcome (output is probability of a "1")
predict(the.model, new.data1, type="response")
predict(the.model, new.data2, type="response")
predict(the.model, new.data3, type="response")
predict(the.model, new.data4, type="response")


# length(which(data$Exercise == 1 & data$Meditate == 0 & data$Sleep.8.Hours == 0))
# length(which(data$Exercise == 1 & data$Meditate == 0 & data$Sleep.8.Hours == 1))
# length(which(data$Exercise == 1 & data$Meditate == 1 & data$Sleep.8.Hours == 0))
# length(which(data$Exercise == 1 & data$Meditate == 1 & data$Sleep.8.Hours == 1))
# 
# length(which(data$Exercise == 0 & data$Meditate == 0 & data$Sleep.8.Hours == 0))




### With variables as factors instead of numeric
data.factors <- data

# Convert char columns to numeric columns
for(i in 1:3) {
  data.factors[,i] <- as.factor(unlist(data[,i]))
}


### Create Logistic Regression Model

the.model.factors <- glm(formula=Exercise ~ Meditate + Sleep.8.Hours,
                         data=data.factors,
                         family=binomial)

summary(the.model)
summary(the.model.factors)
