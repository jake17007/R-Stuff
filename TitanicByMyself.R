setwd("~/Documents/MachineLearningProjects")

titanic.data <- read.csv("TitanicTrain.csv", na.strings = c("","NA"))
titanic.testdata <- read.csv("TitanicTest.csv", na.strings = c("","NA"))

### Assign proper datatypes to factors (test and train data)

str(titanic.data)
titanic.data$Survived <- as.factor(titanic.data$Survived)
titanic.data$Pclass <- as.factor(titanic.data$Pclass)
titanic.data$Name <- as.character(titanic.data$Name)
titanic.data$SibSp <- as.factor(titanic.data$SibSp)
titanic.data$Parch <- as.factor(titanic.data$Parch)
str(titanic.data)

str(titanic.testdata)
titanic.testdata$Pclass <- as.factor(titanic.testdata$Pclass)
titanic.testdata$Name <- as.character(titanic.testdata$Name)
titanic.testdata$SibSp <- as.factor(titanic.testdata$SibSp)
titanic.testdata$Parch <- as.factor(titanic.testdata$Parch)
str(titanic.testdata)

##################################        Title       #######################################
### Create a "Title" variable by parsing the title from the "Name" variable. This will be
### usefull for deducing male or female, perhaps young or old in the absence of age, and
### other inferences.
###

require(stringr)
#-------------------------------      Extract Title     -------------------------------------

### extractTitle( df, col.index ) -
### This function parses the title from the "Name" variable
### INPUTS:   Name - use the exact "Name" value (i.e. "titanic.data[i,"Name"])
### OUTPUTS:  cabin.letter -  returns the title ("Miss.", "Mr.", "Master.", etc.)

extractTitle <- function(Name) {
  name <- as.character(Name)
  
  if (length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

# Loop through all the values of "Name" and store the results in "Titles"
Titles <- NULL
for (i in 1:nrow(titanic.data)) {
  Titles <- c(Titles, extractTitle(titanic.data[i, "Name"]))
}
titanic.data$Title <- as.factor(Titles)

# ##################################        Look for Missing Values       #######################################
# ###
# ###                                          (** Do Not Run **)
# 
# # How many missing values are in the columns that obvsly have a lot of missing data
# sum(is.na(titanic.data$Cabin)) 
# sum(is.na(titanic.data$Cabin))/nrow(titanic.data) # As a percentage
# 
# sum(is.na(titanic.data[,]))
# sum(is.na(titanic.data$PassengerId))
# sum(is.na(titanic.data$Survived))
# sum(is.na(titanic.data$Pclass))
# sum(is.na(titanic.data$Name))
# sum(is.na(titanic.data$Sex))
# sum(is.na(titanic.data$Age))
# sum(is.na(titanic.data$SibSp))
# sum(is.na(titanic.data$Parch))
# sum(is.na(titanic.data$Ticket))
# sum(is.na(titanic.data$Fare))
# sum(is.na(titanic.data$Cabin))
# sum(is.na(titanic.data$Embarked))
# sum(is.na(titanic.data$Title))
# 
# # So there's a large number of missing data for Age (NA=177) and 
# # Cabin (NA=687). Therefore, we want to find what other vars
# # predict whether either of those vars are missing or not.
# 
# # So first looking at Age, we'll make a binary factor column
# # indicating missing or not
# 
# 
# 
# age.missing <- NULL
# for (i in 1:nrow(titanic.data)) {
#   if (is.na(titanic.data$Age[i])) {
#     age.missing <- c(age.missing, 1)
#   } else {
#     age.missing <- c(age.missing, 0)
#   } 
# }
# titanic.data$age.missing <- as.factor(age.missing)
# 
# # Do cross tabs analysis on non-numerical data
# cross.tabs.results <- xtabs(titanic.data$age.missing
#                             ~titanic.data$Pclass,
#                             data = titanic.data)
# summary(cross.tabs.results)
# 
# # What percentage of each category in Title (Miss., Mrs., Mr., Master., Other) are missing,
# # and what percentage are not-missing *Remember, missing=1, not-missing=0
# 
# find.sum.of.missing <- function(column, specific.category, data.frame) {
#   sum <- 0
#   i <- 1
#   for (i in 1:nrow(data.frame)) {
#     if (column[i] == specific.category) {
#       if (titanic.data$age.missing[i] == 2) {
#         sum = sum + 1
#       }
#     }
#   }
#   sum
# }
# 
# # Create a table comparing the two columns
# 
# title.with.age.missing <- data.frame(c(summary(titanic.data$age.missing)))
# 
# summary(titanic.data$Title)
# 
# find.sum.of.missing(titanic.data$Title, "Miss.", titanic.data)
# title.with.age.missing$Miss <- c(182-36,36)
# 
# find.sum.of.missing(titanic.data$Title, "Mrs.", titanic.data)
# title.with.age.missing$Mrs <- c(127-17,17)
# 
# find.sum.of.missing(titanic.data$Title, "Mr.", titanic.data)
# title.with.age.missing$Mr <- c(518-119,119)
# 
# find.sum.of.missing(titanic.data$Title, "Master.", titanic.data)
# title.with.age.missing$Master <- c(40-4,4)
# 
# find.sum.of.missing(titanic.data$Title, "Other", titanic.data)
# title.with.age.missing$Other <- c(24-1,1)
# 
# # Create a proportion between the variables
# data.frame(title.with.age.missing)
# chisq.test(title.with.age.missing)
# 
# sum.of.missing.Title.Miss <- 
# 
# 
# for (i in 1:length(expected)) {
#   total <- 0
#   current <- ((observed[i] - expected[i])^2)/expected[i]
#   total <- total+current
# }
# total
# 
# expected <- c(.04,.2,.58,.14,.03) 
# observed <- c(.02,.2,.67,.1,.005)
# 
# chisq.test(expected, observed)
# 
# titanic.data$age.missing <-as.factor(titanic.data$age.missing)
# chisq.test(titanic.data[,c(2,14)])
# titanic.data[1:3,c(2,14)]
# 
# # See correlations between numeric data
# titanic.data$age.missing <-as.integer(titanic.data$age.missing)
# cor(titanic.data[c(1,6,7,8,10,14)])
# 
# pairs(titanic.data,-titanic.data$Names)
# 
# require(corrplot)
# temp.for.corrplot <- cor(titanic.data[c(1,6,7,8,10,14)])
# corrplot(temp.for.corrplot, method = "square")
# 
# # Perhaps use other datasets to account for missing data such as cabin maps,
# # other known statistics about age on the Titanic, ...
# 
# ### Create a logistic regression model
# 
# glm.fit <- glm(titanic.data$Survived~.
#                -titanic.data$PassengerId
#                -titanic.data$Name
#                -titanic.data$Ticket,
#                data = titanic.data,
#                family = binomial,
#                na.action)
# 
# summary(glm.fit)
# 
# glm.probs <- predict(glm.fit, newdata = )
# 
# 
# #### Create a test data frame
# x <- c("B1", "A2", "G20 G21", "F5", "A1 B2", "", "T20")
# x <- data.frame(x)
# x[x == ""] <- NA
# x$x <- as.character(x$x)
# 
# require(stringr)


##################################        Cabin       #######################################
### Make the "Cabin" variable more useful by separating parsing the letters and numbers 
### and labeling when multiple cabins are attributed to a single passenger
###

#-------------------------------Extract Cabin Letter---------------------------------------

### extractCabinLetter( df, col.index ) -
### This function parses the letters from character values for all the rows of a
### given the column indicated by the user.
### INPUTS:   df - The name of the dataframe being used (do not indicate the column with '$')
###           col.index - The index number of the column the user would like to parse
### OUTPUTS:  cabin.letter -  an vector of character values with length of the size of the 
###                           dataframe; concatenates letters in one value; accounts for NA's
###                           by putting NA

extractCabinLetters <- function(df, col.index) {
  cabin.letter <- NULL
  
  for (i in 1:nrow(df)) {

    if (is.na(df[i, col.index])) {
      cabin.letter[i] <- NA
    } else {
      for (j in 1:26) {
        
        if (length(df[grep(LETTERS[j], df[i, col.index]), col.index]) > 0) {
          if (is.null(cabin.letter[i])) {
            cabin.letter[i] <- LETTERS[j]
          } else if (is.na(cabin.letter[i])){
            cabin.letter[i] <- LETTERS[j]
          } else {
          cabin.letter[i] <- paste(cabin.letter[i], LETTERS[j], sep="")
          }
        }
        
      }
    }
    
  }
  
  return (cabin.letter)
}

### End Function


x.letters <- extractCabinLetters(x,1)
cabin.letters <- data.frame(extractCabinLetters(titanic.data,11))
summary(cabin.letters)

length(titanic.data$Cabin)
other.letters <- data.frame(cabin.letters[which(cabin.letters != "A" &
                                                cabin.letters != "B" &
                                                cabin.letters != "C" &
                                                cabin.letters != "D" &
                                                cabin.letters != "E"),1])

# CabinLetters <- NULL
# for (i in 1:nrow(x)) {
#   CabinLetters <- c(CabinLetters, extractCabinLetters(x))
# }
# x$CabinLetters <- as.factor(CabinLetters)
# 
# debug(extractCabinLetters)


#############################################################################################
# To remove everything created in this R Script use: 
rm(cabin.letters, other.letters, titanic.data, titanic.testdata, other.cabins, Titles, x,
   x.letters, extractCabinLetters, extractTitle, title.with.age.missing, age.missing,
   CabinLetters, expected, observed, find.sum.of.missing, i)