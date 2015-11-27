# MoodTracker.R
# Author      : Jacob Beauchamp
# Date        : 11/27/2015
# Description : I was curious about my average mood every day, so I began to
# track it. I started by setting up a spreadsheet in Google Sheets and
# logging my emotions at 3 random times during my normal waking/active hours 
# (8 am - 11 pm). I used to Parrott's emotions by groups to identify the 
# emotions I felt when my random alarm would go off. (See https://en.wikipedia.
# org/wiki/Contrasting_and_categorization_of_emotions) I used the Mind Jogger
# iPhone app (https://itunes.apple.com/us/app/mind-jogger/id409841508?mt=8)
# to set those 3 random alarms. I logged the following into columns 
# respectively: date, time (24 hour scale), primary emotion, secondary emotion,
# tertiary emotion, mood level on a 1:5 scale (1 = horrible, 5 = amazing),
# energy level on a 1:5 scale (1 = extremely low, 5 = extremely high).



# Set path to directory of the CSV file
setwd("~/Documents/QSProjects")

# CSV file name
file.name <- "Habit Tracker - Mood Tracker.csv"

# Store a dataframe
moodData <- read.csv(file.name, na.strings="", header=T)

# Change column names to be better suited for R
col.names <- c("date", "time", "primary", "secondary", 
               "terciary", "mood.scale", "energy.scale", 
               "notes")
colnames(moodData)  <- col.names

# Assign appropriate datatypes to columns
moodData$date <- as.Date(moodData$date, "%m/%d/%Y")



##### MAKE PLOTS #####
# (run separately)

# Make a barplot showing frequncy of primary moods
primary.counts <- table(moodData$primary)

primary.counts

par(las=2)
par(mar=c(10,5,3,1))
barplot(primary.counts,
        main="Frequncies of Primary Emotions",
        xlab="Emotion",
        ylab="Frequency")

# Make a barplot showing frequncy of mood levels

mood.scale.counts <- table(moodData$mood.scale)

mood.scale.counts

par(las=1)
par(mar=c(5,5,5,1))
barplot(mood.scale.counts,
        main="Frequncies of Mood Levels",
        xlab="Mood (1 = horrible, 5 = amazaing)",
        ylab="Frequency")

# Make a barplot showing frequncy of energy levels

energy.scale.counts <- table(moodData$energy.scale)

energy.scale.counts

par(las=1)
par(mar=c(5,5,5,1))
barplot(energy.scale.counts,
        main="Frequncies of Energy Levels",
        xlab="Energy (1 = extremely low, 5 = extremely high)",
        ylab="Frequency")
