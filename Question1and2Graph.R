# Question 1 and Question 2

setwd("~/Documents/MachineLearningProjects/UCETCodingResearch")

#Import Question 1 Codes and Question 2 Codes
Satisfiers.Jacob <- read.csv("SatisfiersJacob.csv")
Feelings.Affect.Perf.Jacob <- read.csv("FeelingsAffectPerfJacob.csv") 

#Make dataframes for all Satisfier Categories

row1 <- c("a","b","c")
row2 <- c("a")
row3 <- c("b","c")

test.df <- rbind(row1)
