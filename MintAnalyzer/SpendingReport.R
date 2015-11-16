# Set working directory
setwd("~/Documents/Life Stuff/FINANCES")

# Set name of transaction CSV exported from Mint.com
file.name  <- "transactions7-14-15-to-11-6-15.csv"

# Import CSV of transactions exported from Mint.com
transactions  <- (read.csv (paste("ExportsFromMint/", file.name, sep = "")))

# Get rid of the original transaction description provided by the bank
transactions$Original.Description <- NULL

# ---------------------------------------------------------------------------
# make.period : data.frame Date Date -> data.frame
# Takes a the "transactions" data.frame, a start date, and end date
# to produce a new "transactions" data.frame with the dates between
# and including those specified
# EXAMPLE: df <- make.period(df, "column", "0015-08-20", "0015-10-31")

make.period  <- function(data, col, start.date, end.date){  
  # Conver "Date" column to class type Date
  data[,col] <- as.Date(data[,col], "%m/%d/%Y")
  # Filter relevent dates
  data <- with(data, data[(Date >= start.date & Date <= end.date),])
  
  return(data)
}
# ---------------------------------------------------------------------------

# Make a dataframe for entire semester
transactions <- make.period(transactions, "Date", "0015-08-20", "0015-10-31")

# Make a dataframe for individual months
transactions.sept <- make.period(transactions, "Date", "0015-09-01", "0015-09-30")
transactions.oct <- make.period(transactions, "Date", "0015-10-01", "0015-10-31")

# Make a list of all categories
list.of.categories <- levels(factor(transactions$Category))



# ---------------------------------------------------------------------------
# get.category.totals : -> data.frame
# Produces a vector containing the sums of the amounts spent in the 
# respective categories

get.category.totals <- function(df) {
  
  cat.totals <- NULL
  
  for (i in list.of.categories) {
    cat.totals <- c(category.totals, 
                         sum(df$Amount[which(df$Category == i & 
                                             df$Transaction.Type == "debit")])) 
    return(cat.totals)
  }
}
# ---------------------------------------------------------------------------

#### Make a bar plot of the entire semester "Fall 2015 Semester Spending By Category"

category.totals <- get.category.totals(transactions)

# Create a dataframe out of the list of categories and their respecitve totals
spending.by.cat <- data.frame(list.of.categories, category.totals)
# Sort the dataframe by spending in descending order
spending.by.cat <- spending.by.cat[order(-category.totals),]

# Create a bar plot of the spending by category
counts <- table(spending.by.cat$category.totals)

par(las=2)
par(mar=c(10,1,1,1))
barplot(spending.by.cat.min$category.totals,
        main="Fall 2015 Semester Spending By Category",
        xlab="Category",
        ylab="Spending",
        names.arg=spending.by.cat.min$list.of.categories)

spending.by.cat.min <- spending.by.cat[3:nrow(spending.by.cat),]




### Make a bar plot of spending by different months "Spending by Months"

category.totals.sept <- get.category.totals(transactions.sept)

spending.by.cat.sept <- data.frame(list.of.categories, category.totals.sept)













par(mar=c(1,1,1,1))

amounts

cat.table <- table(spending.by.cat)
cat.table







sum(transactions$Amount[which(transactions$Category == "Credit Card Payment" & transactions$Transaction.Type == "debit")])
sum(transactions$Amount[which(transactions$Category == "Casino Gambling" & transactions$Transaction.Type == "debit")])

category.totals <- NULL

for (i in list.of.categories) {
  category.totals <- c(category.totals, sum(transactions$Amount[which(transactions$Category == i & 
                                                                        transactions$Transaction.Type == "debit")])) 
}

tester.totals <- NULL

for (i in list.of.categories) {
  tester.totals <- c(tester.totals, i)
}





cat.table <- table(transactions$Category, transactions$Amount)
cat.table


with(transactions, transactions[])
