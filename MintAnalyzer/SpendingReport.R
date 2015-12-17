# Set working directory
setwd("~/Documents/Life Stuff/FINANCES")

# Set name of transaction CSV exported from Mint.com
file.name  <- "transactions7-14-15-to-12-16-15.csv"

# Import CSV of transactions exported from Mint.com
transactions  <- (read.csv (paste("ExportsFromMint/", file.name, sep = "")))

# Get rid of the original transaction description provided by the bank
transactions$Original.Description <- NULL



# ---------------------------------------------------------------------------
# make.period : data.frame Date Date -> data.frame
# Takes the "transactions" data.frame, a start date, and end date
# to produce a new "transactions" data.frame with the dates between
# and including those specified
# EXAMPLE: df <- make.period(df, "column", "0015-08-20", "0015-10-31")

make.period  <- function(df, col, start.date, end.date){  
  # Conver "Date" column to class type Date
  df[,col] <- as.Date(df[,col], "%m/%d/%Y")
  # Filter relevent dates
  df <- with(df, df[(Date >= start.date & Date <= end.date),])
  
  return(df)
}
# ---------------------------------------------------------------------------



# Make a dataframe for entire semester
transactions <- make.period(transactions, "Date", "2015-08-20", "2015-12-16")

# Make a list of all categories
list.of.categories <- levels(factor(transactions$Category))



# ---------------------------------------------------------------------------
# get.category.totals : data.frame -> data.frame
# Produces a vector containing the sums of the amounts spent in the 
# respective categories

get.category.totals <- function(df) {
  
  cat.totals <- NULL
  
  for (i in list.of.categories) {
    cat.totals <- c(cat.totals, 
                         sum(df[,"Amount"][which(df[,"Category"] == i & 
                                             df[,"Transaction.Type"] == "debit")])) 
  }
  
  return(cat.totals)
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
par(mar=c(10,5,1,1))
barplot(spending.by.cat$category.totals,
        main="Fall 2015 Semester Spending By Category",
        xlab="Category",
        ylab="Spending",
        names.arg=spending.by.cat$list.of.categories)


#######

# Remove unneeded categories of spending
spending.by.cat.min <- spending.by.cat[2:nrow(spending.by.cat),]

# Plot again
par(las=2)
par(mar=c(10,5,1,1))
barplot(spending.by.cat.min$category.totals,
        main="Fall 2015 Semester Spending By Category",
        xlab="Category",
        ylab="Spending",
        names.arg=spending.by.cat.min$list.of.categories)







### *********************************************************************************

### Make a bar plot of spending by different months "Spending by Months"

# Make a transactions dataframe for individual months
transactions.sept <- make.period(transactions, "Date", "2015-10-01", "2015-10-31")
transactions.oct <- make.period(transactions, "Date", "2015-11-01", "2015-11-30")

# Get category spending totals for individual months
# - September
category.totals.sept <- get.category.totals(transactions.sept)
# - October
category.totals.oct <- get.category.totals(transactions.oct)

# Make a dataframe combining months
combined.months <- data.frame(September = category.totals.sept, 
                              October = category.totals.oct,
                              row.names = list.of.categories)

# Make a column to be used for labels
combined.months$Category <- row.names(combined.months)

# Bring data to long format needed for ggplot
require(reshape2)
combined.months.long <- melt(combined.months, value.name="Spending", variable.name="Month", na.rm=TRUE)

# plot and facet by categories
require(ggplot2)
qplot( data=combined.months.long, x = Month, y = Spending, geom="bar", stat = "identity" ) + facet_wrap( "Category" )



######

# Remove unneeded categories of spending
to.be.removed <- which(combined.months.long$Category == "Mortgage & Rent")
combined.months.long.min <- combined.months.long[-to.be.removed,]
# Plot again
qplot( data=combined.months.long.min, x = Month, y = Spending, geom="bar", stat = "identity" ) + facet_wrap( "Category" )

