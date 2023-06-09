##Introduction to R for Finance


### Part 1: The Basics ####
##Your first R script
# Addition!
3 + 5

# Subtraction!
6-4
# Addition 
2 + 2

##Arithmetic in R (1)
# Subtraction
4 - 1

# Multiplication
3 * 4

# Division
4/2

# Exponentiation
2^4

# Modulo
7 %% 3

##Assignment and variables (1)
# Assign 200 to savings
savings <-200 

# Print the value of savings to the console
savings

###Assignment and variables (2)
# Assign 100 to my_money
my_money <- 100

# Assign 200 to dans_money
dans_money <-200

# Add my_money and dans_money
my_money +dans_money

# Add my_money and dans_money again, save the result to our_money
our_money <- my_money +dans_money

###Financial returns (1)
# Variables for starting_cash and 5% return during January
starting_cash <- 200
jan_ret <- 5
jan_mult <- 1 + (jan_ret / 100)

# How much money do you have at the end of January?
post_jan_cash <- starting_cash * jan_mult

# Print post_jan_cash
post_jan_cash 

# January 10% return multiplier
jan_ret_10 <- 10
jan_mult_10 <- 1 + (jan_ret_10 / 100)

# How much money do you have at the end of January now?
post_jan_cash_10 <- starting_cash * jan_mult_10

# Print post_jan_cash_10
post_jan_cash_10 


###Financial returns (2) RET= return

# Starting cash and returns 
starting_cash <- 200
jan_ret <- 4
feb_ret <- 5

# Multipliers
jan_mult <- 1+(jan_ret/100)
feb_mult <- 1+(feb_ret/100)

# Total cash at the end of the two months
total_cash <- starting_cash * jan_mult * feb_mult

# Print total_cash
total_cash

###Data type exploration
# Apple's stock price is a numeric
apple_stock <- 150.45

# Bond credit ratings are characters
credit_rating <- "AAA"

# You like the stock market. TRUE or FALSE?
my_answer <- TRUE

# Print my_answer
my_answer

### Part 2: Vectors and Matrices ####
##c()ombine

# Another numeric vector
ibm_stock <- c(159.82,160.02,159.84)

# Another character vector
finance <- c("stocks","bonds","investments")

# A logical vector
logic <- c(TRUE, FALSE, TRUE)

##Vector names()
# Vectors of 12 months of returns, and month names
ret <- c(5, 2, 3, 7, 8, 3, 5, 9, 1, 4, 6, 3)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Add names to ret
names(ret) <- months

# Print out ret to see the new names!
ret

##Visualize your vector
# Look at the data
apple_stock <- c(109.49, 109.90, 109.11, 109.95, 111.03, 112.12, 113.95, 113.30, 115.19, 115.19, 115.82, 115.97, 116.64, 116.95, 117.06, 116.29, 116.52, 117.26, 116.76, 116.73, 115.82)

# Plot the data points
plot(apple_stock)

# Plot the data as a line graph
plot(apple_stock, type = "l")

###Weighted average (1)
# Weights and returns
micr_ret <- 7
sony_ret <- 9
micr_weight <- .2
sony_weight <- .8

# Portfolio return
portf_ret <-micr_ret *micr_weight + sony_ret *sony_weight 
portf_ret
##Weighted average (2)
# Weights, returns, and company names
ret <- c(7, 9)
weight <- c(.2, .8)
companies <- c("Microsoft","Sony")

# Assign company names to your vectors
names(ret) <- companies
names(weight) <- companies

# Multiply the returns and weights together 
ret_X_weight <- ret * weight

# Print ret_X_weight
ret_X_weight
## Microsoft      Sony 
##       1.4       7.2

# Sum to get the total portfolio return
portf_ret <- sum(ret_X_weight)

# Print portf_ret
portf_ret
## [1] 8.6

##2.7 Weighted Average (3)
# Print ret
ret
## Microsoft      Sony 
##         7         9

# Assign 1/3 to weight
weight <- 1/3

# Create ret_X_weight
ret_X_weight <- ret * weight

# Calculate your portfolio return
portf_ret <- sum(ret_X_weight)

# Vector of length 3 * Vector of length 2?
ret * c(.2, .6)
## Microsoft      Sony 
##       1.4       5.4


###Vector subsetting
# First 6 months of returns
ret <- c(5, 2, 3, 7, 8, 3, 5, 9, 1, 4, 6, 3)

ret[1:6]

# Just March and May
ret[c("Mar","May")]

# Omit the first month of returns
ret[-1]

###2.9 Create A Matrix!
  # A vector of 9 numbers
  my_vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

# 3x3 matrix
my_matrix <- matrix(data = c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)

# Print my_matrix
my_matrix
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9

# Filling across using byrow = TRUE
matrix(data = c(2, 3, 4, 5), nrow = 2, ncol = 2, byrow = TRUE)
##      [,1] [,2]
## [1,]    2    3
## [2,]    4    5

###2.10 Matrix <- Bind Vectors
apple <- c(109.49, 109.90, 109.11, 109.95, 111.03)
ibm <- c(159.82, 160.02, 159.84, 160.35, 164.79)
micr<-c(59.20,59.25,60.22,59.95,61.37,61.01,61.97,62.17,62.98,62.68,62.58,62.30,63.62,63.54,63.54,63.55,63.24,63.28,62.99,62.90,62.14)

cbind(apple, ibm)

name <- "Apple and IBM"
apple <- c(109.49, 109.90, 109.11, 109.95, 111.03)
ibm <- c(159.82, 160.02, 159.84, 160.35, 164.79)
cor_matrix <- cor(cbind(apple, ibm))


##apple    ibm
## [1,] 109.49 159.82
## [2,] 109.90 160.02
## [3,] 109.11 159.84
##  [4,] 109.95 160.35
##[5,] 111.03 164.79

rbind(apple, ibm)

## [,1]   [,2]   [,3]   [,4]   [,5]
## apple 109.49 109.90 109.11 109.95 111.03
## ibm   159.82 160.02 159.84 160.35 164.79
# cbind the vectors together
cbind_stocks <- cbind (apple, ibm,micr)

# Print cbind_stocks
cbind_stocks

# rbind the vectors together
rbind_stocks <- rbind (apple, ibm,micr)

# Print rbind_stocks
rbind_stocks 


###2.11 Visualize Your Matrix

##apple_micr_matrix ===> Need to build it
##apple  micr
##[1,] 109.49 59.20
##[2,] 109.90 59.25
##[3,] 109.11 60.22
##[4,] 109.95 59.95
##[5,] 111.03 61.37
##[6,] 112.12 61.01
##[7,] 113.95 61.97
##[8,] 113.30 62.17
##[9,] 115.19 62.98
##[10,] 115.19 62.68
##[11,] 115.82 62.58
##[12,] 115.97 62.30
##[13,] 116.64 63.62
##[14,] 116.95 63.54
##[15,] 117.06 63.54
##[16,] 116.29 63.55
##[17,] 116.52 63.24
##[18,] 117.26 63.28
##[19,] 116.76 62.99
##[20,] 116.73 62.90
##[21,] 115.82 62.14



###2.12 cor()relation

# Correlation of Apple and IBM
cor(apple, ibm)

# stock matrix
stocks <- cbind(apple, micr, ibm)

# cor() of all three
cor(stocks)

###2.13 Matrix Subsetting
# Third row
stocks[3, ]

# Fourth and fifth row of the ibm column
stocks[4:5, "ibm"]

# apple and micr columns
stocks[ , c("apple","micr")]

### Part 3: Data Frames ####
##Create your first data.frame()
# Variables
company <- c("A", "A", "A", "B", "B", "B", "B")
cash_flow <- c(1000, 4000, 550, 1500, 1100, 750, 6000)
year <- c(1, 3, 4, 1, 2, 4, 5)

# Data frame
cash <- data.frame(company, cash_flow, year)

# Print cash
cash 


##Making head()s and tail()s of your data with some str()ucture
# Call head() for the first 4 rows
head(cash, n = 4)

# Call tail() for the last 3 rows
tail(cash, n = 3)

# Call str()
str(cash) 

## Naming your columns / rows
# Fix your column names
colnames(cash) <- c("company", "cash_flow","year")

# Print out the column names of cash
colnames(cash)

###Accessing and subsetting data frames (1) 
# Third row, second column
cash[3, 2]

# Fifth row of the "year" column
cash[5,"year" ]

### Accessing and subsetting data frames (2)
# Select the year column
cash$year

# Select the cash_flow column and multiply by 2
cash$cash_flow * 2

# Delete the company column
cash$company <- NULL

# Print cash again
cash

###Accessing and subsetting data frames (3)
# Rows about company B
subset(cash, company == "B")

# Rows with cash flows due in 1 year
subset(cash, year == 1)

###Adding new columns
# Quarter cash flow scenario
cash$quarter_cash <- cash$cash_flow * .25

# Double year scenario
cash$double_year <- cash$year * 2

cash

##Present value of projected cash flows (1)
# Present value of $4000, in 3 years, at 5%
present_value_4k <- 4000 * (1.05) ^ -3

# Present value of all cash flows
cash$present_value <- cash$cash_flow * (1.05) ^ -cash$year

# Print out cash
cash

##Exercise Exercise Present value of projected cash flows (2)
# Total present value of cash
total_pv <- sum(cash$present_value)

# Company B information
cash_B <- subset(cash, company == "B")

# Total present value of cash_B
total_pv_B <- sum(cash_B$present_value)


### Part 4: Factors ####
###Create a factor
# credit_rating character vector
credit_rating <- c("BB", "AAA", "AA", "CCC", "AA", "AAA", "B", "BB")

# Create a factor from credit_rating
credit_factor <- factor(credit_rating )

# Print out your new factor

credit_factor
# Call str() on credit_rating
str(credit_rating)

# Call str() on credit_factor
str(credit_factor)

###Factor levels

# Identify unique levels
levels(credit_factor)

# Rename the levels of credit_factor
levels(credit_factor) <- c("2A", "3A", "1B", "2B", "3C")

# Print credit_factor
credit_factor

###Factor summary
# Summarize the character vector, credit_rating

summary(credit_rating)
# Summarize the factor, credit_factor
summary(credit_factor)

###Visualize your factor
# Visualize your factor!
plot(credit_factor)






# Create an ordered factor
credit_factor_ordered <- factor(credit_rating, ordered = TRUE, levels = c("BB","AAA","AA", "CCC","B" ))

# Plot credit_factor_ordered
plot (credit_factor_ordered)


###Subsetting a factor
# Remove the A bonds at positions 3 and 7. Don't drop the A level.
keep_level <- credit_factor[-c(3,7)]

# Plot keep_level
plot (keep_level)

# Remove the A bonds at positions 3 and 7. Drop the A level.
drop_level <-credit_factor[-c(3,7),drop = TRUE]

# Plot drop_level
plot(drop_level)

###stringsAsFactors
# Variables
credit_rating <- c("AAA", "A", "BB")
bond_owners <- c("Dan", "Tom", "Joe")

# Create the data frame of character vectors, bonds
bonds <-data.frame(credit_rating, bond_owners ,stringsAsFactors = FALSE)

# Use str() on bonds
str(bonds)

# Create a factor column in bonds called credit_factor from credit_rating
bonds$credit_factor <- factor(bonds$credit_rating, ordered =TRUE, levels = c("AAA","A","BB"))

# Use str() on bonds again
str(bonds)


### Part 5: Lists ####

##Create a list
# List components
name <- "Apple and IBM"
apple <- c(109.49, 109.90, 109.11, 109.95, 111.03)
ibm <- c(159.82, 160.02, 159.84, 160.35, 164.79)
cor_matrix <- cor(cbind(apple, ibm))

# Create a list
portfolio <- list (name, apple, ibm, cor_matrix)

# View your first list
portfolio

###Named lists
# Add names to your portfolio
names(portfolio)<- c("portfolio_name", "apple", "ibm", "correlation")

# Print portfolio
portfolio

###Access elements in a list
# Second and third elements of portfolio

portfolio [c(2,3)]
# Use $ to get the correlation data
portfolio$correlation 

###Adding to a list
# Add weight: 20% Apple, 80% IBM
portfolio$weight <- c(apple = 0.2, ibm = 0.8)

# Print portfolio
portfolio

# Change the weight variable: 30% Apple, 70% IBM
portfolio$weight <- c(apple = 0.3, ibm = 0.7)

# Print portfolio to see the changes
portfolio

##Removing from a list
# Take a look at portfolio
portfolio

# Remove the microsoft stock prices from your portfolio
portfolio$microsoft <-NULL

## Split it
# Define grouping from year
grouping <- cash$year

# Split cash on your new grouping
split_cash <- split(cash, grouping)

# Look at your split_cash list
split_cash 

# Unsplit split_cash to get the original data back.
original_cash <- unsplit(split_cash, grouping)

# Print original_cash
original_cash

###Split-Apply-Combine
# Print split_cash
split_cash

# Print the cash_flow column of B in split_cash
split_cash$B$cash_flow

# Set the cash_flow column of company A in split_cash to 0
split_cash$A$cash_flow <- 0

# Use the grouping to unsplit split_cash
cash_no_A <- unsplit(split_cash,grouping)

# Print cash_no_A
cash_no_A

####Attributes
# my_matrix and my_factor
my_matrix <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)
rownames(my_matrix) <- c("Row1", "Row2")
colnames(my_matrix) <- c("Col1", "Col2", "Col3")

my_factor <- factor(c("A", "A", "B"), ordered = T, levels = c("A", "B"))

my_matrix
# attributes of my_matrix
attributes(my_matrix)

# Just the dim attribute of my_matrix
attr(my_matrix, which ="dim")

# attributes of my_factor
attributes(my_factor)










