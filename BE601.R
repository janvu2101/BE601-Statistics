#E1. Refer to excel sheet

#E2. Use R to solve
stock <- c(4.0, 14.3, 19.0, -14.7, -26.5, 36.1, 23.8)
bill <- c(6.5, 4.4, 3.8, 6.9, 8.0, 5.8, 4.6)
length(stock) == length(bill)
mean(stock)
mean(bill)
sd(stock)
sd(bill)
summary(stock)
install.packages("tmvnsim")
install.packages("psych")
library(psych)
describe(stock, skew = FALSE, range = FALSE, IQR = FALSE)
describe(bill, fast = TRUE)

#E3.
c1 <- c(1,2,3,4,5,6,7,8)
c2 <- c(1,1,1,1,8,8,8,8)
c3 <- c(1,1,4,4,5,5,8,8)
c4 <- c(-6,-3,0,3,6,9,12,15)
var(c1)
var(c2)
var(c3)
var(c4)

#E4. Solve exercise 52 in Newbold chapter2
# mean = 686, sd = 66
## using Chebyshev's theorem and refer to excel sheet, at least 75% falls in the interval between 554 and 818
## using empirical rule, approximately 95% falls in the interval between 554 and 818.

#E5. recall the five-number summary: min Q1 median Q3 max
df1 <- c(84,77,78,77,80,75,71,63,72,69,70,72,72,72,76,68,76,62,66,70)
df2 <- c(70,70,75,67,73,71,73,67,66,67,66,78,77,61,76,67,65,72,76,70,75,70,79,82,74,70,71,78,66,67)

boxplot(df1,
        main = "Weight of persons of age 75",
        ylab = "Age",
        col = "orange",
        border = "brown",
        horizontal = FALSE,
        notch = FALSE)
boxplot(df1, df2,
        main = "Weight of persons of age 75",
        at = c(1,2),
        names = c("Dataset 1", "Dataset 2"),
        xlab = "Dataset",
        ylab = "Age",
        col = "green",
        border = "darkgreen"
        )
describe(df1, fast = TRUE)
describe(df2, fast = TRUE)

#https://www.datamentor.io/r-programming/box-plot/
