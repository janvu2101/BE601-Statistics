#Exp 4.4, p#158
P_x <- c(0.15,0.3,0.2,0.2,0.1,0.05)
F_x <- cumsum(P_x)
F_x
auto_sales <- data.frame(x = c(0:5),
                         P_x,
                         F_x)

head(auto_sales)
auto_sales$product <- auto_sales$P_x*auto_sales$x
## Expected value
E <- sum(auto_sales$product)
E
auto_sales$mean_difference_squared_weighted <- (auto_sales$x - E)^2*auto_sales$P_x
##variance
var <- sum(auto_sales$mean_difference_squared_weighted)
var

#