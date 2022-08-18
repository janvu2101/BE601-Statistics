5/25
1/6
3*0.5^3
0.22+0.19+0.26
0.19+0.26+0.040.08+0.12
0.08+0.12+0.22+0.34
1-0.42
#create dataframe
x <- c(0:5)
x
P_x <- c(0.15,0.3,0.2,0.2,0.1,0.05)
F_x <- cumsum(P_x)
F_x
auto_sales <- data.frame(x = c(0:5),
                         P_x = c(0.15,0.3,0.2,0.2,0.1,0.05),
                         F_x)

head(auto_sales)
auto_sales$product <- auto_sales$P_x*auto_sales$F_x
sum(auto_sales$product)
