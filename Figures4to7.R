# Figure 4

eq = function(u){(1-u^3)^3}
curve(eq, from=0, to=1, xlab="u", ylab="W",main = "Plot of Tricube Function")

# Figure 5

x = c(1,2,3,4,5,4,6,4,6,5,2,5,2,5,2,7,3,7,8,4.5,7,9,7.5,7,8)
y = c(1,2,3,5,3,3,5,6,4,6,4,6,3,5,2,5,5,5,5,6,7,8,7,6,8)
list(x,y) -> abd
plot(x,y,pch=19,main = "Linear Data Trend")
abline(lm(y ~ x,abd), col = "blue")

# Figure 7

eq = function(u){(1-u^2)^2}
curve(eq, from=0, to=1, xlab="u", ylab="B",main = "Plot of Bisquare Function")