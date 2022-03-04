
# Homework 10 Solution
# WDB
# due 11/15/2021


################### HOW I GENERATED THE DATA #####################


n <- 100

set.seed(1)
x1 <- rnorm(n, 4, 5)
x2 <- .3*x1 + rnorm(n, 2, 1.5)
x3 <- .4*x2 + rnorm(n, -3, 2)
x4 <- rnorm(n, 3, 1.2)
x5 <- rnorm(n, 6, 1)

y <- exp(2 + .5*x2*x5 + 3*(x4^2) + rnorm(n, sd=.5))

z <- data.frame(x1, x2, x3, x4, x5, y)
z <- round(z, 3)

write.csv(z, "simulated.csv", row.names=FALSE)





####################### SOLUTION ############################



# Are any transformations called for?
boxplot(z$x1) # fine
boxplot(z$x2) # fine
boxplot(z$x3) # fine
boxplot(z$x4) # fine
boxplot(z$x5) # fine
boxplot(z$y) # strongly right-skewed
boxplot(log(z$y)) # beautiful, so let's use log(y) instead of y as our response
z$logy <- log(z$y)

attach(z)

# Look at bottom row of pairs plot to determine most predictive explanatory variable
pairs(z[, -6])
# x4 is easily the most predictive

# Try a line to fit x4
plot(x4, logy)
fit1 <- lm(logy ~ x4)
abline(fit1, col=2)

# Look for a pattern remaining in the residuals
plot(x4, fit1$residuals)
abline(h=0, lty=2)
# obvious curved pattern - likely a quadratic would have worked better

# Try a quadratic least-squares fit
fit1 <- lm(logy ~ x4 + I(x4^2))
grid <- seq(min(x4), max(x4), length.out=100)
f <- fit1$coefficients[1] + fit1$coefficients[2]*grid +fit1$coefficients[3]*grid^2
plot(x4, logy)
lines(grid, f, col=2)

# Look for pattern remaining in residuals
plot(x4, fit1$residuals)
abline(h=0, lty=2)
# no obvious pattern
# (can create null line-up to confirm)

summary(fit1)
# indicates that the first-order term isn't needed

# Redo fit without first-order term
fit1 <- lm(logy ~ I(x4^2))

# Look at fit curve
grid <- seq(min(x4), max(x4), length.out=100)
f <- fit1$coefficients[1] + fit1$coefficients[2]*grid^2
plot(x4, logy)
lines(grid, f, col=2)

# Look for pattern remaining in residuals
plot(x4, fit1$residuals)
abline(h=0, lty=2)
# no obvious pattern

summary(fit1)

# Plot residuals against remaining variables to see if they are predictive
plot(x1, fit1$residuals)
plot(x2, fit1$residuals)
plot(x3, fit1$residuals)
plot(x5, fit1$residuals)
# x2 is most predictive of the residuals

# Try a line to fit x2
plot(x2, fit1$residuals)
fit2 <- lm(fit1$residuals ~ x2)
abline(fit2, col=2)

# Look for a pattern remaining in the residuals
# to see if the line captured the relationship
plot(x2, fit2$residuals)
abline(h=0, lty=2)
# no predictive pattern (just increasing noise variance)

# Redo fit of all terms together
fit2 <- lm(logy ~ x2 + I(x4^2))

summary(fit2)

# Plot residuals against remaining variables to see if they are predictive
plot(x1, fit2$residuals)
plot(x3, fit2$residuals)
plot(x5, fit2$residuals)
# x5 is most predictive

# Try a line to fit x5
plot(x5, fit2$residuals)
fit3 <- lm(fit2$residuals ~ x5)
abline(fit3, col=2)

# Look for a pattern remaining in the residuals
# to see if the line captured the relationship
plot(x5, fit3$residuals)
abline(h=0, lty=2)
# no obvious predictive pattern

# Redo fit of all terms together
fit3 <- lm(logy ~ x2 + I(x4^2) + x5)

summary(fit3)

# Plot residuals against remaining variables to see if they are predictive
plot(x1, fit3$residuals)
plot(x3, fit3$residuals)
# no clear patterns

# Check for interactions among our terms
plot(x2*x4^2, fit3$residuals) # no apparent pattern
plot(x2*x5, fit3$residuals) # looks like a quadratic relationship
plot(x4^2*x5, fit3$residuals) # no apparent pattern

# Try a quadratic fit for x2*x5
fit4 <- lm(fit3$residuals ~ I(x2*x5) + I(x2^2*x5^2))
grid <- seq(min(x2*x5), max(x2*x5), length.out=100)
f <- fit4$coefficients[1] + fit4$coefficients[2]*grid +fit4$coefficients[3]*grid^2
plot(x2*x5, fit3$residuals)
lines(grid, f, col=2)

summary(fit4)
# both terms are valuable

# Bring those two additional terms into our overall model
fit4 <- lm(logy ~ x2 +  I(x4^2) + x5 + I(x2*x5) + I(x2^2*x5^2))
summary(fit4)
# indicates several terms my be unnecessary

# drop out the "least significant" which is x2
fit4 <- lm(logy ~ I(x4^2) + x5 + I(x2*x5) + I(x2^2*x5^2))
summary(fit4)

# drop out the "least significant" which is x5
fit4 <- lm(logy ~ I(x4^2) + I(x2*x5) + I(x2^2*x5^2))
summary(fit4)

# drop out the "least significant" which is x2^2*x5^2
fit4 <- lm(logy ~ I(x4^2) + I(x2*x5))
summary(fit4)

# now every term is a keeper.
# The final residuals look Normal:
boxplot(fit4$residuals)

# so here's my guess:
# log(y) = 2.1 + 3.0*x4^2 + .50*x2*x5 + error
# where the error is Normal with standard deviation about .5

