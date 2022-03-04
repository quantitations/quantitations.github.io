
set.seed(1)
x <- 1:5
a <- 5; b <- 3
y <- a + b*x + rnorm(length(x))

plot(x, y, cex=1.5)

# draw the true line that I used to generate the data
abline(a, b, col=6, lty=2); abline(a, b, col=6, lwd=4, lty=2)

# draw the least-squares line
fit1 <- lm(y ~ x)
abline(fit1, col=1, lwd=2)

# draw the least-squares quadratic
fit2 <- lm(y ~ x + I(x^2))
grid <- seq(1, 5, length.out=100)
f2 <- cbind(1, grid, grid^2) %*% fit2$coefficients
lines(grid, f2, col=2, lwd=2)

# draw the least-squares cubic
fit3 <- lm(y ~ x + I(x^2) + I(x^3))
f3 <- cbind(1, grid, grid^2, grid^3) %*% fit3$coefficients
lines(grid, f3, col=3, lwd=2)

# draw the least-squares quartic
fit4 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
f4 <- cbind(1, grid, grid^2, grid^3, grid^4) %*% fit4$coefficients
lines(grid, f4, col=4, lwd=2)


# Same picture zoomed out
plot(x, y, xlim=c(-1, 7), ylim=c(0, 22), cex=1.5)
abline(a, b, col=6, lty=2)
abline(a, b, col=6, lwd=4, lty=2)
abline(fit1, col=1, lwd=2)
grid <- seq(-1, 7, length.out=100)
lines(grid, cbind(1, grid, grid^2) %*% fit2$coefficients, col=2, lwd=2)
lines(grid, cbind(1, grid, grid^2, grid^3) %*% fit3$coefficients, col=3, lwd=2)
lines(grid, cbind(1, grid, grid^2, grid^3, grid^4) %*% fit4$coefficients, col=4, lwd=2)
text(c(1.35), c(19), labels=c("PERFECT FIT !"), col=4, cex=2.5)
text(c(1.25), c(16.5), labels=c("(That's good, right??)"), col=4, cex=1.5)



############################################
############################################

# ITERATIVE FITTING - an informal approach to model selection and fitting data

head(trees)

y <- trees$Volume
x1 <- trees$Girth
x2 <- trees$Height

#######################################
## STEP 1 - greedy selection of explanatory variables
##          and determination of their functional relationships with the response

## Incorporate explanatory variables one-at-a-time.
## - draw every scatterplot
## - bring in the variable that is most predictive (if any)

plot(x1, y)
plot(x2, y)

## In this case, x1 is much more predictive than x2,
## so we'll bring it in first.

## Need to devise a relationship

plot(x1, y)

## try a line

fit1 <- lm(y ~ x1)
abline(fit1, col=2)

## pretty good but perhaps missing a bend in the data...
## The easiest way to visually determine whether you've fully captured the relationship is
## to plot the residuals versus the explanatory variable and look for a remaining pattern...
## But it's easy to be "fooled by randomness"...
## Here's a way to informally test whether the residuals exhibit a pattern:
## NULL LINE-UP - compare the plot to a handful of noise plots - does it stand out?
par(mfrow=c(3, 3), mar=c(1, 1, 1, 1))
answer <- sample(1:9, 1)
for(i in 1:9) {
  if(i == answer) {
    r <- fit1$residuals
    plot(x1, r, col=4, main=i, yaxt="n", xaxt="n")
    abline(h=0, lty=2)
  } else {
    noise <- rnorm(length(y))
    r <- lm(noise ~ x1)$residuals
    plot(x1, r, col=4, main=i, yaxt="n", xaxt="n")
    abline(h=0, lty=2)
  }
}
par(mfrow=c(1, 1), mar=c(5.1, 4.1, 4.1, 2.1))

## Do any of these stand out to you?...

## Which one is the real data?...

answer

## Appears to be a noticable curve remaining in our residuals that likely isn't attributable to noise.
## Let's go back and try a quadratic rather than a line.

fit2 <- lm(y ~ x1 + I(x1^2))
plot(x1, y)
grid <- seq(min(x1), max(x1), length.out=100)
lines(grid, fit2$coefficients[1] + fit2$coefficients[2]*grid + fit2$coefficients[3]*grid^2, col=2)

## Repeat NULL LINE-UP to see if we've captured the apparent pattern

par(mfrow=c(3, 3), mar=c(1, 1, 1, 1))
answer <- sample(1:9, 1)
for(i in 1:9) {
  if(i == answer) {
    r <- fit2$residuals
    plot(x1, r, col=4, main=i, yaxt="n", xaxt="n")
    abline(h=0, lty=2)
  } else {
    noise <- rnorm(length(y))
    r <- lm(noise ~ x1 + I(x1^2))$residuals
    plot(x1, r, col=4, main=i, yaxt="n", xaxt="n")
    abline(h=0, lty=2)
  }
}
par(mfrow=c(1, 1), mar=c(5.1, 4.1, 4.1, 2.1))

## No plot stands out, as far as I'm concerned. No pattern remaining.

answer

## We've determined a relationship between y and x1.
## Next TREAT THE RESIDUALS AS YOUR RESPONSE AND REPEAT THE PROCESS:
## Of the remaining explanatory variables, identify the one that is
## most predictive of the response (if any is).
## In this case, there is only one explanatory variable left...

res <- fit2$residuals
plot(x2, res)

## weak positive relationship, a line seems reasonable

fit3 <- lm(res ~ x2)
abline(fit3, col=2)

## NULL LINE-UP again
## - compare the residual plot to a handful of noise plots - does it stand out?
par(mfrow=c(3, 3), mar=c(1, 1, 1, 1))
answer <- sample(1:9, 1)
for(i in 1:9) {
  if(i == answer) {
    r <- fit3$residuals
    plot(x2, r, col=4, main=i, yaxt="n", xaxt="n")
    abline(h=0, lty=2)
  } else {
    noise <- rnorm(length(y))
    r <- lm(noise ~ x2)$residuals
    plot(x2, r, col=4, main=i, yaxt="n", xaxt="n")
    abline(h=0, lty=2)
  }
}
par(mfrow=c(1, 1), mar=c(5.1, 4.1, 4.1, 2.1))

answer

## No pattern remaining for this variable.

## Go back and create a new overall fit that incorporates all terms thus far

fit4 <- lm(y ~ x1 + I(x1^2) + x2)

## if there were any more explanatory variables remaining, we would see if they predict
## res <- fit4$residuals


#############################
## STEP 2 - remove superfluous terms

## Let's take a look at the significance probabilities of the coefficients of this fit
## to see if any of the terms appear to be unnecessary.
## (The null hypothesis is that the true coefficient's value is 0.
##  How are the significance probabilities calculated?
##  Wait until Chapter 6 when we study Normal errors.)

summary(fit4)


#####################################
## STEP 3 (optional) - check for interactions (products of the available terms)


## I'm going to try x1^2 with x2 first (because x1^2 was more important than x1 based on sig probs)

fit5 <- lm(y ~ x1 + I(x1^2) + x2 + I(x1^2*x2))
summary(fit5)

## Now nothing is significant! The interaction term is causing a great deal of "multicolinearity",
## i.e. the columns of the design matrix are barely linearly independent.
## Let's pare down again, then add back terms in order of importance.
## What's the most important term, based on the sig probs?
## It's the interaction term.

fit6 <- lm(y ~ I(x1^2*x2))
summary(fit6)

## Try adding each remaining term back in, one-at-a-time

summary(lm(y ~ I(x1^2*x2) + x1))
summary(lm(y ~ I(x1^2*x2) + I(x1^2)))
summary(lm(y ~ I(x1^2*x2) + x2))

## None are important in the presence of x1^2*x2.


####################################
## STEP 4 - finalize

## The sig prob indicates that the intercept term is unnecessary as well.
## (Ordinarily I might not bother removing the intercept, but in this case it is sensible
##  based on the nature of these variables -- zero Girth and Height should mean zero Volume)

fit7 <- lm(y ~ I(x1^2*x2) - 1)
summary(fit7)

## What does the scatterplot look like when the response is predicted by x1^2*x2

plot(x1^2*x2, y)

## Draw our fit

abline(0, fit7$coefficients[1], col=2)

## Incredible fit and only one parameter.

## residuals when response is predicted as a linear function of x1^2*x2
plot(x1^2*x2, fit7$residuals)
abline(h=0, lty=2)
## no apparent pattern
boxplot(fit7$residuals) # reasonably Normal

## Here's a reasonable estimate of the function generating the data:
## Volume = .002108 Girth^2 * Height + error
## where error is Normal with mean 0 and standard deviation 2.455

## Thinking about the meaning of the variables, this is intuitively the right model!
## But the iterative fitting process could have led us to this model
## even if we didn't have that kind of foreknowledge about the relationship among the variables.





