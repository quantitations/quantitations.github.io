################# another one-way ANOVA example #################

library(MASS)
head(survey)

x <- survey[, c("Pulse", "Exer")]
# Change the order of Exer from (Freq, None, Some)
# to (None, Some, Freq)
x$Exer <- factor(x$Exer, levels(x$Exer)[c(2, 3, 1)])
# Drop the observations with missing values
x <- x[complete.cases(x), ]
dim(x)
n <- nrow(x)
k <- length(levels(x$Exer))

boxplot(Pulse ~ Exer, data=x, col=2:(k+1))
m <- mean(x$Pulse)
abline(h=m, lty=2)
means <- aggregate(Pulse ~ Exer, data=x, mean)
points(1:nrow(means), means$Pulse, pch=16)

# Calculate the sum of squares from groups
sum.squares.group <- function(v) {
  ss <- length(v)*(mean(v) - m)^2
  return(ss)
}
SSGs <- aggregate(Pulse ~ Exer, data=x, sum.squares.group)
SSGs
SSG <- sum(SSGs$Pulse)
SSG

# Calculate the sum of squares from residuals
sum.squares.res <- function(v) {
  ss <- sum((v - mean(v))^2)
  return(ss)
}
SSRs <- aggregate(Pulse ~ Exer, data=x, sum.squares.res)
SSRs
SSR <- sum(SSRs$Pulse)
SSR


num <- SSG/(k-1)
denom <- SSR/(n-k)
f <- num/denom
f

p <- 1-pf(f, df1=k-1, df2=n-k)
p

a <- aov(Pulse ~ Exer, data=x)
a
summary(a)

fit <- lm(Pulse ~ Exer, data=x)
summary(fit)
