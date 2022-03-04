# PRINCIPAL COMPONENTS ANALYSIS
# StatLab workshop 6/5/2020
# W. D. Brinda





# An observation e.g. x = (2, 4.5, -3) can be thought of as a linear combination
# of the "canonical" basis vectors:
# x = (2)*(1, 0, 0) + (4.5)*(0, 1, 0) + (-3)*(0, 0, 1)

# However, x can be easily expressed with respect to any orthonormal basis for R^3.
# If u1, u2, u3 are orthonormal vectors, then
# x = <x,u1>*u1 + <x,u2>*u2 + <x,u3>*u3

# The dot products <x,u1>, <x,u2>, and <x,u3> are the coordinates of x
# with respect to the basis u1, u2, u3.

# With n observations x1,..., xn in R^d, you could construct a matrix of such
# coordinates to represent all of the observations with respect to u1,... , ud

# <x1,u1>  <x1,u2> ... <x1,ud>
# <x2,u1>  <x2,u2> ... <x2,ud>
#    .        .    .      .
#    .        .     .     .
#    .        .      .    .
# <xn,u1>  <xn,u2> ... <xn,ud>

# An interesting fact: the sum of the variances of the variables (columns)
# is the same no matter which orthonormal basis you use.

# PCA requires finding an orthonormal basis for R^d such that the
# variance of the first coordinate is as large as possible,
# the variance of the second coordinate is as large as possible among
# all directions orthogonal to the first, and so on.

# The following chunks of R code will generate plots to help us visualize this.
# DON'T WORRY ABOUT THE DETAILS OF THE CODE FOR NOW - JUST RUN IT.

# simulating a cloud of data points and calculating coords w.r.t. alt basis
library(MASS)
n <- 40
x <- rnorm(n, mean=10, sd=4)
y <- -16 + 1.5*x + rnorm(n, sd=3)
X <- cbind(x, y)
xbar <- apply(X, 2, mean)
X.cent <- sweep(X, 2, xbar)
s <- svd(X.cent/sqrt(n))
b1 <- s$v[2, 1]/s$v[1, 1]
b2 <- s$v[2, 2]/s$v[1, 2]
X.pc <- X %*% s$v
X.pc1 <- matrix(NA, nrow=n, ncol=2)
for(i in 1:n) {
  X.pc1[i, ] <- X.pc[i, 1]*s$v[, 1]
}
X.pc2 <- matrix(NA, nrow=n, ncol=2)
for(i in 1:n) {
  X.pc2[i, ] <- X.pc[i, 2]*s$v[, 2]
}
X.all <- rbind(X, X.cent, X.pc1, X.pc2)

# original points
eqscplot(X.all, type="n", xlab="x", ylab="y")
abline(h=0, lty=3); abline(v=0, lty=3)
points(X, cex=1.5)

# original points with their coordinates
eqscplot(X.all, type="n", xlab="x", ylab="y")
abline(h=0, lty=3); abline(v=0, lty=3)
points(x, rep(0, n), col="green", cex=.9)
points(rep(0, n), y, col="blue", cex=.9)
points(X, cex=1.5)

# original points with PC axes
eqscplot(X.all, type="n", xlab="x", ylab="y")
abline(h=0, lty=3, col="gray"); abline(v=0, lty=3, col="gray")
abline(0, b1, lty=3); abline(0, b2, lty=3)
points(x, y, cex=1.5)

# original points with PC axes and PC coordinates
eqscplot(X.all, type="n", xlab="x", ylab="y")
abline(h=0, lty=3, col="gray"); abline(v=0, lty=3, col="gray")
abline(0, b1, lty=3); abline(0, b2, lty=3)
points(X.pc1, col="green", cex=.9)
points(X.pc2, col="blue", cex=.9)
points(x, y, cex=1.5)

# Another interesting fact: the variance of the coordinates of a set of
# observations with respect to u is the same as the variance of the coordinates
# of the *centered* dataset with respect to u.
# (The "centered" dataset has the mean vector subtracted from each observation.)

# cloud of data points with mean vector indicated
eqscplot(X.all, type="n", xlab="x", ylab="y")
abline(h=0, lty=3); abline(v=0, lty=3)
points(xbar[1], xbar[2], col="red", pch=18, cex=2)
points(x, y, cex=1.5)

# centered data points
eqscplot(X.all, type="n", xlab="x", ylab="y")
abline(h=0, lty=3); abline(v=0, lty=3)
points(xbar[1], xbar[2], col="pink", pch=18, cex=2)
points(X, col="gray", cex=1.5)
points(0, 0, col="red", pch=18, cex=2)
points(X.cent, cex=1.5)

# The PCA orthonormal basis is easy to find for centered dataset,
# so the first step of the PCA algorithm is centering the data.
# (This is taken care of for you by the built-in PCA functions.)

# To make a long story short, the orthonormal basis vectors with the
# desired variance properties are exactly the principal components
# of the sample covariance matrix. And the variances of the coordinates
# are the corresponding eigenvalues.
# (See the workshop's presentation pdf for the mathematical details.)

# revisiting our simulated data
head(X)

cov(X)
e <- eigen(cov(X))
e$vectors # the columns of this matrix are the "PCA basis" that we're looking for

# The coordinates of the observations with respect to the PCA basis are
M <- X %*% e$vectors
head(M)

# The variances of the coordinates of the new variables are
apply(M, 2, var)
e$values # same as the eigenvalues

# their sum is
sum(e$values)

# what was the sum of the variances of the original coordinates?
apply(X, 2, var)
sum(apply(X, 2, var))


# The coordinates of the *centered* observations with respect to the PCA basis are
xbar <- apply(X, 2, mean)
xbar
X.cent <- sweep(X, 2, xbar)
P <- X.cent %*% e$vectors
head(P)
# Technically, it's the columns of this matrix that are the called
# the *Principal Components* - they are orthogonal to each other:
t(P) %*% P


# Now we'll use built-in PCA functions to get these quantities

p1 <- prcomp(X)
p1
names(p1)
p1$sdev^2 # variances of principal components
p1$rotation # PCA basis vectors
p1$center # mean vector of original dataset
head(p1$x) # principal components

p2 <- princomp(X)
p2
names(p2)
p2$sdev^2 # variances of principal components
p2$loadings # PCA basis vectors
p2$center # mean vector of original dataset
head(p2$scores) # principal components


# these two functions use a different algorithm to get the same results.
# prcomp can be substantially more computationally efficient for high-dimensional data.



# RESCALING

# The PCA isn't usually performed the way I've described it above.
# In practice, there's usually one more step:
# rescaling the original variables so that they all have unit variance.
# To accomplish this, after centering the data, you divide each column by its
# standard deviation. In the end, the PCA basis vectors are the eigenvectors
# of the sample *correlation* matrix rather than the covariance matrix.

# Why rescale?
# Because you usually don't want the choice of units to affect your PCA results.
# E.g. consider the *stackloss* dataset.

head(stackloss)

prcomp(stackloss)$rotation[, 1] # first PCA basis vector

# The temperatures are measured in degrees Celsius.
# What if had instead measured them in degrees Farenheit?

x <- stackloss
x$Water.Temp <- 9/5*x$Water.Temp + 32
prcomp(x)$rotation[, 1]

# Now temperature is much more important in determining the first PC
# - it has much more weight.
# We don't want something so arbitrary as the choice of units to affect
# the results, so we instead standardize every variable.

prcomp(stackloss, scale=TRUE)
prcomp(x, scale=TRUE)
# no difference now

# However, there are cases in which it can be better not to rescale.
# If, for instance, the variables are all in the same units, e.g. every variable
# is a survey question response from 1 (strongly disagree) to 5 (strongly agree).







######################################

#### EXAMPLE - US States crime data


head(USArrests)

X <- USArrests[, c(1, 2, 4)]

p <- princomp(X, cor=TRUE)


### INTERPRETATION

library(tidyverse)

# install.packages("factoextra")
library(factoextra)

fviz_eig(p) # "scree" plot

cumsum(p$sdev^2)/sum(p$sdev^2)
# by including one PC, you've retained nearly 80 percent of the variation,
# by including two PCs, you've retained about 94 percent of the variation
# - pretty good approximation of the overall dataset



## Interpretation of variables

p$loadings
# The first PC is a nearly equally-weighted sum of the three
# (standardized) crime rates - can call this overall violent crime rate
# The second PC has positive coefficients of murder and assault and
# a large negative coefficient for rape - maybe call this nonsexual violent crime.

# For visual intuition:
p %>% fviz_pca_var()
# murder and assault are strongly correlated.
# PC1 is roughly a sum of the three.
# PC2 is roughly a sum of murder and assault minus rape.



## Interpretation of observations

pairs(X)

pairs(p$scores)

# This is the scatterplot that captures the overall shape of the datapoints
# as well as possible:
plot(p$scores[, 1:2])

# Remember these two dimensions retain over 93 percent of variation
# - quite a good approximation of overall shape

# same scatterplot, this time with a ggplot-based function
p %>% fviz_pca_ind(repel = TRUE)

# the states on the right have more crime overall than the ones on the left
# the states toward the bottom have relatively more sexual violent crime.

# What if we color by the variable we left out: percent urban population
p %>%
  fviz_pca_ind(col.ind = USArrests$UrbanPop,
               gradient.cols = c("blue", "yellow", "red"),
               repel = TRUE)

# No strong relationships is apparent to me.


# One can also put both the variables and the observations on the same PCA picture
# - called a biplot.

p %>%
  fviz_pca_biplot(col.ind = USArrests$UrbanPop,
                  col.var = "black",
                  gradient.cols = c("blue", "yellow", "red"),
                  repel = TRUE) ->
  biplot

biplot

# Adding a new observation:
# let's say, Puerto Rico and Guam become states
X.new <- matrix(c(12, 301, 35, 11.3, 255, 29), nrow=2, byrow=TRUE,
                dimnames=list(c("Puerto Rico", "Guam"), c("Murder", "Assault", "Rape")))
X.new # these "crime rates" below are completely made up, by the way
# calculate their principal components:
P.new <- predict(p, X.new)
P.new

fviz_add(biplot, P.new, color = "green")





######################################

#### EXAMPLE - oxidation of ammonia at a chemical plant


help(stackloss)
dim(stackloss)
head(stackloss)


X <- stackloss[, 1:3] # explanatory variables
y <- stackloss$stack.loss # response variable

# Let's only do PCA on the explanatory variables
p <- princomp(X, cor=TRUE)



### INTERPRETATION


fviz_eig(p)

cumsum(p$sdev^2)/sum(p$sdev^2)
# by including two PCs, you've retained over 93 percent of the variation
# - pretty good approximation of the overall dataset



## Interpretation of variables

p$loadings
# The first PC is a nearly equally-weighted sum of air flow, water temperature,
# and acid conocentration

# For visual intuition:
p %>% fviz_pca_var()
# temp and air flow are strongly correlated
# PC1 is roughly a sum of all three.
# PC2 is roughly the sum of water temp and airflow minus acid concentration



## Interpretation of observations

pairs(X)

pairs(p$scores)

# This is the scatterplot that captures the overall shape of the datapoints
# as well as possible:
plot(p$scores[, 1:2])

# Remember these two dimensions retain over 93 percent of variation
# - quite a good approximation of overall shape

# same scatterplot, this time with a ggplot-based function
p %>% fviz_pca_ind(repel = TRUE)


p %>%
  fviz_pca_ind(col.ind = y, repel = TRUE) +
    scale_color_gradient(low="blue", high="red")


plot(p$scores[, 1], y)

# You can do regression by fitting coefficients for the PCs rather than the original varianbles
# - generally results in regularization, i.e. lower variance in your estimator.


# "iterative fitting"
# this is a way that I usually do fitting manually
# when there aren't too many variables
# (regardless of whether or not I'm using PCs)
# In other words, this part isn't about PCA, it's just
# demonstrating what I would do next.
fit1 <- lm(y ~ p$scores[, 1])
abline(fit1, col=2)

# look for pattern in residuals
plot(p$scores[, 1], fit1$residuals) # strong curvature observed

fit2 <- lm(y ~ p$scores[, 1] + I(p$scores[, 1]^2))
summary(fit2)
b <- fit2$coefficients
grid <- seq(min(p$scores[, 1]), max(p$scores[, 1]), length.out=100)
plot(p$scores[, 1], y)
lines(grid, b[1] + b[2]*grid + b[3]*grid^2, col=3)

plot(p$scores[, 1], fit2$residuals) # no apparent pattern remaining
# - this quadratic fit seems to have captured the first PC's relationship well enough

plot(p$scores[, 2], fit2$residuals)
# maybe a quadratic relationship here too?
fit3 <- lm(y ~ p$scores[, 2] + I(p$scores[, 2]^2))
summary(fit3) # relationship not strongly indicated, based on significance probabilities

# interaction?
plot(p$scores[, 1]*p$scores[, 2], fit2$residuals) # nah


# model selected along with coefficients and sig probs:
summary(fit2)










######################################

#### EXAMPLE - DNA data


#install.packages("mlbench")
library(mlbench)

data()
help(DNA)
data(DNA)
dim(DNA)
# fairly high-dimensional dataset

DNA[, 1:180] %>%
  as_tibble() %>%
  mutate_all(as.integer) ->
  X

p <- prcomp(X)
# not rescaling here because the variables
# have the same units and are directly comparable

fviz_eig(p)
cumsum(p$sdev^2)/sum(p$sdev^2)

# Often you're told to look for an "elbow" in the scree plot to determine
# how many PCs to keep - the rest are thought to comprise "noise".
# According to that guideline, I'd only keep the first four in this case.
# Notice that we're only capturing about 8 percent of the total variance here.

# The dimension is probably too high to extract any meaningful interpretation
# of the principal components as variables.
# However, we can plot the observations. Here are the first two PCs.

p %>% fviz_pca_ind(label = "none")

p %>%
  fviz_pca_ind(label = "none",
               col.ind = DNA$Class)

# three very distinct populations are apparent in the plot of PC1 and PC2

# what about for the other PCs that we wanted to keep?
p %>%
  fviz_pca_ind(axes=c(3, 4),
               label = "none",
               col.ind = DNA$Class)

# the groups are fairly distinct in these dimensions as well.

# Let's test whether or not they're drawn from the same distribution in R^4.
# They groups look reasonably Gaussian to the eye,
# and the groups' covariances structures are similar.

m <- manova(p$x[, 1:4] ~ DNA$Class)
summary(m)
# We can reject the null hypothesis that all three groups
# share the same mean vector.

# What about for the testing for difference of individual PCs?
summary.aov(m)
# indeed none of the four individual PCs have the same expectation
# among all groups.



# Bonus material: PCA is designed for separating *points* as much as possible,
# but the best linear transformation for separating *groups* is called
# Linear Discriminant Analysis (LDA).


L <- lda(X, grouping=DNA$Class)
Q <- as.matrix(X) %*% L$scaling
plot(Q, col=DNA$Class)

# Be careful. Realize that there's quite a bit of "noise" available
# in the original dataset. The true distributions won't be quite as well-separated
# as they appear in an LDA plot.



#####################################
