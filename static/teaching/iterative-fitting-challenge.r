
# Homework 10
# YOUR NAME HERE
# due 11/15/2021



### Use "iterative fitting" to try to determine how I generated the values of
### the response vector y from the five explanatory vectors.
### Your final guess should be an equation involving a random error
### along with its distribution -- see the example R script from the lecture.



z <- read.csv("http://quantitations.com/static/teaching/simulated.csv")


## It's often a good idea to start by looking at each variable individually
## and transforming any highly skewed variables to be more symmetric 

boxplot(z$x1)
boxplot(z$x2)
boxplot(z$x3)
boxplot(z$x4)
boxplot(z$x5)
boxplot(z$y)

## Hint: start by finding a simple transformation of y that isn't skewed,
## then relate the explanatory variables to that transformed response.

boxplot(log(z$y))


z$logy <- log(z$y)




pairs(z[, -6])


# for convenience:
attach(z)
# this attaches the objects in z to the global namespace,
# i.e. you can access z$x1 by using simply "x1"








