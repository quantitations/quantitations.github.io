
# install.packages("RColorBrewer")

source("http://quantitations.com/static/mixed-effects-simulations.r")
source("http://quantitations.com/static/mixed-effects-plots.r")


########################################################
############### MIXED EFFECT MODELS ####################
########################################################


##########################################################
## Example 1: Repeated measurements with random subject effects
## -- Simulated blood pressures


# Suppose the expected female blood pressure (BP) is 117
# and that the expected male BP is 121.
# Suppose further that both females' and males' expected BPs
# are Normally distributed with standard deviation 7.
# Assume that any given measurement
# of a person's BP is a Normal draw centered at
# their expected BP with standard deviation 6.
# We'll generate 5 female and 6 male subjects, then we'll generate
# a handful of BP measurements (from 1 to 4) for each of our subjects.

mu.f <- 117; mu.m <- 121
sigma.between <- 7
sigma.within <- 6
n.f <- 5; n.m <- 6
n.rep <- 1:4

set.seed(1)
S <- simulateRM(c(mu.f, mu.m), c(n.f, n.m), sigma.between, n.rep, sigma.within)
x <- S$x
names(x) <- c("bp", "subject", "sex")
levels(x$sex) <- c("F", "M")
x

# Visualize the groups' true average BPs and the subjects' expected BPs
RMplot(x$bp, x$subject, x$sex, S$subject.effects, c(mu.f, mu.m), hide.points=TRUE,
       xlab="subject", ylab="blood pressure",
       main="Groups' expectations along with subjects' expectations")

# Add the BP measurements to the plot
RMplot(x$bp, x$subject, x$sex, S$subject.effects, c(mu.f, mu.m),
       xlab="subject", ylab="blood pressure",
       main="Groups' and subjects' expectations and subjects' measurements")

# Let's also draw a plot of only the BP measurements
# (this is a plot you could make in reality, as true expectations are unknown)
RMplot(x$bp, x$subject, x$sex,
       xlab="subject", ylab="blood pressure",
       main="Subjects' measurements")


# The "lmer" function in the "lme4" package finds the parameter values
# that maximize the likelihood of the mixed effects model.
# The parameters include the fixed effect of sex, the variance of the
# distributions of the subjects' expected BPs, and the error variance.
# The resulting fitted model also provides an estimate
# of each subject's expected BP.


# install.packages("lmerTest")
library(lmerTest)

m <- lmer(bp ~ sex + (1 | subject), data=x)
# the notation (1 | subject) indicates a random intercept effect for each subject

# Estimates of average BPs of females and males
fixef(m)
mu.f.hat <- fixef(m)[1]
mu.m.hat <- mu.f.hat + fixef(m)[2]
# compare true values and estimates
matrix(c(mu.f, mu.f.hat, mu.m, mu.m.hat), nrow=2,
       dimnames=list(c("true", "estimated"), c("female", "male")))

# Estimates of expected BPs of the subjects
ranef(m) # https://rdrr.io/cran/lme4/man/ranef.html
subject.effects.hat <- ranef(m)$subject[, 1]
# compare true values and estimates
cbind(S$subject.effects, subject.effects.hat)

# Plots to compare true expectations to estimated expectations
par(mfrow=c(1, 2))
RMplot(x$bp, x$subject, x$sex, S$subject.effects, c(mu.f, mu.m),
       xlab="subject", ylab="blood pressure", main="Data and true expectations")
RMplot(x$bp, x$subject, x$sex, subject.effects.hat, c(mu.f.hat, mu.m.hat),
       xlab="subject", ylab="blood pressure", main="Data and estimated expectations")
par(mfrow=c(1, 1))

# Note that the expected blood pressures of the particular subjects
# isn't usually something that a researcher cares about.
# Rather, the goal is to make inferences about the fixed effects.

summary(m)

# The summary provides a significance probability for the hypothesis test
# that females and males have the same expected BP.
# Very small p-value --> strong evidence against null.

names(summary(m))
summary(m)$coefficients
summary(m)$coefficients[2, 5] # p-value

# The summary also provides estimates of the standard deviations
# of the errors and of the random subject effects.

sigma.within.hat <- summary(m)$sigma
sigma.within.hat
sigma.within # compare to true sigma.within used in simulation

sigma.between.hat <- attr(summary(m)$varcor$subject, "stddev")
sigma.between.hat
sigma.between # compare to true sigma.between used in simulation

# Confidence intervals for parameters

ci <- confint(m, oldNames=FALSE)
ci

# Additional model selection
# Perhaps the random effects weren't needed.
# We could have used a simpler model that makes the (unreasonable)
# assumption that all females have the same expected BP and
# all males have the same expected BP.
# This is equivalent to the null hypothesis that sigma.s is zero.

ranova(m)

# Small p-value for the null hypothesis.
# And the increased AIC when the random effect is dropped
# also indicates that our model is preferable to the simpler one.


# Fitting with mgcv package

library(mgcv)

m <- gam(bp ~ sex + s(subject, bs="re"), data=x)
summary(m)

# enables you to use random effects in the context of generalized linear modeling

# Can extract various model values to compare, e.g.

sqrt(m$sig2)
gam.vcomp(m)

# mgcv also includes a function "gamm" (generalized additive mixed model)
# which may be preferable if you care about the random effects

m <- gamm(bp ~ sex, random=list(subject=~1), data=x)
names(m)
summary(m$lme)

ranef(m$lme)



##########################################################
## Example 2: Clustered data with random intercepts and slopes
## -- Simulated relationships between parents' income and
##    academic performance at multiple schools.


# Each of 30 schools has its own true line relating log of
# parents' income to expected student performance on the SAT.
# A random sample of between 10 and 15 students were surveyed
# from each school.

n.school <- 30
# for generating explanatory values (parents' log incomes)
log.income.mean <- 4.2; sd.school.mean <- .5 # for generating each school's mean logincome
sd.school <- 1 # for generating logincomes within a school
# for generating true lines
mu.int.cent <- 1050; sigma.int.cent <- 75 # centered intercept
mu.slope <- 40; sigma.slope <- 15 # slope
# for generating response values (SAT scores)
n.obs <- 10:15
sigma <- 100

set.seed(2)
S <- simulateCL(log.income.mean, mu.int.cent, sigma.int.cent, mu.slope, sigma.slope, n.school, sd.school.mean, sd.school, n.obs, sigma)
x <- S$x
names(x) <- c("sat", "log.income", "school")
head(x)

# The schools' randomly drawn intercept and slope effects
cbind(S$int.effects, S$slope.effects)

# calculate expected intercept
mu.int <- mu.int.cent-mu.slope*log.income.mean
# calculate covariance matrix for random effects
var.slope <- sigma.slope^2
var.int <- sigma.int.cent^2 + (log.income.mean^2)*var.slope
cov.int.slope <- -log.income.mean*var.slope
C <- matrix(c(var.int, cov.int.slope, cov.int.slope, var.slope), nrow=2)

# Visualize the schools' true lines
CLplot(x$log.income, x$sat, x$school, mu.int, mu.slope, S$int.effects, S$slope.effects, hide.points=TRUE,
       xlab="log income", ylab="SAT score", main="Schools' true lines")

# Visualize data and the schools' true lines
CLplot(x$log.income, x$sat, x$school, mu.int, mu.slope, S$int.effects, S$slope.effects,
       xlab="log income", ylab="SAT score", main="Schools' true lines along data")

# Visualize data only
CLplot(x$log.income, x$sat, x$school,
       xlab="log income", ylab="SAT score", main="SAT scores and parents' income of students from various schools")

# Fit mixed effects model

m <- lmer(sat ~ log.income + (log.income|school), data=x)
# the notation (log.income | subject) indicates a random coefficient
#effect for log.income and (implicitly) a random intercept effect
# for each school.

fixef(m)
mu.int.hat <- fixef(m)[1]
mu.slope.hat <- fixef(m)[2]
# compare true values and estimates
matrix(c(mu.int, mu.int.hat, mu.slope, mu.slope.hat), nrow=2,
       dimnames=list(c("true", "estimated"), c("intercept", "slope")))

ranef(m)
int.effects.hat <- ranef(m)$school[, 1]
slope.effects.hat <- ranef(m)$school[, 2]
# compare true values and estimates
cbind(S$int.effects, int.effects.hat)
cbind(S$slope.effects, slope.effects.hat)

# Plots to compare the true lines to the estimated lines
par(mfrow=c(1, 2))
CLplot(x$log.income, x$sat, x$school, mu.int, mu.slope, S$int.effects, S$slope.effects,
       xlab="log income", ylab="SAT score", main="Data and true lines")
CLplot(x$log.income, x$sat, x$school, mu.int.hat, mu.slope.hat, int.effects.hat, slope.effects.hat,
       xlab="log income", ylab="SAT score", main="Data and estimated lines")
par(mfrow=c(1, 1))

# Another visualization to compare the  true lines
# (as slope-intercept pairs in a scatterplot)
# to the estimates produced by the mixed effects modeling
expected <- cbind(mu.int+S$int.effects, mu.slope+S$slope.effects)
estimated <- cbind(mu.int.hat+int.effects.hat, mu.slope.hat+slope.effects.hat)
plot(rbind(expected, estimated), type="n",
     xlab="intercept", ylab="slope", main="True and estimated slope-intercept pairs")
pal <- getPalette(n.school)
points(expected, col=pal, pch=18, cex=2)
points(estimated, col=pal, cex=2)

# Again, note that estimation of the specific slopes and intercepts of
# the various schools probably isn't very important to a researcher.
# Most likely, the more important questions are about the fixed
# parameters, especially the expected slope and expected intercept of
# the distribution from which these slopes and intercepts were drawn.

summary(m)

# Null hypothesis: expected slope is zero. 
summary(m)$coefficients[2, 5] # p-value
# Small significance probability --> reject null

# The summary also provides estimates of the standard deviations
# of the errors and of the random subject effects.

summary(m)$sigma # sigma hat
sigma # compare to true sigma used in simulation

C.hat <- summary(m)$varcor$school; attr(C.hat, "stddev") <- NULL; attr(C.hat, "correlation") <- NULL
C.hat
C # compare to true covariance matrix for distribution of random effects used in simulation

# Confidence intervals for parameters

ci <- confint(m, c("(Intercept)", "log.income"), oldNames=FALSE)
ci

# Model selection:
# Compare to a simpler model
# - null hyp: schools have different random slopes but share the same intercept
ranova(m)
# Removing this term results in lower AIC, so the simpler model
# is preferred, according to the AIC criterion.
# Additionally, the significance probability of a likelihood ratio test
# indicates that we don't have much evidence against this null hypothesis.

# Compare to an even simpler null hypothesis
# - null hyp: all schools share the same slope and intercept
ranova(m, reduce.terms=FALSE)
# Removing both random terms results in much larger AIC and
# exceedingly small significance probability of the likelihood ratio
# test, indicating that we should reject this model.

# Refit using the random intercepts and shared slopes model.
m <- lmer(sat ~ log.income + (1 | school), data=x)

# Can repeat inference tasks with this simpler model

fixef(m)
mu.int.hat <- fixef(m)[1]
mu.slope.hat <- fixef(m)[2]
# compare true values and estimates
matrix(c(mu.int, mu.int.hat, mu.slope, mu.slope.hat), nrow=2,
       dimnames=list(c("true", "estimated"), c("intercept", "slope")))

ranef(m)
int.effects.hat <- ranef(m)$school[, 1]
slope.effects.hat <- rep(0, n.school)

# Plots to compare the true lines to the estimated lines
par(mfrow=c(1, 2))
CLplot(x$log.income, x$sat, x$school, mu.int, mu.slope, S$int.effects, S$slope.effects,
       xlab="log income", ylab="SAT score", main="Data and true lines")
CLplot(x$log.income, x$sat, x$school, mu.int.hat, mu.slope.hat, int.effects.hat, slope.effects.hat,
       xlab="log income", ylab="SAT score", main="Data and estimated lines")
par(mfrow=c(1, 1))

summary(m)

confint(m, oldNames=FALSE)

ranova(m)




##########################################################
## Example 3: Clustering and repeated measurements (two levels of random effects)
## -- Simulated school performances

# Suppose the average female SAT score is 1055 and the
# average male score is 1042.
# Suppose further that each of 4 schools has an effect on
# average SAT scores (same effect for both females and males),
# and that these effects are Normally distributed with std dev 50.
# Assume the students of a given school also have *expected* SAT
# scores that are Normally distributed, centered at the average
# for their sex plus their school's effect, with std dev 50.
# However, each student will take the SAT between 1 and 3 times,
# and their actual scores are Normal draws with std dev 50.

mu.f <- 1055; mu.m <- 1042
n.school <- 4
sigma.school <- 50
n.student <- 5:10
sigma.student <- 50
n.rep <- 1:3
sigma <- 50

set.seed(1)
S <- simulateCRM(c(mu.f, mu.m), n.school, sigma.school, n.student, sigma.student, n.rep, sigma)
S
x <- S$x
names(x) <- c("sat", "student", "school", "sex")
levels(x$sex) <- c("F", "M")
head(x, 20)


# Expected SAT scores for groups and for schools
CRMplot(x$sat, x$student, x$school, x$sex, cluster.effects=S$cluster.effects, group.expectations=c(mu.f, mu.m), hide.points=TRUE,
        xlab="school", ylab="SAT score", main="Groups' true average at each school and students' expected values")

# Expected SAT scores for groups, for schools, and for students
CRMplot(x$sat, x$student, x$school, x$sex, S$subject.effects, S$cluster.effects, c(mu.f, mu.m), hide.points=TRUE,
        xlab="school", ylab="SAT score", main="Groups' true average at each school and students' expected values")

# Expected and observed scores
CRMplot(x$sat, x$student, x$school, x$sex, S$subject.effects, S$cluster.effects, c(mu.f, mu.m),
        xlab="school", ylab="SAT score", main="Groups' true average at each school and students' expected values and actual scores")

# Observed scores only
CRMplot(x$sat, x$student, x$school, x$sex,
        xlab="school", ylab="SAT score", main="Students' scores")

# Fit the mixed effects model with a random effect for each student
# and a random effect for each school.

m <- lmer(sat ~ sex + (1 | student) + (1 | school), data=x)
m

# Note: if student IDs for different students
# were repeated within different schools,
# you would need to instead use the formula
# lmer(sat ~ sex + (1 | school/student), data=x)


# Let's see how well the model estimates female and male expected
# SAT scores, along with the students' expected SAT scores
# and the schools' expected SAT scores.

fixef(m)
mu.f.hat <- fixef(m)[1]
mu.m.hat <- mu.f.hat + fixef(m)[2]
# compare true values and estimates
matrix(c(mu.f, mu.f.hat, mu.m, mu.m.hat), nrow=2,
       dimnames=list(c("true", "estimated"), c("female", "male")))

ranef(m)
school.effects.hat <- ranef(m)$school[, 1]
student.effects.hat <- ranef(m)$student[, 1]
# student.effects.hat <- ranef(m)$`student:school`[, 1]


# Compare plots with true expected values to estimated expected values
par(mfrow=c(1, 2))
CRMplot(x$sat, x$student, x$school, x$sex, S$subject.effects, S$cluster.effects, c(mu.f, mu.m),
        xlab="school", ylab="SAT score", main="Data and true expectations")
CRMplot(x$sat, x$student, x$school, x$sex, student.effects.hat, school.effects.hat, c(mu.f.hat, mu.m.hat),
        xlab="school", ylab="SAT score", main="Data and estimated expectations")
par(mfrow=c(1, 1))

# Model selection

summary(m)
# The data doesn't provide strong evidence for difference in females' and males'
# expected SAT scores. That's reasonable, because the actual difference is
# small in our simulation.
# Apparently a larger sample size would be needed to reveal the small effect.

summary(m)$sigma # sigma hat
sigma # compare to true sigma used in simulation

attr(summary(m)$varcor$school, "stddev") # sigma.school hat
sigma.school # compare to true sigma.school used in simulation

attr(summary(m)$varcor$`student:school`, "stddev") # sigma.student hat
sigma.student # compare to true sigma.student used in simulation

# confidence intervals
ci <- confint(m, oldNames=FALSE)
ci

ranova(m)
# Removal of either of the random effects would make the model worse
# according to AIC and should be rejected according to likelihood ratio test. 
# (At least this is the case when the sex effect term is in the model.)




## EXERCISE 1: Use simulated draws from your fitted model to
##             estimate the probability that a female student
##             who takes the SAT once will score at least 1200.

nsim <- 10000





##########################################################
## EXERCISE 2: Speed of light experiments

dim(morley)
head(morley)
help(morley)

x <- morley
x$Speed <- x$Speed + 299000 # see "help" page

table(x$Expt, x$Run)

boxplot(x$Speed ~ x$Expt)

# Fit a mixed effects model in which the experiments'
# expectations are random and the 20 runs are independent draws
# from that experiment's distribution.
# (The only fixed effect will be the intercept.)
# What is the estimated intercept (i.e. the estimated speed of light)?

# Does the 95% confidence interval cover the actual speed of
# light, which is 299792 km/s?
# What about the 99% confidence interval?

# Find the significance probability of a test with null hypothesis that
# all five experiments have the same expectation.










##########################################################
## Example 4: Longitudinal growth
## -- Simulated growth of trees with different fertilizers


# 30 orange trees are planted and randomly assigned to three
# equally-sized groups. Each group is treated with a different fertilizer ("A", "B", "C"),
# and their heights are recorded every year for four years.

# Suppose the trees grow linearly over time,
# and that the expected slope is different (and fixed) for each fertilizer.
# Suppose that each particular tree's slope has its own random effect as well.

slopes <- c(2.7, 3, 3.6) # expected slope for each group
sigma.slope <- .5 # standard deviation with which to draw individual trees' expected slopes
n.g <- 2:4 # number of trees for each treatment
d <- 4 # number of years to measure each tree
sigma <- .5

set.seed(3)
S <- simulateLG(slopes, sigma.slope, n.g, d, sigma)
x <- S$x
head(x)
names(x) <- c("height", "tree", "age", "treatment")
levels(x$treatment) <- c("A", "B", "C")
head(x)

# Group expectations
LGplot(x$height, x$tree, x$age, x$treatment, group.expectations=slopes, hide.points=TRUE,
       xlab="age", ylab="height", main="Expected growth of trees with different treatments")

# Groups' and particular trees' expectations
LGplot(x$height, x$tree, x$age, x$treatment, S$subject.effects, slopes, hide.points=TRUE,
       xlab="age", ylab="height", main="Expected growth of particular trees")

# Actual trees' growth and group expectations
LGplot(x$height, x$tree, x$age, x$treatment, group.expectations=slopes,
       xlab="age", ylab="height", main="Growth of trees and their treatment groups' expectations")

# Data only
LGplot(x$height, x$tree, x$age, x$treatment,
       xlab="age", ylab="height", main="Growth of trees with various treatments")


# We know that the height was 0 at time 0, so we won't bother
# fitting an intercept. We only want to find a slope for each treatment.
# Just so that you understand the formula notation involved,
# let's first do an ordinary least-squares fitting,
# ignoring the tree variable.
lm(height ~ treatment:age - 1, data=x)

# Our previous simulated examples have had independent errors,
# but that doesn't capture the mechanism here.
# In this, the errors of consecutive measurements
# from the "true line" are correlated with each other.
# This is called (temporal) autocorrelation.

# The lme4 package isn't designed to handle autocorrelation,
# but another mixed effects package is: "nlme".

# install.packages("nlme")
library(nlme)

m <- lme(height ~ treatment:age - 1, random=~age-1|tree, data=x, correlation=corAR1())

fixef(m)
slopes.hat <- fixef(m)
# compare true values and estimates
matrix(c(slopes, slopes.hat), nrow=2, byrow=TRUE,
       dimnames=list(c("true", "estimated"), c("A", "B", "C")))

ranef(m)
tree.effects.hat <- ranef(m)$age

# Compare true expectations to estimates
par(mfrow=c(1, 2))
LGplot(x$height, x$tree, x$age, x$treatment, S$subject.effects, slopes, hide.points=TRUE,
       xlab="age", ylab="height", main="Expected growth of treatment groups and particular trees")
LGplot(x$height, x$tree, x$age, x$treatment, tree.effects.hat, slopes.hat, hide.points=TRUE,
       xlab="age", ylab="height", main="Estimated expected growth of treatment groups and particular trees")
par(mfrow=c(1, 1))

# Further inference

summary(m)
# Shows significance probabilities for testing whether each
# treatment's true expected slope is zero. But we presumably
# want to instead compare the treatments. We'll come back to this shortly.

summary(m)$sigma # sigma hat
sigma # compare to true sigma used in simulation

# confidence intervals
intervals(m)

intervals(m)$reStruct$tree[2] # sigma.slope hat
sigma.slope # compare to true sigma.slope

# Test for differences among groups
# install.packages("emmeans")
library(emmeans)
e <- emmeans(m, pairwise ~ treatment)
e
# Note that the values would match up with our slope estimates if we had used
# emmeans(m, pairwise ~ treatment, at=list(age=1))
# but this doesn't affect the reported significance probabilities.

# We have little evidence that groups A and B have different expected slopes.
# Somewhat more evidence that group C has a larger expected slope
# than both groups A and B. However, this p-value isn't very small either
# because of the tiny sample size.
# However, what if we had instead tested the null hypothesis that
# the expected slope of C is greater than the average of the expected
# slopes of A and B?

contrast(e, list(c(-.5, -.5, 1)))
# p-value .064 --> decent evidence against the null,
# i.e. indicating relative effectiveness of treatment C.




##########################################################
## EXERCISE 3: Chicken weights over time with different diets

dim(ChickWeight)
head(ChickWeight)
help(ChickWeight)

x <- ChickWeight
n <- length(levels(x$Chick))

# Let's draw the curve of each chicken's weight, colored by diet
plot(x$Time, x$weight, type="n", xlab="day", ylab="weight",
     main="Growth of chickens on various diets")
for(i in 1:n) {
  these <- which(format(x$Chick)==format(i))
  lines(x$Time[these], x$weight[these], col=x$Diet[these[1]])
}
legend("topleft", legend=levels(x$Diet), fill=levels(x$Diet))

# The weights tend to curve upward (perhaps quadratically?)
# Let's try the plot again with square root transformation of weight

plot(x$Time, sqrt(x$weight), type="n", xlab="day", ylab="square root of weight",
     main="Growth of chickens on various diets")
for(i in 1:n) {
  these <- which(format(x$Chick)==format(i))
  lines(x$Time[these], sqrt(x$weight[these]), col=x$Diet[these[1]])
}
legend("topleft", legend=levels(x$Diet), fill=levels(x$Diet))

# That's much more linear.

x$sqrtweight <- sqrt(x$weight)

# Let's assume that the expected square root of a chicken's weight
# follows a line for the first 21 days of its life.
# The expected slope of this line may be different for the four diets.

# Use mixed effects modeling to estimate the slopes for the four diets
# and to test for pairwise differences among those slopes.

# Draw a plot (without using LGplot) with the (square root transformed) data
# and the diet's estimated expected lines superimposed.

# Find 95% confidence intervals for the four slopes.

# Test for pairwise differences of the slopes.

# Warning, your call to "lme" will likely produce an optimization error.
# This is fairly common in mixed effects modeling in my experience.
# Try googling your error to try to solve it.
# (Or look at my solution if you'd rather.)



