
########################################################
######### SIMULATING HIERARCHICAL DATA #################
########################################################


# Simulating repeated measurements of subjects with groups
simulateRM <- function(group.means, group.ns, sd.subj, n.rep, sigma, decs=0) {
  n <- sum(group.ns)
  subject.effects <- rnorm(n, sd=sd.subj)
  measurement <- c()
  subject <- c()
  group <- c()
  k <- length(group.means)
  id <- 1
  for(j in 1:k) {
    for(i in 1:group.ns[j]) {
      num <- n.rep
      if (length(n.rep) > 1) {
        num <- sample(n.rep, 1)
      }
      measurement <- c(measurement, group.means[j] + subject.effects[id] + rnorm(num, sd=sigma))
      subject <- c(subject, rep(id, num))
      group <- c(group, rep(j, num))
      id <- id+1
    }
  }
  x <- data.frame(measurement=round(measurement, decs), subject=factor(subject), group=factor(group))
  
  return(list(x=x, subject.effects=subject.effects))
}


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



########################################################
########### PLOTTING HIERARCHICAL DATA #################
########################################################



library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Plot for repeated measurements
RMplot <- function(measurements, subjects, groups, subject.effects=NULL,
                   group.expectations=NULL, hide.points=FALSE, legendposition="topleft", ...) {
  # the subjects should be sorted by their groups in the dataset
  subjects <- as.factor(subjects)
  groups <- as.factor(groups)
  N <- length(measurements)
  n <- length(levels(subjects))
  k <- length(levels(groups))
  # create also the group vector corresponding to subjects
  groups.subj <- rep(NA, n)
  for(i in 1:n) {
    groups.subj[i] <- groups[which(subjects==levels(subjects)[i])[1]]
  }
  subject.expectations <- rep(NA, n)
  if(!is.null(subject.effects)) {
    for(i in 1:n) {
      subject.expectations[i] <- group.expectations[groups.subj[i]] + subject.effects[i]
    }
  }
  
  # draw blank plot that will contain everything
  plot(c(1, n), range(measurements, subject.expectations, group.expectations, na.rm=TRUE), type="n", ...)
  for(i in 1:n){
    abline(v=i, col=gray(.8))
  }
  
  pal <- suppressWarnings(brewer.pal(n=k, name="Set1"))
  # draw vertical dividing lines between groups
  divs <- cumsum(table(groups.subj))
  for(d in divs[1:(k-1)]) {
    abline(v=d+.5)
  }
  # draw group expectations if provided
  if(!is.null(group.expectations)) {
    divs <- c(0, divs)
    for(j in 1:k) {
      lines(c(divs[j]+1, divs[j+1]), rep(group.expectations[j], 2), col=pal[j])
    }
  }
  # draw subject expectations if provided
  if(!is.null(subject.expectations)) {
    for(i in 1:n) {
      points(i, subject.expectations[i], col=pal[groups.subj[i]], pch=18)
    }
  }
  # draw measurements unless suppressed
  if(!hide.points) points(subjects, measurements, col=pal[groups])
  legend(legendposition, legend=levels(groups), fill=pal[1:k])
}


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




########################################################
####### FITTING MIXED-EFFECTS MODEL ####################
########################################################

# The "lmer" function in the "lme4" package finds the parameter values
# that maximize the likelihood of the mixed effects model.
# The parameters include the fixed effect of sex, the variance of the
# distributions of the subjects' expected BPs, and the error variance.
# The resulting fitted model also provides an estimate
# of each subject's expected BP.


# install.packages("lmerTest")
library(lmerTest)

m <- lmer(bp ~ sex + (1 | subject), data=x)
# the syntax (1 | subject) indicates a *random* intercept effect for each subject

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
# This is equivalent to the null hypothesis that sigma.between is zero.

ranova(m)

# Small p-value for the null hypothesis of no subject effect.
# But decreased AIC when the random subject effect is dropped.


################################




# We could work out the approximate distribution of costs
# resulting from our fitted model, but for simplicity let's
# approximate that cost distribution via simulation.

myCost <- function(bps) {
  a140 <- sum(bps >= 140)
  a150 <- sum(bps >= 150)
  return(200*a140 + 300*a150)
}

nsim <- 500
costs.freq <- rep(NA, nsim)
for(i in 1:nsim) {
  S <- simulateRM(c(mu.f.hat, mu.m.hat), c(15125, 10725), sigma.between.hat, n.rep=4, sigma.within.hat)
  costs.freq[i] <- myCost(S$x$measurement)
}

#write.csv(costs.freq, "costs.freq.csv", row.names=FALSE)
# For our convenience, I've already saved the result
# as a csv and uploaded it to my website
costs.freq <- read.csv("https://quantitations.com/static/costs.freq.csv")$x


# Gaussian kernel density smoothing
d <- density(costs.freq)
plot(d, main="Estimated Distribution of Cost")

integrate(approxfun(d), lower=600000, upper=620000)




########################################################
#################### BAYESIAN MODELING #################
########################################################



library(nimble)

dataCode <- nimbleCode({
  # Model
  for (i in 1:N.f) {
    for (j in 1:M.f[i]) {
      y.f[i,j] ~ mu.f + b.f[i] + dnorm(sd = sigma.within)
    }
    b.f[i] ~ dnorm(sd = sigma.between)
  }
  
  for (i in 1:N.m) {
    for (j in 1:M.m[i]) {
      y.m[i,j] ~ mu.m + b.m[i] + dnorm(sd = sigma.within)
    }
    b.m[i] ~ dnorm(sd = sigma.between)
  }
  
  # Priors
  mu.m ~ dflat()
  mu.f ~ dflat()
  sigma.within ~ dhalfflat()
  sigma.between ~ dhalfflat()
})



##### Reshaping the data to be read by nimble #####

library(tidyverse)

pivot_wider_autoname <- function(data, id, ...) {
  ids <- as.numeric(as.factor(data[[id]]))
  counts <- rep(0, max(ids))
  n <- nrow(data)
  namecol <- rep(NA, n)
  for(i in 1:n) {
    this_id <- ids[i]
    counts[this_id] <- counts[this_id] + 1
    namecol[i] <- counts[this_id]
  }
  data$auto_names <- namecol
  return(data %>% pivot_wider(names_from = auto_names, ...))
}


###

x %>%
  filter(sex == "F") %>%
  select(!sex) ->
  x.f

x.f %>%
  pivot_wider_autoname(id = "subject", values_from = bp) %>%
  select(!subject) ->
  y.f

###

x %>%
  filter(sex == "M") %>%
  select(!sex) ->
  x.m

x.m %>%
  pivot_wider_autoname(id = "subject", values_from = bp) %>%
  select(!subject) ->
  y.m

##########################



data <- list(y.f = y.f,
             y.m = y.m)
constants <- list(N.f = nrow(y.f),
                  N.m = nrow(y.m),
                  M.f = apply(y.f, 1, function(x) sum(!is.na(x))),
                  M.m = apply(y.m, 1, function(x) sum(!is.na(x))))
inits <- list(mu.f = 120,
              mu.m = 120,
              sigma.within = 1,
              sigma.between = 1)

myModel <- nimbleModel(code=dataCode, constants=constants, data=data, inits=inits)
myModelC <- compileNimble(myModel)
myModelMCMC <- compileNimble(buildMCMC(myModel), project=myModelC)

nSamp <- 500
nSkip <- 100
nBurn <- 1000


samples <- runMCMC(myModelMCMC, niter=nBurn + nSamp*nSkip, nburnin=nBurn, thin=nSkip)

#write.csv(samples, "samples.csv", row.names=FALSE)
samples <- read.csv("https://quantitations.com/static/samples.csv")
head(samples)


plot(samples[ , 'mu.f'], type='l', col="red", xlab='iteration',  ylab=expression(mu), ylim=c(100, 135))
lines(samples[, 'mu.m'], col="blue")
legend("bottomright", c("Female", "Male"), fill=c("red", "blue"))

# Visualize (marginal) posterior beliefs

d.m <- density(samples[ , 'mu.m'])
d.f <- density(samples[ , 'mu.f'])
plot(d.m, col="blue", xlim=c(90, 150), main="Posterior belief about expected BP")
lines(d.f, col="red")
legend("topleft", c("Female", "Male"), fill=c("red", "blue"))
# superimpose true values for comparison
abline(v=mu.m, lwd=3, col=adjustcolor("blue", alpha=0.3))
abline(v=mu.f, lwd=3, col=adjustcolor("red", alpha=0.3))

d.s.b <- density(samples[ , 'sigma.between'])
d.s.w <- density(samples[ , 'sigma.within'])
plot(d.s.w, col="green", xlim=c(2, 14), main="Posterior belief about standard deviations")
lines(d.s.b, col="purple")
legend("topright", c(expression(sigma[w]), expression(sigma[b])),
       fill=c("green", "purple"), text.width=.8)
# superimpose true values for comparison
abline(v=sigma.within, lwd=3, col=adjustcolor("green", alpha=0.3))
abline(v=sigma.between, lwd=3, col=adjustcolor("purple", alpha=0.3))


# What's the posterior probability that male BP is higher than female BP?
# Approximate by finding the proportion of samples with mu.b > mu.f
sum(samples[ , 'mu.m'] > samples[ , 'mu.f'])/nSamp



# Could redo with different prior to see how much that changes things.



# Suppose you have a large number of males and females in your group,
# whose BP will be measured three times each next year
predCode <- nimbleCode({
  # Model
  for (i in 1:N.f) {
    for (j in 1:M) {
      y.f[i,j] ~ mu.f + b.f[i] + dnorm(sd = sigma.within)
    }
    b.f[i] ~ dnorm(sd = sigma.between)
  }
  
  for (i in 1:N.m) {
    for (j in 1:M) {
      y.m[i,j] ~ mu.m + b.m[i] + dnorm(sd = sigma.within)
    }
    b.m[i] ~ dnorm(sd = sigma.between)
  }
  
  # Priors
  mu.m ~ dflat()
  mu.f ~ dflat()
  sigma.within ~ dhalfflat()
  sigma.between ~ dhalfflat()
})

constants <- list(N.f = 15125,
                  N.m = 10725,
                  M = 4)
myModelPred <- nimbleModel(code=predCode, constants=constants)
myModelPredC <- compileNimble(myModelPred)




# What's the distribution of total costs you'll incur
params <- colnames(samples)
costs.bayes <- rep(NA, nSamp)
for(i in 1:nSamp){
  # plug the current sample's parameter values into the model
  values(myModelPredC, params) <- samples[i, ]
  myModelPredC$simulate(c("b.f", "b.m", "y.f", "y.m"), includeData=TRUE)
  result <- round(rbind(myModelPredC$y.f, myModelPredC$y.m))
  costs.bayes[i] <- myCost(as.matrix(result))
}

#write.csv(costs.bayes, "costs.bayes.csv", row.names=FALSE)
costs.bayes <- read.csv("https://quantitations.com/static/costs.bayes.csv")$x

# Gaussian kernel density smoothing
d <- density(costs.bayes)
plot(d, main="Distribution of Cost", xlab="Dollars")

integrate(approxfun(d), lower=600000, upper=1200000)



