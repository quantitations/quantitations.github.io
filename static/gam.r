
# set sample size

n <- 100


# Simulate wind speeds, drawn from a Gamma distribution

set.seed(1)
pdat <- data.frame(WindSpeed = rgamma(n, shape = 30, scale = 3))
MASS::truehist(pdat$WindSpeed)

# Simulate severities,
# drawn from a Gamma distribution (plus constant) with expectation depending on wind speed

a <- 10000
b <- .02
s <- 1000
pdat$Sev <- a + rgamma(n, shape = exp(b*pdat$WindSpeed), scale = s)

# Plot data and true expectation

plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity")
grid <- seq(min(pdat$WindSpeed), max(pdat$WindSpeed), length.out=100)
lines(grid, a + exp(log(s) + b*grid), col="green", lwd=2)
legend("topleft", c("True expectation"), fill=c("green"))

# Fit data with ordinary GLM

m <- glm(Sev ~ WindSpeed,
         family = Gamma(link=log),
         data = pdat)

summary(m)
lines(grid, exp(m$coefficients[1] + m$coefficients[2]*grid), col="red", lwd=5, lty=2)
legend("topleft", c("True expectation", "GLM estimate"), fill=c("green", "red"))


##################################
# MODEL ERROR EXAMPLES

# e.g. true expectation has linear shape

# Simulate severities

b <- .5
s <- 10
pdat$Sev <- a + rgamma(n, shape = b*pdat$WindSpeed, scale = s)

# Plot data and true expectation

plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity")
grid <- seq(min(pdat$WindSpeed), max(pdat$WindSpeed), length.out=100)
abline(a, s*b, col="green", lwd=2)
legend("topleft", c("True expectation"), fill=c("green"))

# Fit data with ordinary GLM

m <- glm(Sev ~ WindSpeed,
         family = Gamma(link=log),
         data = pdat)

summary(m)
lines(grid, exp(m$coefficients[1] + m$coefficients[2]*grid), col="red", lwd=5, lty=2)
legend("topleft", c("True expectation", "GLM estimate"), fill=c("green", "red"))

# e.g. true expectation has "logistic" shape

# Simulate severities

phi_shape <- function(w, inflect=90, zsd=15, mult=40) {
  z <- (w - inflect)/zsd
  return(mult*pnorm(z))
}

s <- 1500
pdat$Sev <- a + rgamma(n, shape = phi_shape(pdat$WindSpeed), scale = s)

# Plot data and true expectation

plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity")
grid <- seq(min(pdat$WindSpeed), max(pdat$WindSpeed), length.out=100)
lines(grid, a + s*phi_shape(grid), col="green", lwd=2)
legend("topleft", c("True expectation"), fill=c("green"))

# Fit data with ordinary GLM

m <- glm(Sev ~ WindSpeed,
         family = Gamma(link=log),
         data = pdat)

lines(grid, exp(m$coefficients[1] + m$coefficients[2]*grid), col="red", lwd=5, lty=2)
legend("topleft", c("True expectation", "GLM estimate"), fill=c("green", "red"))

# Fit data with GAM

library(mgcv)

m <- gam(Sev ~ s(WindSpeed),
         family = Gamma(link=log),
         data = pdat)

lines(grid, predict(m, newdata=data.frame(WindSpeed=grid), type="response"), col="blue", lwd=5, lty=2)
legend("topleft", c("True expectation", "GLM estimate", "GAM estimate"), fill=c("green", "red", "blue"))

plot(m)

######################


# Next create a mixture of two property types
# each with its own severity vs wind speed curve

# Let the original batch be all "Masonry"

pdat$WallType <- "Masonry"

# Simulate additional data corresponding to "Vinyl
# use a differend distribution of wind speeds

pdat2 <- data.frame(WindSpeed = rgamma(n, shape = 25, scale = 3),
                    WallType = "Vinyl")

plot(density(pdat$WindSpeed), col="orange", lwd=2, main="Wind speed exposure")
lines(density(pdat2$WindSpeed), col="purple", lwd=2)
legend("topright", c("Masonry", "Vinyl"), fill=c("orange", "purple"))

# Use a different curve when simulating severities

pdat2$Sev <- a + rgamma(n, shape = phi_shape(pdat2$WindSpeed, inflect=80), scale = s)

# Stack together into one data frame

pdat <- dplyr::bind_rows(pdat, pdat2)
pdat$WallType <- factor(pdat$WallType)

# Plot data and true expectation

palette(c("orange","purple"))
plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity", col=pdat$WallType)
grid <- seq(min(pdat$WindSpeed), max(pdat$WindSpeed), length.out=100)
lines(grid, a + s*phi_shape(grid), col="orange", lwd=2)
lines(grid, a + s*phi_shape(grid, inflect=80), col="purple", lwd=2)
legend("topleft", c("Vinyl true expectation", "Masonry true expectation"), fill=c("purple", "orange"))

# Fit data with ordinary GLM - wall type only, omitting wind speed

m <- glm(Sev ~ WallType,
         family = Gamma(link=log),
         data = pdat)

summary(m)

# Notice that Vinyl appears to have lower expected severity

# Fit data with ordinary GLM - both wall type and wind speed

m <- glm(Sev ~ WindSpeed + WallType,
         family = Gamma(link=log),
         data = pdat)

summary(m)
lines(grid, exp(m$coefficients[1] + m$coefficients[2]*grid + m$coefficients[3]), col="darkred", lwd=5, lty=2)
lines(grid, exp(m$coefficients[1] + m$coefficients[2]*grid), col="pink2", lwd=5, lty=2)
legend("topleft", c("Vinyl true expectation", "Vinyl GLM estimate", "Masonry true expectation", "Masonry GLM estimate"), fill=c("purple", "darkred", "orange", "pink2"))


# At least the coefficient seems to have the right sign at this point.

# Fit data with GAM

m <- gam(Sev ~ s(WindSpeed) + WallType,
         family = Gamma(link=log),
         data = pdat)

summary(m)
lines(grid, predict(m, newdata=data.frame(WindSpeed=grid, WallType="Vinyl"), type="response"), col="darkblue", lwd=5, lty=2)
lines(grid, predict(m, newdata=data.frame(WindSpeed=grid, WallType="Masonry"), type="response"), col="lightblue3", lwd=5, lty=2)
legend("topleft", c("Vinyl true expectation", "Vinyl GLM estimate", "Vinyl GAM estimate", "Masonry true expectation", "Masonry GLM estimate", "Masonry GAM estimate"), fill=c("purple", "darkred", "darkblue", "orange", "pink2", "lightblue3"))

# How does our esimated coefficient compare with the "true" coefficient?
# (There isn't one! The true data-generating mechanism is more complex.)


# Fit data with GAM - allow different wind speed curve for each wall type

m <- gam(Sev ~ s(WindSpeed, by=WallType),
         family = Gamma(link=log),
         data = pdat)

summary(m)

plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity", col=pdat$WallType, main="Estimating curve for each wall type")
lines(grid, a + s*phi_shape(grid), col="orange", lwd=2)
lines(grid, a + s*phi_shape(grid, inflect=80), col="purple", lwd=2)
lines(grid, predict(m, newdata=data.frame(WindSpeed=grid, WallType="Vinyl"), type="response"), col="darkblue", lwd=5, lty=2)
grid2 <- seq(min(pdat$WindSpeed[pdat$WallType=="Masonry"]), max(pdat$WindSpeed[pdat$WallType=="Masonry"]), length.out=100)
lines(grid2, predict(m, newdata=data.frame(WindSpeed=grid2, WallType="Masonry"), type="response"), col="lightblue3", lwd=5, lty=2)
legend("topleft", c("Vinyl true expectation", "Vinyl GAM estimate", "Masonry true expectation", "Masonry GAM estimate"), fill=c("purple", "darkblue", "orange", "lightblue3"))


# Construct variables to have more control over fitting the curves separately

pdat$VinylWindSpeed <- pdat$WindSpeed*(pdat$WallType == "Vinyl")
pdat$MasonryWindSpeed <- pdat$WindSpeed*(pdat$WallType == "Masonry")

m <- gam(Sev ~ s(VinylWindSpeed) + s(MasonryWindSpeed),
         family = Gamma(link=log),
         data = pdat)

summary(m)
plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity", col=pdat$WallType, main="Estimating curve separately")
lines(grid, a + s*phi_shape(grid), col="orange", lwd=2)
lines(grid, a + s*phi_shape(grid, inflect=80), col="purple", lwd=2)
lines(grid, predict(m, newdata=data.frame(VinylWindSpeed=grid, MasonryWindSpeed=0), type="response"), col="darkblue", lwd=5, lty=2)
lines(grid, predict(m, newdata=data.frame(VinylWindSpeed=0, MasonryWindSpeed=grid), type="response"), col="lightblue3", lwd=5, lty=2)
legend("topleft", c("Vinyl true expectation", "Vinyl GAM estimate", "Masonry true expectation", "Masonry GAM estimate"), fill=c("purple", "darkblue", "orange", "lightblue3"))


# Try manually adjusting smoothness

m <- gam(Sev ~ s(VinylWindSpeed, k=3) + s(MasonryWindSpeed),
         family = Gamma(link=log),
         data = pdat)

summary(m)

plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity", col=pdat$WallType, main="Manually adjusting smoothness")
lines(grid, a + s*phi_shape(grid), col="orange", lwd=2)
lines(grid, a + s*phi_shape(grid, inflect=80), col="purple", lwd=2)
lines(grid, predict(m, newdata=data.frame(VinylWindSpeed=grid, MasonryWindSpeed=0), type="response"), col="darkblue", lwd=5, lty=2)
lines(grid, predict(m, newdata=data.frame(VinylWindSpeed=0, MasonryWindSpeed=grid), type="response"), col="lightblue3", lwd=5, lty=2)
legend("topleft", c("Vinyl true expectation", "Vinyl GAM estimate", "Masonry true expectation", "Masonry GAM estimate"), fill=c("purple", "darkblue", "orange", "lightblue3"))


# Enforce monotonicity constraints

library(scam)

m <- scam(Sev ~ s(VinylWindSpeed, bs="mpi") + s(MasonryWindSpeed, bs="mpi"),
         family = Gamma(link=log),
         data = pdat)

summary(m)

plot(pdat$WindSpeed, pdat$Sev, xlab="Wind Speed", ylab="Severity", col=pdat$WallType, main="Enforcing monotonicity constraints")
lines(grid, a + s*phi_shape(grid), col="orange", lwd=2)
lines(grid, a + s*phi_shape(grid, inflect=80), col="purple", lwd=2)
lines(grid, predict(m, newdata=data.frame(VinylWindSpeed=grid, MasonryWindSpeed=0), type="response"), col="darkblue", lwd=5, lty=2)
lines(grid, predict(m, newdata=data.frame(VinylWindSpeed=0, MasonryWindSpeed=grid), type="response"), col="lightblue3", lwd=5, lty=2)
legend("topleft", c("Vinyl true expectation", "Vinyl GAM estimate", "Masonry true expectation", "Masonry GAM estimate"), fill=c("purple", "darkblue", "orange", "lightblue3"))


# Provides a much more accurate understanding of how wall type affects severity
# But doesn't report a coefficient ...
# Clearly, vinyl is worse, but what's the most appropriate rating factor?
# It depends on the distribution of wind speed they might experience.
# For any given distribution of wind speed,
# the appropriate factor can be approximated by simulation:

nsim <- 100000

WindSpeedSim = rgamma(nsim, shape = 28, scale = 3.2)
MASS::truehist(WindSpeedSim)

VSev <- predict(m, newdata=data.frame(VinylWindSpeed=WindSpeedSim, MasonryWindSpeed=0), type="response")
MSev <- predict(m, newdata=data.frame(VinylWindSpeed=0, MasonryWindSpeed=WindSpeedSim), type="response")

VMRatio <- VSev/MSev

MASS::truehist(VMRatio)
sev_factor <- mean(VMRatio)
abline(v=sev_factor, col="blue", lwd=3, lty=2)

sev_factor

# Suppose Vinyl and Masonry have the same expected frequency at every wind speed;
# then this is an appropriate rating factor for customer premiums

# (The log is more comparable to an ordinary GLM coefficient.)

log(sev_factor)


#############





palette("default")
