tensorMult <- function(G, v) {
  # multiply the vector v into the symmetric 3-tensor G
  d <- dim(G)[1]
  M <- matrix(NA, ncol=d, nrow=d)
  for(j in 1:d) {
    M[j, ] <- G[j, , ] %*% v
  }
  return(M)
}

empiricalMoments <- function(X) {
  # Calculate and return the sample mean along with the
  # second- and third-order empirical moments of the points
  n <- nrow(X)
  d <- ncol(X)
  
  # sample mean
  m <- apply(X, 2, mean)
  
  # covariance (second-order central moments)
  Z <- sweep(X, 2, m)
  Sigma <- t(Z) %*% Z / n
  
  # third-order central moments
  Gamma <- array(NA, dim=rep(d, 3))
  for(x in 1:d) {
    for(y in 1:d) {
      for(z in 1:d) {
        s <- 0
        for(j in 1:n) {
          s <- s + Z[j, x]*Z[j, y]*Z[j, z]
        }
        Gamma[x, y, z] <- s/n
      }
    }
  }
  
  return(list(m=m, Sigma=Sigma, Gamma=Gamma))
}


tensorMethod <- function(Sigma, Gamma, k) {
  d <- nrow(Sigma)
  
  # construct whitening and unwhitening transformations
  e <- eigen(Sigma)
  Sigma.whiten <- matrix(0, nrow=d, ncol=d)
  Sigma.unwhiten <- matrix(0, nrow=d, ncol=d)
  for(j in 1:(k-1)) {
    q <- e$vectors[, j]
    l <- sqrt(e$values[j])
    Sigma.whiten <- Sigma.whiten + q %*% t(q) / l
    Sigma.unwhiten <- Sigma.unwhiten + q %*% t(q) * l
  }
  
  # obtain candidate whitened vectors
  q1 <- e$vectors[, 1]; qk <- e$vectors[, k]
  e.white <- eigen(Sigma.whiten %*% tensorMult(Gamma, q1) %*% Sigma.whiten + sqrt(e$values[1])*(q1 %*% t(qk) + qk %*% t(q1)))
  
  # unwhiten, rescale, and correct signs as needed
  mu.hat <- matrix(NA, ncol=d, nrow=k)
  for(j in 1:k) {
    u <- e.white$vectors[, j]
    mu.hat[j, ] <- sqrt(k) * Sigma.unwhiten %*% u
    if(e.white$values[j]*sum(q1*u) < 0) {
      mu.hat[j, ] <- - mu.hat[j, ]
    }
  }
  
  return(mu.hat)
}

empiricalMoments.e1 <- function(X) {
  # Calculate and return the sample mean along with the
  # second- and third-order empirical moments of the points
  n <- nrow(X)
  d <- ncol(X)
  
  # sample mean
  m <- apply(X, 2, mean)
  
  # covariance (second-order central moments)
  Sigma <- cov(X) * (n-1)/n
  
  # third-order moments times e1
  Gamma.e1 <- matrix(0, nrow=d, ncol=d)
  for(i in 1:n) {
    Gamma.e1 <- Gamma.e1 + X[i, 1] * X[i, ] %*% t(X[i, ])
  }
  Gamma.e1 <- Gamma.e1/n
  
  return(list(m=m, Sigma=Sigma, Gamma.e1=Gamma.e1))
}

tensorMethod.e1 <- function(Sigma, Gamma.e1, k) {
  d <- nrow(Sigma)
  
  # construct whitening and unwhitening transformations
  e <- eigen(Sigma)
  Sigma.whiten <- matrix(0, nrow=d, ncol=d)
  Sigma.unwhiten <- matrix(0, nrow=d, ncol=d)
  for(j in 1:(k-1)) {
    if(e$values[j] < 1e-9) break
    q <- e$vectors[, j]
    l <- sqrt(e$values[j])
    Sigma.whiten <- Sigma.whiten + q %*% t(q) / l
    Sigma.unwhiten <- Sigma.unwhiten + q %*% t(q) * l
  }
  
  # obtain candidate whitened vectors
  e1 <- c(1, rep(0, d-1)); qk <- e$vectors[, k]
  e.white <- eigen(Sigma.whiten %*% Gamma.e1 %*% Sigma.whiten + Sigma.unwhiten %*% e1 %*% t(qk) + qk %*% t(e1) %*% Sigma.unwhiten + qk[1]*(diag(d) + qk %*% t(qk)))
  
  # unwhiten, rescale, and correct signs as needed
  mu.hat <- matrix(NA, ncol=d, nrow=k)
  for(j in 1:k) {
    u <- e.white$vectors[, j]
    mu.hat[j, ] <- sqrt(k) * Sigma.unwhiten %*% u
    if(e.white$values[j]*mu.hat[j, 1] < 0) {
      mu.hat[j, ] <- - mu.hat[j, ]
    }
  }
  
  # undo centering
  return(mu.hat)
}


############ EXAMPLE - two components in two dimensions ###########

## Create the component points

mu <- matrix(c(3, 1, -1, -1), ncol=2, byrow=TRUE)
d <- ncol(mu)
k <- nrow(mu)

# standardize them
m <- apply(mu, 2, mean)
e <- eigen(cov(mu))
mu.std <- sweep(mu, 2, m) %*% e$vectors

## Obtain moments of the component points

M <- empiricalMoments(mu.std)

## Apply tensor algorithm to recover the component points

mu.std.hat <- tensorMethod(M$Sigma, M$Gamma, k)
mu.hat <- sweep(mu.std.hat %*% t(e$vectors), 2, -m)

# Verify that the result agrees with original points
plot(mu, col=3, pch="*", cex=6)
points(mu.hat, pch="X", cex=3, col=2)



# Alternatively with centering and Gamma.e1

M.e1 <- empiricalMoments.e1(mu.std)

mu.std.hat <- tensorMethod.e1(M.e1$Sigma, M.e1$Gamma.e1, k)
mu.hat <- sweep(mu.std.hat %*% t(e$vectors), 2, -m)
plot(mu, col=3, pch="*", cex=6)
points(mu.hat, pch="X", cex=3, col=2)



## Now try with data

# Set a true component variance
sigma <- 1

# Generate data
n <- 1000
set.seed(1)
g <- sample(1:k, n, replace=TRUE)
X <- matrix(NA, nrow=n, ncol=d)
for(i in 1:n) {
  X[i, ] <- mu[g[i], ] + rnorm(d, sd=sigma)
}

m.hat <- apply(X, 2, mean)
e.hat <- eigen(cov(X)*(n-1)/n)

X.std <- sweep(X, 2, m.hat) %*% e.hat$vectors

# Estimate sigma and covariance of component points

sigmasq.hat <- mean(e.hat$values[k:d])
Sigma.hat <- matrix(0, nrow=d, ncol=d)
for(j in 1:(k-1)) {
  q <- rep(0, d); q[j] <- 1
  Sigma.hat <- Sigma.hat + (e.hat$values[j] - sigmasq.hat) * q %*% t(q)
}

# Obtain empirical third moments of the data

M.hat <- empiricalMoments(X.std)

# Apply tensor method with components' estimated moments

mu.std.hat <- tensorMethod(Sigma.hat, M.hat$Gamma, k)
mu.hat <- sweep(mu.std.hat %*% t(e.hat$vectors), 2, -m.hat)


# Compare estimates to true parameters

plot(X)
points(mu, col=3, pch="*", cex=6)
points(mu.hat, pch="X", cex=3, col=2)
text(-1, 3, paste("sigma =", sigma), col=3, cex=1.5)
text(-1, 2.5, paste("sigma.hat =", round(sqrt(sigmasq.hat), 3)), col=2, cex=1.5)


# Alternatively with centering and estimating Gamma.e1

M.e1 <- empiricalMoments.e1(X.std)

mu.std.hat <- tensorMethod.e1(Sigma.hat, M.e1$Gamma.e1, k)
mu.hat <- sweep(mu.std.hat %*% t(e.hat$vectors), 2, -m.hat)

plot(X)
points(mu, col=3, pch="*", cex=6)
points(mu.hat, pch="X", cex=3, col=2)

# exactly the same estimate of course, but maybe more computationally efficient




############ EXAMPLE - six components in six dimensions



d <- 6
k <- 6

sigma.mu <- 3 # variance for generating means

mu <- matrix(NA, nrow=k, ncol=d)

# generate each of the k component means, followed by the n draws from that group
set.seed(1)
for(j in 1:k) {
  mu[j, ] <- rnorm(d, sd=sigma.mu)
}

# standardize them
m <- apply(mu, 2, mean)
e <- eigen(cov(mu))
mu.std <- sweep(mu, 2, m) %*% e$vectors

## Obtain moments of the component points

M <- empiricalMoments(mu.std)

## Apply tensor algorithm to recover the component points

mu.std.hat <- tensorMethod(M$Sigma, M$Gamma, k)
mu.hat <- sweep(mu.std.hat %*% t(e$vectors), 2, -m)

# Verify that the result agrees with original points
plot(mu %*% e$vectors, col=3, pch="*", cex=6)
points(mu.hat %*% e$vectors, pch="X", cex=3, col=2)



# Alternatively with centering and Gamma.e1

M.e1 <- empiricalMoments.e1(mu.std)

mu.std.hat <- tensorMethod.e1(M.e1$Sigma, M.e1$Gamma.e1, k)
mu.hat <- sweep(mu.std.hat %*% t(e$vectors), 2, -m)
plot(mu %*% e$vectors, col=3, pch="*", cex=6)
points(mu.hat %*% e$vectors, pch="X", cex=3, col=2)

# exactly the same estimate of course, but maybe more computationally efficient





## Now try with data

# Set a true component variance
sigma <- 1

# Generate data
n <- 1000
set.seed(1)
g <- sample(1:k, n, replace=TRUE)
X <- matrix(NA, nrow=n, ncol=d)
for(i in 1:n) {
  X[i, ] <- mu[g[i], ] + rnorm(d, sd=sigma)
}

m.hat <- apply(X, 2, mean)
e.hat <- eigen(cov(X)*(n-1)/n)

X.std <- sweep(X, 2, m.hat) %*% e.hat$vectors

# Estimate sigma and covariance of component points

sigmasq.hat <- mean(e.hat$values[k:d])
Sigma.hat <- matrix(0, nrow=d, ncol=d)
for(j in 1:(k-1)) {
  #  q <- e.hat$vectors[, j]
  q <- rep(0, d); q[j] <- 1
  Sigma.hat <- Sigma.hat + (e.hat$values[j] - sigmasq.hat) * q %*% t(q)
}

# Obtain empirical third moments of the data

M.hat <- empiricalMoments(X.std)

# Apply tensor method with components' estimated moments

mu.std.hat <- tensorMethod(Sigma.hat, M.hat$Gamma, k)
mu.hat <- sweep(mu.std.hat %*% t(e.hat$vectors), 2, -m.hat)


# Compare estimates to true parameters

png("pca-tensor.png", width=700)
plot(X %*% e$vectors, xlab="First PC", ylab="Second PC",
     main=paste("d = 6, k = 6, n =", n), pch=".", cex=2)
points(mu %*% e$vectors, col=3, pch="O", cex=3)
points(mu.hat %*% e$vectors, pch="X", cex=3, col=2)
legend("bottomright", c("component mean", "tensor method estimate"), col=3:2, pch=c("O", "X"), cex=1.2)
text(-8, -4, expression(paste(sigma," = ", 1)), col=3, cex=1.5)
text(-7.48, -5.1, expression(paste(hat(sigma)," = ", .999)), col=2, cex=1.5)
dev.off()

round(sqrt(sigmasq.hat), 4)


# Alternatively with centering and estimating Gamma.e1

M.e1 <- empiricalMoments.e1(X.std)

mu.std.hat <- tensorMethod.e1(Sigma.hat, M.e1$Gamma.e1, k)
mu.hat <- sweep(mu.std.hat %*% t(e.hat$vectors), 2, -m.hat)

plot(X %*% e$vectors, xlab="First PC", ylab="Second PC",
     main=paste("n =", n))
points(mu %*% e$vectors, col=3, pch="O", cex=3)
points(mu.hat %*% e$vectors, pch="X", cex=3, col=2)
legend("bottomright", c("component mean", "tensor method estimate"), col=3:2, pch=c("O", "X"), cex=1.2)




#####################################################################################




empiricalMoments.e1 <- function(X) {
  # Calculate and return the sample mean along with the
  # second- and third-order empirical moments of the points
  n <- nrow(X)
  d <- ncol(X)
  
  # sample mean
  m <- apply(X, 2, mean)
  
  # covariance (second-order central moments)
  Sigma <- cov(X) * (n-1)/n
  
  # third-order moments times e1
  Gamma.e1 <- matrix(0, nrow=d, ncol=d)
  for(i in 1:n) {
    Gamma.e1 <- Gamma.e1 + X[i, 1] * X[i, ] %*% t(X[i, ])
  }
  Gamma.e1 <- Gamma.e1/n
  
  return(list(m=m, Sigma=Sigma, Gamma.e1=Gamma.e1))
}

expandBasis.3.8 <- function(X, components=FALSE, sigma=1) {
  # expands four-dimensional points into a 14-dimensional space
  # using additional polynomial terms
  
  x1 <- X[, 1]; x2 <- X[, 2]; x3 <- X[, 3]
  x2.2 <- x2^2 + components*sigma^2
  x3.2 <- x3^2 + components*sigma^2
  x1x2 <- x1*x2
  x1x3 <- x1*x3
  x2x3 <- x2*x3
  return(cbind(x1, x2, x3, x2.2, x3.2, x1x2, x1x3, x2x3))
}


tensorMethod.e1 <- function(S, Gamma.e1, k) {
  d <- nrow(S)
  
  # construct whitening and unwhitening transformations
  e <- eigen(S)
  S.whiten <- matrix(0, nrow=d, ncol=d)
  S.unwhiten <- matrix(0, nrow=d, ncol=d)
  for(j in 1:k) {
    if(e$values[j] < 1e-9) break
    q.mat <- e$vectors[, j] %*% t(e$vectors[, j])
    l <- sqrt(e$values[j])
    S.whiten <- S.whiten + q.mat / l
    S.unwhiten <- S.unwhiten + q.mat * l
  }
  
  # obtain candidate whitened vectors
  e.white <- eigen(S.whiten %*% Gamma.e1 %*% S.whiten)
  
  # unwhiten, rescale, and correct signs as needed
  mu.hat <- matrix(NA, ncol=d, nrow=k)
  for(j in 1:k) {
    u <- e.white$vectors[, j]
    mu.hat[j, ] <- sqrt(k) * S.unwhiten %*% u
    if(e.white$values[j]*mu.hat[j, 1] < 0) {
      mu.hat[j, ] <- - mu.hat[j, ]
    }
  }
  
  return(mu.hat)
}




d <- 3
k <- 8

sigma.mu <- 3 # variance for generating means
sigma <- 1 # error variance for generating observations

# generate each of the k component means
mu <- matrix(NA, nrow=k, ncol=d)
set.seed(3)
for(j in 1:k) {
  mu[j, ] <- rnorm(d, sd=sigma.mu)
}




M <- empiricalMoments.e1(mu)
e <- eigen(M$Sigma)
mu.std <- sweep(mu, 2, M$m) %*% e$vectors
#t(mu.std) %*% mu.std / k


## Embed component points into higher-dimensional polynomial space

mu.expand <- expandBasis.3.8(mu.std, components=TRUE, sigma=sigma)

## Obtain moments of the expanded component points

M.expand <- empiricalMoments.e1(mu.expand)

# second moments
S <- M.expand$m %*% t(M.expand$m) + M.expand$Sigma



mu.expand.tensor <- tensorMethod.e1(S, M.expand$Gamma.e1, k)

## Keep first two coordinates and unstandardize

mu.hat <- sweep(mu.expand.tensor[, 1:d] %*% t(e$vectors), 2, -M$m)

# Verify that the result agrees with original points
plot(mu %*% e$vectors, col=3, pch="*", cex=6, xlab="First PC", ylab="Second PC")
points(mu.hat %*% e$vectors, pch="X", cex=3, col=2)






n <- 10000 # observations

X <- matrix(NA, nrow=n, ncol=d)
set.seed(1)
for(i in 1:n) {
  j <- sample(k, 1)
  X[i, ] <- mu[j, ] + sigma*rnorm(d)
}


m.hat <- apply(X, 2, mean)
e.hat <- eigen(cov(X))

X.std <- sweep(X, 2, m.hat) %*% e.hat$vectors

X.expand <- expandBasis.3.8(X.std)

M.hat.expand <- empiricalMoments.e1(X.expand)


# Estimate second moments of mixture

sx2 <- rep(NA, d)
for(j in 1:d) {
  sx2[j] <- mean(X.std[, j]^2) - sigma^2
}

d.expand <- ncol(X.expand)
means <- rep(0, d.expand)
means[4:5] <- sx2[2:3] + sigma^2

cond.covs <- sigma^2*c(1, 1, 1,
                       4*sx2[2]+2*sigma^2, 4*sx2[3]+2*sigma^2,
                       sx2[1]+sx2[2]+sigma^2, sx2[1]+sx2[3]+sigma^2, sx2[2]+sx2[3]+sigma^2)

S.hat <- means %*% t(means) + M.hat.expand$Sigma - diag(cond.covs)

# Estimate Gamma.e1 for mixture

cxyz <- array(NA, dim=rep(d, 3))
for(j in 1:d) {
  for(l in 1:d) {
    for(m in 1:d) {
      cxyz[j, l, m] <- mean(X.std[, j]*X.std[, l]*X.std[, m])
    }
  }
}

C1 <- matrix(0, nrow=d.expand, ncol=d.expand)

C1[2, 6] <- sigma^2*sx2[1]
C1[3, 7] <- sigma^2*sx2[1]

C1[4, 6] <- 2*sigma^2*cxyz[1, 1, 2]
C1[5, 7] <- 2*sigma^2*cxyz[1, 1, 3]

C1[6, 6] <- sigma^2*cxyz[1, 1, 1] + sigma^2*cxyz[2, 2, 1]
C1[7, 7] <- sigma^2*cxyz[1, 1, 1] + sigma^2*cxyz[3, 3, 1]

# B
C1[6, 7] <- sigma^2*cxyz[1, 2, 3]

# C
C1[6, 8] <- sigma^2*cxyz[1, 1, 3]
C1[7, 8] <- sigma^2*cxyz[1, 1, 2]

# E
C1[4, 8] <- 2*sigma^2*cxyz[1, 2, 3]
C1[5, 8] <- 2*sigma^2*cxyz[1, 2, 3]

# F
C1[8, 8] <- sigma^2*cxyz[2, 2, 1]+sigma^2*cxyz[3, 3, 1]

# H
C1[4, 4] <- 4*sigma^2*cxyz[2, 2, 1]
C1[5, 5] <- 4*sigma^2*cxyz[3, 3, 1]


# symmetrize

diags <- diag(C1)
C1 <- C1 + t(C1)
diag(C1) <- diags


# Next estimate C2

C2 <- matrix(0, nrow=d.expand, ncol=d.expand)

C2[1, 4] <- sigma^2*sx2[2]+sigma^4
C2[1, 5] <- sigma^2*sx2[3]+sigma^4

C2[2, 6] <- sigma^2*sx2[2]+sigma^4
C2[3, 7] <- sigma^2*sx2[3]+sigma^4

C2[6, 6] <- 2*sigma^2*cxyz[2, 2, 1]
C2[7, 7] <- 2*sigma^2*cxyz[3, 3, 1]

C2[4, 6] <- sigma^2*cxyz[2, 2, 2]
C2[5, 7] <- sigma^2*cxyz[3, 3, 3]

# A
C2[4, 7] <- sigma^2*cxyz[2, 2, 3]
C2[5, 6] <- sigma^2*cxyz[3, 3, 2]

# B
C2[6, 7] <- 2*sigma^2*cxyz[1, 2, 3]

# C
C2[6, 8] <- sigma^2*cxyz[2, 2, 3]
C2[7, 8] <- sigma^2*cxyz[3, 3, 2]

# symmetrize

diags <- diag(C2)
C2 <- C2 + t(C2)
diag(C2) <- diags


Gamma.e1.hat <- M.hat.expand$Gamma.e1 - C1 - C2


# check moment estimation

#round(S - S.hat, 2)
#round(S, 2)
#round(S.hat, 2)

#round(Gamma.e1.hat - M.expand$Gamma.e1, 2)
#round(M.expand$Gamma.e1, 2)
#round(Gamma.e1.hat, 2)


# obtain component mean estimates

mu.hat.expand <- tensorMethod.e1(S.hat, Gamma.e1.hat, k)

## Keep first d coordinates and unstandardize

mu.hat <- sweep(mu.hat.expand[, 1:d] %*% t(e.hat$vectors), 2, -m.hat)

# Compare estimates to true parameters by plotting

png(paste0("basis-expansion-", n, ".png"), width=700, height=300)
plot(X.std %*% e$vectors, xlab="First PC", ylab="Second PC",
     main=paste("d = 3, k = 8, n =", n), pch=".", cex=2)
points(mu.expand[, 1:d]  %*% e$vectors, col=3, pch="O", cex=3)
points(mu.hat.expand[, 1:d]  %*% e$vectors, pch="X", cex=3, col=2)
legend("topleft", c("component mean", "tensor method estimate"), col=3:2, pch=c("O", "X"), cex=1.2)
dev.off()



# show the plot for n=10,000 and 100,000 and 1,000,000 also for comparison


###############################################################################
# FIVE DIMENSIONAL EXAMPLE, expanded to 20 dimensions to estimate k=20 components
# (incomplete, but it provides all the correction matrix entries)




empiricalMoments.e1 <- function(X) {
  # Calculate and return the sample mean along with the
  # second- and third-order empirical moments of the points
  n <- nrow(X)
  d <- ncol(X)
  
  # sample mean
  m <- apply(X, 2, mean)
  
  # covariance (second-order central moments)
  Sigma <- cov(X) * (n-1)/n
  
  # third-order moments times e1
  Gamma.e1 <- matrix(0, nrow=d, ncol=d)
  for(i in 1:n) {
    Gamma.e1 <- Gamma.e1 + X[i, 1] * X[i, ] %*% t(X[i, ])
  }
  Gamma.e1 <- Gamma.e1/n
  
  return(list(m=m, Sigma=Sigma, Gamma.e1=Gamma.e1))
}

expandBasis.5 <- function(X, sigma=1, components=FALSE) {
  # expands five-dimensional points into a twenty-dimensional space
  # using additional polynomial terms
  
  x1 <- X[, 1]; x2 <- X[, 2]; x3 <- X[, 3]; x4 <- X[, 4]; x5 <- X[, 5]
  x1.2 <- x1^2 + components*sigma^2
  x2.2 <- x2^2 + components*sigma^2
  x3.2 <- x3^2 + components*sigma^2
  x4.2 <- x4^2 + components*sigma^2
  x5.2 <- x5^2 + components*sigma^2
  x1x2 <- x1*x2
  x1x3 <- x1*x3
  x1x4 <- x1*x4
  x1x5 <- x1*x5
  x2x3 <- x2*x3
  x2x4 <- x2*x4
  x2x5 <- x2*x5
  x3x4 <- x3*x4
  x3x5 <- x3*x5
  x4x5 <- x4*x5
  return(cbind(x1, x2, x3, x4, x5, x1.2, x2.2, x3.2, x4.2, x5.2,
               x1x2, x1x3, x1x4, x1x5, x2x3, x2x4, x2x5, x3x4, x3x5, x4x5))
}


tensorMethod.e1 <- function(S, Gamma.e1, k) {
  d <- nrow(S)
  
  # construct whitening and unwhitening transformations
  e <- eigen(S)
  S.whiten <- matrix(0, nrow=d, ncol=d)
  S.unwhiten <- matrix(0, nrow=d, ncol=d)
  for(j in 1:k) {
    if(e$values[j] < 1e-9) break
    q.mat <- e$vectors[, j] %*% t(e$vectors[, j])
    l <- sqrt(e$values[j])
    S.whiten <- S.whiten + q.mat / l
    S.unwhiten <- S.unwhiten + q.mat * l
  }
  
  # obtain candidate whitened vectors
  e.white <- eigen(S.whiten %*% Gamma.e1 %*% S.whiten)
  
  # unwhiten, rescale, and correct signs as needed
  mu.hat <- matrix(NA, ncol=d, nrow=k)
  for(j in 1:k) {
    u <- e.white$vectors[, j]
    mu.hat[j, ] <- sqrt(k) * S.unwhiten %*% u
    if(e.white$values[j]*mu.hat[j, 1] < 0) {
      mu.hat[j, ] <- - mu.hat[j, ]
    }
  }
  
  return(mu.hat)
}




d <- 5
k <- 20

sigma.mu <- 3 # variance for generating means
sigma <- 1 # error variance for generating observations

# generate each of the k component means
mu <- matrix(NA, nrow=k, ncol=d)
set.seed(2)
for(j in 1:k) {
  mu[j, ] <- rnorm(d, sd=sigma.mu)
}




M <- empiricalMoments.e1(mu)
e <- eigen(M$Sigma)
mu.std <- sweep(mu, 2, M$m) %*% e$vectors
#t(mu.std) %*% mu.std / k


## Embed component points into higher-dimensional polynomial space

mu.expand <- expandBasis.5(mu.std, sigma, components=TRUE)

## Obtain moments of the expanded component points

M.expand <- empiricalMoments.e1(mu.expand)

# second moments
S <- M.expand$m %*% t(M.expand$m) + M.expand$Sigma



mu.expand.tensor <- tensorMethod.e1(S, M.expand$Gamma.e1, k)

## Keep first two coordinates and unstandardize

mu.hat <- sweep(mu.expand.tensor[, 1:d] %*% t(e$vectors), 2, -M$m)

# Verify that the result agrees with original points
plot(mu, col=3, pch="*", cex=6)
points(mu.hat, pch="X", cex=3, col=2)






# Calculate quantities needed for expansion and correction

sx2 <- rep(NA, d)
for(j in 1:d) {
  sx2[j] <- mean(mu.std[, j]^2)
}

cxyz <- array(NA, dim=rep(d, 3))
for(j in 1:d) {
  for(l in 1:d) {
    for(m in 1:d) {
      cxyz[j, l, m] <- mean(mu.std[, j]*mu.std[, l]*mu.std[, m])
    }
  }
}


# CONSTRUCT "CORRECTION" matrices

# at this point, it's clear that it would be better not to divide by these denominators
# and instead take them into account with adjusting covariance
# -- oh well

# First estimate C1

d.expand <- 20
C1 <- matrix(0, nrow=d.expand, ncol=d.expand)

C1[1, 6] <- 2*sigma^2*sx2[1]

C1[2, 11] <- sigma^2*sx2[1]
C1[3, 12] <- sigma^2*sx2[1]
C1[4, 13] <- sigma^2*sx2[1]
C1[5, 14] <- sigma^2*sx2[1]

C1[6, 6] <- 4*sigma^2*cxyz[1, 1, 1]

C1[6, 11] <- 2*sigma^2*cxyz[1, 1, 2]
C1[6, 12] <- 2*sigma^2*cxyz[1, 1, 3]
C1[6, 13] <- 2*sigma^2*cxyz[1, 1, 4]
C1[6, 14] <- 2*sigma^2*cxyz[1, 1, 5]

C1[7, 11] <- 2*sigma^2*cxyz[1, 1, 2]
C1[8, 12] <- 2*sigma^2*cxyz[1, 1, 3]
C1[9, 13] <- 2*sigma^2*cxyz[1, 1, 4]
C1[10, 14] <- 2*sigma^2*cxyz[1, 1, 5]

C1[11, 11] <- sigma^2*cxyz[1, 1, 1] + sigma^2*cxyz[2, 2, 1]
C1[12, 12] <- sigma^2*cxyz[1, 1, 1] + sigma^2*cxyz[3, 3, 1]
C1[13, 13] <- sigma^2*cxyz[1, 1, 1] + sigma^2*cxyz[4, 4, 1]
C1[14, 14] <- sigma^2*cxyz[1, 1, 1] + sigma^2*cxyz[5, 5, 1]

# B
C1[11, 12] <- sigma^2*cxyz[1, 2, 3]
C1[11, 13] <- sigma^2*cxyz[1, 2, 4]
C1[11, 14] <- sigma^2*cxyz[1, 2, 5]
C1[12, 13] <- sigma^2*cxyz[1, 3, 4]
C1[12, 14] <- sigma^2*cxyz[1, 3, 5]
C1[13, 14] <- sigma^2*cxyz[1, 4, 5]

# C
C1[11, 15] <- sigma^2*cxyz[1, 1, 3]
C1[11, 16] <- sigma^2*cxyz[1, 1, 4]
C1[11, 17] <- sigma^2*cxyz[1, 1, 5]
C1[12, 15] <- sigma^2*cxyz[1, 1, 2]
C1[12, 18] <- sigma^2*cxyz[1, 1, 4]
C1[12, 19] <- sigma^2*cxyz[1, 1, 5]
C1[13, 16] <- sigma^2*cxyz[1, 1, 2]
C1[13, 18] <- sigma^2*cxyz[1, 1, 3]
C1[13, 20] <- sigma^2*cxyz[1, 1, 5]
C1[14, 17] <- sigma^2*cxyz[1, 1, 2]
C1[14, 19] <- sigma^2*cxyz[1, 1, 3]
C1[14, 20] <- sigma^2*cxyz[1, 1, 4]

# E
C1[7, 15] <- 2*sigma^2*cxyz[1, 2, 3]
C1[7, 16] <- 2*sigma^2*cxyz[1, 2, 4]
C1[7, 17] <- 2*sigma^2*cxyz[1, 2, 5]
C1[8, 15] <- 2*sigma^2*cxyz[1, 3, 2]
C1[8, 18] <- 2*sigma^2*cxyz[1, 3, 4]
C1[8, 19] <- 2*sigma^2*cxyz[1, 3, 5]
C1[9, 16] <- 2*sigma^2*cxyz[1, 4, 2]
C1[9, 18] <- 2*sigma^2*cxyz[1, 4, 3]
C1[9, 20] <- 2*sigma^2*cxyz[1, 4, 5]
C1[10, 17] <- 2*sigma^2*cxyz[1, 5, 2]
C1[10, 19] <- 2*sigma^2*cxyz[1, 5, 3]
C1[10, 20] <- 2*sigma^2*cxyz[1, 5, 4]

# F
C1[15, 15] <- (sigma^2*cxyz[2, 2, 1]+sigma^2*cxyz[3, 3, 1])
C1[16, 16] <- (sigma^2*cxyz[2, 2, 1]+sigma^2*cxyz[4, 4, 1])
C1[17, 17] <- (sigma^2*cxyz[2, 2, 1]+sigma^2*cxyz[5, 5, 1])
C1[18, 18] <- (sigma^2*cxyz[3, 3, 1]+sigma^2*cxyz[4, 4, 1])
C1[19, 19] <- (sigma^2*cxyz[3, 3, 1]+sigma^2*cxyz[5, 5, 1])
C1[20, 20] <- (sigma^2*cxyz[4, 4, 1]+sigma^2*cxyz[5, 5, 1])

# H
C1[7, 7] <- 4*sigma^2*cxyz[2, 2, 1]
C1[8, 8] <- 4*sigma^2*cxyz[3, 3, 1]
C1[9, 9] <- 4*sigma^2*cxyz[4, 4, 1]
C1[10, 10] <- 4*sigma^2*cxyz[5, 5, 1]

# J
C1[15, 16] <- sigma^2*cxyz[1, 3, 4]
C1[15, 17] <- sigma^2*cxyz[1, 3, 5]
C1[15, 18] <- sigma^2*cxyz[1, 2, 4]
C1[15, 19] <- sigma^2*cxyz[1, 2, 5]
C1[16, 17] <- sigma^2*cxyz[1, 4, 5]
C1[16, 18] <- sigma^2*cxyz[1, 2, 3]
C1[16, 20] <- sigma^2*cxyz[1, 2, 5]
C1[17, 19] <- sigma^2*cxyz[1, 2, 3]
C1[17, 20] <- sigma^2*cxyz[1, 2, 4]
C1[18, 19] <- sigma^2*cxyz[1, 4, 5]
C1[18, 20] <- sigma^2*cxyz[1, 3, 5]
C1[19, 20] <- sigma^2*cxyz[1, 3, 4]


# symmetrize

diags <- diag(C1)
C1 <- C1 + t(C1)
diag(C1) <- diags


# Next estimate C2

C2 <- matrix(0, nrow=d.expand, ncol=d.expand)

C2[1, 6] <- 3*sigma^2*sx2[1]+3*sigma^4

C2[1, 7] <- sigma^2*sx2[2]+sigma^4
C2[1, 8] <- sigma^2*sx2[3]+sigma^4
C2[1, 9] <- sigma^2*sx2[4]+sigma^4
C2[1, 10] <- sigma^2*sx2[5]+sigma^4

C2[2, 11] <- sigma^2*sx2[2]+sigma^4
C2[3, 12] <- sigma^2*sx2[3]+sigma^4
C2[4, 13] <- sigma^2*sx2[4]+sigma^4
C2[5, 14] <- sigma^2*sx2[5]+sigma^4

C2[11, 11] <- 2*sigma^2*cxyz[2, 2, 1]
C2[12, 12] <- 2*sigma^2*cxyz[3, 3, 1]
C2[13, 13] <- 2*sigma^2*cxyz[4, 4, 1]
C2[14, 14] <- 2*sigma^2*cxyz[5, 5, 1]

C2[6, 11] <- 3*sigma^2*cxyz[1, 1, 2]
C2[6, 12] <- 3*sigma^2*cxyz[1, 1, 3]
C2[6, 13] <- 3*sigma^2*cxyz[1, 1, 4]
C2[6, 14] <- 3*sigma^2*cxyz[1, 1, 5]

C2[7, 11] <- sigma^2*cxyz[2, 2, 2]
C2[8, 12] <- sigma^2*cxyz[3, 3, 3]
C2[9, 13] <- sigma^2*cxyz[4, 4, 4]
C2[10, 14] <- sigma^2*cxyz[5, 5, 5]

C2[6, 6] <- 4*sigma^2*cxyz[1, 1, 1]

C2[6, 7] <- 2*sigma^2*cxyz[2, 2, 1]
C2[6, 8] <- 2*sigma^2*cxyz[3, 3, 1]
C2[6, 9] <- 2*sigma^2*cxyz[4, 4, 1]
C2[6, 10] <- 2*sigma^2*cxyz[5, 5, 1]

# A
C2[7, 12] <- sigma^2*cxyz[2, 2, 3]
C2[7, 13] <- sigma^2*cxyz[2, 2, 4]
C2[7, 14] <- sigma^2*cxyz[2, 2, 5]
C2[8, 11] <- sigma^2*cxyz[3, 3, 2]
C2[8, 13] <- sigma^2*cxyz[3, 3, 4]
C2[8, 14] <- sigma^2*cxyz[3, 3, 5]
C2[9, 11] <- sigma^2*cxyz[4, 4, 2]
C2[9, 12] <- sigma^2*cxyz[4, 4, 3]
C2[9, 14] <- sigma^2*cxyz[4, 4, 5]
C2[10, 11] <- sigma^2*cxyz[5, 5, 2]
C2[10, 12] <- sigma^2*cxyz[5, 5, 3]
C2[10, 13] <- sigma^2*cxyz[5, 5, 4]

# B
C2[11, 12] <- 2*sigma^2*cxyz[1, 2, 3]
C2[11, 13] <- 2*sigma^2*cxyz[1, 2, 4]
C2[11, 14] <- 2*sigma^2*cxyz[1, 2, 5]
C2[12, 13] <- 2*sigma^2*cxyz[1, 3, 4]
C2[12, 14] <- 2*sigma^2*cxyz[1, 3, 5]
C2[13, 14] <- 2*sigma^2*cxyz[1, 4, 5]

# C
C2[11, 15] <- sigma^2*cxyz[2, 2, 3]
C2[11, 16] <- sigma^2*cxyz[2, 2, 4]
C2[11, 17] <- sigma^2*cxyz[2, 2, 5]
C2[12, 15] <- sigma^2*cxyz[3, 3, 2]
C2[12, 18] <- sigma^2*cxyz[3, 3, 4]
C2[12, 19] <- sigma^2*cxyz[3, 3, 5]
C2[13, 16] <- sigma^2*cxyz[4, 4, 2]
C2[13, 18] <- sigma^2*cxyz[4, 4, 3]
C2[13, 20] <- sigma^2*cxyz[4, 4, 5]
C2[14, 17] <- sigma^2*cxyz[5, 5, 2]
C2[14, 19] <- sigma^2*cxyz[5, 5, 3]
C2[14, 20] <- sigma^2*cxyz[5, 5, 4]

# D
C2[11, 18] <- sigma^2*cxyz[2, 3, 4]
C2[11, 19] <- sigma^2*cxyz[2, 3, 5]
C2[11, 20] <- sigma^2*cxyz[2, 4, 5]
C2[12, 16] <- sigma^2*cxyz[3, 2, 4]
C2[12, 17] <- sigma^2*cxyz[3, 2, 5]
C2[12, 20] <- sigma^2*cxyz[3, 4, 5]
C2[13, 15] <- sigma^2*cxyz[4, 2, 3]
C2[13, 17] <- sigma^2*cxyz[4, 2, 5]
C2[13, 19] <- sigma^2*cxyz[4, 3, 5]
C2[14, 15] <- sigma^2*cxyz[5, 2, 3]
C2[14, 16] <- sigma^2*cxyz[5, 2, 4]
C2[14, 18] <- sigma^2*cxyz[5, 3, 4]

# G
C2[6, 15] <- 2*sigma^2*cxyz[1, 2, 3]
C2[6, 16] <- 2*sigma^2*cxyz[1, 2, 4]
C2[6, 17] <- 2*sigma^2*cxyz[1, 2, 5]
C2[6, 18] <- 2*sigma^2*cxyz[1, 3, 4]
C2[6, 19] <- 2*sigma^2*cxyz[1, 3, 5]
C2[6, 20] <- 2*sigma^2*cxyz[1, 4, 5]

# symmetrize

diags <- diag(C2)
C2 <- C2 + t(C2)
diag(C2) <- diags




cond.covs <- sigma^2*c(1, 1, 1, 1, 1,
                       4*sx2[1]+2*sigma^2, 4*sx2[2]+2*sigma^2, 4*sx2[3]+2*sigma^2, 4*sx2[4]+2*sigma^2, 4*sx2[5]+2*sigma^2,
                       sx2[1]+sx2[2]+sigma^2, sx2[1]+sx2[3]+sigma^2, sx2[1]+sx2[4]+sigma^2, sx2[1]+sx2[5]+sigma^2,
                       sx2[2]+sx2[3]+sigma^2, sx2[2]+sx2[4]+sigma^2, sx2[2]+sx2[5]+sigma^2,
                       sx2[3]+sx2[4]+sigma^2, sx2[3]+sx2[5]+sigma^2,
                       sx2[4]+sx2[5]+sigma^2)



# Okay, let's try the tensor method with these things (NOTE: USING SOME TRUE QUANTITIES)

n <- 100000 # observations per group

# generate n draws from each group
X <- matrix(NA, nrow=n*k, ncol=d)
set.seed(1)
for(j in 1:k) {
  for(i in 1:n) {
    X[(j-1)*n + i, ] <- mu[j, ] + rnorm(d, sd=sigma)
  }
}

M.hat <- empiricalMoments.e1(X)

# I need to use MUCH larger sample sizes:
# rather than keeping all the data, I'm just going to directly generate the (expanded) moments!
# also, just draw directly from mu.std and don't rotate or translate anything

d.expand <- 20
n <- 1000000 # total observations (no longer using the same number per group)
set.seed(1)
means <- rep(0, d.expand)
means[6:10] <- sx2 + sigma^2
Sigma <- matrix(0, nrow=d.expand, ncol=d.expand)
Gamma.e1 <- matrix(0, nrow=d.expand, ncol=d.expand)

# for now, save results for debugging
#X <- matrix(NA, nrow=n, ncol=d.expand)

for(i in 1:n) {
  xi <- mu.std[sample(1:k, 1), ] + sigma*rnorm(d)
  xi.expand <- expandBasis.5(matrix(xi, nrow=1))
  
  #  X[i, ] <- xi.expand
  
  # subtraction of mean is automatic because mean is approximately zero
  # covariance (second-order central moments)
  cent <- xi.expand - means
  Sigma <- Sigma + t(cent) %*% cent
  # third-order moments times e1
  Gamma.e1 <- Gamma.e1 + xi[1] * t(xi.expand) %*% xi.expand
}
Sigma <- Sigma/n
Gamma.e1 <- Gamma.e1/n








#e.hat <- eigen(M.hat$Sigma)

#X.std <- sweep(X, 2, M.hat$m) %*% e.hat$vectors
#X.std <- sweep(X, 2, M.hat$m) %*% e$vectors

#X.expand <- expandBasis.5(X.std, sigma)

## Obtain moments of the expanded component points

#M.hat.expand <- empiricalMoments.e1(X.expand)

S.hat <- means %*% t(means) + Sigma - diag(cond.covs)

Gamma.e1.hat <- Gamma.e1 - C1 - C2

# CHECK ESTIMATION OF Gamma.e1
#M.hat.expand$Gamma.e1 - C1.hat - C2.hat
#M.expand$Gamma.e1
#round(Gamma.e1.hat - M.expand$Gamma.e1, 2)
#Gamma.e1.err[, , j] <- Gamma.e1.hat - M.expand$Gamma.e1

# CHECK ESTIMATION OF S
#round(S-S.hat, 2)

# CHECK ESTIMATION OF MEAN TIMES MEAN TRANSPOSE
#round(M.hat.expand$m %*% t(M.hat.expand$m) - M.expand$m %*% t(M.expand$m), 3)
# these are the only ones that should be non-zero anyway.
# (may as well just use zeros for the rest of the entries)
#M2 <- M.expand$m %*% t(M.expand$m)
#M2.hat <- M.hat.expand$m %*% t(M.hat.expand$m)
#round(M2[6:10, 6:10], 3)
#round(M2.hat[6:10, 6:10], 3)




# Apply tensor method with components' estimated moments

mu.hat.expand <- tensorMethod.e1(S.hat, Gamma.e1.hat, k)

mu.hat.expand <- tensorMethod.e1(S, M.expand$Gamma.e1, k)
# works when you substitute the true moments
# I just need to estimate them well!
# how well ? ...

mu.hat.expand <- tensorMethod.e1(signif(S, 5), signif(M.expand$Gamma.e1, 5), k)
# it appears that five sig figs are needed to get a really good estimate
# (at least in this case)

# What sample size do I need for this?


## Keep first two coordinates and unstandardize

mu.hat <- sweep(mu.hat.expand[, 1:d] %*% t(e.hat$vectors), 2, -M.hat$m)

# Compare estimates to true parameters

#plot(sweep(X[sample(n*k, 500), ], 2, M.hat$m))
#points(mu.expand[, 1:d] %*% t(e$vectors), col=3, pch="*", cex=6)
#points(mu.hat.expand[, 1:d] %*% t(e$vectors), pch="X", cex=3, col=2)
##points(mu.hat.expand[, 1:d] %*% t(e.hat$vectors), pch="X", cex=3, col=2)

plot(X[sample(n, 500), ])
points(mu.expand[, 1:d], col=3, pch="*", cex=6)
points(mu.hat.expand[, 1:d], pch="X", cex=3, col=2)



# you get the right eigenvalues by subtracting sigma^2
# but that doesn't affect the rotation ...



mu.hat.expand[, 1:d] %*% t(e$vectors)
mu.hat.expand[, 1:d] %*% t(e.hat$vectors)

# the eigenvectors in e can have opposite sign from e.hat
# (in practice, this issue takes care of itself
# but it's causing problems in my debugging process)



mu.hat.expand[1, 1:d]
e$vectors[, 1]
e.hat$vectors[, 1]

sum(mu.hat.expand[1, 1:d]*e$vectors[, 1])
sum(mu.hat.expand[1, 1:d]*e.hat$vectors[, 1])



#mu.hat <- sweep(mu.hat.expand[, 1:d] %*% t(e.hat$vectors), 2, -M$m)
mu.hat <- sweep(mu.hat.expand[, 1:d] %*% t(e$vectors), 2, -M$m)

# Verify that the result agrees with original points
plot(mu, col=3, pch="*", cex=6)
points(mu.hat, pch="X", cex=3, col=2)




# tensor method not working because the "whitening" of Gamma.e1 leads to all NAs


# construct whitening and unwhitening transformations
e.tm <- eigen(S.hat)
S.whiten <- matrix(0, nrow=d.expand, ncol=d.expand)
S.unwhiten <- matrix(0, nrow=d.expand, ncol=d.expand)
for(j in 1:k) {
  q.mat <- e.tm$vectors[, j] %*% t(e.tm$vectors[, j])
  if(l < 1e-4) break
  l <- sqrt(e.tm$values[j])
  S.whiten <- S.whiten + q.mat / l
  S.unwhiten <- S.unwhiten + q.mat * l
}

# obtain candidate whitened vectors
e.white <- eigen(S.whiten %*% Gamma.e1.hat %*% S.whiten)

# unwhiten, rescale, and correct signs as needed
mu.hat <- matrix(NA, ncol=d, nrow=k)
for(j in 1:k) {
  u <- e.white$vectors[, j]
  mu.hat[j, ] <- sqrt(k) * S.unwhiten %*% u
  if(e.white$values[j]*mu.hat[j, 1] < 0) {
    mu.hat[j, ] <- - mu.hat[j, ]
  }
}



