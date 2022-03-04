
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
      num <- sample(n.rep, 1)
      measurement <- c(measurement, group.means[j] + subject.effects[id] + rnorm(num, sd=sigma))
      subject <- c(subject, rep(id, num))
      group <- c(group, rep(j, num))
      id <- id+1
    }
  }
  x <- data.frame(measurement=round(measurement, decs), subject=factor(subject), group=factor(group))
  
  return(list(x=x, subject.effects=subject.effects))
}

# Simulating data points drawn from different lines
simulateCL <- function(x.mean, int.cent.mean, sd.int.cent=0, slope.mean, sd.slope=0, n.cluster, sd.cluster.mean, sd.cluster, n.obs, sigma, decs.y=0, decs.x=2) {
  int.cent.effects <- rnorm(n.cluster, sd=sd.int.cent)
  slope.effects <- rnorm(n.cluster, sd=sd.slope)
  cluster <- c()
  x <- c()
  y <- c()
  # Generate observations for each cluster
  for(i in 1:n.cluster) {
    clust.mean <- rnorm(1, mean=x.mean, sd=sd.cluster.mean)
    n <- sample(n.obs, 1)
    x.clust <- rnorm(n, mean=clust.mean, sd=sd.cluster)
    y.clust <- (int.cent.mean + int.cent.effects[i]) + (slope.mean + slope.effects[i])*(x.clust - x.mean) + rnorm(n, sd=sigma)
    cluster <- c(cluster, rep(i, n))
    x <- c(x, x.clust)
    y <- c(y, y.clust)
  }
  x <- data.frame(y=round(y, decs.y), x=round(x, decs.x), cluster=factor(cluster))
  
  return(list(x=x, int.effects=int.cent.effects-slope.effects*x.mean, slope.effects=slope.effects))
}

# Simulating clustered repeated measurements with groups
simulateCRM <- function(group.means, n.clust, sd.clust, n.subj, sd.subj, n.rep, sigma, decs=0) {
  clust.effects <- rnorm(n.clust, sd=sd.clust)
  subj.effects <- c()
  measurement <- c()
  cluster <- c()
  subject <- c()
  group <- c()
  k <- length(group.means)
  id <- 1
  for(j in 1:n.clust) {
    for(g in 1:k) {
      n.s <- sample(n.subj, 1)
      eff <- rnorm(n.s, sd=sd.subj)
      subj.effects <- c(subj.effects, eff)
      for(i in 1:n.s) {
        num <- sample(n.rep, 1)
        measurement <- c(measurement, group.means[g] + clust.effects[j] + eff[i] + rnorm(num, sd=sigma))
        subject <- c(subject, rep(id, num))
        cluster <- c(cluster, rep(j, num))
        group <- c(group, rep(g, num))
        id <- id+1
      }
    }
  }
  x <- data.frame(measurement=round(measurement, decs), subject=factor(subject), cluster=factor(cluster), group=factor(group))
  
  return(list(x=x, cluster.effects=clust.effects, subject.effects=subj.effects))
}

# Simulating longitudinal growth from zero among groups
simulateLG <- function(slopes, sd.slope, n.g, d, sigma, decs=1) {
  # generate the individual trees' expected slopes
  k <- length(slopes)
  measurements <- c()
  groups <- c()
  subj <- c()
  subj.effects <- c()
  id <- 1
  for(j in 1:k) {
    nj <- sample(n.g, 1)
    effs <- rnorm(nj, sd=sd.slope)
    slopes.g <- slopes[j] + effs
    for(i in 1:nj) {
      m <- 0
      for(l in 1:d) {
        m <- m + slopes.g[i] + rnorm(1, sd=sigma)
        measurements <- c(measurements, m)
        subj <- c(subj, id)
      }
      id <- id+1
    }
    subj.effects <- c(subj.effects, effs)
    groups <- c(groups, rep(j, nj*d))
  }
  x <- data.frame(measurements=round(measurements, decs), subject=factor(subj), time=rep(1:d, id-1), groups=factor(groups))

  return(list(x=x, subject.effects=subj.effects))
}

