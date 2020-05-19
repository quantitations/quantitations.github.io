
# install.packages("RColorBrewer")
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

# Plot for two quantitative variables with clusters each having its own line
CLplot <- function(x, y, clusters, int.mean=NULL, slope.mean=NULL, cluster.int.effects=NULL, cluster.slope.effects=NULL, hide.points=FALSE, ...) {
  xrange <- range(x)
  clusters <- as.factor(clusters)
  k <- length(levels(clusters))
  pal <- getPalette(k)
  plot(x, y, type="n", ...)
  if(!hide.points) points(x, y, col=pal[clusters])
  if(!is.null(int.mean)) {
    for(j in 1:k) {
      abline(int.mean+cluster.int.effects[j], slope.mean+cluster.slope.effects[j], col=pal[j], lwd=2)
    }
    abline(int.mean, slope.mean, lwd=6)
  }
}


# Plot for clustered repeated measurements
CRMplot <- function(measurements, subjects, clusters, groups, subject.effects=NULL,
                    cluster.effects=NULL, group.expectations=NULL, hide.points=FALSE, legendposition="topleft", ...) {
  subjects <- as.factor(subjects)
  clusters <- as.factor(clusters)
  groups <- as.factor(groups)
  N <- length(measurements)
  n <- length(levels(subjects))
  m <- length(levels(clusters))
  k <- length(levels(groups))
  # create also the cluster vector corresponding to subjects
  clusters.subj <- rep(NA, n)
  for(i in 1:n) {
    clusters.subj[i] <- clusters[which(subjects==levels(subjects)[i])[1]]
  }
  # create also the group vector corresponding to subjects
  groups.subj <- rep(NA, n)
  for(i in 1:n) {
    groups.subj[i] <- groups[which(subjects==levels(subjects)[i])[1]]
  }
  
  pal <- suppressWarnings(brewer.pal(n=k, name="Set1"))
  
  if(!is.null(cluster.effects)) {
    cluster.exp <- matrix(nrow=m, ncol=k)
    for(j in 1:k) {
      cluster.exp[, j] <- group.expectations[j] + cluster.effects
    }
  } else cluster.exp <- NA
  
  if(!is.null(subject.effects)) {
    subject.exp <- rep(NA, n)
    for(i in 1:n) {
      subject.exp[i] <- group.expectations[groups.subj[i]] + cluster.effects[clusters.subj[i]] + subject.effects[i]
    }
  } else subject.exp <- NA
  
  # draw blank plot that will contain everything
  plot(c(.5, m+.5), range(measurements, cluster.exp, subject.exp, group.expectations, na.rm=TRUE), type="n", ...)
  for(j in seq(.5, m+.5, by=1)){
    abline(v=j, col=gray(.5))
  }
  if(!is.null(group.expectations)) {
    for(j in 1:k) {
      abline(h=group.expectations[j], col=adjustcolor(pal[j], alpha.f=.2), lwd=4)
    }
  }
  
  for(j in 1:m) {
    start <- j-.5
    for(g in 1:k) {
      w <- 1/k
      these <- which(clusters==levels(clusters)[j] & groups==levels(groups)[g])
      subjs <- unique(subjects[these])
      n.s <- length(subjs)
      xvals <- seq(start+.05, start+w-.05, length.out=n.s)
      for(i in 1:n.s) {
        abline(v=xvals[i], col=gray(.9))
        subj <- which(subjects==subjs[i])
        if(!is.null(subject.effects)) {
          points(xvals[i], subject.exp[as.numeric(subjs[i])], pch=18, cex=1.2, col=pal[g])
        }
        if(!hide.points) {
          s <- measurements[subj]
          points(rep(xvals[i], length(s)), s, col=pal[g], cex=.8)
        }
      }
      if(!is.null(cluster.effects)) {
        lines(c(start, start+w), rep(group.expectations[g] + cluster.effects[j], 2), col=pal[g], lwd=2)
      }
      start <- start+w
    }
  }
  legend("topleft", legend=levels(groups), fill=pal[1:k])
}



LGplot <- function(measurements, subjects, times, groups, subject.effects=NULL, group.expectations=NULL, hide.points=FALSE, ...) {
  k <- length(levels(groups))
  n <- length(levels(subjects))
  groups.subj <- rep(NA, n)
  for(i in 1:n) {
    groups.subj[i] <- groups[which(subjects==levels(subjects)[i])[1]]
  }
  
  pal <- suppressWarnings(brewer.pal(n=k, name="Set1"))
  
  plot(times, measurements, type="n", ...)
  
  if(!is.null(group.expectations)) {
    for(j in 1:k) {
      abline(0, group.expectations[j], col=adjustcolor(pal[j], alpha.f=.4), lwd=6)
    }
  }
  if(!is.null(subject.effects)) {
    for(j in 1:k) {
      these <- which(groups.subj==j)
      for(i in these) {
        abline(0, group.expectations[j]+subject.effects[i], col=pal[j], lty=2, lwd=2)
      }
    }
  }
  if(!hide.points) {
    for(i in 1:n) {
      these <- which(subjects==i)
      lines(times[these], measurements[these], col=pal[groups.subj[i]])
    }
  }
  
  legend("topleft", legend=levels(groups), fill=pal)
}


