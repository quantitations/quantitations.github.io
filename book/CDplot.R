CDplot <- function(a, b, ...) {
  # a is quantitative and b is categorical
  categories <- levels(b)
  k <- length(categories)
  densities <- list()
  xmin <- rep(NA, k); xmax <- rep(NA, k)
  ymin <- rep(NA, k); ymax <- rep(NA, k)
  for(i in 1:k) {
    categorydata <- a[b==categories[i]]
    densities[[i]] <- density(categorydata)
    xmin[i] <- min(densities[[i]]$x)
    xmax[i] <- max(densities[[i]]$x)
    ymin[i] <- min(densities[[i]]$y)
    ymax[i] <- max(densities[[i]]$y)
  }
  # NOW DRAW A PLOT AND A LEGEND
  plot(c(min(xmin), max(xmax)), c(min(ymin), max(ymax)), type="n",
       ylab="Density", ...)
  for(i in 1:k) {
    lines(densities[[i]], col=i+1)
  }
  legend("topright", legend=categories,
         text.col=2:(k+1), fill=2:(k+1))
}