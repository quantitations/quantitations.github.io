interval.exponential <- function(a, b) {
  
  # Draw the standard Exponential pdf from 0 to 10
  grid <- seq(0, 10, length.out=100)
  pdf <- sapply(grid, dexp)
  plot(grid, pdf, type="l", col=4, xlab="x", ylab="density",
       main="The standard Exponential pdf")
  abline(h=0, lty=2)
  
  # Shade the region from a to b
  grid <- seq(a, b, by=.01)
  pdf <- sapply(grid, dexp)
  points(grid, pdf, type="h", col=2)
  
  # Find the probability that X is in [a, b]
  p <- pexp(b) - pexp(a)
  say <- paste("P(X is in [", a, ",", b, "]) is about", round(p, 2))
  text(3.5, .4, say)
  
  return(p)
}

interval.exponential(1, 3)