CGSplot <- function(x, y, z, colors=c("blue", "red"), refinement=20, zlab="z", ...) {
  # Draw color gradient scatterplot
  gradient <- colorRampPalette(colors)
  breaks <- seq(min(z), max(z), length.out=refinement)
  par(mar=par("mar") + c(0, 0, 0, 6))
  plot(x, y,
       col=gradient(refinement)[findInterval(z, breaks)], ...)
  # Draw legend
  legend_image <- as.raster(matrix(rev(gradient(refinement)), ncol=1))
  xmax <- max(x)
  xwidth <- xmax - min(x)
  ymin <- min(y)
  ywidth <- max(y) - ymin
  par(mar=par("mar") - c(0, 0, 0, 6))
  rasterImage(legend_image, xmax+.12*xwidth, ymin+.1*ywidth,
              xmax+.19*xwidth, ymin+.9*ywidth)
  text(x=xmax+.24*xwidth, y=seq(ymin+.1*ywidth, ymin+.9*ywidth, length.out=5), labels=seq(min(z), max(z), length.out=5))
  text(x=xmax+.19*xwidth, y=ymin+.98*ywidth, labels=zlab)
}