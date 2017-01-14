source(file.path("R", "waves.R"))
source(file.path("R", "transform.R"))

arma.qqplot_grid <- function (dir, params) {
	wave_params <- arma.load_wave_parameters(dir, params)
	for (name in names(wave_params)) {
		arma.qqplot(wave_params[[name]])
	}
}

arma.wavy_plot <- function (data, t, ...) {
	slice <- data[data$t == t,]
	x <- unique(slice$x)
	y <- unique(slice$y)
	z <- with(slice, {
		n <- sqrt(length(z))
		out <- matrix(nrow=n, ncol=n)
		out[cbind(x, y)] <- z
		out
	})
	nrz <- nrow(z)
	ncz <- ncol(z)
	# Create a function interpolating colors in the range of specified colors
	jet.colors <- colorRampPalette( c("blue", "green") )
	# Generate the desired number of colors from this palette
	nbcol <- 100
	color <- jet.colors(nbcol)
	# Compute the z-value at the facet centres
	zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
	# Recode facet z-values into color indices
	facetcol <- cut(zfacet, nbcol)
	persp(x, y, z, phi=30, theta=30, col=color[facetcol], ...)
}

arma.skew_normal_1_plot <- function(x, params) {
  data <- mapply(
    function (s, k) arma.skew_normal_1(x, s, k),
    params$skewness,
    params$kurtosis
  )
  plot.new()
  plot.window(xlim=range(x),ylim=range(data))
  axis(1); axis(2); box()
  for (i in seq_len(ncol(data))) {
    d <- data[,i]
    lines(x, d, lty=paste(params$linetypes[[i]]))
  }
}


arma.skew_normal_2_plot <- function(x, params) {
  data <- mapply(
    function (a) arma.skew_normal_2(x, a),
    params$alpha
  )
  plot.new()
  plot.window(xlim=range(x),ylim=range(data))
  axis(1); axis(2); box()
  for (i in seq_len(ncol(data))) {
    d <- data[,i]
    lines(x, d, lty=paste(params$linetypes[[i]]))
  }
}

arma.fmt <- function(x, ndigits) {
  format(round(x, ndigits), nsmall=ndigits)
}
