source(file.path("R", "waves.R"))

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
