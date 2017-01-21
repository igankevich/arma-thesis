source(file.path("R", "waves.R"))
source(file.path("R", "transform.R"))

arma.qqplot_grid <- function (dir, params, titles, ...) {
  wave_params <- arma.load_wave_parameters(dir, params)
  i <- 1
	for (name in names(wave_params)) {
    arma.qqplot(wave_params[[name]], 100, titles[[i]], ...)
    i <- i + 1
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
  title(xlab="x", ylab="y")
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
  title(xlab="x", ylab="y")
}

arma.fmt <- function(x, ndigits) {
  format(round(x, ndigits), nsmall=ndigits)
}

arma.plot_partitions <- function() {
	library("rgl")
	part_alpha <- 0.2
	part_col <- "grey"
	part_size <- 2
	sc <- 0.8
	off <- part_size * (1-sc)/2
	bcol <- "grey"
	balpha <- 1.0
	sc2 <- 1.0 - sc
	off2 <- part_size * sc/2
	ccol <- "red"
	calpha <- 1.0
	# whole parts
	a1 <- cube3d(color=part_col, alpha=part_alpha)
	a2 <- cube3d(color=part_col, alpha=part_alpha)
	a3 <- cube3d(color=part_col, alpha=part_alpha)
	shade3d(translate3d(a1, 0*part_size, 0, 0))
	shade3d(translate3d(a2, 1*part_size, 0, 0))
	shade3d(translate3d(a3, 2*part_size, 0, 0))
	# stripped parts
	b1 <- scale3d(cube3d(color=bcol, alpha=balpha), sc, sc, sc)
	b2 <- scale3d(cube3d(color=bcol, alpha=balpha), sc, sc, sc)
	b3 <- scale3d(cube3d(color=bcol, alpha=balpha), sc, sc, sc)
	shade3d(translate3d(b1, 0 + off, off, off))
	shade3d(translate3d(b2, 2 + off, off, off))
	shade3d(translate3d(b3, 4 + off, off, off))
	# overlap intervals
	c1 <- scale3d(cube3d(color=ccol, alpha=calpha), sc2, 1, 1)
	c2 <- scale3d(cube3d(color=ccol, alpha=calpha), sc2, 1, 1)
	c3 <- scale3d(cube3d(color=ccol, alpha=calpha), sc2, 1, 1)
	shade3d(translate3d(c1, 0 + off2, 0, 0))
	shade3d(translate3d(c2, 2 + off2, 0, 0))
	shade3d(translate3d(c3, 4 + off2, 0, 0))
}

arma.plot_ramp_up_interval <- function(label="Ramp-up interval") {
	zeta <- read.csv(file.path("build", "standing_wave", "zeta.csv"))
	t <- round(mean(zeta$t))
	res <- arma.wavy_plot(zeta, t, scale=FALSE)
	library("grDevices")
	ax <- 7
	ay <- 7
	my <- max(zeta$y)
	lines(trans3d(
		c(0,  0, ax, ax, 0),
		c(0, my, my,  0, 0),
		c(0,  0,  0,  0, 0),
		pmat=res
	), col="red", lwd=3) 
	text(trans3d(0, my/2, max(zeta$z)*1.5, pmat=res), label, col="red", font=2) 
	from <- trans3d(0, my/2, max(zeta$z)*1.4, pmat=res)
	to <- trans3d(0, my/2, max(zeta$z)*0.05, pmat=res)
	arrows(from$x, from$y, to$x, to$y, lwd=2, angle=10, length=0.1, col="red")
}

arma.plot_factory_vs_openmp <- function(...) {
  args <- list(...)
  perf <- read.csv(file.path("data", "performance", "factory-vs-openmp.csv"))
  scale <- 10 ** args$power
  x <- perf$nt * perf$nx * perf$ny / scale
  plot.new()
  plot.window(xlim=range(x),ylim=range(perf[c("openmp", "factory")]))
  pts <- pretty(x)
  axis(1, at=pts, labels=sapply(pts, function(x) {as.expression(bquote(.(x) %.% 10 ** .(args$power)))}))
  axis(2)
  box()
  lines(x, perf$openmp, lty="solid")
  lines(x, perf$factory, lty="dashed")
  title(xlab=args$xlab, ylab=args$ylab)
}

arma.plot_factory_vs_openmp_overlap <- function(...) {
  args <- list(...)
  openmp <- read.csv(file.path("data", "performance", "overlap-openmp.csv"), na.strings="")
  factory <- read.csv(file.path("data", "performance", "overlap-factory.csv"), na.strings="")
  openmp$t <- (openmp$t - min(openmp$t)) / args$scale
  factory$t <- (factory$t - min(factory$t)) / args$scale
  plot.new()
  plot.window(xlim=range(c(factory$t, openmp$t)),ylim=range(0, 5))
  axis(1)
  axis(2, at=c(1, 3), labels=args$labels, las=1, hadj=1)
  # OpenMP
  lines(openmp$t, rep.int(3, length(openmp$t)))
  openmp_pts <- openmp[!is.na(openmp$mark),]
  openmp_y <- rep.int(3, length(openmp_pts$t))
  points(openmp_pts$t, openmp_y)
  text(openmp_pts$t, openmp_y, labels=openmp_pts$mark, pos=c(3, 3, 1, 1))
  # Factory
  lines(factory$t, rep.int(1, length(factory$t)))
  factory_pts <- factory[!is.na(factory$mark),]
  factory_y <- rep.int(1, length(factory_pts$t))
  points(factory_pts$t, factory_y)
  text(factory_pts$t, factory_y, labels=factory_pts$mark, pos=c(3, 1, 3, 1))
  title(xlab=args$xlab)
}
