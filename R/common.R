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

arma.qqplot_grid_adj <- function (dir, params, titles, adj, ...) {
  wave_params <- arma.load_wave_parameters(dir, params)
  i <- 1
  for (name in names(wave_params)) {
    ttl = list(title=titles[[i]], adjust=adj)
    arma.qqplot(wave_params[[name]], 100, ttl, ...)
    i <- i + 1
  }
}

arma.wavy_plot_matrix <- function (x, y, z, t, ...) {
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

arma.to_matrix <- function (data, t) {
	slice <- data[data$t == t,]
	x <- unique(slice$x)
	y <- unique(slice$y)
	z <- with(slice, {
		n <- sqrt(length(z))
		out <- matrix(nrow=n, ncol=n)
		out[cbind(x, y)] <- z
		out
	})
	list(x=x,y=y,z=z)
}

arma.wavy_plot <- function (data, t, ...) {
	with(arma.to_matrix(data, t), arma.wavy_plot_matrix(x, y, z, ...))
}

arma.acf_plot <- function (data, t, ...) {
	ifft <- function (arr) { fft(arr, inverse=TRUE) }
	with(
		arma.to_matrix(data, t),
		function () {
			# compute ACF using Wiener---Khinchin theorem
			n <- nrow(z)*ncol(z)
			z <- Re(ifft(abs(fft(z))^2))/n/n
			# slice 1/4th of the matrix
			n1 <- floor(nrow(z)/2)
			n2 <- floor(ncol(z)/2)
			x <- x[1:n1]
			y <- y[1:n2]
			z <- z[1:n1,1:n2]
			arma.wavy_plot_matrix(x, y, z, ...)
		}
	)()
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
	zeta <- read.csv(file.path("build", "arma-benchmarks", "verification-orig", "standing_wave", "zeta.csv"))
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

arma.cube <- function (x, y, z) {
  c <- cube3d(alpha=0.2)
  c$material$lwd <- 4
#  c$material$front <- 'line'
  c$material$back <- 'line'
  c$material$col <- '#ffffff'
  shade3d(translate3d(c, x, y, z))
}

arma.arrow <- function (x1,x2,ps) {
  arrow3d(
    c(x1[1], x1[2], x1[3])*ps,
    c(x2[1], x2[2], x2[3])*ps,
    type="rotation", col="grey", s=1/7)
}

arma.plot_ar_cubes <- function(nx, ny, nz) {
  library("rgl")
  part_size <- 2
  # generate cubes
#  for (i in c(1:nx)) {
#    for (j in c(1:ny)) {
#      for (k in c(1:nz)) {
#        arma.cube(i*part_size, j*part_size, k*part_size)
#      }
#    }
#  }
  # generate arrows
  for (i in c(1:nx)) {
    for (j in c(1:ny)) {
      for (k in c(1:nz)) {
        m1 <- min(i+1, nx)
        m2 <- min(j+1, ny)
        m3 <- min(k+1, nz)
        for (l in c(i:m1)) {
          for (m in c(j:m2)) {
            for (n in c(k:m3)) {
              v1 <- c(i,j,k)
              v2 <- c(l,m,n)
              if (!(i==l && j==m && k==n)) {
                arma.arrow(v1, v2, part_size)
              }
            }
          }
        }
      }
    }
  }
  # rotate the camera
  view3d(45,30)
}

arma.plot_ar_cubes_2d_v2 <- function (nx, ny, xlabel, ylabel, args) {
  if (!('arrow_args' %in% names(args))) {
    args$arrow_args <- list(lwd=3,angle=7)
  }
  if (!('adj_x' %in% names(args))) {
    args$adj_x <- 0.3
  }
  if (!('adj_y' %in% names(args))) {
    args$adj_y <- 0.4
  }
  part_size <- 2
  plot.new()
  plot.window(xlim=c(0,nx*part_size),ylim=rev(c(0,ny*part_size)))
  par(pty="s")
  char_idx <- 1
  adj_x <- args$adj_x
  adj_y <- args$adj_y
  for (i in c(1:nx)-1) {
    for (j in c(1:ny)-1) {
      x0 <- i*part_size
      y0 <- j*part_size
      x1 <- x0 + part_size
      y1 <- y0 + part_size
      rect(x0, y0, x1, y1)
      text(x0+part_size/2, y0+part_size/2, LETTERS[char_idx], cex=1.5)
      m1 <- max(i-1, 0)
      m2 <- max(j-1, 0)
      # draw arrows denoting AR dependencies
      for (l in c(m1:i)) {
        for (m in c(m2:j)) {
          if (!(l==i && m==j)) {
            xl <- l*part_size
            ym <- m*part_size
            x00 <- x0
            y00 <- y0
            if (xl != x0) {
              xl <- xl + adj_x
              x00 <- x00 - adj_x
            }
            if (ym != y0) {
              ym <- ym + adj_y
              y00 <- y00 - adj_y
            }
            do.call(arrows, c(list(
              x0=x00 + part_size/2,
              y0=y00 + part_size/2,
              x1=xl + part_size/2,
              y1=ym + part_size/2),
              args$arrow_args))
          }
        }
      }
      char_idx <- char_idx + 1
    }
  }
  if (!('no_axes' %in% names(args)) | !args$no_axes) {
    axis(3, at=c(0:nx)*part_size, labels=c(0:nx))
    axis(2, at=c(0:ny)*part_size, labels=c(0:ny))
    mtext(xlabel, side=3, line=3)
    mtext(ylabel, side=2, line=3)
  }
}

arma.plot_ar_cubes_2d <- function (nx, ny, xlabel, ylabel) {
  arma.plot_ar_cubes_2d_v2(nx, ny, xlabel, ylabel, list())
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

# a workaround for a bug in ascii package
# which does not honour "decimal.mark" argument
arma.print_ascii <- function (obj) {
	decimal.mark <- options("arma.mark")
	if (is.null(decimal.mark)) {
		decimal.mark <- "."
	}
	replacement <- paste('\\1', decimal.mark, '\\2', sep='')
	cat(gsub('([0-9]+)\\.([0-9]+)', replacement, capture.output(obj$show.org())), sep="\n")
}
