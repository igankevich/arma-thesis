arma.middle_element <- function (x) {
  x[round(length(x) + 0.5)/2]
}

arma.plot_velocity_potential_field <- function (dir, ...) {
  args <- list(...)
  if (!('contour_lwd' %in% names(args))) {
    args$contour_lwd = 1
  }
  if (!('zeta_lwd' %in% names(args))) {
    args$zeta_lwd = 4
  }
  if (!('sky_col' %in% names(args))) {
    args$sky_col = 'white'
  }
  if (!('axis_args' %in% names(args))) {
    args$axis_args = list()
  }
  if (!('box_args' %in% names(args))) {
    args$box_args = list()
  }
  if (!('x_max' %in% names(args))) {
    args$x_max = 0
  }
  if (!('z_min' %in% names(args))) {
    args$z_min = -8
  }
  if (!('points_args' %in% names(args))) {
    args$points_args = list(pch=21,col='black', bg='white')
  }
  if (!('title_args' %in% names(args))) {
    args$title_args = list(xlab="x", ylab="z")
  }
  phi <- read.csv(file.path(dir, 'phi.csv'))
  left_top_x <- 0.1
  right_top_x <- max(phi$x)
  # slice time and Y ranges through the center
  slice_t <- arma.middle_element(unique(phi$t))
  slice_y <- arma.middle_element(unique(phi$y))
  print(paste('Middle elements (TY) = ', slice_t, slice_y))
  phi_slice <- phi[phi$t == slice_t & phi$y == slice_y & phi$x >= left_top_x & phi$z >= args$z_min,]
  x <- unique(phi_slice$x)
  z <- unique(phi_slice$z)
  left_top_z <- max(phi_slice$z)
  right_top_z <- left_top_z
  print(paste('Velocity field size (XZ) = ', length(x), length(z)))

  # convert data frame to matrix
  seq_x <- seq_along(x)
  seq_z <- seq_along(z)
  indices <- data.matrix(expand.grid(seq_x, seq_z))
  u <- with(phi_slice, {
    out <- matrix(nrow=length(seq_x), ncol=length(seq_z))
    out[indices] <- phi
    out
  })

  # get wave profile
  zeta <- read.csv(file.path(dir, 'zeta.csv'))
  zeta_slice <- zeta[zeta$t == slice_t & zeta$y == slice_y & zeta$x >= left_top_x,]

  plot.new()
  plot.window(xlim=range(c(x,args$x_max)),ylim=range(z),asp=1)
  do.call(axis, c(list(side=1), args$axis_args))
  do.call(axis, c(list(side=2), args$axis_args))

  .filled.contour(
    x, z, u,
    levels=args$levels,
    col=args$col
  )


  contour(
    x, z, u,
    levels=args$levels,
    asp=1,
    drawlabels=TRUE,
    add=TRUE,
    lwd=args$contour_lwd
  )

  top_area_x <- c(left_top_x*0.99, zeta_slice$x, right_top_x*1.01)
  top_area_z <- c(left_top_z*1.10, zeta_slice$z, right_top_z*1.10)
  polygon(
    top_area_x,
    top_area_z,
    lwd=args$zeta_lwd,
    border=args$sky_col,
    col=args$sky_col
  )
  lines(zeta_slice$x, zeta_slice$z, lwd=args$zeta_lwd)

  # plot point having larger values than in the "compare_to" test
  if ("compare_to" %in% names(args)) {
    dir0 <- args$compare_to
    phi0 <- read.csv(file.path(dir0, 'phi.csv'))
    phi0_slice <- phi0[phi0$t == slice_t & phi0$y == slice_y & phi0$x >= left_top_x & phi0$z >= args$z_min,]
    phi0_range <- range(phi0_slice[phi0_slice$z <= zeta_slice$z, "phi"])
    large_phi <- phi_slice[phi_slice$z <= zeta_slice$z & (phi_slice$phi < phi0_range[[1]] | phi_slice$phi > phi0_range[[2]]),]
    do.call(points, c(list(x=large_phi$x, y=large_phi$z), args$points_args))
  }

  do.call(title, args$title_args)
}

arma.plot_velocity_potential_field_legend <- function (...) {
  args <- list(...)
  levels <- args$levels
  col <- args$col
  plot.new()
  plot.window(
    xlim = c(0, 1),
    ylim = range(levels),
    xaxs = "i",
    yaxs = "i"
  )
  rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
  axis(4)
  box()
}

arma.plot_velocity <- function(file1, file2, ...) {
  args <- list(...)
  data1 <- read.table(file1)
  data2 <- read.table(file2)
  ylim <- range(c(data1, data2))
  if ("ylim" %in% names(args)) {
    ylim <- args$ylim
  }
  if (!("legend_x" %in% names(args))) {
    args$legend_x <- "topleft"
  }
  if (!("legend_cex" %in% names(args))) {
    args$legend_cex <- par("cex")
  }
  if (!('axis_args' %in% names(args))) {
    args$axis_args = list()
  }
  if (!('title_args' %in% names(args))) {
    args$title_args = list(xlab="x",ylab="u(x)")
  }
  plot.new()
  plot.window(
    xlim = c(0, nrow(data1)-1),
    ylim = ylim
  )
  lines(data1, lty=args$linetypes[[1]])
  lines(data2, lty=args$linetypes[[2]])
  do.call(axis, c(list(side=1), args$axis_args))
  do.call(axis, c(list(side=2), args$axis_args))
  box()
  do.call(title, args$title_args)
  legend(
    args$legend_x,
    c(
      as.expression(bquote(u[1](x))),
      as.expression(bquote(u[2](x)))
    ),
    lty = paste(args$linetypes),
    cex = args$legend_cex
  )
}
