arma.middle_element <- function (x) {
  x[round(length(x) + 0.5)*2/3]
}

arma.read_zeta_slice <- function (filename) {
  # slice time and Y ranges through the center
  zeta <- read.csv(filename)
  slice_t <- arma.middle_element(unique(zeta$t))
  slice_y <- arma.middle_element(unique(zeta$y))

  # get wave profile
  slice_y <- arma.middle_element(unique(zeta$y))
  print(paste('Middle elements of zeta (TY) = ', slice_t, slice_y))
  zeta[zeta$t == slice_t & zeta$y == slice_y,]
}

arma.plot_nonlinear <- function (dirname, args) {

  zeta_linear <- arma.read_zeta_slice(file.path(dirname, 'zeta-linear.csv'))
  zeta_nonlinear <- arma.read_zeta_slice(file.path(dirname, 'zeta-nonlinear.csv'))

  x <- unique(zeta_linear$x)
  z <- unique(zeta_linear$z)

  # plot the graph
  rx <- range(x)
  rz <- range(z)
  aspect_ratio <- 1
  plot.new()
  plot.window(xlim=rx, ylim=rz, asp=1)
  axis(1)
  axis(2)
  lines(zeta_linear$x, zeta_linear$z, lty='dashed')
  lines(zeta_nonlinear$x, zeta_nonlinear$z, lty='solid')
  title(args$title, xlab="x", ylab="z", line=-1.5)
  box()
  legend(
    "bottomright",
    legend=paste(args$graphs),
    lty=paste(args$linetypes)
  )
}
