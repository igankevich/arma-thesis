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

  zeta_none <- arma.read_zeta_slice(file.path(dirname, 'zeta-none.csv'))
  zeta_gcs <- arma.read_zeta_slice(file.path(dirname, 'zeta-gramcharlier.csv'))
  zeta_sn <- arma.read_zeta_slice(file.path(dirname, 'zeta-skewnormal.csv'))

  x <- unique(zeta_none$x)
  z <- unique(zeta_none$z)

  # plot the graph
  rx <- range(x)
  rz <- range(z)
  aspect_ratio <- 1
  plot.new()
  plot.window(xlim=rx, ylim=rz, asp=1)
  axis(1)
  axis(2)
  lines(zeta_none$x, zeta_none$z, lty='solid')
  lines(zeta_gcs$x, zeta_gcs$z, lty='dashed')
  lines(zeta_sn$x, zeta_sn$z, lty='dotted')
  title(args$title, xlab="x", ylab="z", line=-1.5)
  box()
  legend(
    "bottomright",
    legend=paste(args$graphs),
    lty=paste(args$linetypes)
  )
}

arma.wave_height <- function (realisation) {
  sqrt(2*pi*var(realisation))
}

arma.print_wave_height <- function (dirname) {
  zeta_none <- read.csv(file.path(dirname, 'zeta-none.csv'))
  zeta_gcs <- read.csv(file.path(dirname, 'zeta-gramcharlier.csv'))
  zeta_sn <- read.csv(file.path(dirname, 'zeta-skewnormal.csv'))
  library('ascii')
  options(asciiType='org')
  data.frame(
    h1=arma.wave_height(zeta_none$z),
    h2=arma.wave_height(zeta_gcs$z),
    h3=arma.wave_height(zeta_sn$z)
  )
}
