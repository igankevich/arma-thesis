#!/usr/bin/Rscript

arma.bits.qweibull <- function (x, param, sh) {
	sh <- 2.0
	sc = param$mean / gamma(1.0 + 1.0 / sh)
	qweibull(x, shape=sh, scale=sc)
}
arma.bits.qnorm <- function (x, param) {
	qnorm(x, mean=param$mean, sd=param$sd)
}

arma.QFUNCTIONS <- list(
	elevation = arma.bits.qnorm,
  heights_x = function (x, param) { arma.bits.qweibull(x, param, 2.0) },
	heights_y = function (x, param) { arma.bits.qweibull(x, param, 2.0) },
	lengths_x = function (x, param) { arma.bits.qweibull(x, param, 2.3) },
	lengths_y = function (x, param) { arma.bits.qweibull(x, param, 2.3) },
	periods = function (x, param) { arma.bits.qweibull(x, param, 3.0) }
)

arma.ALL_WAVE_CHARACTERISTICS <- list(
	"elevation",
	"heights_x",
	"heights_y",
	"lengths_x",
	"lengths_y",
	"periods"
)

arma.load_wave_parameters <- function (
	dir = "output",
	wave_params = arma.ALL_WAVE_CHARACTERISTICS,
	qfuncs = arma.QFUNCTIONS
) {
	sapply(wave_params, function (filename) {
		param <- list()
		param$filename = filename
		param$data <- read.table(file.path(dir, filename))[[1]]
		param$mean <- mean(param$data)
		param$sd <- sd(param$data)
		param$qfunc <- function (x) { qfuncs[[filename]](x, param) }
		param$min <- param$qfunc(.01)
		param$max <- param$qfunc(.99)
		elem <- list()
		elem[[filename]] <- param
		elem
	})
}

arma.qqplot <- function (param, nsamples=100, ttl, ...) {
  qdata <- param$qfunc(ppoints(nsamples))
	qqplot(
		qdata,
		param$data,
		asp=1,
		xlim=c(param$min, param$max),
		ylim=c(param$min, param$max),
		...
  )
  if (class(ttl) == 'character') {
	title(ttl, line=-2)
  } else {
	title(ttl$title, line=-1, adj=ttl$adjust)
  }
  qqline(param$data, distribution=param$qfunc)
}

#wave_params <- arma.load_wave_parameters()
#par(pty="s", mfrow=c(3, 3))
#for (name in names(wave_params)) {
#	print(name)
#	arma.qqplot(wave_params[[name]])
#}
