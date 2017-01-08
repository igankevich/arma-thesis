source(file.path("R", "waves.R"))

arma.qqplot_grid <- function (dir, params) {
	wave_params <- arma.load_wave_parameters(dir, params)
	for (name in names(wave_params)) {
		arma.qqplot(wave_params[[name]])
	}
}
