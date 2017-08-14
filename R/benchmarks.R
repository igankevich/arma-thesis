source(file.path("build", "arma-benchmarks", "R", "arma.load.R"))

arma.load_benchmark_data <- function(attempt, framework, models, tags) {
	data <- data.frame()
	for (m in models) {
		if (!(m %in% colnames(data))) {
			data[,m] <- rep(NA, nrow(data))
		}
		idx <- 1
		for (t in tags) {
			values <- arma.load(
				file.path("build", "arma-benchmarks", "output"),
				"gpulab1",
        attempt,
				10000,
				framework,
				m,
				t,
				".*\\s+([0-9]+)us.*"
			)
			name <- names(tags)[idx]
			data[if (nchar(name) == 0) t else name,m] <- mean(values/1000/1000)
			idx <- idx + 1
		}
	}
  data
}

arma.print_openmp_vs_opencl <- function(model_names, row_names) {
  library(ascii)
  options(asciiType="org")
  models <- c("ar", "ma", "lh");
  frameworks <- c("openmp", "opencl")
  tags <- list(
    determine_coefficients=c("deteremine_coefficients", "determine_coefficients"),
    "validate",
    "generate_surface",
    nit=c("nit_acf", "nit_realisation"),
    "copy_to_host",
    velocity=c("window_function", "second_function", "fft", "dev_to_host_copy"),
    "write_all"
  )
  all_data <- list()
  for (framework in frameworks) {
    all_data[[framework]] <- arma.load_benchmark_data("a4", framework, models, tags)
  }
  # translate and pretty print in org-mode format
  saved_row_names <- rownames(all_data$openmp)
  for (framework in names(all_data)) {
    rownames(all_data[[framework]]) <- 1:nrow(all_data[[framework]])
  }
  big_table <- merge(all_data$openmp, all_data$opencl, by="row.names")
  rownames(big_table) <- saved_row_names
  big_table[,"Row.names"] <- saved_row_names
  big_table <- big_table[,!(names(big_table) == "ma.y")]
  big_table[,"Row.names"] <- sapply(big_table[,"Row.names"], function (c) get(c, row_names))
  colnames(big_table) <- sapply(colnames(big_table), function (c) get(c, model_names))
#  rownames(big_table) <- sapply(rownames(big_table), function (c) paste("~", c, "~", sep=""))
  print(ascii(c("", "", "", "OpenMP", "", "OpenCL")))
  print(ascii(big_table, include.rownames=FALSE))
}

arma.load_io_benchmark_data <- function(attempt, filesystems, suffix, tags) {
	data <- data.frame()
	for (fs in filesystems) {
		if (!(fs %in% colnames(data))) {
			data[,fs] <- rep(NA, nrow(data))
		}
		idx <- 1
		for (t in tags) {
			values <- arma.load(
				file.path("build", "arma-benchmarks", "output"),
				"gpulab1",
        paste(attempt, fs, suffix, sep="-"),
				10000,
				"openmp",
				"ar",
				t,
				".*\\s+([0-9]+)us.*"
			)
			name <- names(tags)[idx]
			data[if (length(name) == 0 || nchar(name) == 0) t else name,fs] <- mean(values/1000/1000)
			idx <- idx + 1
		}
	}
  data
}

arma.print_sync_vs_async_io <- function(suffix_names, row_names, top_names) {
  library(ascii)
  options(asciiType="org")
  tags <- list("generate_surface", "write_all")
  filesystems <- c("xfs", "nfs", "gfs")
  all_data <- list()
  for (suffix in c("seq", "par")) {
    all_data[[suffix]] <- arma.load_io_benchmark_data("a5", filesystems, suffix, tags)
  }
  # translate and pretty print in org-mode format
  saved_row_names <- rownames(all_data$seq)
  for (suffix in names(all_data)) {
    rownames(all_data[[suffix]]) <- 1:nrow(all_data[[suffix]])
  }
  big_table <- merge(all_data$seq, all_data$par, by="row.names")
  rownames(big_table) <- saved_row_names
  big_table[,"Row.names"] <- saved_row_names
  big_table[,"Row.names"] <- sapply(big_table[,"Row.names"], function (c) get(c, row_names))
  colnames(big_table) <- sapply(colnames(big_table), function (c) get(c, suffix_names))
  print(ascii(c("", "", "", top_names[[1]], "", "",  top_names[[2]])))
  print(ascii(big_table, include.rownames=FALSE))
}
